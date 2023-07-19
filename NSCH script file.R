# Minimum wage and children's mental health
# Analyses using the National Survey of Children's Health
# N.M. Kavanagh, M. McConnell, N. Slopen
# July 17, 2023

# Please direct questions about this script file to nolankavanagh@fas.harvard.edu.

# Clear R environment
rm(list = ls())

# Load packages
library(here)         # Working directory
library(readstata13)  # Dataset tools
library(dplyr)        # Analysis tools
library(psych)        # Analysis tools
library(modelbased)   # Modeling tools
library(lfe)          # Modeling tools
library(survey)       # Survey tools
library(ggplot2)      # Graphing tools
library(usmap)        # Graphing tools
library(viridis)      # Graphing tools
library(cowplot)      # Graphing tools
library(scales)       # Graphing tools
library(arsenal)      # Table tools
library(modelsummary) # Table tools
library(kableExtra)   # Table tools
library(cdlTools)     # FIPS tools

##############################################################################
# Dataset preparation
##############################################################################

# Read NSCH datasets into R
nsch_2016_top <- read.dta13("NSCH data/nsch_2016_topical.dta")
nsch_2017_top <- read.dta13("NSCH data/nsch_2017_topical.dta")
nsch_2018_top <- read.dta13("NSCH data/nsch_2018_topical.dta")
nsch_2019_top <- read.dta13("NSCH data/nsch_2019_topical.dta")
nsch_2020_top <- read.dta13("NSCH data/nsch_2020_topical.dta")
nsch_2021_top <- read.dta13("NSCH data/nsch_2021_topical.dta")

# Treat stratum as character
# Necessary for a smooth merge
nsch_2016_top$stratum <- as.character(nsch_2016_top$stratum)

# Bind all survey years together
nsch <- bind_rows(nsch_2016_top, nsch_2017_top, nsch_2018_top,
                      nsch_2019_top, nsch_2020_top, nsch_2021_top)

# Redefine strata
# Recommended by the NSCH for multi-year analyses
nsch <- nsch %>% mutate(
  strata = case_when(
    stratum %in% c("1")       ~ "1",
    stratum %in% c("2", "2A") ~ "2"
  ))

# Read minimum wage dataset into R
min_wage_df <- read.csv("https://raw.githubusercontent.com/Lislejoem/Minimum-Wage-by-State-1968-to-2020/master/Minimum%20Wage%20Data.csv")

# Convert state names to FIPS
# Necessary for a smooth merge
min_wage_df$fipsst <- cdlTools::fips(min_wage_df$State, to="FIPS")

# Generate new year variable
min_wage_df$year <- min_wage_df$Year

# Reorder dataset
min_wage_df <- min_wage_df %>% arrange(fipsst, year)

# Generate lagged minimum wages
min_wage_df <- min_wage_df %>%
  group_by(fipsst) %>%
  mutate(
    # Not inflation adjusted
    lag_by_1 = lag(Effective.Minimum.Wage, n=1, default=NA),
    
    # Inflation adjusted
    lag_by_1_inf  = lag(Effective.Minimum.Wage.2020.Dollars, n=1,  default=NA),
    lag_by_2_inf  = lag(Effective.Minimum.Wage.2020.Dollars, n=2,  default=NA),
    lag_by_3_inf  = lag(Effective.Minimum.Wage.2020.Dollars, n=3,  default=NA),
    lag_by_4_inf  = lag(Effective.Minimum.Wage.2020.Dollars, n=4,  default=NA),
    lag_by_5_inf  = lag(Effective.Minimum.Wage.2020.Dollars, n=5,  default=NA),
    lag_by_6_inf  = lag(Effective.Minimum.Wage.2020.Dollars, n=6,  default=NA),
    lag_by_7_inf  = lag(Effective.Minimum.Wage.2020.Dollars, n=7,  default=NA),
    lag_by_8_inf  = lag(Effective.Minimum.Wage.2020.Dollars, n=8,  default=NA),
    lag_by_9_inf  = lag(Effective.Minimum.Wage.2020.Dollars, n=9,  default=NA),
    lag_by_10_inf = lag(Effective.Minimum.Wage.2020.Dollars, n=10, default=NA),
    lag_by_11_inf = lag(Effective.Minimum.Wage.2020.Dollars, n=11, default=NA),
    lag_by_12_inf = lag(Effective.Minimum.Wage.2020.Dollars, n=12, default=NA),
    lag_by_13_inf = lag(Effective.Minimum.Wage.2020.Dollars, n=13, default=NA),
    lag_by_14_inf = lag(Effective.Minimum.Wage.2020.Dollars, n=14, default=NA),
    lag_by_15_inf = lag(Effective.Minimum.Wage.2020.Dollars, n=15, default=NA),
    lag_by_16_inf = lag(Effective.Minimum.Wage.2020.Dollars, n=16, default=NA),
    lag_by_17_inf = lag(Effective.Minimum.Wage.2020.Dollars, n=17, default=NA)
    )

# Merge minimum wage and NSCH data
nsch_all <- left_join(nsch, min_wage_df, by=c("fipsst", "year"))

# Clear old datasets from R
rm(nsch_2016_top, nsch_2017_top, nsch_2018_top,
   nsch_2019_top, nsch_2020_top, nsch_2021_top)

##############################################################################
# Supplemental Medicaid dataset preparation
##############################################################################

# Read Medicaid datasets into R
medicaid_1_5_df  <- read.csv("Supplemental data/medicaid_1_5_state_year.csv")
medicaid_6_18_df <- read.csv("Supplemental data/medicaid_6_18_state_year.csv")

# Add columns for 2001 and 2007
# Duplicate data from 2000 and 2006
medicaid_1_5_df$X2001  <- medicaid_1_5_df$X2000
medicaid_1_5_df$X2007  <- medicaid_1_5_df$X2006
medicaid_6_18_df$X2001 <- medicaid_6_18_df$X2000
medicaid_6_18_df$X2007 <- medicaid_6_18_df$X2006

# Restructure data to long format
library(reshape2)
medicaid_1_5_df  <- melt(medicaid_1_5_df, id.vars = c("X"),
                         variable.name = "year", value.name = "elig_1_5")
medicaid_6_18_df <- melt(medicaid_6_18_df, id.vars = c("X"),
                         variable.name = "year", value.name = "elig_6_18")

# Clean year variable
# Drop leading "X" in strings
medicaid_1_5_df$year  <- substring(medicaid_1_5_df$year,  2)
medicaid_6_18_df$year <- substring(medicaid_6_18_df$year, 2)

# Rename columns
colnames(medicaid_1_5_df)  <- c("state", "year", "elig_1_5")
colnames(medicaid_6_18_df) <- c("state", "year", "elig_6_18")

# Convert names to FIPS
# Necessary for smooth merge
medicaid_1_5_df$fipsst  <- cdlTools::fips(medicaid_1_5_df$state,  to="FIPS")
medicaid_6_18_df$fipsst <- cdlTools::fips(medicaid_6_18_df$state, to="FIPS")

# Treat year as numeric
medicaid_1_5_df$year  <- as.numeric(medicaid_1_5_df$year)
medicaid_6_18_df$year <- as.numeric(medicaid_6_18_df$year)

# Merge Medicaid and YRBS data
nsch_all <- left_join(nsch_all, medicaid_1_5_df,  by=c("fipsst", "year"))
nsch_all <- left_join(nsch_all, medicaid_6_18_df, by=c("fipsst", "year"))

##############################################################################
# Supplemental EITC dataset preparation
##############################################################################

# Read EITC dataset into R
eitc <- read.csv("Supplemental data/eitc_state_year.csv")

# Merge EITC and YRBS data
nsch_all <- left_join(nsch_all, eitc, by=c("fipsst", "year"))

##############################################################################
# Supplemental TANF dataset preparation
##############################################################################

# Read TANF dataset into R
tanf <- read.csv("Supplemental data/tanf_state_year.csv")

# Merge TANF and YRBS data
nsch_all <- left_join(nsch_all, tanf, by=c("fipsst", "year"))

##############################################################################
# Variable preparation
##############################################################################

# Treat fixed effects as factors
nsch_all$year   <- as.factor(nsch_all$year)
nsch_all$fipsst <- as.factor(nsch_all$fipsst)

# Child's age
nsch_all <- nsch_all %>% mutate(
  age = case_when(
    sc_age_years %in% c(0:17) ~ sc_age_years
  ))

# Treat age as factor
nsch_all$age <- as.factor(nsch_all$age)

# Dichotomize age for interaction models
nsch_all <- nsch_all %>% mutate(
  age_cat = case_when(
    sc_age_years %in% c(13:17) ~ 0, # Adolescents
    sc_age_years %in% c(0:12)  ~ 1  # All other children
  ))

# Child's sex
nsch_all <- nsch_all %>% mutate(
  sex = case_when(
    sc_sex == 1 ~ "Male",
    sc_sex == 2 ~ "Female"
  ))
nsch_all$sex <- factor(nsch_all$sex, levels = c("Male", "Female"))

# Child's race/ethnicity
nsch_all <- nsch_all %>% mutate(
  race_eth = case_when(
    sc_hispanic_r == 1    ~ "Hispanic/Latino",
    sc_race_r == 1        ~ "White, non-Hispanic/Latino",
    sc_race_r == 2        ~ "Black, non-Hispanic/Latino",
    sc_race_r == 3        ~ "American Indian or Alaska Native",
    sc_race_r %in% c(4:5) ~ "Asian, Native Hawaiian, or Pacific Islander",
    sc_race_r %in% c(6:7) ~ "Other or mixed race",
  ))
nsch_all$race_eth <- factor(nsch_all$race_eth, levels = c("White, non-Hispanic/Latino", "Black, non-Hispanic/Latino", "Hispanic/Latino", "American Indian or Alaska Native", "Asian, Native Hawaiian, or Pacific Islander", "Other or mixed race"))

# Dichotomoize race/ethnicity
# Black/Latino vs. other for interaction models
nsch_all <- nsch_all %>% mutate(
  race_eth_cat = case_when(
    sc_hispanic_r == 1      ~ 0, # Black or Hispanic/Latino
    sc_race_r == 2          ~ 0, # Black or Hispanic/Latino
    sc_race_r %in% c(1,3:7) ~ 1  # All other races
  ))

# Adults' highest educational attainment
nsch_all <- nsch_all %>% mutate(
  adult_edu = case_when(
    higrade_tvis == 1 ~ "Less than high school",
    higrade_tvis == 2 ~ "High school (including vocational)",
    higrade_tvis == 3 ~ "Some college or associate degree",
    higrade_tvis == 4 ~ "College degree or higher"
  ))
nsch_all$adult_edu <- factor(nsch_all$adult_edu, levels = c("Less than high school", "High school (including vocational)", "Some college or associate degree", "College degree or higher"))

# Dichotomize educational attainment
# High school (or less) vs. some college (or more)
# Adults' highest educational attainment
nsch_all <- nsch_all %>% mutate(
  adult_edu_cat = case_when(
    higrade_tvis %in% c(1:2) ~ 0, # High school or less
    higrade_tvis %in% c(3:4) ~ 1  # All other education levels
  ))

# Generate mean estimated FPL
# Later NSCH years generated 6 imputed FPLs if a household was missing income
nsch_all$fpl_mean <- rowMeans(cbind(nsch_all$fpl_i1, nsch_all$fpl_i2, nsch_all$fpl_i3,
                                  nsch_all$fpl_i4, nsch_all$fpl_i5, nsch_all$fpl_i6), na.rm=T)

# Household's federal poverty level
nsch_all <- nsch_all %>% mutate(
  fpl_category = case_when(
    fpl %in% c(50:99)   | fpl_mean < 100 ~ "Less than 100%",
    fpl %in% c(100:199) | fpl_mean < 200 ~ "100% to 199%",
    fpl %in% c(200:299) | fpl_mean < 300 ~ "200% to 299%",
    fpl %in% c(300:399) | fpl_mean < 400 ~ "300% to 399%",
    fpl %in% c(400:999) | fpl_mean < 999 ~ "400% or greater"
  ))
nsch_all$fpl_category <- factor(nsch_all$fpl_category, levels = c("Less than 100%", "100% to 199%", "200% to 299%", "300% to 399%", "400% or greater"))

# Dichotomize FPL
# Low-income (<200% FPL) vs. higher-income
nsch_all <- nsch_all %>% mutate(
  low_income = case_when(
    fpl %in% c(50:199)  | fpl_mean < 200 ~ 0, # Lower income
    fpl %in% c(200:999) | fpl_mean < 999 ~ 1  # Higher income
  ))

# Family structure
nsch_all <- nsch_all %>% mutate(
  family_struc = case_when(
    family %in% c(1,3)  | family_r %in% c(1,3)  ~ "Two parents, married",
    family %in% c(2,4)  | family_r %in% c(2,4)  ~ "Two parents, not married",
    family %in% c(5:8)  | family_r %in% c(5:6)  ~ "Single parent",
    family %in% c(1:99) | family_r %in% c(1:99) ~ "Another family structure"
  ))
nsch_all$family_struc <- factor(nsch_all$family_struc, levels = c("Two parents, married", "Two parents, not married", "Single parent", "Another family structure"))

# Household nativity
nsch_all <- nsch_all %>% mutate(
  nativity = case_when(
    house_gen == 1 ~ "First-generation household",
    house_gen == 2 ~ "Second-generation household",
    house_gen == 3 ~ "Third-generation household or higher"
  ))
nsch_all$nativity <- factor(nsch_all$nativity, levels = c("First-generation household", "Second-generation household", "Third-generation household or higher"))

# Dichotomize household nativity
# First/second-generation vs. higher
nsch_all <- nsch_all %>% mutate(
  nativity_cat = case_when(
    house_gen %in% c(1:2) ~ 0,
    house_gen %in% c(3)   ~ 1
  ))

# Current dignosed depression
nsch_all <- nsch_all %>% mutate(
  depression = case_when(
    k2q32b %in% c(1)                      ~ 1,
    k2q32b %in% c(2) | k2q32a %in% c(1:2) ~ 0
  ))

# Current diagnosed anxiety
nsch_all <- nsch_all %>% mutate(
  anxiety = case_when(
    k2q33b %in% c(1)                      ~ 1,
    k2q33b %in% c(2) | k2q33a %in% c(1:2) ~ 0
  ))

# Current ADD/ADHD
nsch_all <- nsch_all %>% mutate(
  adhd = case_when(
    k2q31b %in% c(1)                      ~ 1,
    k2q31b %in% c(2) | k2q31a %in% c(1:2) ~ 0
  ))

# Current behavior problems
nsch_all <- nsch_all %>% mutate(
  behavior = case_when(
    k2q34b %in% c(1)                      ~ 1,
    k2q34b %in% c(2) | k2q34a %in% c(1:2) ~ 0
  ))

# Stomach/digestive issues
nsch_all <- nsch_all %>% mutate(
  stomach_r = case_when(
    stomach %in% c(1) ~ 1,
    stomach %in% c(2) ~ 0
  ))

# Unmet health needs (any)
nsch_all <- nsch_all %>% mutate(
  unmet_needs = case_when(
    k4q27 %in% c(1) ~ 1,
    k4q27 %in% c(2) ~ 0,
  ))

# Unmet mental health needs
nsch_all <- nsch_all %>% mutate(
  unmet_mental = case_when(
    k4q28x04 %in% c(1)   ~ 1,
    k4q27    %in% c(1:2) ~ 0
  ))

# Missed days of school
# Dichotomize is as 0-6 vs 7+ days
nsch_all <- nsch_all %>% mutate(
  missed_school = case_when(
    k7q02r_r %in% c(1:3,6) ~ 0,
    k7q02r_r %in% c(4:5)   ~ 1
  ))

# Child's employment
nsch_all <- nsch_all %>% mutate(
  child_job = case_when(
    k7q38 == 1 ~ 1,
    k7q38 == 2 ~ 0
  ))

# Lifetime minimum wage
nsch_all <- nsch_all %>% mutate(
  wage_life = case_when(
    sc_age_years == 0 ~ Effective.Minimum.Wage.2020.Dollars,
    sc_age_years == 1 ~ (Effective.Minimum.Wage.2020.Dollars + lag_by_1_inf)/2,
    sc_age_years == 2 ~ (Effective.Minimum.Wage.2020.Dollars + lag_by_1_inf +
                               lag_by_2_inf)/3,
    sc_age_years == 3 ~ (Effective.Minimum.Wage.2020.Dollars + lag_by_1_inf +
                               lag_by_2_inf + lag_by_3_inf)/4,
    sc_age_years == 4 ~ (Effective.Minimum.Wage.2020.Dollars + lag_by_1_inf +
                               lag_by_2_inf + lag_by_3_inf + lag_by_4_inf)/5,
    sc_age_years == 5 ~ (Effective.Minimum.Wage.2020.Dollars + lag_by_1_inf +
                               lag_by_2_inf + lag_by_3_inf + lag_by_4_inf + lag_by_5_inf)/6,
    sc_age_years == 6 ~ (Effective.Minimum.Wage.2020.Dollars + lag_by_1_inf +
                               lag_by_2_inf + lag_by_3_inf + lag_by_4_inf + lag_by_5_inf +
                               lag_by_6_inf)/7,
    sc_age_years == 7 ~ (Effective.Minimum.Wage.2020.Dollars + lag_by_1_inf +
                               lag_by_2_inf + lag_by_3_inf + lag_by_4_inf + lag_by_5_inf +
                               lag_by_6_inf + lag_by_7_inf)/8,
    sc_age_years == 8 ~ (Effective.Minimum.Wage.2020.Dollars + lag_by_1_inf +
                               lag_by_2_inf + lag_by_3_inf + lag_by_4_inf + lag_by_5_inf +
                               lag_by_6_inf + lag_by_7_inf + lag_by_8_inf)/9,
    sc_age_years == 9 ~ (Effective.Minimum.Wage.2020.Dollars + lag_by_1_inf +
                               lag_by_2_inf + lag_by_3_inf + lag_by_4_inf + lag_by_5_inf +
                               lag_by_6_inf + lag_by_7_inf + lag_by_8_inf + lag_by_9_inf)/10,
    sc_age_years == 10 ~ (Effective.Minimum.Wage.2020.Dollars + lag_by_1_inf +
                                lag_by_2_inf + lag_by_3_inf + lag_by_4_inf + lag_by_5_inf +
                                lag_by_6_inf + lag_by_7_inf + lag_by_8_inf + lag_by_9_inf +
                                lag_by_10_inf)/11,
    sc_age_years == 11 ~ (Effective.Minimum.Wage.2020.Dollars + lag_by_1_inf +
                                lag_by_2_inf + lag_by_3_inf + lag_by_4_inf + lag_by_5_inf +
                                lag_by_6_inf + lag_by_7_inf + lag_by_8_inf + lag_by_9_inf +
                                lag_by_10_inf + lag_by_11_inf)/12,
    sc_age_years == 12 ~ (Effective.Minimum.Wage.2020.Dollars + lag_by_1_inf +
                                lag_by_2_inf +  lag_by_3_inf +  lag_by_4_inf + lag_by_5_inf +
                                lag_by_6_inf +  lag_by_7_inf +  lag_by_8_inf + lag_by_9_inf +
                                lag_by_10_inf + lag_by_11_inf + lag_by_12_inf)/13,
    sc_age_years == 13 ~ (Effective.Minimum.Wage.2020.Dollars + lag_by_1_inf +
                                lag_by_2_inf +  lag_by_3_inf +  lag_by_4_inf +  lag_by_5_inf +
                                lag_by_6_inf +  lag_by_7_inf +  lag_by_8_inf +  lag_by_9_inf +
                                lag_by_10_inf + lag_by_11_inf + lag_by_12_inf + lag_by_13_inf)/14,
    sc_age_years == 14 ~ (Effective.Minimum.Wage.2020.Dollars + lag_by_1_inf +
                                lag_by_2_inf +  lag_by_3_inf +  lag_by_4_inf +  lag_by_5_inf +
                                lag_by_6_inf +  lag_by_7_inf +  lag_by_8_inf +  lag_by_9_inf +
                                lag_by_10_inf + lag_by_11_inf + lag_by_12_inf + lag_by_13_inf +
                                lag_by_14_inf)/15,
    sc_age_years == 15 ~ (Effective.Minimum.Wage.2020.Dollars + lag_by_1_inf +
                                lag_by_2_inf +  lag_by_3_inf +  lag_by_4_inf +  lag_by_5_inf +
                                lag_by_6_inf +  lag_by_7_inf +  lag_by_8_inf +  lag_by_9_inf +
                                lag_by_10_inf + lag_by_11_inf + lag_by_12_inf + lag_by_13_inf +
                                lag_by_14_inf + lag_by_15_inf)/16,
    sc_age_years == 16 ~ (Effective.Minimum.Wage.2020.Dollars + lag_by_1_inf +
                                lag_by_2_inf +  lag_by_3_inf +  lag_by_4_inf +  lag_by_5_inf +
                                lag_by_6_inf +  lag_by_7_inf +  lag_by_8_inf +  lag_by_9_inf +
                                lag_by_10_inf + lag_by_11_inf + lag_by_12_inf + lag_by_13_inf +
                                lag_by_14_inf + lag_by_15_inf + lag_by_16_inf)/17,
    sc_age_years == 17 ~ (Effective.Minimum.Wage.2020.Dollars + lag_by_1_inf +
                                lag_by_2_inf +  lag_by_3_inf +  lag_by_4_inf +  lag_by_5_inf +
                                lag_by_6_inf +  lag_by_7_inf +  lag_by_8_inf +  lag_by_9_inf +
                                lag_by_10_inf + lag_by_11_inf + lag_by_12_inf + lag_by_13_inf +
                                lag_by_14_inf + lag_by_15_inf + lag_by_16_inf + lag_by_17_inf)/18
  ))

# State has EITC program
nsch_all <- nsch_all %>% mutate(
  has_eitc = case_when(
    federal_pct > 0 ~ 1,
    TRUE ~ 0
  ))

# Rescale weight so mean is 1
nsch_all$weights <- nsch_all$fwc/1946.27

# Generate nested sampling clusters
nsch_all$cluster <- paste0(nsch_all$strata, "-", nsch_all$fipsst)

##############################################################################
# Table: Demographic characteristics
##############################################################################

# At least 1 outcome
nsch_all <- nsch_all %>% mutate(
  has_outcome = case_when(
    !is.na(depression) | !is.na(anxiety) | !is.na(adhd) |
      !is.na(behavior) | !is.na(stomach_r) | !is.na(unmet_needs) |
      !is.na(unmet_mental) | !is.na(missed_school) | !is.na(child_job) ~ 1
  ))

# Complete cases for covariates
nsch_all_model <- nsch_all %>%
  subset(., age %in% c(3:17)) %>%
  filter_at(vars(Effective.Minimum.Wage, has_outcome, age, sex, race_eth,
                 family_struc, adult_edu, nativity), all_vars(!is.na(.)))

# Child characteristics: unweighted
summary(tableby(~ as.numeric(age) + sex + race_eth + family_struc + adult_edu + nativity + fpl_category,
                nsch_all_model, digits.pct=0), text=T)

# Child characteristics: weighted
summary(tableby(~ as.numeric(age) + sex + race_eth + family_struc + adult_edu + nativity + fpl_category,
                nsch_all_model, digits.pct=0, weights=fwc/1946.27), text=T)

# Mental health outcomes: weighted
summary(tableby(~ depression + anxiety + adhd + behavior + stomach_r +
                  unmet_needs + unmet_mental + missed_school + child_job,
                nsch_all_model, digits.pct=0, weights=weights), text=T)

##############################################################################
# Figure: Associations between FPL and mental health
##############################################################################

# Reset reference FPL category
nsch_all_model$fpl_category <- factor(nsch_all_model$fpl_category, levels = c("400% or greater", "Less than 100%", "100% to 199%", "200% to 299%", "300% to 399%"))

# Depression
model_fpl_dep <- felm(depression ~ fpl_category + 
                        age + sex + race_eth + family_struc + adult_edu + nativity |
                        year + fipsst | 0 | fipsst,
                      data    = nsch_all_model,
                      weights = nsch_all_model$weights)

# Anxiety
model_fpl_anx <- felm(anxiety ~ fpl_category + 
                        age + sex + race_eth + family_struc + adult_edu + nativity |
                        year + fipsst | 0 | fipsst,
                      data    = nsch_all_model,
                      weights = nsch_all_model$weights)

# ADD/ADHD
model_fpl_add <- felm(adhd ~ fpl_category + 
                        age + sex + race_eth + family_struc + adult_edu + nativity |
                        year + fipsst | 0 | fipsst,
                      data    = nsch_all_model,
                      weights = nsch_all_model$weights)

# Behavioral problems
model_fpl_beh <- felm(behavior ~ fpl_category + 
                        age + sex + race_eth + family_struc + adult_edu + nativity |
                        year + fipsst | 0 | fipsst,
                      data    = nsch_all_model,
                      weights = nsch_all_model$weights)

# Digestive issues
model_fpl_dig <- felm(stomach_r ~ fpl_category + 
                        age + sex + race_eth + family_struc + adult_edu + nativity |
                        year + fipsst | 0 | fipsst,
                      data    = nsch_all_model,
                      weights = nsch_all_model$weights)

# Any unmet care
model_fpl_unm <- felm(unmet_needs ~ fpl_category + 
                        age + sex + race_eth + family_struc + adult_edu + nativity |
                        year + fipsst | 0 | fipsst,
                      data    = nsch_all_model,
                      weights = nsch_all_model$weights)

# Unmet mental care
model_fpl_men <- felm(unmet_mental ~ fpl_category + 
                        age + sex + race_eth + family_struc + adult_edu + nativity |
                        year + fipsst | 0 | fipsst,
                      data    = nsch_all_model,
                      weights = nsch_all_model$weights)

# 7+ school absences
model_fpl_sch <- felm(missed_school ~ fpl_category + 
                        age + sex + race_eth + family_struc + adult_edu + nativity |
                        year + fipsst | 0 | fipsst,
                      data    = nsch_all_model,
                      weights = nsch_all_model$weights)

# Child employment
model_fpl_job <- felm(child_job ~ fpl_category + 
                        age + sex + race_eth + family_struc + adult_edu + nativity |
                        year + fipsst | 0 | fipsst,
                      data    = nsch_all_model,
                      weights = nsch_all_model$weights)

# Dataframe of rates
means_all <- as.data.frame(rbind(
  # Depression
  cbind("Depression", "Less than 100%",  coef(model_fpl_dep)[1], model_fpl_dep$cse[1]),
  cbind("Depression", "100% to 199%",    coef(model_fpl_dep)[2], model_fpl_dep$cse[2]),
  cbind("Depression", "200% to 299%",    coef(model_fpl_dep)[3], model_fpl_dep$cse[3]),
  cbind("Depression", "300% to 399%",    coef(model_fpl_dep)[4], model_fpl_dep$cse[4]),
  cbind("Depression", "400% or greater", 0, 0),
  
  # Anxiety
  cbind("Anxiety", "Less than 100%",  coef(model_fpl_anx)[1], model_fpl_anx$cse[1]),
  cbind("Anxiety", "100% to 199%",    coef(model_fpl_anx)[2], model_fpl_anx$cse[2]),
  cbind("Anxiety", "200% to 299%",    coef(model_fpl_anx)[3], model_fpl_anx$cse[3]),
  cbind("Anxiety", "300% to 399%",    coef(model_fpl_anx)[4], model_fpl_anx$cse[4]),
  cbind("Anxiety", "400% or greater", 0, 0),
  
  # ADD/ADHD
  cbind("ADD/ADHD", "Less than 100%",  coef(model_fpl_add)[1], model_fpl_add$cse[1]),
  cbind("ADD/ADHD", "100% to 199%",    coef(model_fpl_add)[2], model_fpl_add$cse[2]),
  cbind("ADD/ADHD", "200% to 299%",    coef(model_fpl_add)[3], model_fpl_add$cse[3]),
  cbind("ADD/ADHD", "300% to 399%",    coef(model_fpl_add)[4], model_fpl_add$cse[4]),
  cbind("ADD/ADHD", "400% or greater", 0, 0),
  
  # Behavioral problems
  cbind("Behavioral prob.", "Less than 100%",  coef(model_fpl_beh)[1], model_fpl_beh$cse[1]),
  cbind("Behavioral prob.", "100% to 199%",    coef(model_fpl_beh)[2], model_fpl_beh$cse[2]),
  cbind("Behavioral prob.", "200% to 299%",    coef(model_fpl_beh)[3], model_fpl_beh$cse[3]),
  cbind("Behavioral prob.", "300% to 399%",    coef(model_fpl_beh)[4], model_fpl_beh$cse[4]),
  cbind("Behavioral prob.", "400% or greater", 0, 0),
  
  # Digestive issues
  cbind("Digestive issues", "Less than 100%",  coef(model_fpl_dig)[1], model_fpl_dig$cse[1]),
  cbind("Digestive issues", "100% to 199%",    coef(model_fpl_dig)[2], model_fpl_dig$cse[2]),
  cbind("Digestive issues", "200% to 299%",    coef(model_fpl_dig)[3], model_fpl_dig$cse[3]),
  cbind("Digestive issues", "300% to 399%",    coef(model_fpl_dig)[4], model_fpl_dig$cse[4]),
  cbind("Digestive issues", "400% or greater", 0, 0),
  
  # Any unmet care
  cbind("Any unmet care", "Less than 100%",  coef(model_fpl_unm)[1], model_fpl_unm$cse[1]),
  cbind("Any unmet care", "100% to 199%",    coef(model_fpl_unm)[2], model_fpl_unm$cse[2]),
  cbind("Any unmet care", "200% to 299%",    coef(model_fpl_unm)[3], model_fpl_unm$cse[3]),
  cbind("Any unmet care", "300% to 399%",    coef(model_fpl_unm)[4], model_fpl_unm$cse[4]),
  cbind("Any unmet care", "400% or greater", 0, 0),
  
  # Unmet mental care
  cbind("Unmet mental care", "Less than 100%",  coef(model_fpl_men)[1], model_fpl_men$cse[1]),
  cbind("Unmet mental care", "100% to 199%",    coef(model_fpl_men)[2], model_fpl_men$cse[2]),
  cbind("Unmet mental care", "200% to 299%",    coef(model_fpl_men)[3], model_fpl_men$cse[3]),
  cbind("Unmet mental care", "300% to 399%",    coef(model_fpl_men)[4], model_fpl_men$cse[4]),
  cbind("Unmet mental care", "400% or greater", 0, 0),
  
  # 7+ school absences
  cbind("7+ school absences", "Less than 100%",  coef(model_fpl_sch)[1], model_fpl_sch$cse[1]),
  cbind("7+ school absences", "100% to 199%",    coef(model_fpl_sch)[2], model_fpl_sch$cse[2]),
  cbind("7+ school absences", "200% to 299%",    coef(model_fpl_sch)[3], model_fpl_sch$cse[3]),
  cbind("7+ school absences", "300% to 399%",    coef(model_fpl_sch)[4], model_fpl_sch$cse[4]),
  cbind("7+ school absences", "400% or greater", 0, 0),
  
  # Child employment
  cbind("Child employment", "Less than 100%",  coef(model_fpl_job)[1], model_fpl_job$cse[1]),
  cbind("Child employment", "100% to 199%",    coef(model_fpl_job)[2], model_fpl_job$cse[2]),
  cbind("Child employment", "200% to 299%",    coef(model_fpl_job)[3], model_fpl_job$cse[3]),
  cbind("Child employment", "300% to 399%",    coef(model_fpl_job)[4], model_fpl_job$cse[4]),
  cbind("Child employment", "400% or greater", 0, 0)
  ))
colnames(means_all) <- c("Outcome", "FPL level", "value", "se")

# Reorder factor levels
means_all$`FPL level` <- factor(means_all$`FPL level`, levels = c("Less than 100%", "100% to 199%", "200% to 299%", "300% to 399%", "400% or greater"))
means_all$Outcome <- factor(means_all$Outcome, levels = c("Depression", "Anxiety", "ADD/ADHD", "Behavioral prob.", "Digestive issues", "Any unmet care", "Unmet mental care", "7+ school absences", "Child employment"))

# Treat adjusted means and SEs as numeric
means_all$value <- as.numeric(as.character(means_all$value))
means_all$se    <- as.numeric(as.character(means_all$se))

# Load package
library(ggh4x)

# Plot means by outcome
plot_means <- ggplot(means_all, aes(x=`FPL level`, y=value, fill=`FPL level`)) +
  geom_hline(yintercept=0, color="black", linewidth=0.25) +
  geom_point(stat = "identity") +
  geom_errorbar(aes(ymin=value-1.96*se, ymax=value+1.96*se), width=0.2) +
  ylab("Adjusted rates of mental health outcomes,\nrelative to children living at 400% FPL") +
  xlab("Household FPL level") +
  theme_test() +
  theme(legend.position = "none",
        text = element_text(size=10, face="bold"),
        axis.ticks = element_blank(),
        axis.text.x = element_text(angle=45, hjust=1, vjust=1),
        panel.grid.major.y = element_line(color="gray", size=0.5),
        panel.grid.minor.y = element_line(color="gray", size=0.25),
        strip.background = element_blank(),
        panel.border = element_blank(),
        panel.spacing = unit(0.5, "cm", data = NULL),
        strip.text = element_text(size=10)) +
  # scale_y_continuous(limits = c(-0.08, 0.06),
  #                    labels = scales::percent,
  #                    breaks = seq(-0.1, 0.1, 0.02),
  #                    minor_breaks = seq(-0.1, 0.1, 0.01)) +
  scale_fill_grey(start=0, end=0.7, name="") +
  facet_wrap(~Outcome, nrow=3, scales="free") +
  facetted_pos_scales(
    y = list(
      Outcome %in% c("Depression", "Anxiety", "ADD/ADHD", "Behavioral prob.", "Digestive issues", "Any unmet care", "Unmet mental care", "7+ school absences") ~
        scale_y_continuous(limits = c(-0.02, 0.08),
                           labels = function(x) paste0(x*100," pp"),
                           breaks = seq(-0.1, 0.1, 0.02),
                           minor_breaks = seq(-0.1, 0.1, 0.01)),
      Outcome == "Child employment" ~ 
        scale_y_continuous(limits = c(-0.08, 0.02),
                           labels = function(x) paste0(x*100," pp"),
                           breaks = seq(-0.1, 0.1, 0.02),
                           minor_breaks = seq(-0.1, 0.1, 0.01))
    )
  )

# Export figure
ggsave(plot=plot_means, file="Exhibits/NSCH adjusted rates of outcomes.pdf",
       width=6, height=9, units='in', dpi=600)

##############################################################################
# Functions for TWFE models
##############################################################################

# Function to extract values from TWFE models.
# Requires: Df for saving coefficients, "lfe" model, title of model.
# Returns: Df of values, ready to pass to "clean_coef_df".
make_coef_df <- function(coef_df, model, TITLE) {
  
  # Get outcome and category labels
  if (model$lhs == "depression") {
    outcome  <- "Depression"
    category <- "Diagnoses"}
  
  if (model$lhs == "anxiety") {
    outcome  <- "Anxiety"
    category <- "Diagnoses"}
  
  if (model$lhs == "adhd") {
    outcome  <- "ADD/ADHD"
    category <- "Diagnoses"}
  
  if (model$lhs == "behavior") {
    outcome  <- "Behavioral prob."
    category <- "Diagnoses"}
  
  if (model$lhs == "stomach_r") {
    outcome  <- "Digestive issues"
    category <- "Sx."}
  
  if (model$lhs == "unmet_needs") {
    outcome  <- "Any unmet care"
    category <- "Health care"}
  
  if (model$lhs == "unmet_mental") {
    outcome  <- "Unmet mental care"
    category <- "Health care"}
  
  if (model$lhs == "missed_school") {
    outcome  <- "7+ school absences"
    category <- "School & Work"}
  
  if (model$lhs == "child_job") {
    outcome  <- "Child employment"
    category <- "School & Work"}
  
  # Add row to coefficient df
  coef_df <- rbind(coef_df, cbind(
    outcome, category, TITLE, model$coef[1], model$cse[1], length(model$residuals)))
  
  # Return coefficient df
  return(coef_df)
}

# Function to clean dataframe of TWFE coefficients.
# Requires: Df with columns specified by "make_coef_df".
# Returns: Cleaned dataframe, ready to use in ggplot2.
clean_coef_df <- function(coef_df) {
  
  # Treat as dataframe
  coef_df <- as.data.frame(coef_df)
  
  # Name columns
  colnames(coef_df) <- c("Outcome", "Category", "Sample", "effect", "se", "n")
  
  # Treat columns as numeric
  coef_df$effect <- as.numeric(coef_df$effect)
  coef_df$se     <- as.numeric(coef_df$se)
  coef_df$n      <- as.numeric(coef_df$n)
  
  # Reorder outcomes
  coef_df$Outcome <- factor(
    coef_df$Outcome, levels=c("Depression", "Anxiety", "ADD/ADHD", "Behavioral prob.", "Digestive issues", "Any unmet care", "Unmet mental care", "7+ school absences", "Child employment"))
  
  # Reorder categories
  coef_df$Category <- factor(
    coef_df$Category, levels=c("Diagnoses", "Sx.", "Health care", "School & Work"))
  
  # Reorder samples
  coef_df$Sample <- factor(
    coef_df$Sample, levels=c(
      
      # Standard models
      "All children (FE only)",
      "All children (FE + age)",
      "All children (fully adjusted)",
      
      # Sub-population models
      "Less than 200% FPL",
      "Adults with high school or less",
      "Black or Hispanic/Latino",
      "First- or second-generation",
      "Adolescents, age 13-17",
      
      # Robustness checks
      "All children (2020 dollars)",
      "All children (lagged wage)",
      
      # Alternate cluster models
      "All children (FE only, state clust.)",
      "All children (FE only, nested clust.)",
      "All children (fully adj., state clust.)",
      "All children (fully adj., nested clust.)"
    ))
  
  # Return df
  return(coef_df)
}

# Function to generate TWFE coefficient plots.
# Requires: Df of coefficients, y title, y limits, color order.
# Returns: Formatted coefficient plot.
print_coef_plot <- function(coef_df, Y_TITLE, Y_MIN, Y_MAX, COLORS) {
  
  # Set grayscale colors
  if (COLORS == "Standard") {
    COLOR_MAX <- 0
    COLOR_MIN <- 0.7
  }
  if (COLORS == "Reversed") {
    COLOR_MAX <- 0.7
    COLOR_MIN <- 0
  }
  
  # Generate coefficient plot
  plot <- ggplot(coef_df, aes(x=Outcome, y=effect, group=Sample, color=Sample)) +
    
    # Null line
    geom_hline(yintercept=0, color="black", linewidth=0.25) +
    
    # Point estimates with distinct shapes
    geom_point(position = position_dodge(width=0.6), size=1, aes(shape=Sample)) +
    scale_shape_manual(values = 1:nlevels(coef_df$Sample)) +
    
    # Confidence intervals
    geom_errorbar(aes(ymin=effect-1.96*se, ymax=effect+1.96*se),
                  position = position_dodge(width=0.6), width=0, linewidth=0.8) +
    geom_errorbar(aes(ymin=effect-2.94*se, ymax=effect+2.94*se),
                  position = position_dodge(width=0.6), width=0.3, alpha=0.5) +
    
    # Titles
    ylab(Y_TITLE) +
    ggtitle("All children (3-17), 2016-2020") +
    
    # Theme modifications
    theme_test() +
    theme(legend.position = "bottom",
          text = element_text(size = 10, face = "bold"),
          axis.text.x = element_text(angle=45, hjust=1, vjust=1),
          axis.title.x = element_blank(),
          axis.ticks = element_blank(),
          strip.background = element_blank(),
          legend.title = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(color="light gray", linewidth=0.5),
          panel.grid.minor.y = element_line(color="light gray", linewidth=0.25)) +
    scale_y_continuous(limits = c(Y_MIN, Y_MAX),
                       breaks = seq(-0.1, 0.1, 0.02),
                       minor_breaks = seq(-0.1, 0.1, 0.01),
                       labels = function(x) paste0(x*100," pp")) +
    scale_color_grey(start=COLOR_MIN, end=COLOR_MAX) +
    facet_grid(~Category, scales="free", space="free_x")
  
  # Return plot
  return(plot)
}

##############################################################################
# Main TWFE models: Minimum wage and mental health
##############################################################################

# Depression
model_min_dep_1 <- felm(depression ~ Effective.Minimum.Wage |
                          year + fipsst | 0 | fipsst,
                        data    = nsch_all_model,
                        weights = nsch_all_model$weights)
model_min_dep_2 <- felm(depression ~ Effective.Minimum.Wage +
                          age + sex + race_eth + family_struc + adult_edu + nativity +
                          elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                          year + fipsst | 0 | fipsst,
                        data    = nsch_all_model,
                        weights = nsch_all_model$weights)

# Anxiety
model_min_anx_1 <- felm(anxiety ~ Effective.Minimum.Wage |
                          year + fipsst | 0 | fipsst,
                        data    = nsch_all_model,
                        weights = nsch_all_model$weights)
model_min_anx_2 <- felm(anxiety ~ Effective.Minimum.Wage +
                          age + sex + race_eth + family_struc + adult_edu + nativity +
                          elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                          year + fipsst | 0 | fipsst,
                        data    = nsch_all_model,
                        weights = nsch_all_model$weights)

# ADD/ADHD
model_min_add_1 <- felm(adhd ~ Effective.Minimum.Wage |
                          year + fipsst | 0 | fipsst,
                        data    = nsch_all_model,
                        weights = nsch_all_model$weights)
model_min_add_2 <- felm(adhd ~ Effective.Minimum.Wage +
                          age + sex + race_eth + family_struc + adult_edu + nativity +
                          elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                          year + fipsst | 0 | fipsst,
                        data    = nsch_all_model,
                        weights = nsch_all_model$weights)

# Behavioral problems
model_min_beh_1 <- felm(behavior ~ Effective.Minimum.Wage |
                          year + fipsst | 0 | fipsst,
                        data    = nsch_all_model,
                        weights = nsch_all_model$weights)
model_min_beh_2 <- felm(behavior ~ Effective.Minimum.Wage +
                          age + sex + race_eth + family_struc + adult_edu + nativity +
                          elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                          year + fipsst | 0 | fipsst,
                        data    = nsch_all_model,
                        weights = nsch_all_model$weights)

# Digestive issues
model_min_dig_1 <- felm(stomach_r ~ Effective.Minimum.Wage |
                          year + fipsst | 0 | fipsst,
                        data    = nsch_all_model,
                        weights = nsch_all_model$weights)
model_min_dig_2 <- felm(stomach_r ~ Effective.Minimum.Wage +
                          age + sex + race_eth + family_struc + adult_edu + nativity +
                          elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                          year + fipsst | 0 | fipsst,
                        data    = nsch_all_model,
                        weights = nsch_all_model$weights)

# Any unmet care
model_min_unm_1 <- felm(unmet_needs ~ Effective.Minimum.Wage |
                          year + fipsst | 0 | fipsst,
                        data    = nsch_all_model,
                        weights = nsch_all_model$weights)
model_min_unm_2 <- felm(unmet_needs ~ Effective.Minimum.Wage +
                          age + sex + race_eth + family_struc + adult_edu + nativity +
                          elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                          year + fipsst | 0 | fipsst,
                        data    = nsch_all_model,
                        weights = nsch_all_model$weights)

# Unmet mental care
model_min_men_1 <- felm(unmet_mental ~ Effective.Minimum.Wage |
                          year + fipsst | 0 | fipsst,
                        data    = nsch_all_model,
                        weights = nsch_all_model$weights)
model_min_men_2 <- felm(unmet_mental ~ Effective.Minimum.Wage +
                          age + sex + race_eth + family_struc + adult_edu + nativity +
                          elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                          year + fipsst | 0 | fipsst,
                        data    = nsch_all_model,
                        weights = nsch_all_model$weights)

# 7+ school absences
model_min_sch_1 <- felm(missed_school ~ Effective.Minimum.Wage |
                          year + fipsst | 0 | fipsst,
                        data    = nsch_all_model,
                        weights = nsch_all_model$weights)
model_min_sch_2 <- felm(missed_school ~ Effective.Minimum.Wage +
                          age + sex + race_eth + family_struc + adult_edu + nativity +
                          elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                          year + fipsst | 0 | fipsst,
                        data    = nsch_all_model,
                        weights = nsch_all_model$weights)

# Child employment
model_min_job_1 <- felm(child_job ~ Effective.Minimum.Wage |
                          year + fipsst | 0 | fipsst,
                        data    = nsch_all_model,
                        weights = nsch_all_model$weights)
model_min_job_2 <- felm(child_job ~ Effective.Minimum.Wage +
                          age + sex + race_eth + family_struc + adult_edu + nativity +
                          elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                          year + fipsst | 0 | fipsst,
                        data    = nsch_all_model,
                        weights = nsch_all_model$weights)

# Get values from models
main_df <- NULL

# All children (FE only)
main_df <- make_coef_df(main_df, model_min_dep_1, "All children (FE only)")
main_df <- make_coef_df(main_df, model_min_anx_1, "All children (FE only)")
main_df <- make_coef_df(main_df, model_min_add_1, "All children (FE only)")
main_df <- make_coef_df(main_df, model_min_beh_1, "All children (FE only)")
main_df <- make_coef_df(main_df, model_min_dig_1, "All children (FE only)")
main_df <- make_coef_df(main_df, model_min_unm_1, "All children (FE only)")
main_df <- make_coef_df(main_df, model_min_men_1, "All children (FE only)")
main_df <- make_coef_df(main_df, model_min_sch_1, "All children (FE only)")
main_df <- make_coef_df(main_df, model_min_job_1, "All children (FE only)")

# All children (fully adjusted)
main_df <- make_coef_df(main_df, model_min_dep_2, "All children (fully adjusted)")
main_df <- make_coef_df(main_df, model_min_anx_2, "All children (fully adjusted)")
main_df <- make_coef_df(main_df, model_min_add_2, "All children (fully adjusted)")
main_df <- make_coef_df(main_df, model_min_beh_2, "All children (fully adjusted)")
main_df <- make_coef_df(main_df, model_min_dig_2, "All children (fully adjusted)")
main_df <- make_coef_df(main_df, model_min_unm_2, "All children (fully adjusted)")
main_df <- make_coef_df(main_df, model_min_men_2, "All children (fully adjusted)")
main_df <- make_coef_df(main_df, model_min_sch_2, "All children (fully adjusted)")
main_df <- make_coef_df(main_df, model_min_job_2, "All children (fully adjusted)")

# Clean dataframe of coefficients
main_df <- clean_coef_df(main_df)

# Get min. and max. N
min(main_df$n); max(main_df$n)

# Generate coefficient plot: Main
plot_main <- print_coef_plot(
  main_df,
  Y_TITLE    = "Association of $1 increase in min. wage\nwith children's mental health",
  Y_MIN      = -0.045,
  Y_MAX      =  0.045,
  COLORS     = "Standard"
)

# Export figure
ggsave(plot=plot_main, file="Exhibits/NSCH coefficient plot, main.pdf",
       width=5, height=4, units='in', dpi=600)

##############################################################################
# TWFE robustness check: Sub-populations
##############################################################################

# Depression
model_min_dep_l <- felm(depression ~ Effective.Minimum.Wage*low_income +
                          age + sex + race_eth + family_struc + adult_edu + nativity +
                          elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                          year + fipsst | 0 | fipsst,
                        data    = nsch_all_model,
                        weights = nsch_all_model$weights)
model_min_dep_a <- felm(depression ~ Effective.Minimum.Wage*age_cat +
                          age + sex + race_eth + family_struc + adult_edu + nativity +
                          elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                          year + fipsst | 0 | fipsst,
                        data    = nsch_all_model,
                        weights = nsch_all_model$weights)
model_min_dep_r <- felm(depression ~ Effective.Minimum.Wage*race_eth_cat +
                          age + sex + race_eth + family_struc + adult_edu + nativity +
                          elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                          year + fipsst | 0 | fipsst,
                        data    = nsch_all_model,
                        weights = nsch_all_model$weights)
model_min_dep_e <- felm(depression ~ Effective.Minimum.Wage*adult_edu_cat +
                          age + sex + race_eth + family_struc + adult_edu + nativity +
                          elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                          year + fipsst | 0 | fipsst,
                        data    = nsch_all_model,
                        weights = nsch_all_model$weights)
model_min_dep_n <- felm(depression ~ Effective.Minimum.Wage*nativity_cat +
                          age + sex + race_eth + family_struc + adult_edu + nativity +
                          elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                          year + fipsst | 0 | fipsst,
                        data    = nsch_all_model,
                        weights = nsch_all_model$weights)

# Anxiety
model_min_anx_l <- felm(anxiety ~ Effective.Minimum.Wage*low_income +
                          age + sex + race_eth + family_struc + adult_edu + nativity +
                          elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                          year + fipsst | 0 | fipsst,
                        data    = nsch_all_model,
                        weights = nsch_all_model$weights)
model_min_anx_a <- felm(anxiety ~ Effective.Minimum.Wage*age_cat +
                          age + sex + race_eth + family_struc + adult_edu + nativity +
                          elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                          year + fipsst | 0 | fipsst,
                        data    = nsch_all_model,
                        weights = nsch_all_model$weights)
model_min_anx_r <- felm(anxiety ~ Effective.Minimum.Wage*race_eth_cat +
                          age + sex + race_eth + family_struc + adult_edu + nativity +
                          elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                          year + fipsst | 0 | fipsst,
                        data    = nsch_all_model,
                        weights = nsch_all_model$weights)
model_min_anx_e <- felm(anxiety ~ Effective.Minimum.Wage*adult_edu_cat +
                          age + sex + race_eth + family_struc + adult_edu + nativity +
                          elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                          year + fipsst | 0 | fipsst,
                        data    = nsch_all_model,
                        weights = nsch_all_model$weights)
model_min_anx_n <- felm(anxiety ~ Effective.Minimum.Wage*nativity_cat +
                          age + sex + race_eth + family_struc + adult_edu + nativity +
                          elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                          year + fipsst | 0 | fipsst,
                        data    = nsch_all_model,
                        weights = nsch_all_model$weights)

# ADD/ADHD
model_min_add_l <- felm(adhd ~ Effective.Minimum.Wage*low_income +
                          age + sex + race_eth + family_struc + adult_edu + nativity +
                          elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                          year + fipsst | 0 | fipsst,
                        data    = nsch_all_model,
                        weights = nsch_all_model$weights)
model_min_add_a <- felm(adhd ~ Effective.Minimum.Wage*age_cat +
                          age + sex + race_eth + family_struc + adult_edu + nativity +
                          elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                          year + fipsst | 0 | fipsst,
                        data    = nsch_all_model,
                        weights = nsch_all_model$weights)
model_min_add_r <- felm(adhd ~ Effective.Minimum.Wage*race_eth_cat +
                          age + sex + race_eth + family_struc + adult_edu + nativity +
                          elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                          year + fipsst | 0 | fipsst,
                        data    = nsch_all_model,
                        weights = nsch_all_model$weights)
model_min_add_e <- felm(adhd ~ Effective.Minimum.Wage*adult_edu_cat +
                          age + sex + race_eth + family_struc + adult_edu + nativity +
                          elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                          year + fipsst | 0 | fipsst,
                        data    = nsch_all_model,
                        weights = nsch_all_model$weights)
model_min_add_n <- felm(adhd ~ Effective.Minimum.Wage*nativity_cat +
                          age + sex + race_eth + family_struc + adult_edu + nativity +
                          elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                          year + fipsst | 0 | fipsst,
                        data    = nsch_all_model,
                        weights = nsch_all_model$weights)

# Behavioral problems
model_min_beh_l <- felm(behavior ~ Effective.Minimum.Wage*low_income +
                          age + sex + race_eth + family_struc + adult_edu + nativity +
                          elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                          year + fipsst | 0 | fipsst,
                        data    = nsch_all_model,
                        weights = nsch_all_model$weights)
model_min_beh_a <- felm(behavior ~ Effective.Minimum.Wage*age_cat +
                          age + sex + race_eth + family_struc + adult_edu + nativity +
                          elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                          year + fipsst | 0 | fipsst,
                        data    = nsch_all_model,
                        weights = nsch_all_model$weights)
model_min_beh_r <- felm(behavior ~ Effective.Minimum.Wage*race_eth_cat +
                          age + sex + race_eth + family_struc + adult_edu + nativity +
                          elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                          year + fipsst | 0 | fipsst,
                        data    = nsch_all_model,
                        weights = nsch_all_model$weights)
model_min_beh_e <- felm(behavior ~ Effective.Minimum.Wage*adult_edu_cat +
                          age + sex + race_eth + family_struc + adult_edu + nativity +
                          elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                          year + fipsst | 0 | fipsst,
                        data    = nsch_all_model,
                        weights = nsch_all_model$weights)
model_min_beh_n <- felm(behavior ~ Effective.Minimum.Wage*nativity_cat +
                          age + sex + race_eth + family_struc + adult_edu + nativity +
                          elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                          year + fipsst | 0 | fipsst,
                        data    = nsch_all_model,
                        weights = nsch_all_model$weights)

# Digestive issues
model_min_dig_l <- felm(stomach_r ~ Effective.Minimum.Wage*low_income +
                          age + sex + race_eth + family_struc + adult_edu + nativity +
                          elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                          year + fipsst | 0 | fipsst,
                        data    = nsch_all_model,
                        weights = nsch_all_model$weights)
model_min_dig_a <- felm(stomach_r ~ Effective.Minimum.Wage*age_cat +
                          age + sex + race_eth + family_struc + adult_edu + nativity +
                          elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                          year + fipsst | 0 | fipsst,
                        data    = nsch_all_model,
                        weights = nsch_all_model$weights)
model_min_dig_r <- felm(stomach_r ~ Effective.Minimum.Wage*race_eth_cat +
                          age + sex + race_eth + family_struc + adult_edu + nativity +
                          elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                          year + fipsst | 0 | fipsst,
                        data    = nsch_all_model,
                        weights = nsch_all_model$weights)
model_min_dig_e <- felm(stomach_r ~ Effective.Minimum.Wage*adult_edu_cat +
                          age + sex + race_eth + family_struc + adult_edu + nativity +
                          elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                          year + fipsst | 0 | fipsst,
                        data    = nsch_all_model,
                        weights = nsch_all_model$weights)
model_min_dig_n <- felm(stomach_r ~ Effective.Minimum.Wage*nativity_cat +
                          age + sex + race_eth + family_struc + adult_edu + nativity +
                          elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                          year + fipsst | 0 | fipsst,
                        data    = nsch_all_model,
                        weights = nsch_all_model$weights)

# Any unmet care
model_min_unm_l <- felm(unmet_needs ~ Effective.Minimum.Wage*low_income +
                          age + sex + race_eth + family_struc + adult_edu + nativity +
                          elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                          year + fipsst | 0 | fipsst,
                        data    = nsch_all_model,
                        weights = nsch_all_model$weights)
model_min_unm_a <- felm(unmet_needs ~ Effective.Minimum.Wage*age_cat +
                          age + sex + race_eth + family_struc + adult_edu + nativity +
                          elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                          year + fipsst | 0 | fipsst,
                        data    = nsch_all_model,
                        weights = nsch_all_model$weights)
model_min_unm_r <- felm(unmet_needs ~ Effective.Minimum.Wage*race_eth_cat +
                          age + sex + race_eth + family_struc + adult_edu + nativity +
                          elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                          year + fipsst | 0 | fipsst,
                        data    = nsch_all_model,
                        weights = nsch_all_model$weights)
model_min_unm_e <- felm(unmet_needs ~ Effective.Minimum.Wage*adult_edu_cat +
                          age + sex + race_eth + family_struc + adult_edu + nativity +
                          elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                          year + fipsst | 0 | fipsst,
                        data    = nsch_all_model,
                        weights = nsch_all_model$weights)
model_min_unm_n <- felm(unmet_needs ~ Effective.Minimum.Wage*nativity_cat +
                          age + sex + race_eth + family_struc + adult_edu + nativity +
                          elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                          year + fipsst | 0 | fipsst,
                        data    = nsch_all_model,
                        weights = nsch_all_model$weights)

# Unmet mental care
model_min_men_l <- felm(unmet_mental ~ Effective.Minimum.Wage*low_income +
                          age + sex + race_eth + family_struc + adult_edu + nativity +
                          elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                          year + fipsst | 0 | fipsst,
                        data    = nsch_all_model,
                        weights = nsch_all_model$weights)
model_min_men_a <- felm(unmet_mental ~ Effective.Minimum.Wage*age_cat +
                          age + sex + race_eth + family_struc + adult_edu + nativity +
                          elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                          year + fipsst | 0 | fipsst,
                        data    = nsch_all_model,
                        weights = nsch_all_model$weights)
model_min_men_r <- felm(unmet_mental ~ Effective.Minimum.Wage*race_eth_cat +
                          age + sex + race_eth + family_struc + adult_edu + nativity +
                          elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                          year + fipsst | 0 | fipsst,
                        data    = nsch_all_model,
                        weights = nsch_all_model$weights)
model_min_men_e <- felm(unmet_mental ~ Effective.Minimum.Wage*adult_edu_cat +
                          age + sex + race_eth + family_struc + adult_edu + nativity +
                          elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                          year + fipsst | 0 | fipsst,
                        data    = nsch_all_model,
                        weights = nsch_all_model$weights)
model_min_men_n <- felm(unmet_mental ~ Effective.Minimum.Wage*nativity_cat +
                          age + sex + race_eth + family_struc + adult_edu + nativity +
                          elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                          year + fipsst | 0 | fipsst,
                        data    = nsch_all_model,
                        weights = nsch_all_model$weights)

# 7+ school absences
model_min_sch_l <- felm(missed_school ~ Effective.Minimum.Wage*low_income +
                          age + sex + race_eth + family_struc + adult_edu + nativity +
                          elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                          year + fipsst | 0 | fipsst,
                        data    = nsch_all_model,
                        weights = nsch_all_model$weights)
model_min_sch_a <- felm(missed_school ~ Effective.Minimum.Wage*age_cat +
                          age + sex + race_eth + family_struc + adult_edu + nativity +
                          elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                          year + fipsst | 0 | fipsst,
                        data    = nsch_all_model,
                        weights = nsch_all_model$weights)
model_min_sch_r <- felm(missed_school ~ Effective.Minimum.Wage*race_eth_cat +
                          age + sex + race_eth + family_struc + adult_edu + nativity +
                          elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                          year + fipsst | 0 | fipsst,
                        data    = nsch_all_model,
                        weights = nsch_all_model$weights)
model_min_sch_e <- felm(missed_school ~ Effective.Minimum.Wage*adult_edu_cat +
                          age + sex + race_eth + family_struc + adult_edu + nativity +
                          elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                          year + fipsst | 0 | fipsst,
                        data    = nsch_all_model,
                        weights = nsch_all_model$weights)
model_min_sch_n <- felm(missed_school ~ Effective.Minimum.Wage*nativity_cat +
                          age + sex + race_eth + family_struc + adult_edu + nativity +
                          elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                          year + fipsst | 0 | fipsst,
                        data    = nsch_all_model,
                        weights = nsch_all_model$weights)

# Child employment
model_min_job_l <- felm(child_job ~ Effective.Minimum.Wage*low_income +
                          age + sex + race_eth + family_struc + adult_edu + nativity +
                          elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                          year + fipsst | 0 | fipsst,
                        data    = nsch_all_model,
                        weights = nsch_all_model$weights)
model_min_job_a <- felm(child_job ~ Effective.Minimum.Wage*age_cat +
                          age + sex + race_eth + family_struc + adult_edu + nativity +
                          elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                          year + fipsst | 0 | fipsst,
                        data    = nsch_all_model,
                        weights = nsch_all_model$weights)
model_min_job_r <- felm(child_job ~ Effective.Minimum.Wage*race_eth_cat +
                          age + sex + race_eth + family_struc + adult_edu + nativity +
                          elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                          year + fipsst | 0 | fipsst,
                        data    = nsch_all_model,
                        weights = nsch_all_model$weights)
model_min_job_e <- felm(child_job ~ Effective.Minimum.Wage*adult_edu_cat +
                          age + sex + race_eth + family_struc + adult_edu + nativity +
                          elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                          year + fipsst | 0 | fipsst,
                        data    = nsch_all_model,
                        weights = nsch_all_model$weights)
model_min_job_n <- felm(child_job ~ Effective.Minimum.Wage*nativity_cat +
                          age + sex + race_eth + family_struc + adult_edu + nativity +
                          elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                          year + fipsst | 0 | fipsst,
                        data    = nsch_all_model,
                        weights = nsch_all_model$weights)

# Get values from models
sub_df <- NULL

# Less than 200% FPL
sub_df <- make_coef_df(sub_df, model_min_dep_l, "Less than 200% FPL")
sub_df <- make_coef_df(sub_df, model_min_anx_l, "Less than 200% FPL")
sub_df <- make_coef_df(sub_df, model_min_add_l, "Less than 200% FPL")
sub_df <- make_coef_df(sub_df, model_min_beh_l, "Less than 200% FPL")
sub_df <- make_coef_df(sub_df, model_min_dig_l, "Less than 200% FPL")
sub_df <- make_coef_df(sub_df, model_min_unm_l, "Less than 200% FPL")
sub_df <- make_coef_df(sub_df, model_min_men_l, "Less than 200% FPL")
sub_df <- make_coef_df(sub_df, model_min_sch_l, "Less than 200% FPL")
sub_df <- make_coef_df(sub_df, model_min_job_l, "Less than 200% FPL")

# Adolescents, age 13-17
sub_df <- make_coef_df(sub_df, model_min_dep_a, "Adolescents, age 13-17")
sub_df <- make_coef_df(sub_df, model_min_anx_a, "Adolescents, age 13-17")
sub_df <- make_coef_df(sub_df, model_min_add_a, "Adolescents, age 13-17")
sub_df <- make_coef_df(sub_df, model_min_beh_a, "Adolescents, age 13-17")
sub_df <- make_coef_df(sub_df, model_min_dig_a, "Adolescents, age 13-17")
sub_df <- make_coef_df(sub_df, model_min_unm_a, "Adolescents, age 13-17")
sub_df <- make_coef_df(sub_df, model_min_men_a, "Adolescents, age 13-17")
sub_df <- make_coef_df(sub_df, model_min_sch_a, "Adolescents, age 13-17")
sub_df <- make_coef_df(sub_df, model_min_job_a, "Adolescents, age 13-17")

# Black or Hispanic/Latino
sub_df <- make_coef_df(sub_df, model_min_dep_r, "Black or Hispanic/Latino")
sub_df <- make_coef_df(sub_df, model_min_anx_r, "Black or Hispanic/Latino")
sub_df <- make_coef_df(sub_df, model_min_add_r, "Black or Hispanic/Latino")
sub_df <- make_coef_df(sub_df, model_min_beh_r, "Black or Hispanic/Latino")
sub_df <- make_coef_df(sub_df, model_min_dig_r, "Black or Hispanic/Latino")
sub_df <- make_coef_df(sub_df, model_min_unm_r, "Black or Hispanic/Latino")
sub_df <- make_coef_df(sub_df, model_min_men_r, "Black or Hispanic/Latino")
sub_df <- make_coef_df(sub_df, model_min_sch_r, "Black or Hispanic/Latino")
sub_df <- make_coef_df(sub_df, model_min_job_r, "Black or Hispanic/Latino")

# Adults with high school or less
sub_df <- make_coef_df(sub_df, model_min_dep_e, "Adults with high school or less")
sub_df <- make_coef_df(sub_df, model_min_anx_e, "Adults with high school or less")
sub_df <- make_coef_df(sub_df, model_min_add_e, "Adults with high school or less")
sub_df <- make_coef_df(sub_df, model_min_beh_e, "Adults with high school or less")
sub_df <- make_coef_df(sub_df, model_min_dig_e, "Adults with high school or less")
sub_df <- make_coef_df(sub_df, model_min_unm_e, "Adults with high school or less")
sub_df <- make_coef_df(sub_df, model_min_men_e, "Adults with high school or less")
sub_df <- make_coef_df(sub_df, model_min_sch_e, "Adults with high school or less")
sub_df <- make_coef_df(sub_df, model_min_job_e, "Adults with high school or less")

# First- or second-generation
sub_df <- make_coef_df(sub_df, model_min_dep_n, "First- or second-generation")
sub_df <- make_coef_df(sub_df, model_min_anx_n, "First- or second-generation")
sub_df <- make_coef_df(sub_df, model_min_add_n, "First- or second-generation")
sub_df <- make_coef_df(sub_df, model_min_beh_n, "First- or second-generation")
sub_df <- make_coef_df(sub_df, model_min_dig_n, "First- or second-generation")
sub_df <- make_coef_df(sub_df, model_min_unm_n, "First- or second-generation")
sub_df <- make_coef_df(sub_df, model_min_men_n, "First- or second-generation")
sub_df <- make_coef_df(sub_df, model_min_sch_n, "First- or second-generation")
sub_df <- make_coef_df(sub_df, model_min_job_n, "First- or second-generation")

# Clean dataframe of coefficients
sub_df <- clean_coef_df(sub_df)

# Add main models for comparison
sub_df <- rbind(sub_df, main_df %>% filter(Sample == "All children (fully adjusted)"))

# Get min. and max. N
min(sub_df$n); max(sub_df$n)

# Generate coefficient plot: Sub-populations
plot_sub <- print_coef_plot(
  sub_df,
  Y_TITLE    = "Association of $1 increase in min. wage\nwith children's mental health",
  Y_MIN      = -0.045,
  Y_MAX      =  0.045,
  COLORS     = "Reversed"
)

# Export figure
ggsave(plot=plot_sub, file="Exhibits/NSCH coefficient plot, sub-population.pdf",
       width=7, height=4, units='in', dpi=600)

##############################################################################
# TWFE robustness check: Alternate specifications
##############################################################################

# Depression
model_min_dep_x <- felm(depression ~ Effective.Minimum.Wage.2020.Dollars +
                          age + sex + race_eth + family_struc + adult_edu + nativity +
                          elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                          year + fipsst | 0 | fipsst,
                        data    = nsch_all_model,
                        weights = nsch_all_model$weights)
model_min_dep_y <- felm(depression ~ lag_by_1 +
                          age + sex + race_eth + family_struc + adult_edu + nativity +
                          elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                          year + fipsst | 0 | fipsst,
                        data    = nsch_all_model,
                        weights = nsch_all_model$weights)

# Anxiety
model_min_anx_x <- felm(anxiety ~ Effective.Minimum.Wage.2020.Dollars +
                          age + sex + race_eth + family_struc + adult_edu + nativity +
                          elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                          year + fipsst | 0 | fipsst,
                        data    = nsch_all_model,
                        weights = nsch_all_model$weights)
model_min_anx_y <- felm(anxiety ~ lag_by_1 +
                          age + sex + race_eth + family_struc + adult_edu + nativity +
                          elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                          year + fipsst | 0 | fipsst,
                        data    = nsch_all_model,
                        weights = nsch_all_model$weights)

# ADD/ADHD
model_min_add_x <- felm(adhd ~ Effective.Minimum.Wage.2020.Dollars +
                          age + sex + race_eth + family_struc + adult_edu + nativity +
                          elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                          year + fipsst | 0 | fipsst,
                        data    = nsch_all_model,
                        weights = nsch_all_model$weights)
model_min_add_y <- felm(adhd ~ lag_by_1 +
                          age + sex + race_eth + family_struc + adult_edu + nativity +
                          elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                          year + fipsst | 0 | fipsst,
                        data    = nsch_all_model,
                        weights = nsch_all_model$weights)

# Behavioral problems
model_min_beh_x <- felm(behavior ~ Effective.Minimum.Wage.2020.Dollars +
                          age + sex + race_eth + family_struc + adult_edu + nativity +
                          elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                          year + fipsst | 0 | fipsst,
                        data    = nsch_all_model,
                        weights = nsch_all_model$weights)
model_min_beh_y <- felm(behavior ~ lag_by_1 +
                          age + sex + race_eth + family_struc + adult_edu + nativity +
                          elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                          year + fipsst | 0 | fipsst,
                        data    = nsch_all_model,
                        weights = nsch_all_model$weights)

# Digestive issues
model_min_dig_x <- felm(stomach_r ~ Effective.Minimum.Wage.2020.Dollars +
                          age + sex + race_eth + family_struc + adult_edu + nativity +
                          elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                          year + fipsst | 0 | fipsst,
                        data    = nsch_all_model,
                        weights = nsch_all_model$weights)
model_min_dig_y <- felm(stomach_r ~ lag_by_1 +
                          age + sex + race_eth + family_struc + adult_edu + nativity +
                          elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                          year + fipsst | 0 | fipsst,
                        data    = nsch_all_model,
                        weights = nsch_all_model$weights)

# Any unmet care
model_min_unm_x <- felm(unmet_needs ~ Effective.Minimum.Wage.2020.Dollars +
                          age + sex + race_eth + family_struc + adult_edu + nativity +
                          elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                          year + fipsst | 0 | fipsst,
                        data    = nsch_all_model,
                        weights = nsch_all_model$weights)
model_min_unm_y <- felm(unmet_needs ~ lag_by_1 +
                          age + sex + race_eth + family_struc + adult_edu + nativity +
                          elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                          year + fipsst | 0 | fipsst,
                        data    = nsch_all_model,
                        weights = nsch_all_model$weights)

# Unmet mental care
model_min_men_x <- felm(unmet_mental ~ Effective.Minimum.Wage.2020.Dollars +
                          age + sex + race_eth + family_struc + adult_edu + nativity +
                          elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                          year + fipsst | 0 | fipsst,
                        data    = nsch_all_model,
                        weights = nsch_all_model$weights)
model_min_men_y <- felm(unmet_mental ~ lag_by_1 +
                          age + sex + race_eth + family_struc + adult_edu + nativity +
                          elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                          year + fipsst | 0 | fipsst,
                        data    = nsch_all_model,
                        weights = nsch_all_model$weights)

# 7+ school absences
model_min_sch_x <- felm(missed_school ~ Effective.Minimum.Wage.2020.Dollars +
                          age + sex + race_eth + family_struc + adult_edu + nativity +
                          elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                          year + fipsst | 0 | fipsst,
                        data    = nsch_all_model,
                        weights = nsch_all_model$weights)
model_min_sch_y <- felm(missed_school ~ lag_by_1 +
                          age + sex + race_eth + family_struc + adult_edu + nativity +
                          elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                          year + fipsst | 0 | fipsst,
                        data    = nsch_all_model,
                        weights = nsch_all_model$weights)

# Child employment
model_min_job_x <- felm(child_job ~ Effective.Minimum.Wage.2020.Dollars +
                          age + sex + race_eth + family_struc + adult_edu + nativity +
                          elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                          year + fipsst | 0 | fipsst,
                        data    = nsch_all_model,
                        weights = nsch_all_model$weights)
model_min_job_y <- felm(child_job ~ lag_by_1 +
                          age + sex + race_eth + family_struc + adult_edu + nativity +
                          elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                          year + fipsst | 0 | fipsst,
                        data    = nsch_all_model,
                        weights = nsch_all_model$weights)

# Get values from models
rob_df <- NULL

# All children (2020 dollars)
rob_df <- make_coef_df(rob_df, model_min_dep_x, "All children (2020 dollars)")
rob_df <- make_coef_df(rob_df, model_min_anx_x, "All children (2020 dollars)")
rob_df <- make_coef_df(rob_df, model_min_add_x, "All children (2020 dollars)")
rob_df <- make_coef_df(rob_df, model_min_beh_x, "All children (2020 dollars)")
rob_df <- make_coef_df(rob_df, model_min_dig_x, "All children (2020 dollars)")
rob_df <- make_coef_df(rob_df, model_min_unm_x, "All children (2020 dollars)")
rob_df <- make_coef_df(rob_df, model_min_men_x, "All children (2020 dollars)")
rob_df <- make_coef_df(rob_df, model_min_sch_x, "All children (2020 dollars)")
rob_df <- make_coef_df(rob_df, model_min_job_x, "All children (2020 dollars)")

# All children (lagged wage)
rob_df <- make_coef_df(rob_df, model_min_dep_y, "All children (lagged wage)")
rob_df <- make_coef_df(rob_df, model_min_anx_y, "All children (lagged wage)")
rob_df <- make_coef_df(rob_df, model_min_add_y, "All children (lagged wage)")
rob_df <- make_coef_df(rob_df, model_min_beh_y, "All children (lagged wage)")
rob_df <- make_coef_df(rob_df, model_min_dig_y, "All children (lagged wage)")
rob_df <- make_coef_df(rob_df, model_min_unm_y, "All children (lagged wage)")
rob_df <- make_coef_df(rob_df, model_min_men_y, "All children (lagged wage)")
rob_df <- make_coef_df(rob_df, model_min_sch_y, "All children (lagged wage)")
rob_df <- make_coef_df(rob_df, model_min_job_y, "All children (lagged wage)")

# Clean dataframe of coefficients
rob_df <- clean_coef_df(rob_df)

# Add main models for comparison
rob_df <- rbind(rob_df, main_df %>% filter(Sample == "All children (fully adjusted)"))

# Get min. and max. N
min(rob_df$n); max(rob_df$n)

# Generate coefficient plot: Alternate specifications
plot_rob <- print_coef_plot(
  rob_df,
  Y_TITLE    = "Association of $1 increase in min. wage\nwith children's mental health",
  Y_MIN      = -0.045,
  Y_MAX      =  0.045,
  COLORS     = "Reversed"
)

# Export figure
ggsave(plot=plot_rob, file="Exhibits/NSCH coefficient plot, robustness.pdf",
       width=7, height=4, units='in', dpi=600)

##############################################################################
# TWFE robustness check: Logistic regression
##############################################################################

# Define complex sampling design
# Used state clusters and apply survey weights
design_all <- svydesign(id=~fipsst, weights=~fwc, data=nsch_all_model)

# Subset to children aged 3-17
design_sub <- subset(design_all, age %in% c(3:17))

# Depression
log_min_dep_1 <- svyglm(depression ~ Effective.Minimum.Wage +
                          year + fipsst,
                        design = design_sub, family="quasibinomial")
log_min_dep_2 <- svyglm(depression ~ Effective.Minimum.Wage +
                          age + sex + race_eth + family_struc + adult_edu + nativity +
                          elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 +
                          year + fipsst,
                        design = design_sub, family="quasibinomial")

# Anxiety
log_min_anx_1 <- svyglm(anxiety ~ Effective.Minimum.Wage +
                          year + fipsst,
                        design = design_sub, family="quasibinomial")
log_min_anx_2 <- svyglm(anxiety ~ Effective.Minimum.Wage +
                          age + sex + race_eth + family_struc + adult_edu + nativity +
                          elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 +
                          year + fipsst,
                        design = design_sub, family="quasibinomial")

# ADD/ADHD
log_min_add_1 <- svyglm(adhd ~ Effective.Minimum.Wage +
                          year + fipsst,
                        design = design_sub, family="quasibinomial")
log_min_add_2 <- svyglm(adhd ~ Effective.Minimum.Wage +
                          age + sex + race_eth + family_struc + adult_edu + nativity +
                          elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 +
                          year + fipsst,
                        design = design_sub, family="quasibinomial")

# Behavioral problems
log_min_beh_1 <- svyglm(behavior ~ Effective.Minimum.Wage +
                          year + fipsst,
                        design = design_sub, family="quasibinomial")
log_min_beh_2 <- svyglm(behavior ~ Effective.Minimum.Wage +
                          age + sex + race_eth + family_struc + adult_edu + nativity +
                          elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 +
                          year + fipsst,
                        design = design_sub, family="quasibinomial")

# Digestive issues
log_min_dig_1 <- svyglm(stomach_r ~ Effective.Minimum.Wage +
                          year + fipsst,
                        design = design_sub, family="quasibinomial")
log_min_dig_2 <- svyglm(stomach_r ~ Effective.Minimum.Wage +
                          age + sex + race_eth + family_struc + adult_edu + nativity +
                          elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 +
                          year + fipsst,
                        design = design_sub, family="quasibinomial")

# Any unmet care
log_min_unm_1 <- svyglm(unmet_needs ~ Effective.Minimum.Wage +
                          year + fipsst,
                        design = design_sub, family="quasibinomial")
log_min_unm_2 <- svyglm(unmet_needs ~ Effective.Minimum.Wage +
                          age + sex + race_eth + family_struc + adult_edu + nativity +
                          elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 +
                          year + fipsst,
                        design = design_sub, family="quasibinomial")

# Unmet mental care
log_min_men_1 <- svyglm(unmet_mental ~ Effective.Minimum.Wage +
                          year + fipsst,
                        design = design_sub, family="quasibinomial")
log_min_men_2 <- svyglm(unmet_mental ~ Effective.Minimum.Wage +
                          age + sex + race_eth + family_struc + adult_edu + nativity +
                          elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 +
                          year + fipsst,
                        design = design_sub, family="quasibinomial")

# 7+ school absences
log_min_sch_1 <- svyglm(missed_school ~ Effective.Minimum.Wage +
                          year + fipsst,
                        design = design_sub, family="quasibinomial")
log_min_sch_2 <- svyglm(missed_school ~ Effective.Minimum.Wage +
                          age + sex + race_eth + family_struc + adult_edu + nativity +
                          elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 +
                          year + fipsst,
                        design = design_sub, family="quasibinomial")

# Child employment
log_min_job_1 <- svyglm(child_job ~ Effective.Minimum.Wage +
                          year + fipsst,
                        design = design_sub, family="quasibinomial")
log_min_job_2 <- svyglm(child_job ~ Effective.Minimum.Wage +
                          age + sex + race_eth + family_struc + adult_edu + nativity +
                          elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 +
                          year + fipsst,
                        design = design_sub, family="quasibinomial")

# Get values from models
log_df <- as.data.frame(rbind(
  # Depression
  cbind("Depression", "Diagnoses", "All children (FE only)",
        coef(log_min_dep_1)[2],
        SE(log_min_dep_1)[2],
        length(log_min_dep_1$residuals)),
  
  cbind("Depression", "Diagnoses", "All children (fully adjusted)",
        coef(log_min_dep_2)[2],
        SE(log_min_dep_2)[2],
        length(log_min_dep_2$residuals)),
  
  # Anxiety
  cbind("Anxiety", "Diagnoses", "All children (FE only)",
        coef(log_min_anx_1)[2],
        SE(log_min_anx_1)[2],
        length(log_min_anx_1$residuals)),
  
  cbind("Anxiety", "Diagnoses", "All children (fully adjusted)",
        coef(log_min_anx_2)[2],
        SE(log_min_anx_2)[2],
        length(log_min_anx_2$residuals)),
  
  # ADD/ADHD
  cbind("ADD/ADHD", "Diagnoses", "All children (FE only)",
        coef(log_min_add_1)[2],
        SE(log_min_add_1)[2],
        length(log_min_add_1$residuals)),
  
  cbind("ADD/ADHD", "Diagnoses", "All children (fully adjusted)",
        coef(log_min_add_2)[2],
        SE(log_min_add_2)[2],
        length(log_min_add_2$residuals)),
  
  # Behavioral problems
  cbind("Behavioral prob.", "Diagnoses", "All children (FE only)",
        coef(log_min_beh_1)[2],
        SE(log_min_beh_1)[2],
        length(log_min_beh_1$residuals)),
  
  cbind("Behavioral prob.", "Diagnoses", "All children (fully adjusted)",
        coef(log_min_beh_2)[2],
        SE(log_min_beh_2)[2],
        length(log_min_beh_2$residuals)),
  
  # Digestive issues
  cbind("Digestive issues", "Sx.", "All children (FE only)",
        coef(log_min_dig_1)[2],
        SE(log_min_dig_1)[2],
        length(log_min_dig_1$residuals)),
  
  cbind("Digestive issues", "Sx.", "All children (fully adjusted)",
        coef(log_min_dig_2)[2],
        SE(log_min_dig_2)[2],
        length(log_min_dig_2$residuals)),
  
  # Any unmet care
  cbind("Any unmet care", "Health care", "All children (FE only)",
        coef(log_min_unm_1)[2],
        SE(log_min_unm_1)[2],
        length(log_min_unm_1$residuals)),
  
  cbind("Any unmet care", "Health care", "All children (fully adjusted)",
        coef(log_min_unm_2)[2],
        SE(log_min_unm_2)[2],
        length(log_min_unm_2$residuals)),
  
  # Unmet mental care
  cbind("Unmet mental care", "Health care", "All children (FE only)",
        coef(log_min_men_1)[2],
        SE(log_min_men_1)[2],
        length(log_min_men_1$residuals)),
  
  cbind("Unmet mental care", "Health care", "All children (fully adjusted)",
        coef(log_min_men_2)[2],
        SE(log_min_men_2)[2],
        length(log_min_men_2$residuals)),
  
  # 7+ school absences
  cbind("7+ school absences", "School & Work", "All children (FE only)",
        coef(log_min_sch_1)[2],
        SE(log_min_sch_1)[2],
        length(log_min_sch_1$residuals)),
  
  cbind("7+ school absences", "School & Work", "All children (fully adjusted)",
        coef(log_min_sch_2)[2],
        SE(log_min_sch_2)[2],
        length(log_min_sch_2$residuals)),
  
  # Child employment
  cbind("Child employment", "School & Work", "All children (FE only)",
        coef(log_min_job_1)[2],
        SE(log_min_job_1)[2],
        length(log_min_job_1$residuals)),
  
  cbind("Child employment", "School & Work", "All children (fully adjusted)",
        coef(log_min_job_2)[2],
        SE(log_min_job_2)[2],
        length(log_min_job_2$residuals))
))

# Name columns
colnames(log_df) <- c("Outcome", "Category", "Sample", "log_odds", "se", "n")

# Reorder outcomes
log_df$Outcome <- factor(
  log_df$Outcome, levels=c("Depression", "Anxiety", "ADD/ADHD", "Behavioral prob.", "Digestive issues", "Any unmet care", "Unmet mental care", "7+ school absences", "Child employment"))

# Reorder categories
log_df$Category <- factor(
  log_df$Category, levels=c("Diagnoses", "Sx.", "Health care", "School & Work"))

# Reorder samples
log_df$Sample <- factor(log_df$Sample, levels=c("All children (FE only)",
                                                "All children (fully adjusted)"))

# Treat columns as numeric
log_df$log_odds <- as.numeric(log_df$log_odds)
log_df$se       <- as.numeric(log_df$se)
log_df$n        <- as.numeric(log_df$n)

# Get min. and max. N
min(log_df$n); max(log_df$n)

# Generate coefficient plot: Logistic
plot_log <- ggplot(log_df, aes(x=Outcome, y=exp(log_odds), group=Sample, color=Sample)) +
  geom_hline(yintercept=0, color="black", linewidth=0.25) +
  geom_point(position = position_dodge(width=0.6), size=1, aes(shape=Sample)) +
  scale_shape_manual(values = 1:nlevels(log_df$Sample)) +
  geom_errorbar(aes(ymin=exp(log_odds - 1.96*se), ymax=exp(log_odds + 1.96*se)),
                position = position_dodge(width=0.6), width=0, linewidth=0.8) +
  geom_errorbar(aes(ymin=exp(log_odds - 2.94*se), ymax=exp(log_odds + 2.94*se)),
                position = position_dodge(width=0.6), width=0.3, alpha=0.5) +
  ylab("Association of $1 increase in min. wage\nwith children's mental health") +
  ggtitle("All children (3-17), 2016-2020") +
  theme_test() +
  theme(legend.position = "bottom",
        text = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(angle=45, hjust=1, vjust=1),
        axis.title.x = element_blank(),
        axis.ticks = element_blank(),
        strip.background = element_blank(),
        legend.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color="light gray", linewidth=0.5),
        panel.grid.minor.y = element_line(color="light gray", linewidth=0.25)) +
  scale_y_continuous(trans = "log10", limits=c(0.25,4),
                     labels=c("0.25", "0.5", "OR = 1", "2", "4"),
                     breaks=c(0.25, 0.5, 1, 2, 4),
                     minor_breaks = seq(0.25, 10, 0.25)) +
  scale_color_grey(start=0.7, end=0) +
  facet_grid(~Category, scales="free", space="free_x")

# Export figure
ggsave(plot=plot_log, file="Exhibits/NSCH coefficient plot, logistic.pdf",
       width=5, height=4, units='in', dpi=600)

##############################################################################
# TWFE robustness check: Lifetime minimum wage
##############################################################################

# Depression
life_min_dep_1 <- felm(depression ~ wage_life + age |
                          year + fipsst | 0 | fipsst,
                        data    = nsch_all_model,
                        weights = nsch_all_model$weights)
life_min_dep_2 <- felm(depression ~ wage_life +
                          age + sex + race_eth + family_struc + adult_edu + nativity +
                          elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                          year + fipsst | 0 | fipsst,
                        data    = nsch_all_model,
                        weights = nsch_all_model$weights)

# Anxiety
life_min_anx_1 <- felm(anxiety ~ wage_life + age |
                          year + fipsst | 0 | fipsst,
                        data    = nsch_all_model,
                        weights = nsch_all_model$weights)
life_min_anx_2 <- felm(anxiety ~ wage_life +
                          age + sex + race_eth + family_struc + adult_edu + nativity +
                          elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                          year + fipsst | 0 | fipsst,
                        data    = nsch_all_model,
                        weights = nsch_all_model$weights)

# ADD/ADHD
life_min_add_1 <- felm(adhd ~ wage_life + age |
                          year + fipsst | 0 | fipsst,
                        data    = nsch_all_model,
                        weights = nsch_all_model$weights)
life_min_add_2 <- felm(adhd ~ wage_life +
                          age + sex + race_eth + family_struc + adult_edu + nativity +
                          elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                          year + fipsst | 0 | fipsst,
                        data    = nsch_all_model,
                        weights = nsch_all_model$weights)

# Behavioral problems
life_min_beh_1 <- felm(behavior ~ wage_life + age |
                          year + fipsst | 0 | fipsst,
                        data    = nsch_all_model,
                        weights = nsch_all_model$weights)
life_min_beh_2 <- felm(behavior ~ wage_life +
                          age + sex + race_eth + family_struc + adult_edu + nativity +
                          elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                          year + fipsst | 0 | fipsst,
                        data    = nsch_all_model,
                        weights = nsch_all_model$weights)

# Digestive issues
life_min_dig_1 <- felm(stomach_r ~ wage_life + age |
                          year + fipsst | 0 | fipsst,
                        data    = nsch_all_model,
                        weights = nsch_all_model$weights)
life_min_dig_2 <- felm(stomach_r ~ wage_life +
                          age + sex + race_eth + family_struc + adult_edu + nativity +
                          elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                          year + fipsst | 0 | fipsst,
                        data    = nsch_all_model,
                        weights = nsch_all_model$weights)

# Any unmet care
life_min_unm_1 <- felm(unmet_needs ~ wage_life + age |
                          year + fipsst | 0 | fipsst,
                        data    = nsch_all_model,
                        weights = nsch_all_model$weights)
life_min_unm_2 <- felm(unmet_needs ~ wage_life +
                          age + sex + race_eth + family_struc + adult_edu + nativity +
                          elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                          year + fipsst | 0 | fipsst,
                        data    = nsch_all_model,
                        weights = nsch_all_model$weights)

# Unmet mental care
life_min_men_1 <- felm(unmet_mental ~ wage_life + age |
                          year + fipsst | 0 | fipsst,
                        data    = nsch_all_model,
                        weights = nsch_all_model$weights)
life_min_men_2 <- felm(unmet_mental ~ wage_life +
                          age + sex + race_eth + family_struc + adult_edu + nativity +
                          elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                          year + fipsst | 0 | fipsst,
                        data    = nsch_all_model,
                        weights = nsch_all_model$weights)

# 7+ school absences
life_min_sch_1 <- felm(missed_school ~ wage_life + age |
                          year + fipsst | 0 | fipsst,
                        data    = nsch_all_model,
                        weights = nsch_all_model$weights)
life_min_sch_2 <- felm(missed_school ~ wage_life +
                          age + sex + race_eth + family_struc + adult_edu + nativity +
                          elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                          year + fipsst | 0 | fipsst,
                        data    = nsch_all_model,
                        weights = nsch_all_model$weights)

# Child employment
life_min_job_1 <- felm(child_job ~ wage_life + age |
                          year + fipsst | 0 | fipsst,
                        data    = nsch_all_model,
                        weights = nsch_all_model$weights)
life_min_job_2 <- felm(child_job ~ wage_life +
                          age + sex + race_eth + family_struc + adult_edu + nativity +
                          elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                          year + fipsst | 0 | fipsst,
                        data    = nsch_all_model,
                        weights = nsch_all_model$weights)

# Get values from models
life_df <- NULL

# All children (FE + age)
life_df <- make_coef_df(life_df, life_min_dep_1, "All children (FE + age)")
life_df <- make_coef_df(life_df, life_min_anx_1, "All children (FE + age)")
life_df <- make_coef_df(life_df, life_min_add_1, "All children (FE + age)")
life_df <- make_coef_df(life_df, life_min_beh_1, "All children (FE + age)")
life_df <- make_coef_df(life_df, life_min_dig_1, "All children (FE + age)")
life_df <- make_coef_df(life_df, life_min_unm_1, "All children (FE + age)")
life_df <- make_coef_df(life_df, life_min_men_1, "All children (FE + age)")
life_df <- make_coef_df(life_df, life_min_sch_1, "All children (FE + age)")
life_df <- make_coef_df(life_df, life_min_job_1, "All children (FE + age)")

# All children (fully adjusted)
life_df <- make_coef_df(life_df, life_min_dep_2, "All children (fully adjusted)")
life_df <- make_coef_df(life_df, life_min_anx_2, "All children (fully adjusted)")
life_df <- make_coef_df(life_df, life_min_add_2, "All children (fully adjusted)")
life_df <- make_coef_df(life_df, life_min_beh_2, "All children (fully adjusted)")
life_df <- make_coef_df(life_df, life_min_dig_2, "All children (fully adjusted)")
life_df <- make_coef_df(life_df, life_min_unm_2, "All children (fully adjusted)")
life_df <- make_coef_df(life_df, life_min_men_2, "All children (fully adjusted)")
life_df <- make_coef_df(life_df, life_min_sch_2, "All children (fully adjusted)")
life_df <- make_coef_df(life_df, life_min_job_2, "All children (fully adjusted)")

# Clean dataframe of coefficients
life_df <- clean_coef_df(life_df)

# Get min. and max. N
min(life_df$n); max(life_df$n)

# Generate coefficient plot: Lifetime wage
plot_life <- print_coef_plot(
  life_df,
  Y_TITLE    = "Association of $1 increase in lifetime\nmin. wage with children's mental health",
  Y_MIN      = -0.065,
  Y_MAX      =  0.065,
  COLORS     = "Standard"
)

# Export figure
ggsave(plot=plot_life, file="Exhibits/NSCH coefficient plot, lifetime.pdf",
       width=5, height=4, units='in', dpi=600)

##############################################################################
# TWFE robustness check: Nested clusters
##############################################################################

# Depression
model_min_dep_1c <- felm(depression ~ Effective.Minimum.Wage |
                           year + fipsst | 0 | cluster,
                         data    = nsch_all_model,
                         weights = nsch_all_model$weights)
model_min_dep_2c <- felm(depression ~ Effective.Minimum.Wage +
                           age + sex + race_eth + family_struc + adult_edu + nativity +
                           elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                           year + fipsst | 0 | cluster,
                         data    = nsch_all_model,
                         weights = nsch_all_model$weights)

# Anxiety
model_min_anx_1c <- felm(anxiety ~ Effective.Minimum.Wage |
                           year + fipsst | 0 | cluster,
                         data    = nsch_all_model,
                         weights = nsch_all_model$weights)
model_min_anx_2c <- felm(anxiety ~ Effective.Minimum.Wage +
                           age + sex + race_eth + family_struc + adult_edu + nativity +
                           elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                           year + fipsst | 0 | cluster,
                         data    = nsch_all_model,
                         weights = nsch_all_model$weights)

# ADD/ADHD
model_min_add_1c <- felm(adhd ~ Effective.Minimum.Wage |
                           year + fipsst | 0 | cluster,
                         data    = nsch_all_model,
                         weights = nsch_all_model$weights)
model_min_add_2c <- felm(adhd ~ Effective.Minimum.Wage +
                           age + sex + race_eth + family_struc + adult_edu + nativity +
                           elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                           year + fipsst | 0 | cluster,
                         data    = nsch_all_model,
                         weights = nsch_all_model$weights)

# Behavioral problems
model_min_beh_1c <- felm(behavior ~ Effective.Minimum.Wage |
                           year + fipsst | 0 | cluster,
                         data    = nsch_all_model,
                         weights = nsch_all_model$weights)
model_min_beh_2c <- felm(behavior ~ Effective.Minimum.Wage +
                           age + sex + race_eth + family_struc + adult_edu + nativity +
                           elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                           year + fipsst | 0 | cluster,
                         data    = nsch_all_model,
                         weights = nsch_all_model$weights)

# Digestive issues
model_min_dig_1c <- felm(stomach_r ~ Effective.Minimum.Wage |
                           year + fipsst | 0 | cluster,
                         data    = nsch_all_model,
                         weights = nsch_all_model$weights)
model_min_dig_2c <- felm(stomach_r ~ Effective.Minimum.Wage +
                           age + sex + race_eth + family_struc + adult_edu + nativity +
                           elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                           year + fipsst | 0 | cluster,
                         data    = nsch_all_model,
                         weights = nsch_all_model$weights)

# Any umet needs
model_min_unm_1c <- felm(unmet_needs ~ Effective.Minimum.Wage |
                           year + fipsst | 0 | cluster,
                         data    = nsch_all_model,
                         weights = nsch_all_model$weights)
model_min_unm_2c <- felm(unmet_needs ~ Effective.Minimum.Wage +
                           age + sex + race_eth + family_struc + adult_edu + nativity +
                           elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                           year + fipsst | 0 | cluster,
                         data    = nsch_all_model,
                         weights = nsch_all_model$weights)

# Unmet mental needs
model_min_men_1c <- felm(unmet_mental ~ Effective.Minimum.Wage |
                           year + fipsst | 0 | cluster,
                         data    = nsch_all_model,
                         weights = nsch_all_model$weights)
model_min_men_2c <- felm(unmet_mental ~ Effective.Minimum.Wage +
                           age + sex + race_eth + family_struc + adult_edu + nativity +
                           elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                           year + fipsst | 0 | cluster,
                         data    = nsch_all_model,
                         weights = nsch_all_model$weights)

# 7+ school absences
model_min_sch_1c <- felm(missed_school ~ Effective.Minimum.Wage |
                           year + fipsst | 0 | cluster,
                         data    = nsch_all_model,
                         weights = nsch_all_model$weights)
model_min_sch_2c <- felm(missed_school ~ Effective.Minimum.Wage +
                           age + sex + race_eth + family_struc + adult_edu + nativity +
                           elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                           year + fipsst | 0 | cluster,
                         data    = nsch_all_model,
                         weights = nsch_all_model$weights)

# Child employment
model_min_job_1c <- felm(child_job ~ Effective.Minimum.Wage |
                           year + fipsst | 0 | cluster,
                         data    = nsch_all_model,
                         weights = nsch_all_model$weights)
model_min_job_2c <- felm(child_job ~ Effective.Minimum.Wage +
                           age + sex + race_eth + family_struc + adult_edu + nativity +
                           elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                           year + fipsst | 0 | cluster,
                         data    = nsch_all_model,
                         weights = nsch_all_model$weights)

# Get values from models
clust_df <- NULL

# All children (FE only, nested clust.)
clust_df <- make_coef_df(clust_df, model_min_dep_1c, "All children (FE only, nested clust.)")
clust_df <- make_coef_df(clust_df, model_min_anx_1c, "All children (FE only, nested clust.)")
clust_df <- make_coef_df(clust_df, model_min_add_1c, "All children (FE only, nested clust.)")
clust_df <- make_coef_df(clust_df, model_min_beh_1c, "All children (FE only, nested clust.)")
clust_df <- make_coef_df(clust_df, model_min_dig_1c, "All children (FE only, nested clust.)")
clust_df <- make_coef_df(clust_df, model_min_unm_1c, "All children (FE only, nested clust.)")
clust_df <- make_coef_df(clust_df, model_min_men_1c, "All children (FE only, nested clust.)")
clust_df <- make_coef_df(clust_df, model_min_sch_1c, "All children (FE only, nested clust.)")
clust_df <- make_coef_df(clust_df, model_min_job_1c, "All children (FE only, nested clust.)")

# All children (fully adj., nested clust.)
clust_df <- make_coef_df(clust_df, model_min_dep_2c, "All children (fully adj., nested clust.)")
clust_df <- make_coef_df(clust_df, model_min_anx_2c, "All children (fully adj., nested clust.)")
clust_df <- make_coef_df(clust_df, model_min_add_2c, "All children (fully adj., nested clust.)")
clust_df <- make_coef_df(clust_df, model_min_beh_2c, "All children (fully adj., nested clust.)")
clust_df <- make_coef_df(clust_df, model_min_dig_2c, "All children (fully adj., nested clust.)")
clust_df <- make_coef_df(clust_df, model_min_unm_2c, "All children (fully adj., nested clust.)")
clust_df <- make_coef_df(clust_df, model_min_men_2c, "All children (fully adj., nested clust.)")
clust_df <- make_coef_df(clust_df, model_min_sch_2c, "All children (fully adj., nested clust.)")
clust_df <- make_coef_df(clust_df, model_min_job_2c, "All children (fully adj., nested clust.)")

# Add main models for comparison
# All children (FE only, state clust.)
clust_df <- make_coef_df(clust_df, model_min_dep_1, "All children (FE only, state clust.)")
clust_df <- make_coef_df(clust_df, model_min_anx_1, "All children (FE only, state clust.)")
clust_df <- make_coef_df(clust_df, model_min_add_1, "All children (FE only, state clust.)")
clust_df <- make_coef_df(clust_df, model_min_beh_1, "All children (FE only, state clust.)")
clust_df <- make_coef_df(clust_df, model_min_dig_1, "All children (FE only, state clust.)")
clust_df <- make_coef_df(clust_df, model_min_unm_1, "All children (FE only, state clust.)")
clust_df <- make_coef_df(clust_df, model_min_men_1, "All children (FE only, state clust.)")
clust_df <- make_coef_df(clust_df, model_min_sch_1, "All children (FE only, state clust.)")
clust_df <- make_coef_df(clust_df, model_min_job_1, "All children (FE only, state clust.)")

# All children (fully adj., state clust.)
clust_df <- make_coef_df(clust_df, model_min_dep_2, "All children (fully adj., state clust.)")
clust_df <- make_coef_df(clust_df, model_min_anx_2, "All children (fully adj., state clust.)")
clust_df <- make_coef_df(clust_df, model_min_add_2, "All children (fully adj., state clust.)")
clust_df <- make_coef_df(clust_df, model_min_beh_2, "All children (fully adj., state clust.)")
clust_df <- make_coef_df(clust_df, model_min_dig_2, "All children (fully adj., state clust.)")
clust_df <- make_coef_df(clust_df, model_min_unm_2, "All children (fully adj., state clust.)")
clust_df <- make_coef_df(clust_df, model_min_men_2, "All children (fully adj., state clust.)")
clust_df <- make_coef_df(clust_df, model_min_sch_2, "All children (fully adj., state clust.)")
clust_df <- make_coef_df(clust_df, model_min_job_2, "All children (fully adj., state clust.)")

# Clean dataframe of coefficients
clust_df <- clean_coef_df(clust_df)

# Get min. and max. N
min(clust_df$n); max(clust_df$n)

# Generate coefficient plot: Nested clusters
plot_clust <- print_coef_plot(
  clust_df,
  Y_TITLE    = "Association of $1 increase in min. wage\nwith children's mental health",
  Y_MIN      = -0.045,
  Y_MAX      =  0.045,
  COLORS     = "Standard"
)

# Adjust legend
plot_clust <- plot_clust + guides(shape = guide_legend(nrow = 2))

# Export figure
ggsave(plot=plot_clust, file="Exhibits/NSCH coefficient plot, nested clusters.pdf",
       width=7, height=4, units='in', dpi=600)
