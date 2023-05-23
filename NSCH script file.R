# Minimum wage and children's health
# N.M. Kavanagh, N. Slopen
# March 6, 2023

# Please direct questions about this script file to nolankavanagh@fas.harvard.edu.

# Clear R environment
rm(list = ls())

# Load packages
library(here)         # Working directory
library(readstata13)  # Dataset tools
library(dplyr)        # Analysis tools
library(psych)        # Analysis tools
library(modelbased)   # Modeling tools
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
nsch_all <- bind_rows(nsch_2016_top, nsch_2017_top, nsch_2018_top,
                      nsch_2019_top, nsch_2020_top, nsch_2021_top)

# Redefine strata
# Recommended by the NSCH for multi-year analyses
nsch_all <- nsch_all %>% mutate(
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
merged <- left_join(nsch_all, min_wage_df, by=c("fipsst", "year"))

# Clear old datasets from R
rm(nsch_2016_top, nsch_2017_top, nsch_2018_top,
   nsch_2019_top, nsch_2020_top, nsch_2021_top)

##############################################################################
# Variable preparation
##############################################################################

# Treat fixed effects as factors
merged$year   <- as.factor(merged$year)
merged$fipsst <- as.factor(merged$fipsst)

# Child's age
merged <- merged %>% mutate(
  age = case_when(
    sc_age_years %in% c(0:17) ~ sc_age_years
  ))

# Treat age as factor
merged$age <- as.factor(merged$age)

# Dichotomize age for interaction models
merged <- merged %>% mutate(
  age_cat = case_when(
    sc_age_years %in% c(0:12)  ~ "Children",
    sc_age_years %in% c(13:17) ~ "Adolescents"
  ))
merged$age_cat <- factor(merged$age_cat, levels = c("Adolescents", "Children"))

# Child's sex
merged <- merged %>% mutate(
  sex = case_when(
    sc_sex == 1 ~ "Male",
    sc_sex == 2 ~ "Female"
  ))
merged$sex <- factor(merged$sex, levels = c("Male", "Female"))

# Child's race/ethnicity
merged <- merged %>% mutate(
  race_eth = case_when(
    sc_hispanic_r == 1    ~ "Hispanic/Latino",
    sc_race_r == 1        ~ "White, non-Hispanic/Latino",
    sc_race_r == 2        ~ "Black, non-Hispanic/Latino",
    sc_race_r == 3        ~ "American Indian or Alaska Native",
    sc_race_r %in% c(4:5) ~ "Asian, Native Hawaiian, or Pacific Islander",
    sc_race_r %in% c(6:7) ~ "Other or mixed race",
  ))
merged$race_eth <- factor(merged$race_eth, levels = c("White, non-Hispanic/Latino", "Black, non-Hispanic/Latino", "Hispanic/Latino", "American Indian or Alaska Native", "Asian, Native Hawaiian, or Pacific Islander", "Other or mixed race"))

# Dichotomoize race/ethnicity
# Black/Latino vs. other for interaction models
merged <- merged %>% mutate(
  race_eth_cat = case_when(
    sc_hispanic_r == 1      ~ 0, # Black or Hispanic/Latino
    sc_race_r == 2          ~ 0, # Black or Hispanic/Latino
    sc_race_r %in% c(1,3:7) ~ 1  # Other races
  ))

# Adults' highest educational attainment
merged <- merged %>% mutate(
  adult_edu = case_when(
    higrade_tvis == 1 ~ "Less than high school",
    higrade_tvis == 2 ~ "High school (including vocational)",
    higrade_tvis == 3 ~ "Some college or associate degree",
    higrade_tvis == 4 ~ "College degree or higher"
  ))
merged$adult_edu <- factor(merged$adult_edu, levels = c("Less than high school", "High school (including vocational)", "Some college or associate degree", "College degree or higher"))

# Dichotomize educational attainment
# High school (or less) vs. some college (or more)
# Adults' highest educational attainment
merged <- merged %>% mutate(
  adult_edu_cat = case_when(
    higrade_tvis %in% c(1:2) ~ 0,
    higrade_tvis %in% c(3:4) ~ 1
  ))

# Generate mean estimated FPL
# Later NSCH years generated 6 imputed FPLs if a household was missing income
merged$fpl_mean <- rowMeans(cbind(merged$fpl_i1, merged$fpl_i2, merged$fpl_i3,
                                  merged$fpl_i4, merged$fpl_i5, merged$fpl_i6), na.rm=T)

# Household's federal poverty level
merged <- merged %>% mutate(
  fpl_category = case_when(
    fpl %in% c(50:99)   | fpl_mean < 100 ~ "Less than 100%",
    fpl %in% c(100:199) | fpl_mean < 200 ~ "100% to 199%",
    fpl %in% c(200:299) | fpl_mean < 300 ~ "200% to 299%",
    fpl %in% c(300:399) | fpl_mean < 400 ~ "300% to 399%",
    fpl %in% c(400:999) | fpl_mean < 999 ~ "400% or greater"
  ))
merged$fpl_category <- factor(merged$fpl_category, levels = c("Less than 100%", "100% to 199%", "200% to 299%", "300% to 399%", "400% or greater"))

# Dichotomize FPL for interaction models
# Low-income (<200% FPL) vs. higher-income
merged <- merged %>% mutate(
  low_income = case_when(
    fpl %in% c(50:199)  | fpl_mean < 200 ~ "Lower-income",
    fpl %in% c(200:999) | fpl_mean < 999 ~ "Higher-income"
  ))
merged$low_income <- factor(merged$low_income, levels = c("Lower-income", "Higher-income"))

# Child's employment
merged <- merged %>% mutate(
  child_job = case_when(
    k7q38 == 1 ~ 1,
    k7q38 == 2 ~ 0
  ))

# Family structure
merged <- merged %>% mutate(
  family_struc = case_when(
    family %in% c(1,3) | family_r %in% c(1,3) ~ "Two parents, married",
    family %in% c(2,4) | family_r %in% c(2,4) ~ "Two parents, not married",
    family %in% c(5:8) | family_r %in% c(5:6) ~ "Single parent",
    TRUE ~ "Another family structure or unknown"
  ))
merged$family_struc <- factor(merged$family_struc, levels = c("Two parents, married", "Two parents, not married", "Single parent", "Another family structure or unknown"))

# Household nativity
merged <- merged %>% mutate(
  nativity = case_when(
    house_gen == 1 ~ "First-generation household",
    house_gen == 2 ~ "Second-generation household",
    house_gen == 3 ~ "Third-generation household or higher",
    TRUE ~ "Child and/or parental nativity unknown",
  ))
merged$nativity <- factor(merged$nativity, levels = c("First-generation household", "Second-generation household", "Third-generation household or higher", "Child and/or parental nativity unknown"))

# Dichotomize household nativity
# First/second-generation vs. higher
merged <- merged %>% mutate(
  nativity_cat = case_when(
    house_gen %in% c(1:2) ~ 0,
    TRUE                  ~ 1,
  ))

# Current dignosed depression
merged <- merged %>% mutate(
  depression = case_when(
    k2q32b %in% c(1)                      ~ 1,
    k2q32b %in% c(2) | k2q32a %in% c(1:2) ~ 0
  ))

# Current diagnosed anxiety
merged <- merged %>% mutate(
  anxiety = case_when(
    k2q33b %in% c(1)                      ~ 1,
    k2q33b %in% c(2) | k2q33a %in% c(1:2) ~ 0
  ))

# Current behavior problems
merged <- merged %>% mutate(
  behavior = case_when(
    k2q34b %in% c(1)                      ~ 1,
    k2q34b %in% c(2) | k2q34a %in% c(1:2) ~ 0
  ))

# Current ADD/ADHD
merged <- merged %>% mutate(
  adhd = case_when(
    k2q31b %in% c(1)                      ~ 1,
    k2q31b %in% c(2) | k2q31a %in% c(1:2) ~ 0
  ))

# Stomach/digestive issues
merged <- merged %>% mutate(
  stomach_r = case_when(
    stomach %in% c(1) ~ 1,
    stomach %in% c(2) ~ 0
  ))

# Missed days of school
# Dichotomize is as 0-6 vs 7+ days
merged <- merged %>% mutate(
  missed_school = case_when(
    k7q02r_r %in% c(1:3) ~ 0,
    k7q02r_r %in% c(4:5) ~ 1
  ))

# Unmet health needs (any)
merged <- merged %>% mutate(
  unmet_needs = case_when(
    k4q27 %in% c(1) ~ 1,
    k4q27 %in% c(2) ~ 0,
  ))

# Unmet mental health needs
merged <- merged %>% mutate(
  unmet_mental = case_when(
    k4q28x04 %in% c(1)   ~ 1,
    k4q27    %in% c(1:2) ~ 0
  ))

# Lifetime minimum wage
merged <- merged %>% mutate(
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

##############################################################################
# Specify complex design
##############################################################################

# Rescale weight so mean is 1
merged$weights <- merged$fwc/1946.27

# Define complex sampling design
# Nest strata within states and apply survey weights
design_all <- svydesign(id=~fipsst, strata=~strata, nest=TRUE,
                        weights=~weights, data=merged)

# Subset to children aged 3-17
design_sub <- subset(design_all, age %in% c(3:17))

##############################################################################
# Table 1: Demographic characteristics
##############################################################################

# At least 1 outcome
merged <- merged %>% mutate(
  has_outcome = case_when(
    !is.na(depression) | !is.na(anxiety) | !is.na(behavior) | !is.na(adhd) |
      !is.na(stomach_r) | !is.na(missed_school) ~ 1
  ))

# Complete cases for covariates
merged_demo <- merged %>%
  subset(., age %in% c(3:17)) %>%
  filter_at(vars(Effective.Minimum.Wage, has_outcome, age, sex, race_eth,
                 adult_edu, fpl_category), all_vars(!is.na(.)))

# Child characteristics: unweighted
# This line provides demographic characteristics without survey weights.
summary(tableby(~ as.numeric(age) + sex + race_eth + family_struc + adult_edu + fpl_category + nativity,
                merged_demo, digits.pct=0), text=T)

# Child characteristics: weighted
# This line provides demographic characteristics with survey weights.
summary(tableby(~ as.numeric(age) + sex + race_eth + family_struc + adult_edu + fpl_category + nativity,
                merged_demo, digits.pct=0, weights=fwc/1946.27), text=T)

##############################################################################
# Figure 2: Associations between FPL and outcomes
##############################################################################

# Explore unadjusted associations
prop.table(svytable(~fpl_category + depression,    design=design_sub), 1)
prop.table(svytable(~fpl_category + anxiety,       design=design_sub), 1)
prop.table(svytable(~fpl_category + behavior,      design=design_sub), 1)
prop.table(svytable(~fpl_category + adhd,          design=design_sub), 1)
prop.table(svytable(~fpl_category + stomach_r,     design=design_sub), 1)
prop.table(svytable(~fpl_category + unmet_needs,   design=design_sub), 1)
prop.table(svytable(~fpl_category + unmet_mental,  design=design_sub), 1)
prop.table(svytable(~fpl_category + missed_school, design=design_sub), 1)

# Explore adjusted associations
model_fpl_dep <- svyglm(depression ~ fpl_category +
                          age + sex + race_eth + family_struc + adult_edu + nativity +
                          year + fipsst,
                        design = design_sub)
summary(model_fpl_dep)

model_fpl_anx <- svyglm(anxiety ~ fpl_category +
                          age + sex + race_eth + family_struc + adult_edu + nativity +
                          year + fipsst,
                        design = design_sub)
summary(model_fpl_anx)

model_fpl_add <- svyglm(adhd ~ fpl_category +
                          age + sex + race_eth + family_struc + adult_edu + nativity +
                          year + fipsst,
                        design = design_sub)
summary(model_fpl_add)

model_fpl_beh <- svyglm(behavior ~ fpl_category +
                          age + sex + race_eth + family_struc + adult_edu + nativity +
                          year + fipsst,
                        design = design_sub)
summary(model_fpl_beh)

model_fpl_unm <- svyglm(unmet_needs ~ fpl_category +
                          age + sex + race_eth + family_struc + adult_edu + nativity +
                          year + fipsst,
                        design = design_sub)
summary(model_fpl_unm)

model_fpl_men <- svyglm(unmet_mental ~ fpl_category +
                          age + sex + race_eth + family_struc + adult_edu + nativity +
                          year + fipsst,
                        design = design_sub)
summary(model_fpl_men)

model_fpl_dig <- svyglm(stomach_r ~ fpl_category +
                          age + sex + race_eth + family_struc + adult_edu + nativity +
                          year + fipsst,
                        design = design_sub)
summary(model_fpl_dig)

model_fpl_sch <- svyglm(missed_school ~ fpl_category +
                          age + sex + race_eth + family_struc + adult_edu + nativity +
                          year + fipsst,
                        design = design_sub)
summary(model_fpl_sch)

# Generate marginal means
means_dep <- estimate_means(model_fpl_dep, at="fpl_category")
means_anx <- estimate_means(model_fpl_anx, at="fpl_category")
means_add <- estimate_means(model_fpl_add, at="fpl_category")
means_beh <- estimate_means(model_fpl_beh, at="fpl_category")
means_dig <- estimate_means(model_fpl_dig, at="fpl_category")
means_unm <- estimate_means(model_fpl_unm, at="fpl_category")
means_men <- estimate_means(model_fpl_men, at="fpl_category")
means_sch <- estimate_means(model_fpl_sch, at="fpl_category")

# Dataframe of rates
means_all <- as.data.frame(rbind(
  # Depression
  cbind("Depression", "Less than 100%",  means_dep$Mean[1], means_dep$SE[1]),
  cbind("Depression", "100% to 199%",    means_dep$Mean[2], means_dep$SE[2]),
  cbind("Depression", "200% to 299%",    means_dep$Mean[3], means_dep$SE[3]),
  cbind("Depression", "300% to 399%",    means_dep$Mean[4], means_dep$SE[4]),
  cbind("Depression", "400% or greater", means_dep$Mean[5], means_dep$SE[5]),
  
  # Anxiety
  cbind("Anxiety", "Less than 100%",  means_anx$Mean[1], means_anx$SE[1]),
  cbind("Anxiety", "100% to 199%",    means_anx$Mean[2], means_anx$SE[2]),
  cbind("Anxiety", "200% to 299%",    means_anx$Mean[3], means_anx$SE[3]),
  cbind("Anxiety", "300% to 399%",    means_anx$Mean[4], means_anx$SE[4]),
  cbind("Anxiety", "400% or greater", means_anx$Mean[5], means_anx$SE[5]),
  
  # ADD/ADHD
  cbind("ADD/ADHD", "Less than 100%",  means_add$Mean[1], means_add$SE[1]),
  cbind("ADD/ADHD", "100% to 199%",    means_add$Mean[2], means_add$SE[2]),
  cbind("ADD/ADHD", "200% to 299%",    means_add$Mean[3], means_add$SE[3]),
  cbind("ADD/ADHD", "300% to 399%",    means_add$Mean[4], means_add$SE[4]),
  cbind("ADD/ADHD", "400% or greater", means_add$Mean[5], means_add$SE[5]),
  
  # Behavioral problems
  cbind("Behavioral prob.", "Less than 100%",  means_beh$Mean[1], means_beh$SE[1]),
  cbind("Behavioral prob.", "100% to 199%",    means_beh$Mean[2], means_beh$SE[2]),
  cbind("Behavioral prob.", "200% to 299%",    means_beh$Mean[3], means_beh$SE[3]),
  cbind("Behavioral prob.", "300% to 399%",    means_beh$Mean[4], means_beh$SE[4]),
  cbind("Behavioral prob.", "400% or greater", means_beh$Mean[5], means_beh$SE[5]),
  
  # Digestive issues
  cbind("Digestive issues", "Less than 100%",  means_dig$Mean[1], means_dig$SE[1]),
  cbind("Digestive issues", "100% to 199%",    means_dig$Mean[2], means_dig$SE[2]),
  cbind("Digestive issues", "200% to 299%",    means_dig$Mean[3], means_dig$SE[3]),
  cbind("Digestive issues", "300% to 399%",    means_dig$Mean[4], means_dig$SE[4]),
  cbind("Digestive issues", "400% or greater", means_dig$Mean[5], means_dig$SE[5]),
  
  # Unmet health care needs (any)
  cbind("Unmet health care\n(of any kind)", "Less than 100%",  means_unm$Mean[1], means_unm$SE[1]),
  cbind("Unmet health care\n(of any kind)", "100% to 199%",    means_unm$Mean[2], means_unm$SE[2]),
  cbind("Unmet health care\n(of any kind)", "200% to 299%",    means_unm$Mean[3], means_unm$SE[3]),
  cbind("Unmet health care\n(of any kind)", "300% to 399%",    means_unm$Mean[4], means_unm$SE[4]),
  cbind("Unmet health care\n(of any kind)", "400% or greater", means_unm$Mean[5], means_unm$SE[5]),
  
  # Unmet health care needs (mental health)
  cbind("Unmet health care\n(mental health)", "Less than 100%",  means_men$Mean[1], means_men$SE[1]),
  cbind("Unmet health care\n(mental health)", "100% to 199%",    means_men$Mean[2], means_men$SE[2]),
  cbind("Unmet health care\n(mental health)", "200% to 299%",    means_men$Mean[3], means_men$SE[3]),
  cbind("Unmet health care\n(mental health)", "300% to 399%",    means_men$Mean[4], means_men$SE[4]),
  cbind("Unmet health care\n(mental health)", "400% or greater", means_men$Mean[5], means_men$SE[5]),
  
  # 7+ school absences
  cbind("7+ school absences", "Less than 100%",  means_sch$Mean[1], means_sch$SE[1]),
  cbind("7+ school absences", "100% to 199%",    means_sch$Mean[2], means_sch$SE[2]),
  cbind("7+ school absences", "200% to 299%",    means_sch$Mean[3], means_sch$SE[3]),
  cbind("7+ school absences", "300% to 399%",    means_sch$Mean[4], means_sch$SE[4]),
  cbind("7+ school absences", "400% or greater", means_sch$Mean[5], means_sch$SE[5])
))
colnames(means_all) <- c("Outcome", "FPL level", "value", "se")

# Reorder factor levels
means_all$`FPL level` <- factor(means_all$`FPL level`, levels = c("Less than 100%", "100% to 199%", "200% to 299%", "300% to 399%", "400% or greater"))
means_all$Outcome <- factor(means_all$Outcome, levels = c("Depression", "Anxiety", "ADD/ADHD", "Behavioral prob.", "Digestive issues", "Unmet health care\n(of any kind)", "Unmet health care\n(mental health)", "7+ school absences"))

# Treat adjusted means and SEs as numeric
means_all$value <- as.numeric(as.character(means_all$value))
means_all$se    <- as.numeric(as.character(means_all$se))

# Plot means by outcome
plot_means <- ggplot(means_all, aes(x=`FPL level`, y=value, fill=`FPL level`)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin=value-1.96*se, ymax=value+1.96*se), width=0.2) +
  ylab("Rate of mental health outcome,\nadjusted for demographics") +
  xlab("Household FPL level") +
  theme_test() +
  theme(legend.position = "bottom",
        text = element_text(size=10, face="bold"),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        panel.grid.major.y = element_line(color="gray", size=0.5),
        panel.grid.minor.y = element_line(color="gray", size=0.25),
        strip.background = element_blank(),
        panel.border = element_blank(),
        panel.spacing = unit(0.5, "cm", data = NULL),
        strip.text = element_text(size=10)) +
  scale_y_continuous(labels = scales::percent,
                     breaks=c(0, 0.05, 0.10, 0.15)) +
  scale_fill_grey(start=0.4, end=0.8, name="") +
  facet_grid(~Outcome, nrow=2, scales="free", space="free")
print(plot_means)

# Export figure
ggsave(plot=plot_means, file="Adjusted rates of outcomes.pdf", width=7, height=7, units='in', dpi=600)

##############################################################################
# Figure 1: U.S. maps for minimum wages
##############################################################################

# Generate new state variable
# Lowercase necessary for maps function
min_wage_df$state <- min_wage_df$State

# Generate lagged minimum wage
# Gets minimum wage of 4 years prior in same state
min_wage_df <- min_wage_df %>%
  group_by(state) %>%
  mutate(lag_by_4 = lag(Effective.Minimum.Wage, n=4, default=NA))

# Compute change in minimum wage
# Note: Must use 2020 df when making map
min_wage_df$change <- min_wage_df$Effective.Minimum.Wage - min_wage_df$lag_by_4

# Subset to years of interest
min_wage_2016 <- subset(min_wage_df, year %in% c(2016))
min_wage_2020 <- subset(min_wage_df, year %in% c(2020))

# Descriptives about minimum wage
table(subset(min_wage_2020, fipsst %in% c(0:56))$change)
table(subset(min_wage_2020, fipsst %in% c(0:56))$change == 0)

# Map for minimum wage in 2016
map_2016 <- plot_usmap(regions="states", data=min_wage_2016,
                           values="Effective.Minimum.Wage", size=0.4) +
  ggtitle("2016") +
  theme(legend.position = "right",
        text = element_text(size = 10, face = "bold"),
        plot.title = element_text(size=14, face="bold", hjust=0.5)) +
  viridis::scale_fill_viridis(limits=c(7.25, 14), breaks=c(7.25, 8, 10, 12, 14), labels=c("\n$7.25\n(fed. min.)", "$8", "$10", "$12", "$14"), name="Effective\nmin. wage")

# Map for minimum wage in 2020
map_2020 <- plot_usmap(regions="states", data=min_wage_2020,
                       values="Effective.Minimum.Wage", size=0.4) +
  ggtitle("2020") +
  theme(legend.position = "right",
        text = element_text(size = 10, face = "bold"),
        plot.title = element_text(size=14, face="bold", hjust=0.5)) +
  viridis::scale_fill_viridis(limits=c(7.25, 14), breaks=c(7.25, 8, 10, 12, 14), labels=c("\n$7.25\n(fed. min.)", "$8", "$10", "$12", "$14"), name="Effective\nmin. wage")

# Map for change in minimum wage
map_change <- plot_usmap(regions="states", data=min_wage_2020,
                       values="change", size=0.4) +
  ggtitle("Change") +
  theme(legend.position = "right",
        text = element_text(size = 10, face = "bold"),
        plot.title = element_text(size=14, face="bold", hjust=0.5)) +
  viridis::scale_fill_viridis(limits=c(0, 4.5), breaks=c(0, 1, 2, 3, 4), labels=c("$0", "$1", "$2", "$3", "$4"), name="Increase in\nmin. wage", oob=squish)

# library(scales)
# show_col(viridis_pal()(6))

# Compile figures into object
maps_all <- plot_grid(map_2016, map_2020, map_change, nrow=3)

# Export figure
ggsave(plot=maps_all, file="Map of minimum wages, NSCH.pdf", width=5, height=7, units='in', dpi=600)

##############################################################################
# Effect of minimum wage on mental health outcomes
##############################################################################

# Generate sampling clusters
merged$cluster <- paste0(merged$strata, "-", merged$fipsst)

# Subset to children 3-17
merged_sub <- subset(merged, age %in% c(3:17))

library(lfe)

# Models for depression
model_min_dep_1 <- felm(depression ~ Effective.Minimum.Wage |
                          year + fipsst | 0 | cluster,
                        data    = merged_sub,
                        weights = merged_sub$weights)
model_min_dep_2 <- felm(depression ~ Effective.Minimum.Wage +
                          age + sex + race_eth + family_struc + adult_edu + nativity |
                          year + fipsst | 0 | cluster,
                        data    = merged_sub,
                        weights = merged_sub$weights)
model_min_dep_3 <- felm(depression ~ Effective.Minimum.Wage*low_income +
                          age + sex + race_eth + family_struc + adult_edu + nativity |
                          year + fipsst | 0 | cluster,
                        data    = merged_sub,
                        weights = merged_sub$weights)
model_min_dep_4 <- felm(depression ~ Effective.Minimum.Wage.2020.Dollars +
                          age + sex + race_eth + family_struc + adult_edu + nativity |
                          year + fipsst | 0 | cluster,
                        data    = merged_sub,
                        weights = merged_sub$weights)
model_min_dep_5 <- felm(depression ~ lag_by_1 +
                          age + sex + race_eth + family_struc + adult_edu + nativity |
                          year + fipsst | 0 | cluster,
                        data    = merged_sub,
                        weights = merged_sub$weights)
model_min_dep_6 <- felm(depression ~ Effective.Minimum.Wage*age_cat +
                          age + sex + race_eth + family_struc + adult_edu + nativity |
                          year + fipsst | 0 | cluster,
                        data    = merged_sub,
                        weights = merged_sub$weights)
model_min_dep_7 <- felm(depression ~ Effective.Minimum.Wage*race_eth_cat +
                          age + sex + race_eth + family_struc + adult_edu + nativity |
                          year + fipsst | 0 | cluster,
                        data    = merged_sub,
                        weights = merged_sub$weights)
model_min_dep_8 <- felm(depression ~ Effective.Minimum.Wage*adult_edu_cat +
                          age + sex + race_eth + family_struc + adult_edu + nativity |
                          year + fipsst | 0 | cluster,
                        data    = merged_sub,
                        weights = merged_sub$weights)
model_min_dep_9 <- felm(depression ~ Effective.Minimum.Wage*nativity_cat +
                          age + sex + race_eth + family_struc + adult_edu + nativity |
                          year + fipsst | 0 | cluster,
                        data    = merged_sub,
                        weights = merged_sub$weights)

# # Models for depression
# model_min_dep_1 <- svyglm(depression ~ Effective.Minimum.Wage +
#                             year + fipsst,
#                           design = design_sub)
# model_min_dep_2 <- svyglm(depression ~ Effective.Minimum.Wage +
#                             age + sex + race_eth + family_struc + adult_edu + nativity +
#                             year + fipsst,
#                           design = design_sub)
# model_min_dep_3 <- svyglm(depression ~ Effective.Minimum.Wage*low_income +
#                             age + sex + race_eth + family_struc + adult_edu + nativity +
#                             year + fipsst,
#                           design = design_sub)
# model_min_dep_4 <- svyglm(depression ~ Effective.Minimum.Wage.2020.Dollars +
#                             age + sex + race_eth + family_struc + adult_edu + nativity +
#                             year + fipsst,
#                           design = design_sub)
# model_min_dep_5 <- svyglm(depression ~ lag_by_1 +
#                             age + sex + race_eth + family_struc + adult_edu + nativity +
#                             year + fipsst,
#                           design = design_sub)
# model_min_dep_6 <- svyglm(depression ~ Effective.Minimum.Wage*age_cat +
#                             age + sex + race_eth + family_struc + adult_edu + nativity +
#                             year + fipsst,
#                           design = design_sub)
# model_min_dep_7 <- svyglm(depression ~ Effective.Minimum.Wage*race_eth_cat +
#                             age + sex + race_eth + family_struc + adult_edu + nativity +
#                             year + fipsst,
#                           design = design_sub)
# model_min_dep_8 <- svyglm(depression ~ Effective.Minimum.Wage*adult_edu_cat +
#                             age + sex + race_eth + family_struc + adult_edu + nativity +
#                             year + fipsst,
#                           design = design_sub)
# model_min_dep_9 <- svyglm(depression ~ Effective.Minimum.Wage*nativity_cat +
#                             age + sex + race_eth + family_struc + adult_edu + nativity +
#                             year + fipsst,
#                           design = design_sub)

# Models for anxiety
model_min_anx_1 <- felm(anxiety ~ Effective.Minimum.Wage |
                          year + fipsst | 0 | cluster,
                        data    = merged_sub,
                        weights = merged_sub$weights)
model_min_anx_2 <- felm(anxiety ~ Effective.Minimum.Wage +
                          age + sex + race_eth + family_struc + adult_edu + nativity |
                          year + fipsst | 0 | cluster,
                        data    = merged_sub,
                        weights = merged_sub$weights)
model_min_anx_3 <- felm(anxiety ~ Effective.Minimum.Wage*low_income +
                          age + sex + race_eth + family_struc + adult_edu + nativity |
                          year + fipsst | 0 | cluster,
                        data    = merged_sub,
                        weights = merged_sub$weights)
model_min_anx_4 <- felm(anxiety ~ Effective.Minimum.Wage.2020.Dollars +
                          age + sex + race_eth + family_struc + adult_edu + nativity |
                          year + fipsst | 0 | cluster,
                        data    = merged_sub,
                        weights = merged_sub$weights)
model_min_anx_5 <- felm(anxiety ~ lag_by_1 +
                          age + sex + race_eth + family_struc + adult_edu + nativity |
                          year + fipsst | 0 | cluster,
                        data    = merged_sub,
                        weights = merged_sub$weights)
model_min_anx_6 <- felm(anxiety ~ Effective.Minimum.Wage*age_cat +
                          age + sex + race_eth + family_struc + adult_edu + nativity |
                          year + fipsst | 0 | cluster,
                        data    = merged_sub,
                        weights = merged_sub$weights)
model_min_anx_7 <- felm(anxiety ~ Effective.Minimum.Wage*race_eth_cat +
                          age + sex + race_eth + family_struc + adult_edu + nativity |
                          year + fipsst | 0 | cluster,
                        data    = merged_sub,
                        weights = merged_sub$weights)
model_min_anx_8 <- felm(anxiety ~ Effective.Minimum.Wage*adult_edu_cat +
                          age + sex + race_eth + family_struc + adult_edu + nativity |
                          year + fipsst | 0 | cluster,
                        data    = merged_sub,
                        weights = merged_sub$weights)
model_min_anx_9 <- felm(anxiety ~ Effective.Minimum.Wage*nativity_cat +
                          age + sex + race_eth + family_struc + adult_edu + nativity |
                          year + fipsst | 0 | cluster,
                        data    = merged_sub,
                        weights = merged_sub$weights)

# model_min_anx_1 <- svyglm(anxiety ~ Effective.Minimum.Wage +
#                             year + fipsst,
#                           design = design_sub)
# model_min_anx_2 <- svyglm(anxiety ~ Effective.Minimum.Wage +
#                             age + sex + race_eth + family_struc + adult_edu + nativity +
#                             year + fipsst,
#                           design = design_sub)
# model_min_anx_3 <- svyglm(anxiety ~ Effective.Minimum.Wage*low_income +
#                             age + sex + race_eth + family_struc + adult_edu + nativity +
#                             year + fipsst,
#                           design = design_sub)
# model_min_anx_4 <- svyglm(anxiety ~ Effective.Minimum.Wage.2020.Dollars +
#                             age + sex + race_eth + family_struc + adult_edu + nativity +
#                             year + fipsst,
#                           design = design_sub)
# model_min_anx_5 <- svyglm(anxiety ~ lag_by_1 +
#                             age + sex + race_eth + family_struc + adult_edu + nativity +
#                             year + fipsst,
#                           design = design_sub)
# model_min_anx_6 <- svyglm(anxiety ~ Effective.Minimum.Wage*age_cat +
#                             age + sex + race_eth + family_struc + adult_edu + nativity +
#                             year + fipsst,
#                           design = design_sub)
# model_min_anx_7 <- svyglm(anxiety ~ Effective.Minimum.Wage*race_eth_cat +
#                             age + sex + race_eth + family_struc + adult_edu + nativity +
#                             year + fipsst,
#                           design = design_sub)
# model_min_anx_8 <- svyglm(anxiety ~ Effective.Minimum.Wage*adult_edu_cat +
#                             age + sex + race_eth + family_struc + adult_edu + nativity +
#                             year + fipsst,
#                           design = design_sub)
# model_min_anx_9 <- svyglm(anxiety ~ Effective.Minimum.Wage*nativity_cat +
#                             age + sex + race_eth + family_struc + adult_edu + nativity +
#                             year + fipsst,
#                           design = design_sub)

# Models for ADD/ADHD
model_min_add_1 <- felm(adhd ~ Effective.Minimum.Wage |
                          year + fipsst | 0 | cluster,
                        data    = merged_sub,
                        weights = merged_sub$weights)
model_min_add_2 <- felm(adhd ~ Effective.Minimum.Wage +
                          age + sex + race_eth + family_struc + adult_edu + nativity |
                          year + fipsst | 0 | cluster,
                        data    = merged_sub,
                        weights = merged_sub$weights)
model_min_add_3 <- felm(adhd ~ Effective.Minimum.Wage*low_income +
                          age + sex + race_eth + family_struc + adult_edu + nativity |
                          year + fipsst | 0 | cluster,
                        data    = merged_sub,
                        weights = merged_sub$weights)
model_min_add_4 <- felm(adhd ~ Effective.Minimum.Wage.2020.Dollars +
                          age + sex + race_eth + family_struc + adult_edu + nativity |
                          year + fipsst | 0 | cluster,
                        data    = merged_sub,
                        weights = merged_sub$weights)
model_min_add_5 <- felm(adhd ~ lag_by_1 +
                          age + sex + race_eth + family_struc + adult_edu + nativity |
                          year + fipsst | 0 | cluster,
                        data    = merged_sub,
                        weights = merged_sub$weights)
model_min_add_6 <- felm(adhd ~ Effective.Minimum.Wage*age_cat +
                          age + sex + race_eth + family_struc + adult_edu + nativity |
                          year + fipsst | 0 | cluster,
                        data    = merged_sub,
                        weights = merged_sub$weights)
model_min_add_7 <- felm(adhd ~ Effective.Minimum.Wage*race_eth_cat +
                          age + sex + race_eth + family_struc + adult_edu + nativity |
                          year + fipsst | 0 | cluster,
                        data    = merged_sub,
                        weights = merged_sub$weights)
model_min_add_8 <- felm(adhd ~ Effective.Minimum.Wage*adult_edu_cat +
                          age + sex + race_eth + family_struc + adult_edu + nativity |
                          year + fipsst | 0 | cluster,
                        data    = merged_sub,
                        weights = merged_sub$weights)
model_min_add_9 <- felm(adhd ~ Effective.Minimum.Wage*nativity_cat +
                          age + sex + race_eth + family_struc + adult_edu + nativity |
                          year + fipsst | 0 | cluster,
                        data    = merged_sub,
                        weights = merged_sub$weights)

# model_min_add_1 <- svyglm(adhd ~ Effective.Minimum.Wage +
#                             year + fipsst,
#                           design = design_sub)
# model_min_add_2 <- svyglm(adhd ~ Effective.Minimum.Wage +
#                             age + sex + race_eth + family_struc + adult_edu + nativity +
#                             year + fipsst,
#                           design = design_sub)
# model_min_add_3 <- svyglm(adhd ~ Effective.Minimum.Wage*low_income +
#                             age + sex + race_eth + family_struc + adult_edu + nativity +
#                             year + fipsst,
#                           design = design_sub)
# model_min_add_4 <- svyglm(adhd ~ Effective.Minimum.Wage.2020.Dollars +
#                             age + sex + race_eth + family_struc + adult_edu + nativity +
#                             year + fipsst,
#                           design = design_sub)
# model_min_add_5 <- svyglm(adhd ~ lag_by_1 +
#                             age + sex + race_eth + family_struc + adult_edu + nativity +
#                             year + fipsst,
#                           design = design_sub)
# model_min_add_6 <- svyglm(adhd ~ Effective.Minimum.Wage*age_cat +
#                             age + sex + race_eth + family_struc + adult_edu + nativity +
#                             year + fipsst,
#                           design = design_sub)
# model_min_add_7 <- svyglm(adhd ~ Effective.Minimum.Wage*race_eth_cat +
#                             age + sex + race_eth + family_struc + adult_edu + nativity +
#                             year + fipsst,
#                           design = design_sub)
# model_min_add_8 <- svyglm(adhd ~ Effective.Minimum.Wage*adult_edu_cat +
#                             age + sex + race_eth + family_struc + adult_edu + nativity +
#                             year + fipsst,
#                           design = design_sub)
# model_min_add_9 <- svyglm(adhd ~ Effective.Minimum.Wage*nativity_cat +
#                             age + sex + race_eth + family_struc + adult_edu + nativity +
#                             year + fipsst,
#                           design = design_sub)

# Models for behavioral problems
model_min_beh_1 <- felm(behavior ~ Effective.Minimum.Wage |
                          year + fipsst | 0 | cluster,
                        data    = merged_sub,
                        weights = merged_sub$weights)
model_min_beh_2 <- felm(behavior ~ Effective.Minimum.Wage +
                          age + sex + race_eth + family_struc + adult_edu + nativity |
                          year + fipsst | 0 | cluster,
                        data    = merged_sub,
                        weights = merged_sub$weights)
model_min_beh_3 <- felm(behavior ~ Effective.Minimum.Wage*low_income +
                          age + sex + race_eth + family_struc + adult_edu + nativity |
                          year + fipsst | 0 | cluster,
                        data    = merged_sub,
                        weights = merged_sub$weights)
model_min_beh_4 <- felm(behavior ~ Effective.Minimum.Wage.2020.Dollars +
                          age + sex + race_eth + family_struc + adult_edu + nativity |
                          year + fipsst | 0 | cluster,
                        data    = merged_sub,
                        weights = merged_sub$weights)
model_min_beh_5 <- felm(behavior ~ lag_by_1 +
                          age + sex + race_eth + family_struc + adult_edu + nativity |
                          year + fipsst | 0 | cluster,
                        data    = merged_sub,
                        weights = merged_sub$weights)
model_min_beh_6 <- felm(behavior ~ Effective.Minimum.Wage*age_cat +
                          age + sex + race_eth + family_struc + adult_edu + nativity |
                          year + fipsst | 0 | cluster,
                        data    = merged_sub,
                        weights = merged_sub$weights)
model_min_beh_7 <- felm(behavior ~ Effective.Minimum.Wage*race_eth_cat +
                          age + sex + race_eth + family_struc + adult_edu + nativity |
                          year + fipsst | 0 | cluster,
                        data    = merged_sub,
                        weights = merged_sub$weights)
model_min_beh_8 <- felm(behavior ~ Effective.Minimum.Wage*adult_edu_cat +
                          age + sex + race_eth + family_struc + adult_edu + nativity |
                          year + fipsst | 0 | cluster,
                        data    = merged_sub,
                        weights = merged_sub$weights)
model_min_beh_9 <- felm(behavior ~ Effective.Minimum.Wage*nativity_cat +
                          age + sex + race_eth + family_struc + adult_edu + nativity |
                          year + fipsst | 0 | cluster,
                        data    = merged_sub,
                        weights = merged_sub$weights)

# model_min_beh_1 <- svyglm(behavior ~ Effective.Minimum.Wage +
#                             year + fipsst,
#                           design = design_sub)
# model_min_beh_2 <- svyglm(behavior ~ Effective.Minimum.Wage +
#                             age + sex + race_eth + family_struc + adult_edu + nativity +
#                             year + fipsst,
#                           design = design_sub)
# model_min_beh_3 <- svyglm(behavior ~ Effective.Minimum.Wage*low_income +
#                             age + sex + race_eth + family_struc + adult_edu + nativity +
#                             year + fipsst,
#                           design = design_sub)
# model_min_beh_4 <- svyglm(behavior ~ Effective.Minimum.Wage.2020.Dollars +
#                             age + sex + race_eth + family_struc + adult_edu + nativity +
#                             year + fipsst,
#                           design = design_sub)
# model_min_beh_5 <- svyglm(behavior ~ lag_by_1 +
#                             age + sex + race_eth + family_struc + adult_edu + nativity +
#                             year + fipsst,
#                           design = design_sub)
# model_min_beh_6 <- svyglm(behavior ~ Effective.Minimum.Wage*age_cat +
#                             age + sex + race_eth + family_struc + adult_edu + nativity +
#                             year + fipsst,
#                           design = design_sub)
# model_min_beh_7 <- svyglm(behavior ~ Effective.Minimum.Wage*race_eth_cat +
#                             age + sex + race_eth + family_struc + adult_edu + nativity +
#                             year + fipsst,
#                           design = design_sub)
# model_min_beh_8 <- svyglm(behavior ~ Effective.Minimum.Wage*adult_edu_cat +
#                             age + sex + race_eth + family_struc + adult_edu + nativity +
#                             year + fipsst,
#                           design = design_sub)
# model_min_beh_9 <- svyglm(behavior ~ Effective.Minimum.Wage*nativity_cat +
#                             age + sex + race_eth + family_struc + adult_edu + nativity +
#                             year + fipsst,
#                           design = design_sub)

# Models for digestive issues
model_min_dig_1 <- felm(stomach_r ~ Effective.Minimum.Wage |
                          year + fipsst | 0 | cluster,
                        data    = merged_sub,
                        weights = merged_sub$weights)
model_min_dig_2 <- felm(stomach_r ~ Effective.Minimum.Wage +
                          age + sex + race_eth + family_struc + adult_edu + nativity |
                          year + fipsst | 0 | cluster,
                        data    = merged_sub,
                        weights = merged_sub$weights)
model_min_dig_3 <- felm(stomach_r ~ Effective.Minimum.Wage*low_income +
                          age + sex + race_eth + family_struc + adult_edu + nativity |
                          year + fipsst | 0 | cluster,
                        data    = merged_sub,
                        weights = merged_sub$weights)
model_min_dig_4 <- felm(stomach_r ~ Effective.Minimum.Wage.2020.Dollars +
                          age + sex + race_eth + family_struc + adult_edu + nativity |
                          year + fipsst | 0 | cluster,
                        data    = merged_sub,
                        weights = merged_sub$weights)
model_min_dig_5 <- felm(stomach_r ~ lag_by_1 +
                          age + sex + race_eth + family_struc + adult_edu + nativity |
                          year + fipsst | 0 | cluster,
                        data    = merged_sub,
                        weights = merged_sub$weights)
model_min_dig_6 <- felm(stomach_r ~ Effective.Minimum.Wage*age_cat +
                          age + sex + race_eth + family_struc + adult_edu + nativity |
                          year + fipsst | 0 | cluster,
                        data    = merged_sub,
                        weights = merged_sub$weights)
model_min_dig_7 <- felm(stomach_r ~ Effective.Minimum.Wage*race_eth_cat +
                          age + sex + race_eth + family_struc + adult_edu + nativity |
                          year + fipsst | 0 | cluster,
                        data    = merged_sub,
                        weights = merged_sub$weights)
model_min_dig_8 <- felm(stomach_r ~ Effective.Minimum.Wage*adult_edu_cat +
                          age + sex + race_eth + family_struc + adult_edu + nativity |
                          year + fipsst | 0 | cluster,
                        data    = merged_sub,
                        weights = merged_sub$weights)
model_min_dig_9 <- felm(stomach_r ~ Effective.Minimum.Wage*nativity_cat +
                          age + sex + race_eth + family_struc + adult_edu + nativity |
                          year + fipsst | 0 | cluster,
                        data    = merged_sub,
                        weights = merged_sub$weights)

# model_min_dig_1 <- svyglm(stomach_r ~ Effective.Minimum.Wage +
#                             year + fipsst,
#                           design = design_sub)
# model_min_dig_2 <- svyglm(stomach_r ~ Effective.Minimum.Wage +
#                             age + sex + race_eth + family_struc + adult_edu + nativity +
#                             year + fipsst,
#                           design = design_sub)
# model_min_dig_3 <- svyglm(stomach_r ~ Effective.Minimum.Wage*low_income +
#                             age + sex + race_eth + family_struc + adult_edu + nativity +
#                             year + fipsst,
#                           design = design_sub)
# model_min_dig_4 <- svyglm(stomach_r ~ Effective.Minimum.Wage.2020.Dollars +
#                             age + sex + race_eth + family_struc + adult_edu + nativity +
#                             year + fipsst,
#                           design = design_sub)
# model_min_dig_5 <- svyglm(stomach_r ~ lag_by_1 +
#                             age + sex + race_eth + family_struc + adult_edu + nativity +
#                             year + fipsst,
#                           design = design_sub)
# model_min_dig_6 <- svyglm(stomach_r ~ Effective.Minimum.Wage*age_cat +
#                             age + sex + race_eth + family_struc + adult_edu + nativity +
#                             year + fipsst,
#                           design = design_sub)
# model_min_dig_7 <- svyglm(stomach_r ~ Effective.Minimum.Wage*race_eth_cat +
#                             age + sex + race_eth + family_struc + adult_edu + nativity +
#                             year + fipsst,
#                           design = design_sub)
# model_min_dig_8 <- svyglm(stomach_r ~ Effective.Minimum.Wage*adult_edu_cat +
#                             age + sex + race_eth + family_struc + adult_edu + nativity +
#                             year + fipsst,
#                           design = design_sub)
# model_min_dig_9 <- svyglm(stomach_r ~ Effective.Minimum.Wage*nativity_cat +
#                             age + sex + race_eth + family_struc + adult_edu + nativity +
#                             year + fipsst,
#                           design = design_sub)

# Models for unmet health needs (any)
model_min_unm_1 <- felm(unmet_needs ~ Effective.Minimum.Wage |
                          year + fipsst | 0 | cluster,
                        data    = merged_sub,
                        weights = merged_sub$weights)
model_min_unm_2 <- felm(unmet_needs ~ Effective.Minimum.Wage +
                          age + sex + race_eth + family_struc + adult_edu + nativity |
                          year + fipsst | 0 | cluster,
                        data    = merged_sub,
                        weights = merged_sub$weights)
model_min_unm_3 <- felm(unmet_needs ~ Effective.Minimum.Wage*low_income +
                          age + sex + race_eth + family_struc + adult_edu + nativity |
                          year + fipsst | 0 | cluster,
                        data    = merged_sub,
                        weights = merged_sub$weights)
model_min_unm_4 <- felm(unmet_needs ~ Effective.Minimum.Wage.2020.Dollars +
                          age + sex + race_eth + family_struc + adult_edu + nativity |
                          year + fipsst | 0 | cluster,
                        data    = merged_sub,
                        weights = merged_sub$weights)
model_min_unm_5 <- felm(unmet_needs ~ lag_by_1 +
                          age + sex + race_eth + family_struc + adult_edu + nativity |
                          year + fipsst | 0 | cluster,
                        data    = merged_sub,
                        weights = merged_sub$weights)
model_min_unm_6 <- felm(unmet_needs ~ Effective.Minimum.Wage*age_cat +
                          age + sex + race_eth + family_struc + adult_edu + nativity |
                          year + fipsst | 0 | cluster,
                        data    = merged_sub,
                        weights = merged_sub$weights)
model_min_unm_7 <- felm(unmet_needs ~ Effective.Minimum.Wage*race_eth_cat +
                          age + sex + race_eth + family_struc + adult_edu + nativity |
                          year + fipsst | 0 | cluster,
                        data    = merged_sub,
                        weights = merged_sub$weights)
model_min_unm_8 <- felm(unmet_needs ~ Effective.Minimum.Wage*adult_edu_cat +
                          age + sex + race_eth + family_struc + adult_edu + nativity |
                          year + fipsst | 0 | cluster,
                        data    = merged_sub,
                        weights = merged_sub$weights)
model_min_unm_9 <- felm(unmet_needs ~ Effective.Minimum.Wage*nativity_cat +
                          age + sex + race_eth + family_struc + adult_edu + nativity |
                          year + fipsst | 0 | cluster,
                        data    = merged_sub,
                        weights = merged_sub$weights)

# model_min_unm_1 <- svyglm(unmet_needs ~ Effective.Minimum.Wage +
#                             year + fipsst,
#                           design = design_sub)
# model_min_unm_2 <- svyglm(unmet_needs ~ Effective.Minimum.Wage +
#                             age + sex + race_eth + family_struc + adult_edu + nativity +
#                             year + fipsst,
#                           design = design_sub)
# model_min_unm_3 <- svyglm(unmet_needs ~ Effective.Minimum.Wage*low_income +
#                             age + sex + race_eth + family_struc + adult_edu + nativity +
#                             year + fipsst,
#                           design = design_sub)
# model_min_unm_4 <- svyglm(unmet_needs ~ Effective.Minimum.Wage.2020.Dollars +
#                             age + sex + race_eth + family_struc + adult_edu + nativity +
#                             year + fipsst,
#                           design = design_sub)
# model_min_unm_5 <- svyglm(unmet_needs ~ lag_by_1 +
#                             age + sex + race_eth + family_struc + adult_edu + nativity +
#                             year + fipsst,
#                           design = design_sub)
# model_min_unm_6 <- svyglm(unmet_needs ~ Effective.Minimum.Wage*age_cat +
#                             age + sex + race_eth + family_struc + adult_edu + nativity +
#                             year + fipsst,
#                           design = design_sub)
# model_min_unm_7 <- svyglm(unmet_needs ~ Effective.Minimum.Wage*race_eth_cat +
#                             age + sex + race_eth + family_struc + adult_edu + nativity +
#                             year + fipsst,
#                           design = design_sub)
# model_min_unm_8 <- svyglm(unmet_needs ~ Effective.Minimum.Wage*adult_edu_cat +
#                             age + sex + race_eth + family_struc + adult_edu + nativity +
#                             year + fipsst,
#                           design = design_sub)
# model_min_unm_9 <- svyglm(unmet_needs ~ Effective.Minimum.Wage*nativity_cat +
#                             age + sex + race_eth + family_struc + adult_edu + nativity +
#                             year + fipsst,
#                           design = design_sub)

# Models for unmet mental health needs
model_min_men_1 <- felm(unmet_mental ~ Effective.Minimum.Wage |
                          year + fipsst | 0 | cluster,
                        data    = merged_sub,
                        weights = merged_sub$weights)
model_min_men_2 <- felm(unmet_mental ~ Effective.Minimum.Wage +
                          age + sex + race_eth + family_struc + adult_edu + nativity |
                          year + fipsst | 0 | cluster,
                        data    = merged_sub,
                        weights = merged_sub$weights)
model_min_men_3 <- felm(unmet_mental ~ Effective.Minimum.Wage*low_income +
                          age + sex + race_eth + family_struc + adult_edu + nativity |
                          year + fipsst | 0 | cluster,
                        data    = merged_sub,
                        weights = merged_sub$weights)
model_min_men_4 <- felm(unmet_mental ~ Effective.Minimum.Wage.2020.Dollars +
                          age + sex + race_eth + family_struc + adult_edu + nativity |
                          year + fipsst | 0 | cluster,
                        data    = merged_sub,
                        weights = merged_sub$weights)
model_min_men_5 <- felm(unmet_mental ~ lag_by_1 +
                          age + sex + race_eth + family_struc + adult_edu + nativity |
                          year + fipsst | 0 | cluster,
                        data    = merged_sub,
                        weights = merged_sub$weights)
model_min_men_6 <- felm(unmet_mental ~ Effective.Minimum.Wage*age_cat +
                          age + sex + race_eth + family_struc + adult_edu + nativity |
                          year + fipsst | 0 | cluster,
                        data    = merged_sub,
                        weights = merged_sub$weights)
model_min_men_7 <- felm(unmet_mental ~ Effective.Minimum.Wage*race_eth_cat +
                          age + sex + race_eth + family_struc + adult_edu + nativity |
                          year + fipsst | 0 | cluster,
                        data    = merged_sub,
                        weights = merged_sub$weights)
model_min_men_8 <- felm(unmet_mental ~ Effective.Minimum.Wage*adult_edu_cat +
                          age + sex + race_eth + family_struc + adult_edu + nativity |
                          year + fipsst | 0 | cluster,
                        data    = merged_sub,
                        weights = merged_sub$weights)
model_min_men_9 <- felm(unmet_mental ~ Effective.Minimum.Wage*nativity_cat +
                          age + sex + race_eth + family_struc + adult_edu + nativity |
                          year + fipsst | 0 | cluster,
                        data    = merged_sub,
                        weights = merged_sub$weights)

# model_min_men_1 <- svyglm(unmet_mental ~ Effective.Minimum.Wage +
#                             year + fipsst,
#                           design = design_sub)
# model_min_men_2 <- svyglm(unmet_mental ~ Effective.Minimum.Wage +
#                             age + sex + race_eth + family_struc + adult_edu + nativity +
#                             year + fipsst,
#                           design = design_sub)
# model_min_men_3 <- svyglm(unmet_mental ~ Effective.Minimum.Wage*low_income +
#                             age + sex + race_eth + family_struc + adult_edu + nativity +
#                             year + fipsst,
#                           design = design_sub)
# model_min_men_4 <- svyglm(unmet_mental ~ Effective.Minimum.Wage.2020.Dollars +
#                             age + sex + race_eth + family_struc + adult_edu + nativity +
#                             year + fipsst,
#                           design = design_sub)
# model_min_men_5 <- svyglm(unmet_mental ~ lag_by_1 +
#                             age + sex + race_eth + family_struc + adult_edu + nativity +
#                             year + fipsst,
#                           design = design_sub)
# model_min_men_6 <- svyglm(unmet_mental ~ Effective.Minimum.Wage*age_cat +
#                             age + sex + race_eth + family_struc + adult_edu + nativity +
#                             year + fipsst,
#                           design = design_sub)
# model_min_men_7 <- svyglm(unmet_mental ~ Effective.Minimum.Wage*race_eth_cat +
#                             age + sex + race_eth + family_struc + adult_edu + nativity +
#                             year + fipsst,
#                           design = design_sub)
# model_min_men_8 <- svyglm(unmet_mental ~ Effective.Minimum.Wage*adult_edu_cat +
#                             age + sex + race_eth + family_struc + adult_edu + nativity +
#                             year + fipsst,
#                           design = design_sub)
# model_min_men_9 <- svyglm(unmet_mental ~ Effective.Minimum.Wage*nativity_cat +
#                             age + sex + race_eth + family_struc + adult_edu + nativity +
#                             year + fipsst,
#                           design = design_sub)

# Models for missing school
model_min_sch_1 <- felm(missed_school ~ Effective.Minimum.Wage |
                          year + fipsst | 0 | cluster,
                        data    = merged_sub,
                        weights = merged_sub$weights)
model_min_sch_2 <- felm(missed_school ~ Effective.Minimum.Wage +
                          age + sex + race_eth + family_struc + adult_edu + nativity |
                          year + fipsst | 0 | cluster,
                        data    = merged_sub,
                        weights = merged_sub$weights)
model_min_sch_3 <- felm(missed_school ~ Effective.Minimum.Wage*low_income +
                          age + sex + race_eth + family_struc + adult_edu + nativity |
                          year + fipsst | 0 | cluster,
                        data    = merged_sub,
                        weights = merged_sub$weights)
model_min_sch_4 <- felm(missed_school ~ Effective.Minimum.Wage.2020.Dollars +
                          age + sex + race_eth + family_struc + adult_edu + nativity |
                          year + fipsst | 0 | cluster,
                        data    = merged_sub,
                        weights = merged_sub$weights)
model_min_sch_5 <- felm(missed_school ~ lag_by_1 +
                          age + sex + race_eth + family_struc + adult_edu + nativity |
                          year + fipsst | 0 | cluster,
                        data    = merged_sub,
                        weights = merged_sub$weights)
model_min_sch_6 <- felm(missed_school ~ Effective.Minimum.Wage*age_cat +
                          age + sex + race_eth + family_struc + adult_edu + nativity |
                          year + fipsst | 0 | cluster,
                        data    = merged_sub,
                        weights = merged_sub$weights)
model_min_sch_7 <- felm(missed_school ~ Effective.Minimum.Wage*race_eth_cat +
                          age + sex + race_eth + family_struc + adult_edu + nativity |
                          year + fipsst | 0 | cluster,
                        data    = merged_sub,
                        weights = merged_sub$weights)
model_min_sch_8 <- felm(missed_school ~ Effective.Minimum.Wage*adult_edu_cat +
                          age + sex + race_eth + family_struc + adult_edu + nativity |
                          year + fipsst | 0 | cluster,
                        data    = merged_sub,
                        weights = merged_sub$weights)
model_min_sch_9 <- felm(missed_school ~ Effective.Minimum.Wage*nativity_cat +
                          age + sex + race_eth + family_struc + adult_edu + nativity |
                          year + fipsst | 0 | cluster,
                        data    = merged_sub,
                        weights = merged_sub$weights)

# model_min_sch_1 <- svyglm(missed_school ~ Effective.Minimum.Wage +
#                             year + fipsst,
#                           design = design_sub)
# model_min_sch_2 <- svyglm(missed_school ~ Effective.Minimum.Wage +
#                             age + sex + race_eth + family_struc + adult_edu + nativity +
#                             year + fipsst,
#                           design = design_sub)
# model_min_sch_3 <- svyglm(missed_school ~ Effective.Minimum.Wage*low_income +
#                             age + sex + race_eth + family_struc + adult_edu + nativity +
#                             year + fipsst,
#                           design = design_sub)
# model_min_sch_4 <- svyglm(missed_school ~ Effective.Minimum.Wage.2020.Dollars +
#                             age + sex + race_eth + family_struc + adult_edu + nativity +
#                             year + fipsst,
#                           design = design_sub)
# model_min_sch_5 <- svyglm(missed_school ~ lag_by_1 +
#                             age + sex + race_eth + family_struc + adult_edu + nativity +
#                             year + fipsst,
#                           design = design_sub)
# model_min_sch_6 <- svyglm(missed_school ~ Effective.Minimum.Wage*age_cat +
#                             age + sex + race_eth + family_struc + adult_edu + nativity +
#                             year + fipsst,
#                           design = design_sub)
# model_min_sch_7 <- svyglm(missed_school ~ Effective.Minimum.Wage*race_eth_cat +
#                             age + sex + race_eth + family_struc + adult_edu + nativity +
#                             year + fipsst,
#                           design = design_sub)
# model_min_sch_8 <- svyglm(missed_school ~ Effective.Minimum.Wage*adult_edu_cat +
#                             age + sex + race_eth + family_struc + adult_edu + nativity +
#                             year + fipsst,
#                           design = design_sub)
# model_min_sch_9 <- svyglm(missed_school ~ Effective.Minimum.Wage*nativity_cat +
#                             age + sex + race_eth + family_struc + adult_edu + nativity +
#                             year + fipsst,
#                           design = design_sub)

# Models for child employment
model_min_job_1 <- felm(child_job ~ Effective.Minimum.Wage |
                          year + fipsst | 0 | cluster,
                        data    = merged_sub,
                        weights = merged_sub$weights)
model_min_job_2 <- felm(child_job ~ Effective.Minimum.Wage +
                          age + sex + race_eth + family_struc + adult_edu + nativity |
                          year + fipsst | 0 | cluster,
                        data    = merged_sub,
                        weights = merged_sub$weights)
model_min_job_3 <- felm(child_job ~ Effective.Minimum.Wage*low_income +
                          age + sex + race_eth + family_struc + adult_edu + nativity |
                          year + fipsst | 0 | cluster,
                        data    = merged_sub,
                        weights = merged_sub$weights)
model_min_job_4 <- felm(child_job ~ Effective.Minimum.Wage.2020.Dollars +
                          age + sex + race_eth + family_struc + adult_edu + nativity |
                          year + fipsst | 0 | cluster,
                        data    = merged_sub,
                        weights = merged_sub$weights)
model_min_job_5 <- felm(child_job ~ lag_by_1 +
                          age + sex + race_eth + family_struc + adult_edu + nativity |
                          year + fipsst | 0 | cluster,
                        data    = merged_sub,
                        weights = merged_sub$weights)
model_min_job_6 <- felm(child_job ~ Effective.Minimum.Wage*age_cat +
                          age + sex + race_eth + family_struc + adult_edu + nativity |
                          year + fipsst | 0 | cluster,
                        data    = merged_sub,
                        weights = merged_sub$weights)
model_min_job_7 <- felm(child_job ~ Effective.Minimum.Wage*race_eth_cat +
                          age + sex + race_eth + family_struc + adult_edu + nativity |
                          year + fipsst | 0 | cluster,
                        data    = merged_sub,
                        weights = merged_sub$weights)
model_min_job_8 <- felm(child_job ~ Effective.Minimum.Wage*adult_edu_cat +
                          age + sex + race_eth + family_struc + adult_edu + nativity |
                          year + fipsst | 0 | cluster,
                        data    = merged_sub,
                        weights = merged_sub$weights)
model_min_job_9 <- felm(child_job ~ Effective.Minimum.Wage*nativity_cat +
                          age + sex + race_eth + family_struc + adult_edu + nativity |
                          year + fipsst | 0 | cluster,
                        data    = merged_sub,
                        weights = merged_sub$weights)

# model_min_job_1 <- svyglm(child_job ~ Effective.Minimum.Wage +
#                             year + fipsst,
#                           design = design_sub)
# model_min_job_2 <- svyglm(child_job ~ Effective.Minimum.Wage +
#                             age + sex + race_eth + family_struc + adult_edu + nativity +
#                             year + fipsst,
#                           design = design_sub)
# model_min_job_3 <- svyglm(child_job ~ Effective.Minimum.Wage*low_income +
#                             age + sex + race_eth + family_struc + adult_edu + nativity +
#                             year + fipsst,
#                           design = design_sub)
# model_min_job_4 <- svyglm(child_job ~ Effective.Minimum.Wage.2020.Dollars +
#                             age + sex + race_eth + family_struc + adult_edu + nativity +
#                             year + fipsst,
#                           design = design_sub)
# model_min_job_5 <- svyglm(child_job ~ lag_by_1 +
#                             age + sex + race_eth + family_struc + adult_edu + nativity +
#                             year + fipsst,
#                           design = design_sub)
# model_min_job_6 <- svyglm(child_job ~ Effective.Minimum.Wage*age_cat +
#                             age + sex + race_eth + family_struc + adult_edu + nativity +
#                             year + fipsst,
#                           design = design_sub)
# model_min_job_7 <- svyglm(child_job ~ Effective.Minimum.Wage*race_eth_cat +
#                             age + sex + race_eth + family_struc + adult_edu + nativity +
#                             year + fipsst,
#                           design = design_sub)
# model_min_job_8 <- svyglm(child_job ~ Effective.Minimum.Wage*adult_edu_cat +
#                             age + sex + race_eth + family_struc + adult_edu + nativity +
#                             year + fipsst,
#                           design = design_sub)
# model_min_job_9 <- svyglm(child_job ~ Effective.Minimum.Wage*nativity_cat +
#                             age + sex + race_eth + family_struc + adult_edu + nativity +
#                             year + fipsst,
#                           design = design_sub)

# Get range of sample sizes
list <- cbind(
  # Depression
  length(model_min_dep_1$residuals), length(model_min_dep_2$residuals),
  length(model_min_dep_3$residuals), length(model_min_dep_4$residuals),
  length(model_min_dep_5$residuals), length(model_min_dep_6$residuals),
  length(model_min_dep_7$residuals),
  
  # Anxiety
  length(model_min_anx_1$residuals), length(model_min_anx_2$residuals),
  length(model_min_anx_3$residuals), length(model_min_anx_4$residuals),
  length(model_min_anx_5$residuals), length(model_min_anx_6$residuals),
  length(model_min_anx_7$residuals),
  
  # Behavioral problems
  length(model_min_beh_1$residuals), length(model_min_beh_2$residuals),
  length(model_min_beh_3$residuals), length(model_min_beh_4$residuals),
  length(model_min_beh_5$residuals), length(model_min_beh_6$residuals),
  length(model_min_beh_7$residuals),
  
  # Digestive issues
  length(model_min_dig_1$residuals), length(model_min_dig_2$residuals),
  length(model_min_dig_3$residuals), length(model_min_dig_4$residuals),
  length(model_min_dig_5$residuals), length(model_min_dig_6$residuals),
  length(model_min_dig_7$residuals),
  
  # 7+ school absences
  length(model_min_sch_1$residuals), length(model_min_sch_2$residuals),
  length(model_min_sch_3$residuals), length(model_min_sch_4$residuals),
  length(model_min_sch_5$residuals), length(model_min_sch_6$residuals),
  length(model_min_sch_7$residuals)
  )
min(list); max(list)

# Extract coefficients of interest
main_vals <- as.data.frame(rbind(
  # Depression
  cbind("Depression", "Diagnoses", "All children (unadjusted)",
        model_min_dep_1$coefficients[1], model_min_dep_1$cse[1]),
  cbind("Depression", "Diagnoses", "All children (adjusted)",
        model_min_dep_2$coefficients[1], model_min_dep_2$cse[1]),
  cbind("Depression", "Diagnoses", "Less than 200% FPL",
        model_min_dep_3$coefficients[1], model_min_dep_3$cse[1]),
  cbind("Depression", "Diagnoses", "Adults with high school or less",
        model_min_dep_8$coefficients[1], model_min_dep_8$cse[1]),
  cbind("Depression", "Diagnoses", "Black or Hispanic/Latino",
        model_min_dep_7$coefficients[1], model_min_dep_7$cse[1]),
  cbind("Depression", "Diagnoses", "First- or second-generation",
        model_min_dep_9$coefficients[1], model_min_dep_9$cse[1]),
  cbind("Depression", "Diagnoses", "Adolescents, age 13-17",
        model_min_dep_6$coefficients[1], model_min_dep_6$cse[1]),
  cbind("Depression", "Diagnoses", "All children (2020 dollars)",
        model_min_dep_4$coefficients[1], model_min_dep_4$cse[1]),
  cbind("Depression", "Diagnoses", "All children (lagged wage)",
        model_min_dep_5$coefficients[1], model_min_dep_5$cse[1]),

  # Anxiety
  cbind("Anxiety", "Diagnoses", "All children (unadjusted)",
        model_min_anx_1$coefficients[1], model_min_anx_1$cse[1]),
  cbind("Anxiety", "Diagnoses", "All children (adjusted)",
        model_min_anx_2$coefficients[1], model_min_anx_2$cse[1]),
  cbind("Anxiety", "Diagnoses", "Less than 200% FPL",
        model_min_anx_3$coefficients[1], model_min_anx_3$cse[1]),
  cbind("Anxiety", "Diagnoses", "Adults with high school or less",
        model_min_anx_8$coefficients[1], model_min_anx_8$cse[1]),
  cbind("Anxiety", "Diagnoses", "Black or Hispanic/Latino",
        model_min_anx_7$coefficients[1], model_min_anx_7$cse[1]),
  cbind("Anxiety", "Diagnoses", "First- or second-generation",
        model_min_anx_9$coefficients[1], model_min_anx_9$cse[1]),
  cbind("Anxiety", "Diagnoses", "Adolescents, age 13-17",
        model_min_anx_6$coefficients[1], model_min_anx_6$cse[1]),
  cbind("Anxiety", "Diagnoses", "All children (2020 dollars)",
        model_min_anx_4$coefficients[1], model_min_anx_4$cse[1]),
  cbind("Anxiety", "Diagnoses", "All children (lagged wage)",
        model_min_anx_5$coefficients[1], model_min_anx_5$cse[1]),

  # ADD/ADHD
  cbind("ADD/ADHD", "Diagnoses", "All children (unadjusted)",
        model_min_add_1$coefficients[1], model_min_add_1$cse[1]),
  cbind("ADD/ADHD", "Diagnoses", "All children (adjusted)",
        model_min_add_2$coefficients[1], model_min_add_2$cse[1]),
  cbind("ADD/ADHD", "Diagnoses", "Less than 200% FPL",
        model_min_add_3$coefficients[1], model_min_add_3$cse[1]),
  cbind("ADD/ADHD", "Diagnoses", "Adults with high school or less",
        model_min_add_8$coefficients[1], model_min_add_8$cse[1]),
  cbind("ADD/ADHD", "Diagnoses", "Black or Hispanic/Latino",
        model_min_add_7$coefficients[1], model_min_add_7$cse[1]),
  cbind("ADD/ADHD", "Diagnoses", "First- or second-generation",
        model_min_add_9$coefficients[1], model_min_add_9$cse[1]),
  cbind("ADD/ADHD", "Diagnoses", "Adolescents, age 13-17",
        model_min_add_6$coefficients[1], model_min_add_6$cse[1]),
  cbind("ADD/ADHD", "Diagnoses", "All children (2020 dollars)",
        model_min_add_4$coefficients[1], model_min_add_4$cse[1]),
  cbind("ADD/ADHD", "Diagnoses", "All children (lagged wage)",
        model_min_add_5$coefficients[1], model_min_add_5$cse[1]),

  # Behavioral problems
  cbind("Behavioral prob.", "Diagnoses", "All children (unadjusted)",
        model_min_beh_1$coefficients[1], model_min_beh_1$cse[1]),
  cbind("Behavioral prob.", "Diagnoses", "All children (adjusted)",
        model_min_beh_2$coefficients[1], model_min_beh_2$cse[1]),
  cbind("Behavioral prob.", "Diagnoses", "Less than 200% FPL",
        model_min_beh_3$coefficients[1], model_min_beh_3$cse[1]),
  cbind("Behavioral prob.", "Diagnoses", "Adults with high school or less",
        model_min_beh_8$coefficients[1], model_min_beh_8$cse[1]),
  cbind("Behavioral prob.", "Diagnoses", "Black or Hispanic/Latino",
        model_min_beh_7$coefficients[1], model_min_beh_7$cse[1]),
  cbind("Behavioral prob.", "Diagnoses", "First- or second-generation",
        model_min_beh_9$coefficients[1], model_min_beh_9$cse[1]),
  cbind("Behavioral prob.", "Diagnoses", "Adolescents, age 13-17",
        model_min_beh_6$coefficients[1], model_min_beh_6$cse[1]),
  cbind("Behavioral prob.", "Diagnoses", "All children (2020 dollars)",
        model_min_beh_4$coefficients[1], model_min_beh_4$cse[1]),
  cbind("Behavioral prob.", "Diagnoses", "All children (lagged wage)",
        model_min_beh_5$coefficients[1], model_min_beh_5$cse[1]),

  # Digestive issues
  cbind("Digestive issues", "Sx.", "All children (unadjusted)",
        model_min_dig_1$coefficients[1], model_min_dig_1$cse[1]),
  cbind("Digestive issues", "Sx.", "All children (adjusted)",
        model_min_dig_2$coefficients[1], model_min_dig_2$cse[1]),
  cbind("Digestive issues", "Sx.", "Less than 200% FPL",
        model_min_dig_3$coefficients[1], model_min_dig_3$cse[1]),
  cbind("Digestive issues", "Sx.", "Adults with high school or less",
        model_min_dig_8$coefficients[1], model_min_dig_8$cse[1]),
  cbind("Digestive issues", "Sx.", "Black or Hispanic/Latino",
        model_min_dig_7$coefficients[1], model_min_dig_7$cse[1]),
  cbind("Digestive issues", "Sx.", "First- or second-generation",
        model_min_dig_9$coefficients[1], model_min_dig_9$cse[1]),
  cbind("Digestive issues", "Sx.", "Adolescents, age 13-17",
        model_min_dig_6$coefficients[1], model_min_dig_6$cse[1]),
  cbind("Digestive issues", "Sx.", "All children (2020 dollars)",
        model_min_dig_4$coefficients[1], model_min_dig_4$cse[1]),
  cbind("Digestive issues", "Sx.", "All children (lagged wage)",
        model_min_dig_5$coefficients[1], model_min_dig_5$cse[1]),

  # Unmet health care needs (any)
  cbind("Any unmet care", "Health care", "All children (unadjusted)",
        model_min_unm_1$coefficients[1], model_min_unm_1$cse[1]),
  cbind("Any unmet care", "Health care", "All children (adjusted)",
        model_min_unm_2$coefficients[1], model_min_unm_2$cse[1]),
  cbind("Any unmet care", "Health care", "Less than 200% FPL",
        model_min_unm_3$coefficients[1], model_min_unm_3$cse[1]),
  cbind("Any unmet care", "Health care", "Adults with high school or less",
        model_min_unm_8$coefficients[1], model_min_unm_8$cse[1]),
  cbind("Any unmet care", "Health care", "Black or Hispanic/Latino",
        model_min_unm_7$coefficients[1], model_min_unm_7$cse[1]),
  cbind("Any unmet care", "Health care", "First- or second-generation",
        model_min_unm_9$coefficients[1], model_min_unm_9$cse[1]),
  cbind("Any unmet care", "Health care", "Adolescents, age 13-17",
        model_min_unm_6$coefficients[1], model_min_unm_6$cse[1]),
  cbind("Any unmet care", "Health care", "All children (2020 dollars)",
        model_min_unm_4$coefficients[1], model_min_unm_4$cse[1]),
  cbind("Any unmet care", "Health care", "All children (lagged wage)",
        model_min_unm_5$coefficients[1], model_min_unm_5$cse[1]),

  # Unmet mental health care
  cbind("Unmet mental care", "Health care", "All children (unadjusted)",
        model_min_men_1$coefficients[1], model_min_men_1$cse[1]),
  cbind("Unmet mental care", "Health care", "All children (adjusted)",
        model_min_men_2$coefficients[1], model_min_men_2$cse[1]),
  cbind("Unmet mental care", "Health care", "Less than 200% FPL",
        model_min_men_3$coefficients[1], model_min_men_3$cse[1]),
  cbind("Unmet mental care", "Health care", "Adults with high school or less",
        model_min_men_8$coefficients[1], model_min_men_8$cse[1]),
  cbind("Unmet mental care", "Health care", "Black or Hispanic/Latino",
        model_min_men_7$coefficients[1], model_min_men_7$cse[1]),
  cbind("Unmet mental care", "Health care", "First- or second-generation",
        model_min_men_9$coefficients[1], model_min_men_9$cse[1]),
  cbind("Unmet mental care", "Health care", "Adolescents, age 13-17",
        model_min_men_6$coefficients[1], model_min_men_6$cse[1]),
  cbind("Unmet mental care", "Health care", "All children (2020 dollars)",
        model_min_men_4$coefficients[1], model_min_men_4$cse[1]),
  cbind("Unmet mental care", "Health care", "All children (lagged wage)",
        model_min_men_5$coefficients[1], model_min_men_5$cse[1]),

  # Missed 1+ week of school
  cbind("7+ school absences", "Life", "All children (unadjusted)",
        model_min_sch_1$coefficients[1], model_min_sch_1$cse[1]),
  cbind("7+ school absences", "Life", "All children (adjusted)",
        model_min_sch_2$coefficients[1], model_min_sch_2$cse[1]),
  cbind("7+ school absences", "Life", "Less than 200% FPL",
        model_min_sch_3$coefficients[1], model_min_sch_3$cse[1]),
  cbind("7+ school absences", "Life", "Adults with high school or less",
        model_min_sch_8$coefficients[1], model_min_sch_8$cse[1]),
  cbind("7+ school absences", "Life", "Black or Hispanic/Latino",
        model_min_sch_7$coefficients[1], model_min_sch_7$cse[1]),
  cbind("7+ school absences", "Life", "First- or second-generation",
        model_min_sch_9$coefficients[1], model_min_sch_9$cse[1]),
  cbind("7+ school absences", "Life", "Adolescents, age 13-17",
        model_min_sch_6$coefficients[1], model_min_sch_6$cse[1]),
  cbind("7+ school absences", "Life", "All children (2020 dollars)",
        model_min_sch_4$coefficients[1], model_min_sch_4$cse[1]),
  cbind("7+ school absences", "Life", "All children (lagged wage)",
        model_min_sch_5$coefficients[1], model_min_sch_5$cse[1]),
  
  # Child employment
  cbind("Child employment", "Life", "All children (unadjusted)",
        model_min_job_1$coefficients[1], model_min_job_1$cse[1]),
  cbind("Child employment", "Life", "All children (adjusted)",
        model_min_job_2$coefficients[1], model_min_job_2$cse[1]),
  cbind("Child employment", "Life", "Less than 200% FPL",
        model_min_job_3$coefficients[1], model_min_job_3$cse[1]),
  cbind("Child employment", "Life", "Adults with high school or less",
        model_min_job_8$coefficients[1], model_min_job_8$cse[1]),
  cbind("Child employment", "Life", "Black or Hispanic/Latino",
        model_min_job_7$coefficients[1], model_min_job_7$cse[1]),
  cbind("Child employment", "Life", "First- or second-generation",
        model_min_job_9$coefficients[1], model_min_job_9$cse[1]),
  cbind("Child employment", "Life", "Adolescents, age 13-17",
        model_min_job_6$coefficients[1], model_min_job_6$cse[1]),
  cbind("Child employment", "Life", "All children (2020 dollars)",
        model_min_job_4$coefficients[1], model_min_job_4$cse[1]),
  cbind("Child employment", "Life", "All children (lagged wage)",
        model_min_job_5$coefficients[1], model_min_job_5$cse[1])
  
  # cbind("Child employment",    "All children (unadjusted)",
  #       model_min_job_1$coefficients[2], SE(model_min_job_1)[2]),
  # cbind("Child employment",    "All children (adjusted)",
  #       model_min_job_2$coefficients[2], SE(model_min_job_2)[2]),
  # cbind("Child employment",    "Less than 200% FPL",
  #       model_min_job_3$coefficients[2], SE(model_min_job_3)[2]),
  # cbind("Child employment",    "Adults with high school or less",
  #       model_min_job_8$coefficients[2], SE(model_min_job_8)[2]),
  # cbind("Child employment",    "Black or Hispanic/Latino",
  #       model_min_job_7$coefficients[2], SE(model_min_job_7)[2]),
  # cbind("Child employment",    "First- or second-generation",
  #       model_min_job_9$coefficients[2], SE(model_min_job_9)[2]),
  # cbind("Child employment",    "Adolescents, age 13-17",
  #       model_min_job_6$coefficients[2], SE(model_min_job_6)[2]),
  # cbind("Child employment",    "All children (2020 dollars)",
  #       model_min_job_4$coefficients[2], SE(model_min_job_4)[2]),
  # cbind("Child employment",    "All children (lagged wage)",
  #       model_min_job_5$coefficients[2], SE(model_min_job_5)[2])
))
colnames(main_vals) <- c("Outcome", "Category", "Sample", "effect", "se")

# Reorder factor variables
main_vals$Outcome <- factor(main_vals$Outcome, levels=c("Depression", "Anxiety", "ADD/ADHD", "Behavioral prob.", "Digestive issues", "Any unmet care", "Unmet mental care", "7+ school absences", "Child employment"))
main_vals$Category <- factor(main_vals$Category, levels=c("Diagnoses", "Sx.", "Health care", "Life"))
main_vals$Sample <- factor(main_vals$Sample, levels=c("All children (unadjusted)", "All children (adjusted)", "Less than 200% FPL", "Adults with high school or less", "Black or Hispanic/Latino", "First- or second-generation", "Adolescents, age 13-17", "All children (2020 dollars)", "All children (lagged wage)"))

# Treat columns as numeric
main_vals$effect <- as.numeric(main_vals$effect)
main_vals$se     <- as.numeric(main_vals$se)

# Generate coefficient plot
plot_main <- ggplot(subset(main_vals, Sample %in% c("All children (unadjusted)",
                                                    "All children (adjusted)")),
                    aes(x=Outcome, y=effect, group=Sample, color=Sample)) +
  geom_point(position = position_dodge(width=0.6), size=1, aes(shape=Sample)) +
  scale_shape_manual(values = 1:nlevels(main_vals$Sample)) +
  geom_errorbar(aes(ymin=effect-1.96*se, ymax=effect+1.96*se),
                position = position_dodge(width=0.6), width=0.4) +
  ylab("Effect of $1 increase in minimum wage\non mental health outcomes") +
  theme_test() +
  theme(legend.position = "right",
        text = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(angle=45, hjust=1, vjust=1),
        axis.title.x = element_blank(),
        axis.ticks = element_blank(),
        strip.background = element_blank(),
        legend.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color="light gray", linewidth=0.5),
        panel.grid.minor.y = element_line(color="light gray", linewidth=0.25)) +
  geom_hline(yintercept=0, color="black", linewidth=0.25) +
  scale_y_continuous(limits = c(-0.05, 0.05),
                     breaks = seq(-0.04, 0.04, 0.02),
                     minor_breaks = seq(-0.05, 0.05, 0.01),
                     labels = function(x) paste0(x*100," pp")) +
  scale_color_grey(start=0, end=0.7) +
  ggtitle("All children (3-17), 2016-2020") +
  facet_grid(~Category, scales="free", space="free_x")
print(plot_main)

# Export figure
ggsave(plot=plot_main, file="Coefficient plot, main.pdf", width=6, height=4, units='in', dpi=600)

##############################################################################
# Robustness check: Logistic regression
##############################################################################

# Models for depression
log_min_dep_1 <- svyglm(depression ~ Effective.Minimum.Wage +
                          year + fipsst,
                        design = design_sub, family="quasibinomial")
log_min_dep_2 <- svyglm(depression ~ Effective.Minimum.Wage +
                          age + sex + race_eth + family_struc + adult_edu + nativity +
                          year + fipsst,
                        design = design_sub, family="quasibinomial")

# Models for anxiety
log_min_anx_1 <- svyglm(anxiety ~ Effective.Minimum.Wage +
                          year + fipsst,
                        design = design_sub, family="quasibinomial")
log_min_anx_2 <- svyglm(anxiety ~ Effective.Minimum.Wage +
                          age + sex + race_eth + family_struc + adult_edu + nativity +
                          year + fipsst,
                        design = design_sub, family="quasibinomial")

# Models for ADD/ADHD
log_min_add_1 <- svyglm(adhd ~ Effective.Minimum.Wage +
                          year + fipsst,
                        design = design_sub, family="quasibinomial")
log_min_add_2 <- svyglm(adhd ~ Effective.Minimum.Wage +
                          age + sex + race_eth + family_struc + adult_edu + nativity +
                          year + fipsst,
                        design = design_sub, family="quasibinomial")

# Models for behavioral problems
log_min_beh_1 <- svyglm(behavior ~ Effective.Minimum.Wage +
                          year + fipsst,
                        design = design_sub, family="quasibinomial")
log_min_beh_2 <- svyglm(behavior ~ Effective.Minimum.Wage +
                          age + sex + race_eth + family_struc + adult_edu + nativity +
                          year + fipsst,
                        design = design_sub, family="quasibinomial")

# Models for digestive issues
log_min_dig_1 <- svyglm(stomach_r ~ Effective.Minimum.Wage +
                          year + fipsst,
                        design = design_sub, family="quasibinomial")
log_min_dig_2 <- svyglm(stomach_r ~ Effective.Minimum.Wage +
                          age + sex + race_eth + family_struc + adult_edu + nativity +
                          year + fipsst,
                        design = design_sub, family="quasibinomial")

# Models for unmet health needs (any)
log_min_unm_1 <- svyglm(unmet_needs ~ Effective.Minimum.Wage +
                          year + fipsst,
                        design = design_sub, family="quasibinomial")
log_min_unm_2 <- svyglm(unmet_needs ~ Effective.Minimum.Wage +
                          age + sex + race_eth + family_struc + adult_edu + nativity +
                          year + fipsst,
                        design = design_sub, family="quasibinomial")

# Models for unmet mental health needs
log_min_men_1 <- svyglm(unmet_mental ~ Effective.Minimum.Wage +
                          year + fipsst,
                        design = design_sub, family="quasibinomial")
log_min_men_2 <- svyglm(unmet_mental ~ Effective.Minimum.Wage +
                          age + sex + race_eth + family_struc + adult_edu + nativity +
                          year + fipsst,
                        design = design_sub, family="quasibinomial")

# Models for missing school
log_min_sch_1 <- svyglm(missed_school ~ Effective.Minimum.Wage +
                          year + fipsst,
                        design = design_sub, family="quasibinomial")
log_min_sch_2 <- svyglm(missed_school ~ Effective.Minimum.Wage +
                          age + sex + race_eth + family_struc + adult_edu + nativity +
                          year + fipsst,
                        design = design_sub, family="quasibinomial")

# Extract coefficients of interest
logistic_vals <- as.data.frame(rbind(
  # Depression
  cbind("Depression",       "All children (unadjusted)",
        exp(coef(log_min_dep_1))[2],
        exp(confint(log_min_dep_1))[2,1], exp(confint(log_min_dep_1))[2,2]),
  cbind("Depression",       "All children (adjusted)",
        exp(coef(log_min_dep_2))[2],
        exp(confint(log_min_dep_2))[2,1], exp(confint(log_min_dep_2))[2,2]),
  
  # Anxiety
  cbind("Anxiety",          "All children (unadjusted)",
        exp(coef(log_min_anx_1))[2],
        exp(confint(log_min_anx_1))[2,1], exp(confint(log_min_anx_1))[2,2]),
  cbind("Anxiety",          "All children (adjusted)",
        exp(coef(log_min_anx_2))[2],
        exp(confint(log_min_anx_2))[2,1], exp(confint(log_min_anx_2))[2,2]),
  
  # ADD/ADHD
  cbind("ADD/ADHD",          "All children (unadjusted)",
        exp(coef(log_min_add_1))[2],
        exp(confint(log_min_add_1))[2,1], exp(confint(log_min_add_1))[2,2]),
  cbind("ADD/ADHD",          "All children (adjusted)",
        exp(coef(log_min_add_2))[2],
        exp(confint(log_min_add_2))[2,1], exp(confint(log_min_add_2))[2,2]),
  
  # Behavioral problems
  cbind("Behavioral prob.", "All children (unadjusted)",
        exp(coef(log_min_beh_1))[2],
        exp(confint(log_min_beh_1))[2,1], exp(confint(log_min_beh_1))[2,2]),
  cbind("Behavioral prob.", "All children (adjusted)",
        exp(coef(log_min_beh_2))[2],
        exp(confint(log_min_beh_2))[2,1], exp(confint(log_min_beh_2))[2,2]),
  
  # Digestive issues
  cbind("Digestive issues", "All children (unadjusted)",
        exp(coef(log_min_dig_1))[2],
        exp(confint(log_min_dig_1))[2,1], exp(confint(log_min_dig_1))[2,2]),
  cbind("Digestive issues", "All children (adjusted)",
        exp(coef(log_min_dig_2))[2],
        exp(confint(log_min_dig_2))[2,1], exp(confint(log_min_dig_2))[2,2]),
  
  # Unmet health care needs (any)
  cbind("Any unmet care", "All children (unadjusted)",
        exp(coef(log_min_unm_1))[2],
        exp(confint(log_min_unm_1))[2,1], exp(confint(log_min_unm_1))[2,2]),
  cbind("Any unmet care", "All children (adjusted)",
        exp(coef(log_min_unm_2))[2],
        exp(confint(log_min_unm_2))[2,1], exp(confint(log_min_unm_2))[2,2]),
  
  # Unmet mental health care
  cbind("Unmet mental care", "All children (unadjusted)",
        exp(coef(log_min_men_1))[2],
        exp(confint(log_min_men_1))[2,1], exp(confint(log_min_men_1))[2,2]),
  cbind("Unmet mental care", "All children (adjusted)",
        exp(coef(log_min_men_2))[2],
        exp(confint(log_min_men_2))[2,1], exp(confint(log_min_men_2))[2,2]),
  
  # Missed 1+ week of school
  cbind("7+ school absences",    "All children (unadjusted)",
        exp(coef(log_min_sch_1))[2],
        exp(confint(log_min_sch_1))[2,1], exp(confint(log_min_sch_1))[2,2]),
  cbind("7+ school absences",    "All children (adjusted)",
        exp(coef(log_min_sch_2))[2],
        exp(confint(log_min_sch_2))[2,1], exp(confint(log_min_sch_2))[2,2])
))
colnames(logistic_vals) <- c("Outcome", "Sample", "or", "conf_low", "conf_high")

# Reorder factor variables
logistic_vals$Outcome <- factor(logistic_vals$Outcome, levels=c("Depression", "Anxiety", "ADD/ADHD", "Behavioral prob.", "Digestive issues", "Any unmet care", "Unmet mental care", "7+ school absences"))
logistic_vals$Sample <- factor(logistic_vals$Sample, levels=c("All children (unadjusted)", "All children (adjusted)", "Less than 200% FPL", "Black or Hispanic/Latino", "Adolescents, age 13-17", "All children (2020 dollars)", "All children (lagged wage)", "All children (lifetime wage, infl. adj.)"))

# Treat columns as numeric
logistic_vals$or        <- as.numeric(logistic_vals$or)
logistic_vals$conf_low  <- as.numeric(logistic_vals$conf_low)
logistic_vals$conf_high <- as.numeric(logistic_vals$conf_high)

# Generate coefficient plot
plot_int <- ggplot(logistic_vals, aes(x=Outcome, y=or,
                                         group=Sample, color=Sample)) +
  geom_point(position = position_dodge(width=0.6), size=1, aes(shape=Sample)) +
  scale_shape_manual(values = 1:nlevels(logistic_vals$Sample)) +
  geom_errorbar(aes(ymin=conf_low, ymax=conf_high),
                position = position_dodge(width=0.6), width=0.4) +
  ylab("Effect of $1 increase in minimum wage\non mental health outcomes (odds ratio)") +
  theme_test() +
  theme(legend.position = "right",
        text = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(angle=45, hjust=1, vjust=1),
        axis.title.x = element_blank(),
        axis.ticks = element_blank(),
        strip.background = element_blank(),
        legend.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color="light gray", linewidth=0.5),
        panel.grid.minor.y = element_line(color="light gray", linewidth=0.25)) +
  geom_hline(yintercept=1, color="black", linewidth=0.25) +
  scale_y_continuous(trans = "log10", limits=c(0.25,4),
                     labels=c(0.25,0.5,1,2,4),
                     breaks=c(0.25,0.5,1,2,4),
                     minor_breaks = seq(0.25,10,0.25)) +
  scale_color_grey(start=0, end=0.7)

# Export figure
ggsave(plot=plot_int, file="Coefficient plot, logistic.pdf", width=6, height=4, units='in', dpi=600)

##############################################################################
# Effect of lifetime minimum wage on mental health outcomes
##############################################################################

# Models for depression
life_min_dep_1 <- svyglm(depression ~ wage_life +
                           age + year + fipsst,
                         design = design_sub)
life_min_dep_2 <- svyglm(depression ~ wage_life +
                           age + sex + race_eth + family_struc + adult_edu + nativity +
                           year + fipsst,
                         design = design_sub)
life_min_dep_3 <- svyglm(depression ~ wage_life*low_income +
                           age + sex + race_eth + family_struc + adult_edu + nativity +
                           year + fipsst,
                         design = design_sub)
life_min_dep_6 <- svyglm(depression ~ wage_life*age_cat +
                           age + sex + race_eth + family_struc + adult_edu + nativity +
                           year + fipsst,
                         design = design_sub)
life_min_dep_7 <- svyglm(depression ~ wage_life*race_eth_cat +
                           age + sex + race_eth + family_struc + adult_edu + nativity +
                           year + fipsst,
                         design = design_sub)
life_min_dep_8 <- svyglm(depression ~ wage_life*adult_edu_cat +
                           age + sex + race_eth + family_struc + adult_edu + nativity +
                           year + fipsst,
                         design = design_sub)
life_min_dep_9 <- svyglm(depression ~ wage_life*nativity_cat +
                           age + sex + race_eth + family_struc + adult_edu + nativity +
                           year + fipsst,
                         design = design_sub)

# Models for anxiety
life_min_anx_1 <- svyglm(anxiety ~ wage_life +
                           age + year + fipsst,
                         design = design_sub)
life_min_anx_2 <- svyglm(anxiety ~ wage_life +
                           age + sex + race_eth + family_struc + adult_edu + nativity +
                           year + fipsst,
                         design = design_sub)
life_min_anx_3 <- svyglm(anxiety ~ wage_life*low_income +
                           age + sex + race_eth + family_struc + adult_edu + nativity +
                           year + fipsst,
                         design = design_sub)
life_min_anx_6 <- svyglm(anxiety ~ wage_life*age_cat +
                           age + sex + race_eth + family_struc + adult_edu + nativity +
                           year + fipsst,
                         design = design_sub)
life_min_anx_7 <- svyglm(anxiety ~ wage_life*race_eth_cat +
                           age + sex + race_eth + family_struc + adult_edu + nativity +
                           year + fipsst,
                         design = design_sub)
life_min_anx_8 <- svyglm(anxiety ~ wage_life*adult_edu_cat +
                           age + sex + race_eth + family_struc + adult_edu + nativity +
                           year + fipsst,
                         design = design_sub)
life_min_anx_9 <- svyglm(anxiety ~ wage_life*nativity_cat +
                           age + sex + race_eth + family_struc + adult_edu + nativity +
                           year + fipsst,
                         design = design_sub)

# Models for ADD/ADHD
life_min_add_1 <- svyglm(adhd ~ wage_life +
                           age + year + fipsst,
                         design = design_sub)
life_min_add_2 <- svyglm(adhd ~ wage_life +
                           age + sex + race_eth + family_struc + adult_edu + nativity +
                           year + fipsst,
                         design = design_sub)
life_min_add_3 <- svyglm(adhd ~ wage_life*low_income +
                           age + sex + race_eth + family_struc + adult_edu + nativity +
                           year + fipsst,
                         design = design_sub)
life_min_add_6 <- svyglm(adhd ~ wage_life*age_cat +
                           age + sex + race_eth + family_struc + adult_edu + nativity +
                           year + fipsst,
                         design = design_sub)
life_min_add_7 <- svyglm(adhd ~ wage_life*race_eth_cat +
                           age + sex + race_eth + family_struc + adult_edu + nativity +
                           year + fipsst,
                         design = design_sub)
life_min_add_8 <- svyglm(adhd ~ wage_life*adult_edu_cat +
                           age + sex + race_eth + family_struc + adult_edu + nativity +
                           year + fipsst,
                         design = design_sub)
life_min_add_9 <- svyglm(adhd ~ wage_life*nativity_cat +
                           age + sex + race_eth + family_struc + adult_edu + nativity +
                           year + fipsst,
                         design = design_sub)

# Models for behavioral problems
life_min_beh_1 <- svyglm(behavior ~ wage_life +
                           age + year + fipsst,
                         design = design_sub)
life_min_beh_2 <- svyglm(behavior ~ wage_life +
                           age + sex + race_eth + family_struc + adult_edu + nativity +
                           year + fipsst,
                         design = design_sub)
life_min_beh_3 <- svyglm(behavior ~ wage_life*low_income +
                           age + sex + race_eth + family_struc + adult_edu + nativity +
                           year + fipsst,
                         design = design_sub)
life_min_beh_6 <- svyglm(behavior ~ wage_life*age_cat +
                           age + sex + race_eth + family_struc + adult_edu + nativity +
                           year + fipsst,
                         design = design_sub)
life_min_beh_7 <- svyglm(behavior ~ wage_life*race_eth_cat +
                           age + sex + race_eth + family_struc + adult_edu + nativity +
                           year + fipsst,
                         design = design_sub)
life_min_beh_8 <- svyglm(behavior ~ wage_life*adult_edu_cat +
                           age + sex + race_eth + family_struc + adult_edu + nativity +
                           year + fipsst,
                         design = design_sub)
life_min_beh_9 <- svyglm(behavior ~ wage_life*nativity_cat +
                           age + sex + race_eth + family_struc + adult_edu + nativity +
                           year + fipsst,
                         design = design_sub)

# Models for digestive issues
life_min_dig_1 <- svyglm(stomach_r ~ wage_life +
                           age + year + fipsst,
                         design = design_sub)
life_min_dig_2 <- svyglm(stomach_r ~ wage_life +
                           age + sex + race_eth + family_struc + adult_edu + nativity +
                           year + fipsst,
                         design = design_sub)
life_min_dig_3 <- svyglm(stomach_r ~ wage_life*low_income +
                           age + sex + race_eth + family_struc + adult_edu + nativity +
                           year + fipsst,
                         design = design_sub)
life_min_dig_6 <- svyglm(stomach_r ~ wage_life*age_cat +
                           age + sex + race_eth + family_struc + adult_edu + nativity +
                           year + fipsst,
                         design = design_sub)
life_min_dig_7 <- svyglm(stomach_r ~ wage_life*race_eth_cat +
                           age + sex + race_eth + family_struc + adult_edu + nativity +
                           year + fipsst,
                         design = design_sub)
life_min_dig_8 <- svyglm(stomach_r ~ wage_life*adult_edu_cat +
                           age + sex + race_eth + family_struc + adult_edu + nativity +
                           year + fipsst,
                         design = design_sub)
life_min_dig_9 <- svyglm(stomach_r ~ wage_life*nativity_cat +
                           age + sex + race_eth + family_struc + adult_edu + nativity +
                           year + fipsst,
                         design = design_sub)

# Models for unmet health needs (any)
life_min_unm_1 <- svyglm(unmet_needs ~ wage_life +
                           age + year + fipsst,
                         design = design_sub)
life_min_unm_2 <- svyglm(unmet_needs ~ wage_life +
                           age + sex + race_eth + family_struc + adult_edu + nativity +
                           year + fipsst,
                         design = design_sub)
life_min_unm_3 <- svyglm(unmet_needs ~ wage_life*low_income +
                           age + sex + race_eth + family_struc + adult_edu + nativity +
                           year + fipsst,
                         design = design_sub)
life_min_unm_6 <- svyglm(unmet_needs ~ wage_life*age_cat +
                           age + sex + race_eth + family_struc + adult_edu + nativity +
                           year + fipsst,
                         design = design_sub)
life_min_unm_7 <- svyglm(unmet_needs ~ wage_life*race_eth_cat +
                           age + sex + race_eth + family_struc + adult_edu + nativity +
                           year + fipsst,
                         design = design_sub)
life_min_unm_8 <- svyglm(unmet_needs ~ wage_life*adult_edu_cat +
                           age + sex + race_eth + family_struc + adult_edu + nativity +
                           year + fipsst,
                         design = design_sub)
life_min_unm_9 <- svyglm(unmet_needs ~ wage_life*nativity_cat +
                           age + sex + race_eth + family_struc + adult_edu + nativity +
                           year + fipsst,
                         design = design_sub)

# Models for unmet mental health needs
life_min_men_1 <- svyglm(unmet_mental ~ wage_life +
                           age + year + fipsst,
                         design = design_sub)
life_min_men_2 <- svyglm(unmet_mental ~ wage_life +
                           age + sex + race_eth + family_struc + adult_edu + nativity +
                           year + fipsst,
                         design = design_sub)
life_min_men_3 <- svyglm(unmet_mental ~ wage_life*low_income +
                           age + sex + race_eth + family_struc + adult_edu + nativity +
                           year + fipsst,
                         design = design_sub)
life_min_men_6 <- svyglm(unmet_mental ~ wage_life*age_cat +
                           age + sex + race_eth + family_struc + adult_edu + nativity +
                           year + fipsst,
                         design = design_sub)
life_min_men_7 <- svyglm(unmet_mental ~ wage_life*race_eth_cat +
                           age + sex + race_eth + family_struc + adult_edu + nativity +
                           year + fipsst,
                         design = design_sub)
life_min_men_8 <- svyglm(unmet_mental ~ wage_life*adult_edu_cat +
                           age + sex + race_eth + family_struc + adult_edu + nativity +
                           year + fipsst,
                         design = design_sub)
life_min_men_9 <- svyglm(unmet_mental ~ wage_life*nativity_cat +
                           age + sex + race_eth + family_struc + adult_edu + nativity +
                           year + fipsst,
                         design = design_sub)

# Models for missing school
life_min_sch_1 <- svyglm(missed_school ~ wage_life +
                           age + year + fipsst,
                         design = design_sub)
life_min_sch_2 <- svyglm(missed_school ~ wage_life +
                           age + sex + race_eth + family_struc + adult_edu + nativity +
                           year + fipsst,
                         design = design_sub)
life_min_sch_3 <- svyglm(missed_school ~ wage_life*low_income +
                           age + sex + race_eth + family_struc + adult_edu + nativity +
                           year + fipsst,
                         design = design_sub)
life_min_sch_6 <- svyglm(missed_school ~ wage_life*age_cat +
                           age + sex + race_eth + family_struc + adult_edu + nativity +
                           year + fipsst,
                         design = design_sub)
life_min_sch_7 <- svyglm(missed_school ~ wage_life*race_eth_cat +
                           age + sex + race_eth + family_struc + adult_edu + nativity +
                           year + fipsst,
                         design = design_sub)
life_min_sch_8 <- svyglm(missed_school ~ wage_life*adult_edu_cat +
                           age + sex + race_eth + family_struc + adult_edu + nativity +
                           year + fipsst,
                         design = design_sub)
life_min_sch_9 <- svyglm(missed_school ~ wage_life*nativity_cat +
                           age + sex + race_eth + family_struc + adult_edu + nativity +
                           year + fipsst,
                         design = design_sub)

# Extract coefficients of interest
life_vals <- as.data.frame(rbind(
  # Depression
  cbind("Depression",       "All children (unadjusted)",
        life_min_dep_1$coefficients[2], SE(life_min_dep_1)[2]),
  cbind("Depression",       "All children (adjusted)",
        life_min_dep_2$coefficients[2], SE(life_min_dep_2)[2]),
  cbind("Depression",       "Less than 200% FPL",
        life_min_dep_3$coefficients[2], SE(life_min_dep_3)[2]),
  cbind("Depression",       "Adults with high school or less",
        life_min_dep_8$coefficients[2], SE(life_min_dep_8)[2]),
  cbind("Depression",       "Black or Hispanic/Latino",
        life_min_dep_7$coefficients[2], SE(life_min_dep_7)[2]),
  cbind("Depression",       "First- or second-generation",
        life_min_dep_9$coefficients[2], SE(life_min_dep_9)[2]),
  cbind("Depression",       "Adolescents, age 13-17",
        life_min_dep_6$coefficients[2], SE(life_min_dep_6)[2]),
  
  # Anxiety
  cbind("Anxiety",          "All children (unadjusted)",
        life_min_anx_1$coefficients[2], SE(life_min_anx_1)[2]),
  cbind("Anxiety",          "All children (adjusted)",
        life_min_anx_2$coefficients[2], SE(life_min_anx_2)[2]),
  cbind("Anxiety",          "Less than 200% FPL",
        life_min_anx_3$coefficients[2], SE(life_min_anx_3)[2]),
  cbind("Anxiety",          "Adults with high school or less",
        life_min_anx_8$coefficients[2], SE(life_min_anx_8)[2]),
  cbind("Anxiety",          "Black or Hispanic/Latino",
        life_min_anx_7$coefficients[2], SE(life_min_anx_7)[2]),
  cbind("Anxiety",          "First- or second-generation",
        life_min_anx_9$coefficients[2], SE(life_min_anx_9)[2]),
  cbind("Anxiety",          "Adolescents, age 13-17",
        life_min_anx_6$coefficients[2], SE(life_min_anx_6)[2]),
  
  # ADD/ADHD
  cbind("ADD/ADHD",          "All children (unadjusted)",
        life_min_add_1$coefficients[2], SE(life_min_add_1)[2]),
  cbind("ADD/ADHD",          "All children (adjusted)",
        life_min_add_2$coefficients[2], SE(life_min_add_2)[2]),
  cbind("ADD/ADHD",          "Less than 200% FPL",
        life_min_add_3$coefficients[2], SE(life_min_add_3)[2]),
  cbind("ADD/ADHD",          "Adults with high school or less",
        life_min_add_8$coefficients[2], SE(life_min_add_8)[2]),
  cbind("ADD/ADHD",          "Black or Hispanic/Latino",
        life_min_add_7$coefficients[2], SE(life_min_add_7)[2]),
  cbind("ADD/ADHD",          "First- or second-generation",
        life_min_add_9$coefficients[2], SE(life_min_add_9)[2]),
  cbind("ADD/ADHD",          "Adolescents, age 13-17",
        life_min_add_6$coefficients[2], SE(life_min_add_6)[2]),
  
  # Behavioral problems
  cbind("Behavioral prob.", "All children (unadjusted)",
        life_min_beh_1$coefficients[2], SE(life_min_beh_1)[2]),
  cbind("Behavioral prob.", "All children (adjusted)",
        life_min_beh_2$coefficients[2], SE(life_min_beh_2)[2]),
  cbind("Behavioral prob.", "Less than 200% FPL",
        life_min_beh_3$coefficients[2], SE(life_min_beh_3)[2]),
  cbind("Behavioral prob.", "Adults with high school or less",
        life_min_beh_8$coefficients[2], SE(life_min_beh_8)[2]),
  cbind("Behavioral prob.", "Black or Hispanic/Latino",
        life_min_beh_7$coefficients[2], SE(life_min_beh_7)[2]),
  cbind("Behavioral prob.", "First- or second-generation",
        life_min_beh_9$coefficients[2], SE(life_min_beh_9)[2]),
  cbind("Behavioral prob.", "Adolescents, age 13-17",
        life_min_beh_6$coefficients[2], SE(life_min_beh_6)[2]),
  
  # Digestive issues
  cbind("Digestive issues", "All children (unadjusted)",
        life_min_dig_1$coefficients[2], SE(life_min_dig_1)[2]),
  cbind("Digestive issues", "All children (adjusted)",
        life_min_dig_2$coefficients[2], SE(life_min_dig_2)[2]),
  cbind("Digestive issues", "Less than 200% FPL",
        life_min_dig_3$coefficients[2], SE(life_min_dig_3)[2]),
  cbind("Digestive issues", "Adults with high school or less",
        life_min_dig_8$coefficients[2], SE(life_min_dig_8)[2]),
  cbind("Digestive issues", "Black or Hispanic/Latino",
        life_min_dig_7$coefficients[2], SE(life_min_dig_7)[2]),
  cbind("Digestive issues", "First- or second-generation",
        life_min_dig_9$coefficients[2], SE(life_min_dig_9)[2]),
  cbind("Digestive issues", "Adolescents, age 13-17",
        life_min_dig_6$coefficients[2], SE(life_min_dig_6)[2]),
  
  # Unmet health care needs (any)
  cbind("Any unmet care", "All children (unadjusted)",
        life_min_unm_1$coefficients[2], SE(life_min_unm_1)[2]),
  cbind("Any unmet care", "All children (adjusted)",
        life_min_unm_2$coefficients[2], SE(life_min_unm_2)[2]),
  cbind("Any unmet care", "Less than 200% FPL",
        life_min_unm_3$coefficients[2], SE(life_min_unm_3)[2]),
  cbind("Any unmet care", "Adults with high school or less",
        life_min_unm_8$coefficients[2], SE(life_min_unm_8)[2]),
  cbind("Any unmet care", "Black or Hispanic/Latino",
        life_min_unm_7$coefficients[2], SE(life_min_unm_7)[2]),
  cbind("Any unmet care", "First- or second-generation",
        life_min_unm_9$coefficients[2], SE(life_min_unm_9)[2]),
  cbind("Any unmet care", "Adolescents, age 13-17",
        life_min_unm_6$coefficients[2], SE(life_min_unm_6)[2]),
  
  # Unmet mental health care
  cbind("Unmet mental care", "All children (unadjusted)",
        life_min_men_1$coefficients[2], SE(life_min_men_1)[2]),
  cbind("Unmet mental care", "All children (adjusted)",
        life_min_men_2$coefficients[2], SE(life_min_men_2)[2]),
  cbind("Unmet mental care", "Less than 200% FPL",
        life_min_men_3$coefficients[2], SE(life_min_men_3)[2]),
  cbind("Unmet mental care", "Adults with high school or less",
        life_min_men_8$coefficients[2], SE(life_min_men_8)[2]),
  cbind("Unmet mental care", "Black or Hispanic/Latino",
        life_min_men_7$coefficients[2], SE(life_min_men_7)[2]),
  cbind("Unmet mental care", "First- or second-generation",
        life_min_men_9$coefficients[2], SE(life_min_men_9)[2]),
  cbind("Unmet mental care", "Adolescents, age 13-17",
        life_min_men_6$coefficients[2], SE(life_min_men_6)[2]),
  
  # Missed 1+ week of school
  cbind("7+ school absences",    "All children (unadjusted)",
        life_min_sch_1$coefficients[2], SE(life_min_sch_1)[2]),
  cbind("7+ school absences",    "All children (adjusted)",
        life_min_sch_2$coefficients[2], SE(life_min_sch_2)[2]),
  cbind("7+ school absences",    "Less than 200% FPL",
        life_min_sch_3$coefficients[2], SE(life_min_sch_3)[2]),
  cbind("7+ school absences",    "Adults with high school or less",
        life_min_sch_8$coefficients[2], SE(life_min_sch_8)[2]),
  cbind("7+ school absences",    "Black or Hispanic/Latino",
        life_min_sch_7$coefficients[2], SE(life_min_sch_7)[2]),
  cbind("7+ school absences",    "First- or second-generation",
        life_min_sch_9$coefficients[2], SE(life_min_sch_9)[2]),
  cbind("7+ school absences",    "Adolescents, age 13-17",
        life_min_sch_6$coefficients[2], SE(life_min_sch_6)[2])
))
colnames(life_vals) <- c("Outcome", "Sample", "effect", "se")

# Reorder factor variables
life_vals$Outcome <- factor(life_vals$Outcome, levels=c("Depression", "Anxiety", "ADD/ADHD", "Behavioral prob.", "Digestive issues", "Any unmet care", "Unmet mental care", "7+ school absences"))
life_vals$Sample <- factor(life_vals$Sample, levels=c("All children (unadjusted)", "All children (adjusted)", "Less than 200% FPL", "Adults with high school or less", "Black or Hispanic/Latino", "First- or second-generation", "Adolescents, age 13-17"))

# Treat columns as numeric
life_vals$effect <- as.numeric(life_vals$effect)
life_vals$se     <- as.numeric(life_vals$se)

# Generate coefficient plot
plot_life <- ggplot(life_vals, aes(x=Outcome, y=effect,
                                         group=Sample, color=Sample)) +
  geom_point(position = position_dodge(width=0.6), size=1, aes(shape=Sample)) +
  scale_shape_manual(values = 1:nlevels(life_vals$Sample)) +
  geom_errorbar(aes(ymin=effect-1.96*se, ymax=effect+1.96*se),
                position = position_dodge(width=0.6), width=0.4) +
  ylab("Effect of lifetime $1 increase in minimum\nwage on mental health outcomes") +
  theme_test() +
  theme(legend.position = "right",
        text = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(angle=45, hjust=1, vjust=1),
        axis.title.x = element_blank(),
        axis.ticks = element_blank(),
        strip.background = element_blank(),
        legend.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color="light gray", linewidth=0.5),
        panel.grid.minor.y = element_line(color="light gray", linewidth=0.25)) +
  geom_hline(yintercept=0, color="black", linewidth=0.25) +
  scale_y_continuous(limits = c(-0.05, 0.05),
                     breaks = seq(-0.04, 0.04, 0.02),
                     minor_breaks = seq(-0.05, 0.05, 0.01),
                     labels = function(x) paste0(x*100," pp")) +
  scale_color_grey(start=0, end=0.7)

# Export figure
ggsave(plot=plot_life, file="Coefficient plot, lifetime.pdf", width=8, height=4, units='in', dpi=600)
