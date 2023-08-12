# Minimum wage and children's mental health
# Analyses using the Youth Risk Behavior Surveillance System
# N.M. Kavanagh, M. McConnell, N. Slopen
# August 12, 2023

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

# Read YRBS datasets into R
yrbs_st_am <- read.dta13("YRBS data/sadc_2019_state_a_m.dta")
yrbs_st_nz <- read.dta13("YRBS data/sadc_2019_state_n_z.dta")

# Recode Arizona abbreviation
# Only relevant for A-to-M dataset
yrbs_st_am <- yrbs_st_am %>% mutate(
  sitecode_2 = case_when(
    sitecode == "AZB" ~ "AZ",
    TRUE ~ sitecode
  ))

# Convert state abbreviations to FIPS
# Necessary for a smooth merge
yrbs_st_am$fipsst <- cdlTools::fips(yrbs_st_am$sitecode_2, to="FIPS")
yrbs_st_nz$fipsst <- cdlTools::fips(yrbs_st_nz$sitecode,   to="FIPS")

# Bind all states together
yrbs <- bind_rows(yrbs_st_am, yrbs_st_nz)

# Read minimum wage dataset into R
min_wage_df <- read.csv("https://raw.githubusercontent.com/Lislejoem/Minimum-Wage-by-State-1968-to-2020/master/Minimum%20Wage%20Data.csv")

# Convert state names to FIPS
# Necessary for a smooth merge
min_wage_df$fipsst <- cdlTools::fips(min_wage_df$State, to="FIPS")

# Generate new year variable
min_wage_df$year <- min_wage_df$Year

# Reorder dataset
min_wage_df <- min_wage_df %>% arrange(fipsst, year)

# Generate indicator for above federal minimum
min_wage_df <- min_wage_df %>% mutate(
  above_fed = case_when(
    State.Minimum.Wage > Federal.Minimum.Wage ~ 1,
    TRUE ~ 0
  ))

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
    lag_by_17_inf = lag(Effective.Minimum.Wage.2020.Dollars, n=17, default=NA),
    lag_by_18_inf = lag(Effective.Minimum.Wage.2020.Dollars, n=18, default=NA)
    )

# Merge minimum wage and YRBS data
yrbs_all <- left_join(yrbs, min_wage_df, by=c("fipsst", "year"))

# Clear old datasets from R
rm(yrbs_st_am, yrbs_st_nz)

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
yrbs_all <- left_join(yrbs_all, medicaid_1_5_df,  by=c("fipsst", "year"))
yrbs_all <- left_join(yrbs_all, medicaid_6_18_df, by=c("fipsst", "year"))

##############################################################################
# Supplemental EITC dataset preparation
##############################################################################

# Read EITC dataset into R
eitc <- read.csv("Supplemental data/eitc_state_year.csv")

# Merge EITC and YRBS data
yrbs_all <- left_join(yrbs_all, eitc, by=c("fipsst", "year"))

##############################################################################
# Supplemental TANF dataset preparation
##############################################################################

# Read TANF dataset into R
tanf <- read.csv("Supplemental data/tanf_state_year.csv")

# Merge TANF and YRBS data
yrbs_all <- left_join(yrbs_all, tanf, by=c("fipsst", "year"))

##############################################################################
# Variable preparation
##############################################################################

# Treat fixed effects as factors
yrbs_all$year   <- as.factor(yrbs_all$year)
yrbs_all$fipsst <- as.factor(yrbs_all$fipsst)

# Child's age
yrbs_all <- yrbs_all %>% mutate(
  age_2 = case_when(
    age == 1 ~ "12 years old or younger",
    age == 2 ~ "13 years old",
    age == 3 ~ "14 years old",
    age == 4 ~ "15 years old",
    age == 5 ~ "16 years old",
    age == 6 ~ "17 years old",
    age == 7 ~ "18 years old or older"
  ))

# Child's sex
yrbs_all <- yrbs_all %>% mutate(
  sex_2 = case_when(
    sex == 1 ~ "Female",
    sex == 2 ~ "Male"
  ))
yrbs_all$sex_2 <- factor(yrbs_all$sex_2, levels = c("Male", "Female"))

# Child's race/ethnicity
yrbs_all <- yrbs_all %>% mutate(
  race_eth_2 = case_when(
    race7 == 1        ~ "American Indian or Alaska Native",
    race7 %in% c(2,5) ~ "Asian, Native Hawaiian, or Pacific Islander",
    race7 == 3        ~ "Black, non-Hispanic/Latino",
    race7 == 4        ~ "Hispanic/Latino",
    race7 == 6        ~ "White, non-Hispanic/Latino",
    race7 == 7        ~ "Other or mixed race"
  ))
yrbs_all$race_eth_2 <- factor(yrbs_all$race_eth_2, levels = c("White, non-Hispanic/Latino", "Black, non-Hispanic/Latino", "Hispanic/Latino", "American Indian or Alaska Native", "Asian, Native Hawaiian, or Pacific Islander", "Other or mixed race"))

# Dichotomoize race/ethnicity
# Black/Latino vs. other for interaction models
yrbs_all <- yrbs_all %>% mutate(
  race_eth_cat = case_when(
    race7 %in% c(3:4)     ~ 0, # Black or Hispanic/Latino
    race7 %in% c(1:2,5:7) ~ 1  # Other races
  ))

# Child's educational grade
yrbs_all <- yrbs_all %>% mutate(
  grade_2 = case_when(
    grade == 1 ~ "9th grade",
    grade == 2 ~ "10th grade",
    grade == 3 ~ "11th grade",
    grade == 4 ~ "12th grade"
  ))
yrbs_all$grade_2 <- factor(yrbs_all$grade_2, levels = c("9th grade", "10th grade", "11th grade", "12th grade"))

# Sad or hopeless for 2+ weeks
yrbs_all <- yrbs_all %>% mutate(
  sad_hopeless = case_when(
    q25 == 1 ~ 1,
    q25 == 2 ~ 0
  ))

# Seriously considered suicide
yrbs_all <- yrbs_all %>% mutate(
  cons_suicide = case_when(
    q26 == 1 ~ 1,
    q26 == 2 ~ 0
  ))

# Made suicide attempt
yrbs_all <- yrbs_all %>% mutate(
  suicide_att = case_when(
    q28 %in% c(2:5) ~ 1,
    q28 == 1        ~ 0
  ))

# Alcohol in past 30 days
yrbs_all <- yrbs_all %>% mutate(
  alcohol = case_when(
    q41 %in% c(2:7) ~ 1,
    q41 == 1        ~ 0
  ))

# Marijuana in past 30 days
yrbs_all <- yrbs_all %>% mutate(
  marijuana = case_when(
    q47 %in% c(2:6) ~ 1,
    q47 == 1        ~ 0
  ))

# Physical fight
yrbs_all <- yrbs_all %>% mutate(
  fight = case_when(
    q17 %in% c(2:8) ~ 1,
    q17 == 1        ~ 0
  ))

# Lifetime minimum wage
yrbs_all <- yrbs_all %>% mutate(
  wage_life = case_when(
    # 12 years or younger
    age == 1 ~ (Effective.Minimum.Wage.2020.Dollars + lag_by_1_inf +
                  lag_by_2_inf +  lag_by_3_inf +  lag_by_4_inf + lag_by_5_inf +
                  lag_by_6_inf +  lag_by_7_inf +  lag_by_8_inf + lag_by_9_inf +
                  lag_by_10_inf + lag_by_11_inf + lag_by_12_inf)/13,
    
    # 13 years
    age == 2 ~ (Effective.Minimum.Wage.2020.Dollars + lag_by_1_inf +
                  lag_by_2_inf +  lag_by_3_inf +  lag_by_4_inf +  lag_by_5_inf +
                  lag_by_6_inf +  lag_by_7_inf +  lag_by_8_inf +  lag_by_9_inf +
                  lag_by_10_inf + lag_by_11_inf + lag_by_12_inf + lag_by_13_inf)/14,
    
    # 14 years
    age == 3 ~ (Effective.Minimum.Wage.2020.Dollars + lag_by_1_inf +
                  lag_by_2_inf +  lag_by_3_inf +  lag_by_4_inf +  lag_by_5_inf +
                  lag_by_6_inf +  lag_by_7_inf +  lag_by_8_inf +  lag_by_9_inf +
                  lag_by_10_inf + lag_by_11_inf + lag_by_12_inf + lag_by_13_inf +
                  lag_by_14_inf)/15,
    
    # 15 years
    age == 4 ~ (Effective.Minimum.Wage.2020.Dollars + lag_by_1_inf +
                  lag_by_2_inf +  lag_by_3_inf +  lag_by_4_inf +  lag_by_5_inf +
                  lag_by_6_inf +  lag_by_7_inf +  lag_by_8_inf +  lag_by_9_inf +
                  lag_by_10_inf + lag_by_11_inf + lag_by_12_inf + lag_by_13_inf +
                  lag_by_14_inf + lag_by_15_inf)/16,
    
    # 16 years
    age == 5 ~ (Effective.Minimum.Wage.2020.Dollars + lag_by_1_inf +
                  lag_by_2_inf +  lag_by_3_inf +  lag_by_4_inf +  lag_by_5_inf +
                  lag_by_6_inf +  lag_by_7_inf +  lag_by_8_inf +  lag_by_9_inf +
                  lag_by_10_inf + lag_by_11_inf + lag_by_12_inf + lag_by_13_inf +
                  lag_by_14_inf + lag_by_15_inf + lag_by_16_inf)/17,
    
    # 17 years
    age == 6 ~ (Effective.Minimum.Wage.2020.Dollars + lag_by_1_inf +
                  lag_by_2_inf +  lag_by_3_inf +  lag_by_4_inf +  lag_by_5_inf +
                  lag_by_6_inf +  lag_by_7_inf +  lag_by_8_inf +  lag_by_9_inf +
                  lag_by_10_inf + lag_by_11_inf + lag_by_12_inf + lag_by_13_inf +
                  lag_by_14_inf + lag_by_15_inf + lag_by_16_inf + lag_by_17_inf)/18,
    
    # 18 years or older
    age == 7 ~ (Effective.Minimum.Wage.2020.Dollars + lag_by_1_inf +
                  lag_by_2_inf +  lag_by_3_inf +  lag_by_4_inf +  lag_by_5_inf +
                  lag_by_6_inf +  lag_by_7_inf +  lag_by_8_inf +  lag_by_9_inf +
                  lag_by_10_inf + lag_by_11_inf + lag_by_12_inf + lag_by_13_inf +
                  lag_by_14_inf + lag_by_15_inf + lag_by_16_inf + lag_by_17_inf +
                  lag_by_18_inf)/19
  ))

# State has EITC program
yrbs_all <- yrbs_all %>% mutate(
  has_eitc = case_when(
    federal_pct > 0 ~ 1,
    TRUE ~ 0
  ))

# Rescale weight so mean is 1
yrbs_all$weight_2 <- yrbs_all$weight/66.61

# Generate nested sampling clusters
yrbs_all$cluster <- paste0(yrbs_all$fipsst, "-", yrbs_all$PSU, "-", yrbs_all$stratum)

# Generate age-by-year fixed effects
yrbs_all$age_year <- paste0(yrbs_all$age_2, "-", yrbs_all$year)

##############################################################################
# Table: Demographic characteristics
##############################################################################

# At least 1 outcome
yrbs_all <- yrbs_all %>% mutate(
  has_outcome = case_when(
    !is.na(sad_hopeless) | !is.na(cons_suicide) | !is.na(suicide_att) |
      !is.na(fight) | !is.na(alcohol) | !is.na(marijuana) ~ 1
  ))

# Complete cases for covariates
yrbs_all_model <- yrbs_all %>%
  subset(., year %in% c(2000:2020)) %>%
  filter_at(vars(Effective.Minimum.Wage, has_outcome,
                 age_2, sex_2, race_eth_2, grade_2), all_vars(!is.na(.)))

# Child characteristics: unweighted
summary(tableby(~ age_2 + sex_2 + race_eth_2 + grade_2,
                yrbs_all_model, digits.pct=0), text=T)

# Child characteristics: weighted
summary(tableby(~ age_2 + sex_2 + race_eth_2 + grade_2,
                yrbs_all_model, digits.pct=0, weights=weight_2), text=T)

# Mental health outcomes: weighted
summary(tableby(~ sad_hopeless + cons_suicide + suicide_att +
                  alcohol + marijuana + fight,
                yrbs_all_model, digits.pct=0, weights=weight_2), text=T)

# State participation by year
write.csv(table(yrbs_all_model$st_abbr.x, yrbs_all_model$year),
          "Exhibits/YRBS state participation.csv")

##############################################################################
# Graph: Map of minimum wages
##############################################################################

# Rename states with abbreviations
min_wage_df <- min_wage_df %>% mutate(
  state_map_order = case_when(
    State == "Alabama"        ~ "Ala.",
    State == "Alaska"         ~ "Alaska",
    State == "Arizona"        ~ "Ariz.",
    State == "Arkansas"       ~ "Ark.",
    State == "California"     ~ "Calif.",
    State == "Colorado"       ~ "Colo.",
    State == "Connecticut"    ~ "Conn.",
    State == "Delaware"       ~ "Del.",
    State == "District of Columbia" ~ "D.C.",
    State == "Florida"        ~ "Fla.",
    State == "Georgia"        ~ "Ga.",
    State == "Hawaii"         ~ "Hawaii",
    State == "Idaho"          ~ "Idaho",
    State == "Illinois"       ~ "Ill.",
    State == "Indiana"        ~ "Ind.",
    State == "Iowa"           ~ "Iowa",
    State == "Kansas"         ~ "Kan.",
    State == "Kentucky"       ~ "Ky.",
    State == "Louisiana"      ~ "La.",
    State == "Maine"          ~ "Maine",
    State == "Maryland"       ~ "Md.",
    State == "Massachusetts"  ~ "Mass.",
    State == "Michigan"       ~ "Mich.",
    State == "Minnesota"      ~ "Minn.",
    State == "Mississippi"    ~ "Miss.",
    State == "Missouri"       ~ "Mo.",
    State == "Montana"        ~ "Mont.",
    State == "Nebraska"       ~ "Neb.",
    State == "Nevada"         ~ "Nev.",
    State == "New Hampshire"  ~ "N.H.",
    State == "New Jersey"     ~ "N.J.",
    State == "New Mexico"     ~ "N.M.",
    State == "New York"       ~ "N.Y.",
    State == "North Carolina" ~ "N.C.",
    State == "North Dakota"   ~ "N.D.",
    State == "Ohio"           ~ "Ohio",
    State == "Oklahoma"       ~ "Okla.",
    State == "Oregon"         ~ "Ore.",
    State == "Pennsylvania"   ~ "Pa.",
    State == "Rhode Island"   ~ "R.I.",
    State == "South Carolina" ~ "S.C.",
    State == "South Dakota"   ~ "S.D.",
    State == "Tennessee"      ~ "Tenn.",
    State == "Texas"          ~ "Texas",
    State == "Utah"           ~ "Utah",
    State == "Vermont"        ~ "Vt.",
    State == "Virginia"       ~ "Va.",
    State == "Washington"     ~ "Wash.",
    State == "West Virginia"  ~ "W.Va.",
    State == "Wisconsin"      ~ "Wis.",
    State == "Wyoming"        ~ "Wyo."
  ))

# Generate blanks for empty facets
bl = sapply(1:37, function(n) paste(rep(" ", n), collapse=""))

# Set facet order for states
min_wage_df$state_map_order <-
  factor(min_wage_df$state_map_order,
         levels = c("Alaska", bl[2:10], "Maine", bl[11:19], "Vt.", "N.H.",
                    "Wash.", "Idaho", "Mont.", "N.D.", "Minn.", "Ill.", "Wis.", "Mich.", "N.Y.", "R.I.", "Mass.",
                    "Ore.", "Nev.", "Wyo.", "S.D.", "Iowa", "Ind.", "Ohio", "Pa.", "N.J.", "Conn.", bl[20],
                    "Calif.", "Utah", "Colo.", "Neb.", "Mo.", "Ky.", "W.Va.", "Va.", "Md.", "Del.", bl[21],
                    bl[22], "Ariz.", "N.M.", "Kan.", "Ark.", "Tenn.", "N.C.", "S.C.", "D.C.", bl[23:24],
                    bl[25:27], "Okla.", "La.", "Miss.", "Ala.", "Ga.", bl[28:30],
                    "Hawaii", bl[31:32], "Texas", bl[33:36], "Fla."))

# Subset to study period and states
min_wage_df_sub <- subset(min_wage_df, year %in% c(2001:2020) &
                            fipsst %in% c(1:56))

# Plot minimum wages in shape of U.S.
wage_map_plot <- ggplot(min_wage_df_sub, aes(x=year, y=Effective.Minimum.Wage)) +
  geom_line() +
  facet_wrap(~state_map_order, ncol = 11, drop=F, strip.position="bottom") +
  ylab("Effective minimum wage") +
  xlab("Year (2001-2020)") +
  theme_classic() +
  theme(text = element_text(size = 10, face = "bold"),
        axis.text.x = element_blank(),
        strip.background = element_blank(),
        axis.line  = element_blank(),
        axis.ticks = element_blank()) +
  scale_y_continuous(breaks=c(5.15, 14),
                     labels=c("$5.15", "$14"))

# Export figure
ggsave(plot=wage_map_plot, file="Exhibits/Minimum wage map plot.pdf", width=7, height=5, units='in', dpi=600)

##############################################################################
# Functions for TWFE models
##############################################################################

# Function to extract values from TWFE models.
# Requires: Df for saving coefficients, "lfe" model, title of model.
# Returns: Df of values, ready to pass to "clean_coef_df".
make_coef_df <- function(coef_df, model, TITLE) {
  
  # Get outcome and category labels
  if (model$lhs == "sad_hopeless") {
    outcome  <- "Sad or hopeless"
    category <- "Symptoms"}
  
  if (model$lhs == "cons_suicide") {
    outcome  <- "Considered suicide"
    category <- "Symptoms"}
  
  if (model$lhs == "suicide_att") {
    outcome  <- "Attempted suicide"
    category <- "Symptoms"}
  
  if (model$lhs == "alcohol") {
    outcome  <- "Recent alcohol"
    category <- "Substances"}
  
  if (model$lhs == "marijuana") {
    outcome  <- "Recent marijuana"
    category <- "Substances"}
  
  if (model$lhs == "fight") {
    outcome  <- "Physical fight"
    category <- "Violence"}
  
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
    coef_df$Outcome, levels=c("Sad or hopeless", "Considered suicide", "Attempted suicide", "Recent alcohol", "Recent marijuana", "Physical fight"))
  
  # Reorder categories
  coef_df$Category <- factor(
    coef_df$Category, levels=c("Symptoms", "Substances", "Violence"))
  
  # Reorder samples
  coef_df$Sample <- factor(
    coef_df$Sample, levels=c(
      
      # Standard models
      "Adolescents (FE only)",
      "Adolescents (fully adjusted)",
      
      # Sub-population models
      "Black or Hispanic/Latino",
      
      # Robustness checks
      "Adolescents (2020 dollars)",
      "Adolescents (lagged wage)",
      
      # Alternate cluster models
      "Adolescents (FE only, state clust.)",
      "Adolescents (FE only, nested clust.)",
      "Adolescents (fully adj., state clust.)",
      "Adolescents (fully adj., nested clust.)"
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
    ggtitle("Adolescents (12-18), 2001-2019") +
    
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

# Sad or hopeless
model_min_sad_1 <- felm(sad_hopeless ~ Effective.Minimum.Wage |
                          age_year + fipsst | 0 | fipsst,
                        data    = yrbs_all_model,
                        weights = yrbs_all_model$weight_2)
model_min_sad_2 <- felm(sad_hopeless ~ Effective.Minimum.Wage +
                          age_2 + sex_2 + race_eth_2 + grade_2 +
                          elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                          age_year + fipsst | 0 | fipsst,
                        data    = yrbs_all_model,
                        weights = yrbs_all_model$weight_2)

# Considered suicide
model_min_con_1 <- felm(cons_suicide ~ Effective.Minimum.Wage |
                          age_year + fipsst | 0 | fipsst,
                        data    = yrbs_all_model,
                        weights = yrbs_all_model$weight_2)
model_min_con_2 <- felm(cons_suicide ~ Effective.Minimum.Wage +
                          age_2 + sex_2 + race_eth_2 + grade_2 +
                          elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                          age_year + fipsst | 0 | fipsst,
                        data    = yrbs_all_model,
                        weights = yrbs_all_model$weight_2)

# Attempted suicide
model_min_att_1 <- felm(suicide_att ~ Effective.Minimum.Wage |
                          age_year + fipsst | 0 | fipsst,
                        data    = yrbs_all_model,
                        weights = yrbs_all_model$weight_2)
model_min_att_2 <- felm(suicide_att ~ Effective.Minimum.Wage +
                          age_2 + sex_2 + race_eth_2 + grade_2 +
                          elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                          age_year + fipsst | 0 | fipsst,
                        data    = yrbs_all_model,
                        weights = yrbs_all_model$weight_2)

# Recent alcohol
model_min_alc_1 <- felm(alcohol ~ Effective.Minimum.Wage |
                          age_year + fipsst | 0 | fipsst,
                        data    = yrbs_all_model,
                        weights = yrbs_all_model$weight_2)
model_min_alc_2 <- felm(alcohol ~ Effective.Minimum.Wage +
                          age_2 + sex_2 + race_eth_2 + grade_2 +
                          elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                          age_year + fipsst | 0 | fipsst,
                        data    = yrbs_all_model,
                        weights = yrbs_all_model$weight_2)

# Recent marijuana
model_min_mjn_1 <- felm(marijuana ~ Effective.Minimum.Wage |
                          age_year + fipsst | 0 | fipsst,
                        data    = yrbs_all_model,
                        weights = yrbs_all_model$weight_2)
model_min_mjn_2 <- felm(marijuana ~ Effective.Minimum.Wage +
                          age_2 + sex_2 + race_eth_2 + grade_2 +
                          elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                          age_year + fipsst | 0 | fipsst,
                        data    = yrbs_all_model,
                        weights = yrbs_all_model$weight_2)

# Physical fight
model_min_fgh_1 <- felm(fight ~ Effective.Minimum.Wage |
                          age_year + fipsst | 0 | fipsst,
                        data    = yrbs_all_model,
                        weights = yrbs_all_model$weight_2)
model_min_fgh_2 <- felm(fight ~ Effective.Minimum.Wage +
                          age_2 + sex_2 + race_eth_2 + grade_2 +
                          elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                          age_year + fipsst | 0 | fipsst,
                        data    = yrbs_all_model,
                        weights = yrbs_all_model$weight_2)

# Get values from models
main_df <- NULL

# Adolescents (FE only)
main_df <- make_coef_df(main_df, model_min_sad_1, "Adolescents (FE only)")
main_df <- make_coef_df(main_df, model_min_con_1, "Adolescents (FE only)")
main_df <- make_coef_df(main_df, model_min_att_1, "Adolescents (FE only)")
main_df <- make_coef_df(main_df, model_min_alc_1, "Adolescents (FE only)")
main_df <- make_coef_df(main_df, model_min_mjn_1, "Adolescents (FE only)")
main_df <- make_coef_df(main_df, model_min_fgh_1, "Adolescents (FE only)")

# Adolescents (fully adjusted)
main_df <- make_coef_df(main_df, model_min_sad_2, "Adolescents (fully adjusted)")
main_df <- make_coef_df(main_df, model_min_con_2, "Adolescents (fully adjusted)")
main_df <- make_coef_df(main_df, model_min_att_2, "Adolescents (fully adjusted)")
main_df <- make_coef_df(main_df, model_min_alc_2, "Adolescents (fully adjusted)")
main_df <- make_coef_df(main_df, model_min_mjn_2, "Adolescents (fully adjusted)")
main_df <- make_coef_df(main_df, model_min_fgh_2, "Adolescents (fully adjusted)")

# Clean dataframe of coefficients
main_df <- clean_coef_df(main_df)

# Get min. and max. N
min(main_df$n); max(main_df$n)

# Generate coefficient plot: Main (for main text)
plot_main_1 <- ggplot(main_df, aes(x=Outcome, y=effect, group=Sample, color=Sample)) +
  geom_hline(yintercept=0, color="black", linewidth=0.25) +
  geom_point(position = position_dodge(width=0.6), size=1, aes(shape=Sample)) +
  scale_shape_manual(values = 1:nlevels(main_df$Sample)) +
  geom_errorbar(aes(ymin=effect-1.96*se, ymax=effect+1.96*se),
                position = position_dodge(width=0.6), width=0.2) +
  ylab("Association of $1 increase in min. wage\nwith adolescents' mental health") +
  ggtitle("Adolescents (12-18), 2001-2019") +
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
  scale_y_continuous(limits = c(-0.045, 0.045),
                     breaks = seq(-0.1, 0.1, 0.02),
                     minor_breaks = seq(-0.1, 0.1, 0.01),
                     labels = function(x) paste0(x*100," pp")) +
  scale_color_grey(start=0.7, end=0) +
  facet_grid(~Category, scales="free", space="free_x")

# Export figure
ggsave(plot=plot_main_1, file="Exhibits/YRBS coefficient plot, main 1.pdf",
       width=5, height=4, units='in', dpi=600)

# Generate coefficient plot: Main (for appendix)
plot_main_2 <- print_coef_plot(
  main_df,
  Y_TITLE    = "Association of $1 increase in min. wage\nwith adolescents' mental health",
  Y_MIN      = -0.045,
  Y_MAX      =  0.045,
  COLORS     = "Standard"
)

# Export figure
ggsave(plot=plot_main_2, file="Exhibits/YRBS coefficient plot, main 2.pdf",
       width=5, height=4, units='in', dpi=600)

# Additional row in table
cov_row <- as.data.frame(
  rbind(cbind("Demographic controls",     "No",  "Yes", "No",  "Yes", "No",  "Yes"),
        cbind("State policy controls",    "No",  "Yes", "No",  "Yes", "No",  "Yes"),
        cbind("State & age-by-year FEs",  "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"),
        cbind("Cluster robust SEs", "State", "State", "State", "State", "State", "State")))

# Compile results into tables
modelsummary(list("(1)" = model_min_sad_1,
                  "(2)" = model_min_sad_2,
                  "(1)" = model_min_con_1,
                  "(2)" = model_min_con_2,
                  "(1)" = model_min_att_1,
                  "(2)" = model_min_att_2),
             gof_omit    = "Log*|AIC|BIC|F|RMSE|Std",
             coef_omit   = "^(?!Effective.Minimum.Wage)",
             coef_rename = c("Effective.Minimum.Wage" =
                               "$1 increase in min. wage"),
             statistic   = c("[{conf.low}, {conf.high}]"),
             # conf_level  = 0.99667,
             add_rows    = cov_row) %>%
  add_header_above(c(" " = 1, "Sad or hopeless" = 2, "Cons. suicide" = 2, "Att. suicide" = 2))

modelsummary(list("(1)" = model_min_alc_1,
                  "(2)" = model_min_alc_2,
                  "(1)" = model_min_mjn_1,
                  "(2)" = model_min_mjn_2,
                  "(1)" = model_min_fgh_1,
                  "(2)" = model_min_fgh_2),
             gof_omit    = "Log*|AIC|BIC|F|RMSE|Std",
             coef_omit   = "^(?!Effective.Minimum.Wage)",
             coef_rename = c("Effective.Minimum.Wage" =
                               "$1 increase in min. wage"),
             statistic   = c("[{conf.low}, {conf.high}]"),
             # conf_level  = 0.99667,
             add_rows    = cov_row) %>%
  add_header_above(c(" " = 1, "Alcohol" = 2, "Marijuana" = 2, "Phys. fight" = 2))

##############################################################################
# TWFE robustness check: Sub-population
##############################################################################

# # Sad or hopeless
# model_min_sad_r <- felm(sad_hopeless ~ Effective.Minimum.Wage*race_eth_cat +
#                           age_2 + sex_2 + race_eth_2 + grade_2 +
#                           elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
#                           age_year + fipsst | 0 | fipsst,
#                         data    = yrbs_all_model,
#                         weights = yrbs_all_model$weight_2)
# 
# # Considered suicide
# model_min_con_r <- felm(cons_suicide ~ Effective.Minimum.Wage*race_eth_cat +
#                           age_2 + sex_2 + race_eth_2 + grade_2 +
#                           elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
#                           age_year + fipsst | 0 | fipsst,
#                         data    = yrbs_all_model,
#                         weights = yrbs_all_model$weight_2)
# 
# # Attempted suicide
# model_min_att_r <- felm(suicide_att ~ Effective.Minimum.Wage*race_eth_cat +
#                           age_2 + sex_2 + race_eth_2 + grade_2 +
#                           elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
#                           age_year + fipsst | 0 | fipsst,
#                         data    = yrbs_all_model,
#                         weights = yrbs_all_model$weight_2)
# 
# # Recent alcohol
# model_min_alc_r <- felm(alcohol ~ Effective.Minimum.Wage*race_eth_cat +
#                           age_2 + sex_2 + race_eth_2 + grade_2 +
#                           elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
#                           age_year + fipsst | 0 | fipsst,
#                         data    = yrbs_all_model,
#                         weights = yrbs_all_model$weight_2)
# 
# # Recent marijuana
# model_min_mjn_r <- felm(marijuana ~ Effective.Minimum.Wage*race_eth_cat +
#                           age_2 + sex_2 + race_eth_2 + grade_2 +
#                           elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
#                           age_year + fipsst | 0 | fipsst,
#                         data    = yrbs_all_model,
#                         weights = yrbs_all_model$weight_2)
# 
# # Physical fight
# model_min_fgh_r <- felm(fight ~ Effective.Minimum.Wage*race_eth_cat +
#                           age_2 + sex_2 + race_eth_2 + grade_2 +
#                           elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
#                           age_year + fipsst | 0 | fipsst,
#                         data    = yrbs_all_model,
#                         weights = yrbs_all_model$weight_2)

# Generate subgroup of interest
yrbs_all_model_r <- subset(yrbs_all_model, race7 %in% c(3:4)) # Black or Hispanic/Latino

# Sad or hopeless
model_min_sad_r <- felm(sad_hopeless ~ Effective.Minimum.Wage +
                          age_2 + sex_2 + race_eth_2 + grade_2 +
                          elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                          age_year + fipsst | 0 | fipsst,
                        data    = yrbs_all_model_r,
                        weights = yrbs_all_model_r$weight_2)

# Considered suicide
model_min_con_r <- felm(cons_suicide ~ Effective.Minimum.Wage +
                          age_2 + sex_2 + race_eth_2 + grade_2 +
                          elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                          age_year + fipsst | 0 | fipsst,
                        data    = yrbs_all_model_r,
                        weights = yrbs_all_model_r$weight_2)

# Attempted suicide
model_min_att_r <- felm(suicide_att ~ Effective.Minimum.Wage +
                          age_2 + sex_2 + race_eth_2 + grade_2 +
                          elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                          age_year + fipsst | 0 | fipsst,
                        data    = yrbs_all_model_r,
                        weights = yrbs_all_model_r$weight_2)

# Recent alcohol
model_min_alc_r <- felm(alcohol ~ Effective.Minimum.Wage +
                          age_2 + sex_2 + race_eth_2 + grade_2 +
                          elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                          age_year + fipsst | 0 | fipsst,
                        data    = yrbs_all_model_r,
                        weights = yrbs_all_model_r$weight_2)

# Recent marijuana
model_min_mjn_r <- felm(marijuana ~ Effective.Minimum.Wage +
                          age_2 + sex_2 + race_eth_2 + grade_2 +
                          elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                          age_year + fipsst | 0 | fipsst,
                        data    = yrbs_all_model_r,
                        weights = yrbs_all_model_r$weight_2)

# Physical fight
model_min_fgh_r <- felm(fight ~ Effective.Minimum.Wage +
                          age_2 + sex_2 + race_eth_2 + grade_2 +
                          elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                          age_year + fipsst | 0 | fipsst,
                        data    = yrbs_all_model_r,
                        weights = yrbs_all_model_r$weight_2)

# Get values from models
sub_df <- NULL

# Black or Hispanic/Latino
sub_df <- make_coef_df(sub_df, model_min_sad_r, "Black or Hispanic/Latino")
sub_df <- make_coef_df(sub_df, model_min_con_r, "Black or Hispanic/Latino")
sub_df <- make_coef_df(sub_df, model_min_att_r, "Black or Hispanic/Latino")
sub_df <- make_coef_df(sub_df, model_min_alc_r, "Black or Hispanic/Latino")
sub_df <- make_coef_df(sub_df, model_min_mjn_r, "Black or Hispanic/Latino")
sub_df <- make_coef_df(sub_df, model_min_fgh_r, "Black or Hispanic/Latino")

# Clean dataframe of coefficients
sub_df <- clean_coef_df(sub_df)

# Add main models for comparison
sub_df <- rbind(sub_df, main_df %>% filter(Sample == "Adolescents (fully adjusted)"))

# Get min. and max. N
min(sub_df$n); max(sub_df$n)

# Generate coefficient plot: Sub-population
plot_sub <- print_coef_plot(
  sub_df,
  Y_TITLE    = "Association of $1 increase in min. wage\nwith adolescents' mental health",
  Y_MIN      = -0.045,
  Y_MAX      =  0.045,
  COLORS     = "Reversed"
)

# Export figure
ggsave(plot=plot_sub, file="Exhibits/YRBS coefficient plot, sub-population.pdf",
       width=5, height=4, units='in', dpi=600)

##############################################################################
# TWFE robustness check: Alternate specifications
##############################################################################

# Sad or hopeless
model_min_sad_x <- felm(sad_hopeless ~ Effective.Minimum.Wage.2020.Dollars +
                          age_2 + sex_2 + race_eth_2 + grade_2 +
                          elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                          age_year + fipsst | 0 | fipsst,
                        data    = yrbs_all_model,
                        weights = yrbs_all_model$weight_2)
model_min_sad_y <- felm(sad_hopeless ~ lag_by_1 +
                          age_2 + sex_2 + race_eth_2 + grade_2 +
                          elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                          age_year + fipsst | 0 | fipsst,
                        data    = yrbs_all_model,
                        weights = yrbs_all_model$weight_2)

# Considered suicide
model_min_con_x <- felm(cons_suicide ~ Effective.Minimum.Wage.2020.Dollars +
                          age_2 + sex_2 + race_eth_2 + grade_2 +
                          elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                          age_year + fipsst | 0 | fipsst,
                        data    = yrbs_all_model,
                        weights = yrbs_all_model$weight_2)
model_min_con_y <- felm(cons_suicide ~ lag_by_1 +
                          age_2 + sex_2 + race_eth_2 + grade_2 +
                          elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                          age_year + fipsst | 0 | fipsst,
                        data    = yrbs_all_model,
                        weights = yrbs_all_model$weight_2)

# Attempted suicide
model_min_att_x <- felm(suicide_att ~ Effective.Minimum.Wage.2020.Dollars +
                          age_2 + sex_2 + race_eth_2 + grade_2 +
                          elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                          age_year + fipsst | 0 | fipsst,
                        data    = yrbs_all_model,
                        weights = yrbs_all_model$weight_2)
model_min_att_y <- felm(suicide_att ~ lag_by_1 +
                          age_2 + sex_2 + race_eth_2 + grade_2 +
                          elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                          age_year + fipsst | 0 | fipsst,
                        data    = yrbs_all_model,
                        weights = yrbs_all_model$weight_2)

# Recent alcohol
model_min_alc_x <- felm(alcohol ~ Effective.Minimum.Wage.2020.Dollars +
                          age_2 + sex_2 + race_eth_2 + grade_2 +
                          elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                          age_year + fipsst | 0 | fipsst,
                        data    = yrbs_all_model,
                        weights = yrbs_all_model$weight_2)
model_min_alc_y <- felm(alcohol ~ lag_by_1 +
                          age_2 + sex_2 + race_eth_2 + grade_2 +
                          elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                          age_year + fipsst | 0 | fipsst,
                        data    = yrbs_all_model,
                        weights = yrbs_all_model$weight_2)

# Recent marijuana
model_min_mjn_x <- felm(marijuana ~ Effective.Minimum.Wage.2020.Dollars +
                          age_2 + sex_2 + race_eth_2 + grade_2 +
                          elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                          age_year + fipsst | 0 | fipsst,
                        data    = yrbs_all_model,
                        weights = yrbs_all_model$weight_2)
model_min_mjn_y <- felm(marijuana ~ lag_by_1 +
                          age_2 + sex_2 + race_eth_2 + grade_2 +
                          elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                          age_year + fipsst | 0 | fipsst,
                        data    = yrbs_all_model,
                        weights = yrbs_all_model$weight_2)

# Physical fight
model_min_fgh_x <- felm(fight ~ Effective.Minimum.Wage.2020.Dollars +
                          age_2 + sex_2 + race_eth_2 + grade_2 +
                          elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                          age_year + fipsst | 0 | fipsst,
                        data    = yrbs_all_model,
                        weights = yrbs_all_model$weight_2)
model_min_fgh_y <- felm(fight ~ lag_by_1 +
                          age_2 + sex_2 + race_eth_2 + grade_2 +
                          elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                          age_year + fipsst | 0 | fipsst,
                        data    = yrbs_all_model,
                        weights = yrbs_all_model$weight_2)

# Get values from models
rob_df <- NULL

# Adolescents (2020 dollars)
rob_df <- make_coef_df(rob_df, model_min_sad_x, "Adolescents (2020 dollars)")
rob_df <- make_coef_df(rob_df, model_min_con_x, "Adolescents (2020 dollars)")
rob_df <- make_coef_df(rob_df, model_min_att_x, "Adolescents (2020 dollars)")
rob_df <- make_coef_df(rob_df, model_min_alc_x, "Adolescents (2020 dollars)")
rob_df <- make_coef_df(rob_df, model_min_mjn_x, "Adolescents (2020 dollars)")
rob_df <- make_coef_df(rob_df, model_min_fgh_x, "Adolescents (2020 dollars)")

# Adolescents (lagged wage)
rob_df <- make_coef_df(rob_df, model_min_sad_y, "Adolescents (lagged wage)")
rob_df <- make_coef_df(rob_df, model_min_con_y, "Adolescents (lagged wage)")
rob_df <- make_coef_df(rob_df, model_min_att_y, "Adolescents (lagged wage)")
rob_df <- make_coef_df(rob_df, model_min_alc_y, "Adolescents (lagged wage)")
rob_df <- make_coef_df(rob_df, model_min_mjn_y, "Adolescents (lagged wage)")
rob_df <- make_coef_df(rob_df, model_min_fgh_y, "Adolescents (lagged wage)")

# Clean dataframe of coefficients
rob_df <- clean_coef_df(rob_df)

# Add main models for comparison
rob_df <- rbind(rob_df, main_df %>% filter(Sample == "Adolescents (fully adjusted)"))

# Get min. and max. N
min(rob_df$n); max(rob_df$n)

# Generate coefficient plot: Alternate specifications
plot_rob <- print_coef_plot(
  rob_df,
  Y_TITLE    = "Association of $1 increase in min. wage\nwith adolescents' mental health",
  Y_MIN      = -0.045,
  Y_MAX      =  0.045,
  COLORS     = "Reversed"
)

# Export figure
ggsave(plot=plot_rob, file="Exhibits/YRBS coefficient plot, robustness.pdf",
       width=7, height=4, units='in', dpi=600)

##############################################################################
# TWFE robustness check: Logistic regression
##############################################################################

# Define complex sampling design
# Used state clusters and apply survey weights
design_yrbs <- svydesign(id=~fipsst, weights=~weight_2, data=yrbs_all_model)

# Sad or hopeless
log_min_sad_1 <- svyglm(sad_hopeless ~ Effective.Minimum.Wage +
                          age_year + fipsst,
                        design = design_yrbs, family="quasibinomial")
log_min_sad_2 <- svyglm(sad_hopeless ~ Effective.Minimum.Wage +
                          age_2 + sex_2 + race_eth_2 + grade_2 +
                          elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 +
                          age_year + fipsst,
                        design = design_yrbs, family="quasibinomial")

# Considered suicide
log_min_con_1 <- svyglm(cons_suicide ~ Effective.Minimum.Wage +
                          age_year + fipsst,
                        design = design_yrbs, family="quasibinomial")
log_min_con_2 <- svyglm(cons_suicide ~ Effective.Minimum.Wage +
                          age_2 + sex_2 + race_eth_2 + grade_2 +
                          elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 +
                          age_year + fipsst,
                        design = design_yrbs, family="quasibinomial")

# Attempted suicide
log_min_att_1 <- svyglm(suicide_att ~ Effective.Minimum.Wage +
                          age_year + fipsst,
                        design = design_yrbs, family="quasibinomial")
log_min_att_2 <- svyglm(suicide_att ~ Effective.Minimum.Wage +
                          age_2 + sex_2 + race_eth_2 + grade_2 +
                          elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 +
                          age_year + fipsst,
                        design = design_yrbs, family="quasibinomial")

# Recent alcohol
log_min_alc_1 <- svyglm(alcohol ~ Effective.Minimum.Wage +
                          age_year + fipsst,
                        design = design_yrbs, family="quasibinomial")
log_min_alc_2 <- svyglm(alcohol ~ Effective.Minimum.Wage +
                          age_2 + sex_2 + race_eth_2 + grade_2 +
                          elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 +
                          age_year + fipsst,
                        design = design_yrbs, family="quasibinomial")

# Recent marijuana
log_min_mjn_1 <- svyglm(marijuana ~ Effective.Minimum.Wage +
                          age_year + fipsst,
                        design = design_yrbs, family="quasibinomial")
log_min_mjn_2 <- svyglm(marijuana ~ Effective.Minimum.Wage +
                          age_2 + sex_2 + race_eth_2 + grade_2 +
                          elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 +
                          age_year + fipsst,
                        design = design_yrbs, family="quasibinomial")

# Physical fight
log_min_fgh_1 <- svyglm(fight ~ Effective.Minimum.Wage +
                          age_year + fipsst,
                        design = design_yrbs, family="quasibinomial")
log_min_fgh_2 <- svyglm(fight ~ Effective.Minimum.Wage +
                          age_2 + sex_2 + race_eth_2 + grade_2 +
                          elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 +
                          age_year + fipsst,
                        design = design_yrbs, family="quasibinomial")

# Get values from models
log_df <- as.data.frame(rbind(
  # Sad or hopeless
  cbind("Sad or hopeless", "Symptoms", "Adolescents (FE only)",
        coef(log_min_sad_1)[2],
        SE(log_min_sad_1)[2],
        length(log_min_sad_1$residuals)),
  
  cbind("Sad or hopeless", "Symptoms", "Adolescents (fully adjusted)",
        coef(log_min_sad_2)[2],
        SE(log_min_sad_2)[2],
        length(log_min_sad_2$residuals)),
  
  # Considered suicide
  cbind("Considered suicide", "Symptoms", "Adolescents (FE only)",
        coef(log_min_con_1)[2],
        SE(log_min_con_1)[2],
        length(log_min_con_1$residuals)),
  
  cbind("Considered suicide", "Symptoms", "Adolescents (fully adjusted)",
        coef(log_min_con_2)[2],
        SE(log_min_con_2)[2],
        length(log_min_con_2$residuals)),
  
  # Attempted suicide
  cbind("Attempted suicide", "Symptoms", "Adolescents (FE only)",
        coef(log_min_att_1)[2],
        SE(log_min_att_1)[2],
        length(log_min_att_1$residuals)),
  
  cbind("Attempted suicide", "Symptoms", "Adolescents (fully adjusted)",
        coef(log_min_att_2)[2],
        SE(log_min_att_2)[2],
        length(log_min_att_2$residuals)),
  
  # Recent alcohol
  cbind("Recent alcohol", "Substances", "Adolescents (FE only)",
        coef(log_min_alc_1)[2],
        SE(log_min_alc_1)[2],
        length(log_min_alc_1$residuals)),
  
  cbind("Recent alcohol", "Substances", "Adolescents (fully adjusted)",
        coef(log_min_alc_2)[2],
        SE(log_min_alc_2)[2],
        length(log_min_alc_2$residuals)),
  
  # Recent marijuana
  cbind("Recent marijuana", "Substances", "Adolescents (FE only)",
        coef(log_min_mjn_1)[2],
        SE(log_min_mjn_1)[2],
        length(log_min_mjn_1$residuals)),
  
  cbind("Recent marijuana", "Substances", "Adolescents (fully adjusted)",
        coef(log_min_mjn_2)[2],
        SE(log_min_mjn_2)[2],
        length(log_min_mjn_2$residuals)),
  
  # Physical fight
  cbind("Physical fight", "Violence", "Adolescents (FE only)",
        coef(log_min_fgh_1)[2],
        SE(log_min_fgh_1)[2],
        length(log_min_fgh_1$residuals)),
  
  cbind("Physical fight", "Violence", "Adolescents (fully adjusted)",
        coef(log_min_fgh_2)[2],
        SE(log_min_fgh_2)[2],
        length(log_min_fgh_2$residuals))
))

# Name columns
colnames(log_df) <- c("Outcome", "Category", "Sample", "log_odds", "se", "n")

# Reorder outcomes
log_df$Outcome <- factor(
  log_df$Outcome, levels=c("Sad or hopeless", "Considered suicide", "Attempted suicide", "Recent alcohol", "Recent marijuana", "Physical fight"))

# Reorder categories
log_df$Category <- factor(
  log_df$Category, levels=c("Symptoms", "Substances", "Violence"))

# Reorder samples
log_df$Sample <- factor(log_df$Sample, levels=c("Adolescents (FE only)",
                                                "Adolescents (fully adjusted)"))

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
  ggtitle("Adolescents (12-18), 2001-2019") +
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
ggsave(plot=plot_log, file="Exhibits/YRBS coefficient plot, logistic.pdf",
       width=5, height=4, units='in', dpi=600)

##############################################################################
# TWFE robustness check: Lifetime minimum wage
##############################################################################

# Sad or hopeless
life_min_sad_1 <- felm(sad_hopeless ~ wage_life |
                         age_year + fipsst | 0 | fipsst,
                       data    = yrbs_all_model,
                       weights = yrbs_all_model$weight_2)
life_min_sad_2 <- felm(sad_hopeless ~ wage_life +
                         age_2 + sex_2 + race_eth_2 + grade_2 +
                         elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                         age_year + fipsst | 0 | fipsst,
                       data    = yrbs_all_model,
                       weights = yrbs_all_model$weight_2)

# Considered suicide
life_min_con_1 <- felm(cons_suicide ~ wage_life |
                         age_year + fipsst | 0 | fipsst,
                       data    = yrbs_all_model,
                       weights = yrbs_all_model$weight_2)
life_min_con_2 <- felm(cons_suicide ~ wage_life +
                         age_2 + sex_2 + race_eth_2 + grade_2 +
                         elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                         age_year + fipsst | 0 | fipsst,
                       data    = yrbs_all_model,
                       weights = yrbs_all_model$weight_2)

# Attempted suicide
life_min_att_1 <- felm(suicide_att ~ wage_life |
                         age_year + fipsst | 0 | fipsst,
                       data    = yrbs_all_model,
                       weights = yrbs_all_model$weight_2)
life_min_att_2 <- felm(suicide_att ~ wage_life +
                         age_2 + sex_2 + race_eth_2 + grade_2 +
                         elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                         age_year + fipsst | 0 | fipsst,
                       data    = yrbs_all_model,
                       weights = yrbs_all_model$weight_2)

# Recent alcohol
life_min_alc_1 <- felm(alcohol ~ wage_life |
                         age_year + fipsst | 0 | fipsst,
                       data    = yrbs_all_model,
                       weights = yrbs_all_model$weight_2)
life_min_alc_2 <- felm(alcohol ~ wage_life +
                         age_2 + sex_2 + race_eth_2 + grade_2 +
                         elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                         age_year + fipsst | 0 | fipsst,
                       data    = yrbs_all_model,
                       weights = yrbs_all_model$weight_2)

# Recent marijuana use
life_min_mjn_1 <- felm(marijuana ~ wage_life |
                         age_year + fipsst | 0 | fipsst,
                       data    = yrbs_all_model,
                       weights = yrbs_all_model$weight_2)
life_min_mjn_2 <- felm(marijuana ~ wage_life +
                         age_2 + sex_2 + race_eth_2 + grade_2 +
                         elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                         age_year + fipsst | 0 | fipsst,
                       data    = yrbs_all_model,
                       weights = yrbs_all_model$weight_2)

# Physical fight
life_min_fgh_1 <- felm(fight ~ wage_life |
                         age_year + fipsst | 0 | fipsst,
                       data    = yrbs_all_model,
                       weights = yrbs_all_model$weight_2)
life_min_fgh_2 <- felm(fight ~ wage_life +
                         age_2 + sex_2 + race_eth_2 + grade_2 +
                         elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                         age_year + fipsst | 0 | fipsst,
                       data    = yrbs_all_model,
                       weights = yrbs_all_model$weight_2)

# Get values from models
life_df <- NULL

# Adolescents (FE only)
life_df <- make_coef_df(life_df, life_min_sad_1, "Adolescents (FE only)")
life_df <- make_coef_df(life_df, life_min_con_1, "Adolescents (FE only)")
life_df <- make_coef_df(life_df, life_min_att_1, "Adolescents (FE only)")
life_df <- make_coef_df(life_df, life_min_alc_1, "Adolescents (FE only)")
life_df <- make_coef_df(life_df, life_min_mjn_1, "Adolescents (FE only)")
life_df <- make_coef_df(life_df, life_min_fgh_1, "Adolescents (FE only)")

# Adolescents (fully adjusted)
life_df <- make_coef_df(life_df, life_min_sad_2, "Adolescents (fully adjusted)")
life_df <- make_coef_df(life_df, life_min_con_2, "Adolescents (fully adjusted)")
life_df <- make_coef_df(life_df, life_min_att_2, "Adolescents (fully adjusted)")
life_df <- make_coef_df(life_df, life_min_alc_2, "Adolescents (fully adjusted)")
life_df <- make_coef_df(life_df, life_min_mjn_2, "Adolescents (fully adjusted)")
life_df <- make_coef_df(life_df, life_min_fgh_2, "Adolescents (fully adjusted)")

# Clean dataframe of coefficients
life_df <- clean_coef_df(life_df)

# Get min. and max. N
min(life_df$n); max(life_df$n)

# Generate coefficient plot: Lifetime wage
plot_life <- print_coef_plot(
  life_df,
  Y_TITLE    = "Association of $1 increase in lifetime\nmin. wage with adolescents' mental health",
  Y_MIN      = -0.065,
  Y_MAX      =  0.065,
  COLORS     = "Standard"
)

# Export figure
ggsave(plot=plot_life, file="Exhibits/YRBS coefficient plot, lifetime.pdf",
       width=5, height=4, units='in', dpi=600)

##############################################################################
# TWFE robustness check: Nested clusters
##############################################################################

# Sad or hopeless
model_min_sad_1c <- felm(sad_hopeless ~ Effective.Minimum.Wage |
                           age_year + fipsst | 0 | cluster,
                         data    = yrbs_all_model,
                         weights = yrbs_all_model$weight_2)
model_min_sad_2c <- felm(sad_hopeless ~ Effective.Minimum.Wage +
                           age_2 + sex_2 + race_eth_2 + grade_2 +
                           elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                           age_year + fipsst | 0 | cluster,
                         data    = yrbs_all_model,
                         weights = yrbs_all_model$weight_2)

# Considered suicide
model_min_con_1c <- felm(cons_suicide ~ Effective.Minimum.Wage |
                           age_year + fipsst | 0 | cluster,
                         data    = yrbs_all_model,
                         weights = yrbs_all_model$weight_2)
model_min_con_2c <- felm(cons_suicide ~ Effective.Minimum.Wage +
                           age_2 + sex_2 + race_eth_2 + grade_2 +
                           elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                           age_year + fipsst | 0 | cluster,
                         data    = yrbs_all_model,
                         weights = yrbs_all_model$weight_2)

# Attempted suicide
model_min_att_1c <- felm(suicide_att ~ Effective.Minimum.Wage |
                           age_year + fipsst | 0 | cluster,
                         data    = yrbs_all_model,
                         weights = yrbs_all_model$weight_2)
model_min_att_2c <- felm(suicide_att ~ Effective.Minimum.Wage +
                           age_2 + sex_2 + race_eth_2 + grade_2 +
                           elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                           age_year + fipsst | 0 | cluster,
                         data    = yrbs_all_model,
                         weights = yrbs_all_model$weight_2)

# Recent alcohol
model_min_alc_1c <- felm(alcohol ~ Effective.Minimum.Wage |
                           age_year + fipsst | 0 | cluster,
                         data    = yrbs_all_model,
                         weights = yrbs_all_model$weight_2)
model_min_alc_2c <- felm(alcohol ~ Effective.Minimum.Wage +
                           age_2 + sex_2 + race_eth_2 + grade_2 +
                           elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                           age_year + fipsst | 0 | cluster,
                         data    = yrbs_all_model,
                         weights = yrbs_all_model$weight_2)

# Recent marijuana
model_min_mjn_1c <- felm(marijuana ~ Effective.Minimum.Wage |
                           age_year + fipsst | 0 | cluster,
                         data    = yrbs_all_model,
                         weights = yrbs_all_model$weight_2)
model_min_mjn_2c <- felm(marijuana ~ Effective.Minimum.Wage +
                           age_2 + sex_2 + race_eth_2 + grade_2 +
                           elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                           age_year + fipsst | 0 | cluster,
                         data    = yrbs_all_model,
                         weights = yrbs_all_model$weight_2)

# Physical fight
model_min_fgh_1c <- felm(fight ~ Effective.Minimum.Wage |
                           age_year + fipsst | 0 | cluster,
                         data    = yrbs_all_model,
                         weights = yrbs_all_model$weight_2)
model_min_fgh_2c <- felm(fight ~ Effective.Minimum.Wage +
                           age_2 + sex_2 + race_eth_2 + grade_2 +
                           elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                           age_year + fipsst | 0 | cluster,
                         data    = yrbs_all_model,
                         weights = yrbs_all_model$weight_2)

# Get values from models
clust_df <- NULL

# Adolescents (FE only, nested clust.)
clust_df <- make_coef_df(clust_df, model_min_sad_1c, "Adolescents (FE only, nested clust.)")
clust_df <- make_coef_df(clust_df, model_min_con_1c, "Adolescents (FE only, nested clust.)")
clust_df <- make_coef_df(clust_df, model_min_att_1c, "Adolescents (FE only, nested clust.)")
clust_df <- make_coef_df(clust_df, model_min_alc_1c, "Adolescents (FE only, nested clust.)")
clust_df <- make_coef_df(clust_df, model_min_mjn_1c, "Adolescents (FE only, nested clust.)")
clust_df <- make_coef_df(clust_df, model_min_fgh_1c, "Adolescents (FE only, nested clust.)")

# Adolescents (fully adj., nested clust.)
clust_df <- make_coef_df(clust_df, model_min_sad_2c, "Adolescents (fully adj., nested clust.)")
clust_df <- make_coef_df(clust_df, model_min_con_2c, "Adolescents (fully adj., nested clust.)")
clust_df <- make_coef_df(clust_df, model_min_att_2c, "Adolescents (fully adj., nested clust.)")
clust_df <- make_coef_df(clust_df, model_min_alc_2c, "Adolescents (fully adj., nested clust.)")
clust_df <- make_coef_df(clust_df, model_min_mjn_2c, "Adolescents (fully adj., nested clust.)")
clust_df <- make_coef_df(clust_df, model_min_fgh_2c, "Adolescents (fully adj., nested clust.)")

# Add main models for comparison
# Adolescents (FE only, state clust.)
clust_df <- make_coef_df(clust_df, model_min_sad_1, "Adolescents (FE only, state clust.)")
clust_df <- make_coef_df(clust_df, model_min_con_1, "Adolescents (FE only, state clust.)")
clust_df <- make_coef_df(clust_df, model_min_att_1, "Adolescents (FE only, state clust.)")
clust_df <- make_coef_df(clust_df, model_min_alc_1, "Adolescents (FE only, state clust.)")
clust_df <- make_coef_df(clust_df, model_min_mjn_1, "Adolescents (FE only, state clust.)")
clust_df <- make_coef_df(clust_df, model_min_fgh_1, "Adolescents (FE only, state clust.)")

# Adolescents (fully adj., state clust.)
clust_df <- make_coef_df(clust_df, model_min_sad_2, "Adolescents (fully adj., state clust.)")
clust_df <- make_coef_df(clust_df, model_min_con_2, "Adolescents (fully adj., state clust.)")
clust_df <- make_coef_df(clust_df, model_min_att_2, "Adolescents (fully adj., state clust.)")
clust_df <- make_coef_df(clust_df, model_min_alc_2, "Adolescents (fully adj., state clust.)")
clust_df <- make_coef_df(clust_df, model_min_mjn_2, "Adolescents (fully adj., state clust.)")
clust_df <- make_coef_df(clust_df, model_min_fgh_2, "Adolescents (fully adj., state clust.)")

# Clean dataframe of coefficients
clust_df <- clean_coef_df(clust_df)

# Get min. and max. N
min(clust_df$n); max(clust_df$n)

# Generate coefficient plot: Nested clusters
plot_clust <- print_coef_plot(
  clust_df,
  Y_TITLE    = "Association of $1 increase in min. wage\nwith adolescents' mental health",
  Y_MIN      = -0.045,
  Y_MAX      =  0.045,
  COLORS     = "Standard"
)

# Adjust legend
plot_clust <- plot_clust + guides(shape = guide_legend(nrow = 2))

# Export figure
ggsave(plot=plot_clust, file="Exhibits/YRBS coefficient plot, nested clusters.pdf",
       width=7, height=4, units='in', dpi=600)

##############################################################################
# Functions for DID/event study models
##############################################################################

# Treat year as categorical
yrbs_all_model$year_cat <- relevel(as.factor(yrbs_all_model$year), "2013")

# Define treated and control states
TREATED <- c("AR","DE","HI","MD","MT","NE","NJ","NY","SD","WV")
CONTROL <- c("AL","GA","IA","ID","KS","KY","LA","MS","NC","ND",
             "NH","NV","OK","PA","SC","TN","TX","UT","VA","WI","WY")
TREATED_2 <- cdlTools::fips(TREATED, to="FIPS")
CONTROL_2 <- cdlTools::fips(CONTROL, to="FIPS")

# Code treatment groups
yrbs_all_model <- yrbs_all_model %>% mutate(
  event_treated = case_when(
    fipsst %in% TREATED_2 ~ 1,
    fipsst %in% CONTROL_2 ~ 0
  ))

# Code treatment years
yrbs_all_model <- yrbs_all_model %>% mutate(
  treated_years = case_when(
    year %in% c(2011:2013) ~ 0,
    year %in% c(2014:2019) ~ 1
  ))

# Subset dataset to relevant years
yrbs_event <- subset(yrbs_all_model, year %in% c(2011:2019) &
                       fipsst %in% c(TREATED_2, CONTROL_2))

# Function to extract and clean values from event study models.
# Requires: Df for saving coefficients, "lfe" model, title of model.
# Returns: Df of values, requires cleaning before passing to "print_event_plot".
make_event_df <- function(event_df, event_model, TITLE) {
  
  # Get outcome labels
  if (event_model$lhs == "sad_hopeless") {
    outcome  <- "Sad or hopeless"}
  
  if (event_model$lhs == "cons_suicide") {
    outcome  <- "Considered suicide"}
  
  if (event_model$lhs == "suicide_att") {
    outcome  <- "Attempted suicide"}
  
  if (event_model$lhs == "alcohol") {
    outcome  <- "Recent alcohol"}
  
  if (event_model$lhs == "marijuana") {
    outcome  <- "Recent marijuana"}
  
  if (event_model$lhs == "fight") {
    outcome  <- "Physical fight"}
  
  # Get coefficients of model
  coef <- data.frame(cbind(outcome, event_model$coefficients, event_model$cse, TITLE))
  colnames(coef) <- c("outcome", "effect", "se", "name"); coef$var <- rownames(coef)
  
  # Only include year coefficients
  coef <- coef %>% filter(grepl("event_treated:year_cat", var))
  
  # Rename period variable
  coef$period <- gsub("event_treated:year_cat", "", coef$var)
  
  # Add zero period
  coef <- data.frame(rbind(coef, c(
    outcome, 0, 0, TITLE, "event_treated:year_cat2013", 2013)))
  
  # Treat columns as numeric
  coef$effect  <- as.numeric(coef$effect)
  coef$se      <- as.numeric(coef$se)
  coef$period  <- as.numeric(coef$period)
  
  # Add to existing event study df
  event_df <- rbind(event_df, coef)
  
  return(event_df)
}

# Function to generate event study plots.
# Requires: Df of coefficients, y title, y limits, color order.
# Returns: Formatted event study plot.
print_event_plot <- function(event_df, Y_TITLE, Y_MIN, Y_MAX, COLORS) {
  
  # Set grayscale colors
  if (COLORS == "Standard") {
    COLOR_MAX <- 0
    COLOR_MIN <- 0.7
  }
  if (COLORS == "Reversed") {
    COLOR_MAX <- 0.7
    COLOR_MIN <- 0
  }
  
  # Generate event study plot
  plot <- ggplot(event_df, aes(x=period, y=effect, group=name, color=name)) +
    
    # Null line
    geom_hline(yintercept=0, color="black") +
    
    # Confidence intervals
    geom_errorbar(aes(ymin=effect-1.96*se, ymax=effect+1.96*se),
                  position = position_dodge(width=0.6), width=0, linewidth=0.8) +
    geom_errorbar(aes(ymin=effect-2.64*se, ymax=effect+2.64*se),
                  position = position_dodge(width=0.6), width=0.3, alpha=0.5) +
    
    # Lines and point estimates with distinct shapes
    geom_line(position = position_dodge(width = 0.5)) +
    geom_point(position = position_dodge(width = 0.5), size=1, aes(shape=name)) +
    scale_shape_manual(values = 1:nlevels(event_df$name)) +
    
    # Titles
    xlab("Year") +
    ylab(Y_TITLE) +
    
    # Theme modifications
    theme_test() +
    theme(legend.position = "bottom",
          text = element_text(size = 10, face = "bold"),
          axis.title.x = element_blank(),
          axis.ticks = element_blank(),
          strip.background = element_blank(),
          legend.title = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(color="light gray", linewidth=0.5),
          panel.grid.minor.y = element_line(color="light gray", linewidth=0.25)) +
    scale_y_continuous(breaks=seq(-0.1, 0.1, 0.05),
                       minor_breaks = seq(-0.1, 0.1, 0.025),
                       limits=c(Y_MIN, Y_MAX),
                       labels = function(x) paste0(x*100," pp")) +
    scale_x_continuous(breaks=seq(2011,2019,2)) +
    annotate("text", x=2016.5, y=0.075, label="Raised minimum wage",
             fontface=2, size=2.5, hjust=0.5, vjust=0.5) +
    annotate("rect", xmin=2013.5, xmax=2019.5,
             ymin=-Inf, ymax=Inf, alpha=0.2, fill="grey70") +
    scale_color_grey(start=0.6, end=0) +
    facet_wrap(.~outcome, ncol=2)
  
  # Return plot
  return(plot)
}

##############################################################################
# Graphs: Descriptives for DID/event studies
##############################################################################

# Make dataframe of states
library(datasets)
event_map_df <- data.frame(datasets::state.abb)
colnames(event_map_df) <- "state_abb"
event_map_df <- rbind(event_map_df, "DC")

# Add FIPS codes
event_map_df$fips <- cdlTools::fips(event_map_df$state_abb, to="FIPS")

# Code treatment groups
event_map_df <- event_map_df %>% mutate(
  value = case_when(
    state_abb %in% TREATED ~ "Raised min. wage in 2014-2015",
    state_abb %in% CONTROL ~ "Used federal min. from 2011-2019",
    TRUE ~ "Not included"
  ))
event_map_df$value <- factor(event_map_df$value, levels = c("Raised min. wage in 2014-2015", "Used federal min. from 2011-2019", "Not included"))

# Map for event study states
event_map <- plot_usmap(regions="states", data=event_map_df,
                        values="value", size=0.4) +
  theme(legend.position = "right",
        text = element_text(size = 10, face = "bold"),
        plot.title = element_text(size=14, face="bold", hjust=0.5)) +
  scale_fill_manual(name = "", values = c("grey30","grey60","grey99"))

# Subset minimum wage df to treated state-years
min_wage_event <- subset(min_wage_df, fipsst %in% c(CONTROL_2, TREATED_2) &
                           year %in% c(2011:2019))

# Define treated years
min_wage_event <- min_wage_event %>% mutate(
  treated_year = case_when(
    fipsst %in% c(10,30,34,36) ~ 2014,
    fipsst %in% c(5,15,24,31,46,54) ~ 2015
  ))

# Graph minimum wages of treated states
event_desc_plot <- ggplot(subset(min_wage_event, fipsst %in% TREATED_2),
       aes(x=year, y=Effective.Minimum.Wage, group=State)) +
  geom_hline(yintercept=7.25, color="black") +
  geom_vline(xintercept=2013, color="black", linetype="dashed") +
  geom_vline(xintercept=2015, color="black", linetype="dashed") +
  geom_line(aes(group=State)) +
  ylab("Effective minimum wage") +
  xlab("Year") +
  theme_test() +
  theme(legend.position = "bottom",
        text = element_text(size = 10, face = "bold"),
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle=45, hjust=1, vjust=1),
        axis.ticks = element_blank(),
        strip.background = element_blank(),
        legend.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color="light gray", linewidth=0.5),
        panel.grid.minor.y = element_line(color="light gray", linewidth=0.25)) +
  scale_y_continuous(limits = c(6,12),
                     breaks = seq(0, 12, 2),
                     minor_breaks = seq(0, 12, 0.5),
                     labels = function(x) paste0("$",x)) +
  scale_x_continuous(limits = c(2011, 2019),
                     breaks = seq(2011, 2019, 2)) +
  facet_wrap(.~State, nrow=2)

# Compile figures
event_desc_1 <- plot_grid(event_map, event_desc_plot, rel_heights=c(0.7,1),
                        nrow=2, labels=c("A.", "B."))

# Export figure
ggsave(plot=event_desc_1, file="Exhibits/YRBS event studies, descriptives 1.pdf",
       width=6, height=5, units='in', dpi=600)

# Get mean wages in treated states
yrbs_all_model %>%
  filter(fipsst %in% TREATED_2) %>%
  group_by(treated_years) %>%
  summarise(mean = weighted.mean(x=Effective.Minimum.Wage, w=weight_2))

# Get mean wages in treated states by year
mean_tx_wages <- yrbs_all_model %>%
  filter(fipsst %in% TREATED_2) %>%
  group_by(year) %>%
  summarise(mean = weighted.mean(x=Effective.Minimum.Wage, w=weight_2))
mean_tx_wages$year <- as.numeric(as.character(mean_tx_wages$year))
mean_tx_wages$mean <- as.numeric(as.character(mean_tx_wages$mean))

# Graph minimum wages of treated states
mean_wage_plot <- ggplot(mean_tx_wages, aes(x=year, y=mean)) +
  geom_hline(yintercept=7.25, color="black") +
  geom_vline(xintercept=2013, color="black", linetype="dashed") +
  geom_vline(xintercept=2015, color="black", linetype="dashed") +
  geom_point() + geom_line() +
  ylab("Weighted mean of effective\nmin. wage in treated states") +
  xlab("Year") +
  theme_test() +
  theme(legend.position = "bottom",
        text = element_text(size = 10, face = "bold"),
        axis.title.x = element_blank(),
        axis.ticks = element_blank(),
        strip.background = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color="light gray", linewidth=0.5),
        panel.grid.minor.y = element_line(color="light gray", linewidth=0.25)) +
  scale_y_continuous(limits = c(6,12),
                     breaks = seq(0, 12, 2),
                     minor_breaks = seq(0, 12, 0.5),
                     labels = function(x) paste0("$",x)) +
  scale_x_continuous(limits = c(2010.5, 2019),
                     breaks = seq(2011, 2019, 2)) +
  geom_text(aes(label = paste0("$", round(mean, 2))),
            nudge_x=-0.3, nudge_y=0.4, size=3, fontface="bold")

# Export figure
ggsave(plot=mean_wage_plot, file="Exhibits/YRBS event studies, descriptives 2.pdf",
       width=4, height=3, units='in', dpi=600)

##############################################################################
# Main DID models: Minimum wage and mental health
##############################################################################

# Sad or hopeless
did_sad_1 <- felm(sad_hopeless ~ event_treated*treated_years |
                    age_year + fipsst | 0 | fipsst,
                  data    = yrbs_event,
                  weights = yrbs_event$weight_2)
did_sad_2 <- felm(sad_hopeless ~ event_treated*treated_years +
                    age_2 + sex_2 + race_eth_2 + grade_2 +
                    elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                    age_year + fipsst | 0 | fipsst,
                  data    = yrbs_event,
                  weights = yrbs_event$weight_2)

# Considered suicide
did_con_1 <- felm(cons_suicide ~ event_treated*treated_years |
                    age_year + fipsst | 0 | fipsst,
                  data    = yrbs_event,
                  weights = yrbs_event$weight_2)
did_con_2 <- felm(cons_suicide ~ event_treated*treated_years +
                    age_2 + sex_2 + race_eth_2 + grade_2 +
                    elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                    age_year + fipsst | 0 | fipsst,
                  data    = yrbs_event,
                  weights = yrbs_event$weight_2)

# Attempted suicide
did_att_1 <- felm(suicide_att ~ event_treated*treated_years |
                    age_year + fipsst | 0 | fipsst,
                  data    = yrbs_event,
                  weights = yrbs_event$weight_2)
did_att_2 <- felm(suicide_att ~ event_treated*treated_years +
                    age_2 + sex_2 + race_eth_2 + grade_2 +
                    elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                    age_year + fipsst | 0 | fipsst,
                  data    = yrbs_event,
                  weights = yrbs_event$weight_2)

# Recent alcohol
did_alc_1 <- felm(alcohol ~ event_treated*treated_years |
                    age_year + fipsst | 0 | fipsst,
                  data    = yrbs_event,
                  weights = yrbs_event$weight_2)
did_alc_2 <- felm(alcohol ~ event_treated*treated_years +
                    age_2 + sex_2 + race_eth_2 + grade_2 +
                    elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                    age_year + fipsst | 0 | fipsst,
                  data    = yrbs_event,
                  weights = yrbs_event$weight_2)

# Recent alcohol
did_mjn_1 <- felm(marijuana ~ event_treated*treated_years |
                    age_year + fipsst | 0 | fipsst,
                  data    = yrbs_event,
                  weights = yrbs_event$weight_2)
did_mjn_2 <- felm(marijuana ~ event_treated*treated_years +
                    age_2 + sex_2 + race_eth_2 + grade_2 +
                    elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                    age_year + fipsst | 0 | fipsst,
                  data    = yrbs_event,
                  weights = yrbs_event$weight_2)

# Physical fight
did_fgh_1 <- felm(fight ~ event_treated*treated_years |
                    age_year + fipsst | 0 | fipsst,
                  data    = yrbs_event,
                  weights = yrbs_event$weight_2)
did_fgh_2 <- felm(fight ~ event_treated*treated_years +
                    age_2 + sex_2 + race_eth_2 + grade_2 +
                    elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                    age_year + fipsst | 0 | fipsst,
                  data    = yrbs_event,
                  weights = yrbs_event$weight_2)

# Additional row in table
cov_row <- as.data.frame(
  rbind(cbind("Demographic controls",     "No",  "Yes", "No",  "Yes", "No",  "Yes"),
        cbind("State policy controls",    "No",  "Yes", "No",  "Yes", "No",  "Yes"),
        cbind("State & age-by-year FEs",  "Yes", "Yes", "Yes", "Yes", "Yes", "Yes"),
        cbind("Cluster robust SEs", "State", "State", "State", "State", "State", "State")))

# Compile results into tables
modelsummary(list("(1)" = did_sad_1,
                  "(2)" = did_sad_2,
                  "(1)" = did_con_1,
                  "(2)" = did_con_2,
                  "(1)" = did_att_1,
                  "(2)" = did_att_2),
             gof_omit    = "Log*|AIC|BIC|F|RMSE|Std",
             coef_omit   = "^(?!event_treated:treated_years)",
             coef_rename = c("event_treated:treated_years" =
                               "Effect of raise in wage"),
             statistic   = c("[{conf.low}, {conf.high}]"),
             # conf_level  = 0.99167,
             add_rows    = cov_row) %>%
  add_header_above(c(" " = 1, "Sad or hopeless" = 2, "Cons. suicide" = 2, "Att. suicide" = 2))

modelsummary(list("(1)" = did_alc_1,
                  "(2)" = did_alc_2,
                  "(1)" = did_mjn_1,
                  "(2)" = did_mjn_2,
                  "(1)" = did_fgh_1,
                  "(2)" = did_fgh_2),
             gof_omit    = "Log*|AIC|BIC|F|RMSE|Std",
             coef_omit   = "^(?!event_treated:treated_years)",
             coef_rename = c("event_treated:treated_years" =
                               "Effect of raise in wage"),
             statistic   = c("[{conf.low}, {conf.high}]"),
             conf_level  = 0.99167,
             add_rows    = cov_row) %>%
  add_header_above(c(" " = 1, "Alcohol" = 2, "Marijuana" = 2, "Phys. fight" = 2))

##############################################################################
# Main event studies: Minimum wage and mental health
##############################################################################

# Sad or hopeless
event_sad_1 <- felm(sad_hopeless ~ event_treated*year_cat |
                      age_year + fipsst | 0 | fipsst,
                    data    = yrbs_event,
                    weights = yrbs_event$weight_2)
event_sad_2 <- felm(sad_hopeless ~ event_treated*year_cat +
                      age_2 + sex_2 + race_eth_2 + grade_2 +
                      elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                      age_year + fipsst | 0 | fipsst,
                    data    = yrbs_event,
                    weights = yrbs_event$weight_2)

# Considered suicide
event_con_1 <- felm(cons_suicide ~ event_treated*year_cat |
                      age_year + fipsst | 0 | fipsst,
                    data    = yrbs_event,
                    weights = yrbs_event$weight_2)
event_con_2 <- felm(cons_suicide ~ event_treated*year_cat +
                      age_2 + sex_2 + race_eth_2 + grade_2 +
                      elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                      age_year + fipsst | 0 | fipsst,
                    data    = yrbs_event,
                    weights = yrbs_event$weight_2)

# Attempted suicide
event_att_1 <- felm(suicide_att ~ event_treated*year_cat |
                      age_year + fipsst | 0 | fipsst,
                    data    = yrbs_event,
                    weights = yrbs_event$weight_2)
event_att_2 <- felm(suicide_att ~ event_treated*year_cat +
                      age_2 + sex_2 + race_eth_2 + grade_2 +
                      elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                      age_year + fipsst | 0 | fipsst,
                    data    = yrbs_event,
                    weights = yrbs_event$weight_2)

# Recent alcohol
event_alc_1 <- felm(alcohol ~ event_treated*year_cat |
                      age_year + fipsst | 0 | fipsst,
                    data    = yrbs_event,
                    weights = yrbs_event$weight_2)
event_alc_2 <- felm(alcohol ~ event_treated*year_cat +
                      age_2 + sex_2 + race_eth_2 + grade_2 +
                      elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                      age_year + fipsst | 0 | fipsst,
                    data    = yrbs_event,
                    weights = yrbs_event$weight_2)

# Recent alcohol
event_mjn_1 <- felm(marijuana ~ event_treated*year_cat |
                      age_year + fipsst | 0 | fipsst,
                    data    = yrbs_event,
                    weights = yrbs_event$weight_2)
event_mjn_2 <- felm(marijuana ~ event_treated*year_cat +
                      age_2 + sex_2 + race_eth_2 + grade_2 +
                      elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                      age_year + fipsst | 0 | fipsst,
                    data    = yrbs_event,
                    weights = yrbs_event$weight_2)

# Physical fight
event_fgh_1 <- felm(fight ~ event_treated*year_cat |
                      age_year + fipsst | 0 | fipsst,
                    data    = yrbs_event,
                    weights = yrbs_event$weight_2)
event_fgh_2 <- felm(fight ~ event_treated*year_cat +
                      age_2 + sex_2 + race_eth_2 + grade_2 +
                      elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                      age_year + fipsst | 0 | fipsst,
                    data    = yrbs_event,
                    weights = yrbs_event$weight_2)

# Get values from models
event_df <- NULL

# Adolescents (FE only)
event_df <- make_event_df(event_df, event_sad_1, "Adolescents (FE only)")
event_df <- make_event_df(event_df, event_con_1, "Adolescents (FE only)")
event_df <- make_event_df(event_df, event_att_1, "Adolescents (FE only)")
event_df <- make_event_df(event_df, event_alc_1, "Adolescents (FE only)")
event_df <- make_event_df(event_df, event_mjn_1, "Adolescents (FE only)")
event_df <- make_event_df(event_df, event_fgh_1, "Adolescents (FE only)")

# Adolescents (fully adjusted)
event_df <- make_event_df(event_df, event_sad_2, "Adolescents (fully adjusted)")
event_df <- make_event_df(event_df, event_con_2, "Adolescents (fully adjusted)")
event_df <- make_event_df(event_df, event_att_2, "Adolescents (fully adjusted)")
event_df <- make_event_df(event_df, event_alc_2, "Adolescents (fully adjusted)")
event_df <- make_event_df(event_df, event_mjn_2, "Adolescents (fully adjusted)")
event_df <- make_event_df(event_df, event_fgh_2, "Adolescents (fully adjusted)")

# Reorder outcomes
event_df$outcome <- factor(
  event_df$outcome, levels=c("Sad or hopeless", "Considered suicide", "Attempted suicide", "Recent alcohol", "Recent marijuana", "Physical fight"))

# Generate event study plot: Main
event_plot <- print_event_plot(
  event_df,
  Y_TITLE    = "Effect of raising minimum wage\non indicated outcome by year",
  Y_MIN      = -0.1,
  Y_MAX      =  0.1,
  COLORS     = "Standard"
)

# Export figure
ggsave(plot=event_plot, file="Exhibits/YRBS event studies, main.pdf",
       width=6, height=6, units='in', dpi=600)

##############################################################################
# Event study robustness check: Nested clusters
##############################################################################

# Sad or hopeless
event_sad_1c <- felm(sad_hopeless ~ event_treated*year_cat |
                       age_year + fipsst | 0 | cluster,
                     data    = yrbs_event,
                     weights = yrbs_event$weight_2)
event_sad_2c <- felm(sad_hopeless ~ event_treated*year_cat +
                       age_2 + sex_2 + race_eth_2 + grade_2 +
                       elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                       age_year + fipsst | 0 | cluster,
                     data    = yrbs_event,
                     weights = yrbs_event$weight_2)

# Considered suicide
event_con_1c <- felm(cons_suicide ~ event_treated*year_cat |
                       age_year + fipsst | 0 | cluster,
                     data    = yrbs_event,
                     weights = yrbs_event$weight_2)
event_con_2c <- felm(cons_suicide ~ event_treated*year_cat +
                       age_2 + sex_2 + race_eth_2 + grade_2 +
                       elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                       age_year + fipsst | 0 | cluster,
                     data    = yrbs_event,
                     weights = yrbs_event$weight_2)

# Attempted suicide
event_att_1c <- felm(suicide_att ~ event_treated*year_cat |
                       age_year + fipsst | 0 | cluster,
                     data    = yrbs_event,
                     weights = yrbs_event$weight_2)
event_att_2c <- felm(suicide_att ~ event_treated*year_cat +
                       age_2 + sex_2 + race_eth_2 + grade_2 +
                       elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                       age_year + fipsst | 0 | cluster,
                     data    = yrbs_event,
                     weights = yrbs_event$weight_2)

# Recent alcohol
event_alc_1c <- felm(alcohol ~ event_treated*year_cat |
                       age_year + fipsst | 0 | cluster,
                     data    = yrbs_event,
                     weights = yrbs_event$weight_2)
event_alc_2c <- felm(alcohol ~ event_treated*year_cat +
                       age_2 + sex_2 + race_eth_2 + grade_2 +
                       elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                       age_year + fipsst | 0 | cluster,
                     data    = yrbs_event,
                     weights = yrbs_event$weight_2)

# Recent alcohol
event_mjn_1c <- felm(marijuana ~ event_treated*year_cat |
                       age_year + fipsst | 0 | cluster,
                     data    = yrbs_event,
                     weights = yrbs_event$weight_2)
event_mjn_2c <- felm(marijuana ~ event_treated*year_cat +
                       age_2 + sex_2 + race_eth_2 + grade_2 +
                       elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                       age_year + fipsst | 0 | cluster,
                     data    = yrbs_event,
                     weights = yrbs_event$weight_2)

# Physical fight
event_fgh_1c <- felm(fight ~ event_treated*year_cat |
                       age_year + fipsst | 0 | cluster,
                     data    = yrbs_event,
                     weights = yrbs_event$weight_2)
event_fgh_2c <- felm(fight ~ event_treated*year_cat +
                       age_2 + sex_2 + race_eth_2 + grade_2 +
                       elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                       age_year + fipsst | 0 | cluster,
                     data    = yrbs_event,
                     weights = yrbs_event$weight_2)

# Get values from models
ev_clust_df <- NULL

# Adolescents (FE only, nested clust.)
ev_clust_df <- make_event_df(ev_clust_df, event_sad_1c, "Adolescents (FE only, nested clust.)")
ev_clust_df <- make_event_df(ev_clust_df, event_con_1c, "Adolescents (FE only, nested clust.)")
ev_clust_df <- make_event_df(ev_clust_df, event_att_1c, "Adolescents (FE only, nested clust.)")
ev_clust_df <- make_event_df(ev_clust_df, event_alc_1c, "Adolescents (FE only, nested clust.)")
ev_clust_df <- make_event_df(ev_clust_df, event_mjn_1c, "Adolescents (FE only, nested clust.)")
ev_clust_df <- make_event_df(ev_clust_df, event_fgh_1c, "Adolescents (FE only, nested clust.)")

# Adolescents (fully adj., nested clust.)
ev_clust_df <- make_event_df(ev_clust_df, event_sad_2c,
                             "Adolescents (fully adj., nested clust.)")
ev_clust_df <- make_event_df(ev_clust_df, event_con_2c,
                             "Adolescents (fully adj., nested clust.)")
ev_clust_df <- make_event_df(ev_clust_df, event_att_2c,
                             "Adolescents (fully adj., nested clust.)")
ev_clust_df <- make_event_df(ev_clust_df, event_alc_2c,
                             "Adolescents (fully adj., nested clust.)")
ev_clust_df <- make_event_df(ev_clust_df, event_mjn_2c,
                             "Adolescents (fully adj., nested clust.)")
ev_clust_df <- make_event_df(ev_clust_df, event_fgh_2c,
                             "Adolescents (fully adj., nested clust.)")

# Reorder outcomes
ev_clust_df$outcome <- factor(
  ev_clust_df$outcome, levels=c("Sad or hopeless", "Considered suicide", "Attempted suicide", "Recent alcohol", "Recent marijuana", "Physical fight"))

# Generate event study plot: Nested clusters
event_clust_plot <- print_event_plot(
  ev_clust_df,
  Y_TITLE    = "Effect of raising minimum wage\non indicated outcome by year",
  Y_MIN      = -0.1,
  Y_MAX      =  0.1,
  COLORS     = "Standard"
)

# Export figure
ggsave(plot=event_clust_plot, file="Exhibits/YRBS event studies, nested clusters.pdf",
       width=6, height=6, units='in', dpi=600)

##############################################################################
# Event study robustness check: Strictly balanced panel
##############################################################################

# Function to extract and clean values from TWFE models.
# Requires: Df for saving coefficients, "lfe" model, title of model.
# Returns: Df of values, ready to pass to "clean_coef_df".
make_event_df_2 <- function(event_df, event_model, TITLE) {
  
  # Get outcome labels
  if (event_model$lhs == "sad_hopeless") {
    outcome  <- "Sad or hopeless"}
  
  if (event_model$lhs == "cons_suicide") {
    outcome  <- "Considered suicide"}
  
  if (event_model$lhs == "suicide_att") {
    outcome  <- "Attempted suicide"}
  
  if (event_model$lhs == "alcohol") {
    outcome  <- "Recent alcohol"}
  
  if (event_model$lhs == "marijuana") {
    outcome  <- "Recent marijuana"}
  
  if (event_model$lhs == "fight") {
    outcome  <- "Physical fight"}
  
  # Get coefficients of model
  coef <- data.frame(cbind(outcome, event_model$coefficients, event_model$cse, TITLE))
  colnames(coef) <- c("outcome", "effect", "se", "name"); coef$var <- rownames(coef)
  
  # Only include year coefficients
  coef <- coef %>% filter(grepl("event_tx_balance:year_cat", var))
  
  # Rename period variable
  coef$period <- gsub("event_tx_balance:year_cat", "", coef$var)
  
  # Add zero period
  coef <- data.frame(rbind(coef, c(
    outcome, 0, 0, TITLE, "event_tx_balance:year_cat2013", 2013)))
  
  # Treat columns as numeric
  coef$effect  <- as.numeric(coef$effect)
  coef$se      <- as.numeric(coef$se)
  coef$period  <- as.numeric(coef$period)
  
  # Add to existing event study df
  event_df <- rbind(event_df, coef)
  
  return(event_df)
}

# Define treated and control states
TREATED_BALANCE <- c("AR","HI","MD","MT","NE","NY","WV")
CONTROL_BALANCE <- c("ID","KY","NC","ND","NH","OK","SC","TN","VA")
TREATED_BALANCE_2 <- cdlTools::fips(TREATED_BALANCE, to="FIPS")
CONTROL_BALANCE_2 <- cdlTools::fips(CONTROL_BALANCE, to="FIPS")

# Code treatment groups
yrbs_event <- yrbs_event %>% mutate(
  event_tx_balance = case_when(
    fipsst %in% TREATED_BALANCE_2 ~ 1,
    fipsst %in% CONTROL_BALANCE_2 ~ 0
  ))

# Sad or hopeless
event_sad_1b <- felm(sad_hopeless ~ event_tx_balance*year_cat |
                       age_year + fipsst | 0 | fipsst,
                     data    = yrbs_event,
                     weights = yrbs_event$weight_2)
event_sad_2b <- felm(sad_hopeless ~ event_tx_balance*year_cat +
                       age_2 + sex_2 + race_eth_2 + grade_2 +
                       elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                       age_year + fipsst | 0 | fipsst,
                     data    = yrbs_event,
                     weights = yrbs_event$weight_2)

# Considered suicide
event_con_1b <- felm(cons_suicide ~ event_tx_balance*year_cat |
                       age_year + fipsst | 0 | fipsst,
                     data    = yrbs_event,
                     weights = yrbs_event$weight_2)
event_con_2b <- felm(cons_suicide ~ event_tx_balance*year_cat +
                       age_2 + sex_2 + race_eth_2 + grade_2 +
                       elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                       age_year + fipsst | 0 | fipsst,
                     data    = yrbs_event,
                     weights = yrbs_event$weight_2)

# Attempted suicide
event_att_1b <- felm(suicide_att ~ event_tx_balance*year_cat |
                       age_year + fipsst | 0 | fipsst,
                     data    = yrbs_event,
                     weights = yrbs_event$weight_2)
event_att_2b <- felm(suicide_att ~ event_tx_balance*year_cat +
                       age_2 + sex_2 + race_eth_2 + grade_2 +
                       elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                       age_year + fipsst | 0 | fipsst,
                     data    = yrbs_event,
                     weights = yrbs_event$weight_2)

# Recent alcohol
event_alc_1b <- felm(alcohol ~ event_tx_balance*year_cat |
                       age_year + fipsst | 0 | fipsst,
                     data    = yrbs_event,
                     weights = yrbs_event$weight_2)
event_alc_2b <- felm(alcohol ~ event_tx_balance*year_cat +
                       age_2 + sex_2 + race_eth_2 + grade_2 +
                       elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                       age_year + fipsst | 0 | fipsst,
                     data    = yrbs_event,
                     weights = yrbs_event$weight_2)

# Recent alcohol
event_mjn_1b <- felm(marijuana ~ event_tx_balance*year_cat |
                       age_year + fipsst | 0 | fipsst,
                     data    = yrbs_event,
                     weights = yrbs_event$weight_2)
event_mjn_2b <- felm(marijuana ~ event_tx_balance*year_cat +
                       age_2 + sex_2 + race_eth_2 + grade_2 +
                       elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                       age_year + fipsst | 0 | fipsst,
                     data    = yrbs_event,
                     weights = yrbs_event$weight_2)

# Physical fight
event_fgh_1b <- felm(fight ~ event_tx_balance*year_cat |
                       age_year + fipsst | 0 | fipsst,
                     data    = yrbs_event,
                     weights = yrbs_event$weight_2)
event_fgh_2b <- felm(fight ~ event_tx_balance*year_cat +
                       age_2 + sex_2 + race_eth_2 + grade_2 +
                       elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                       age_year + fipsst | 0 | fipsst,
                     data    = yrbs_event,
                     weights = yrbs_event$weight_2)

# Get values from models
balance_df <- NULL

# Adolescents (FE only)
balance_df <- make_event_df_2(balance_df, event_sad_1b, "Adolescents (FE only)")
balance_df <- make_event_df_2(balance_df, event_con_1b, "Adolescents (FE only)")
balance_df <- make_event_df_2(balance_df, event_att_1b, "Adolescents (FE only)")
balance_df <- make_event_df_2(balance_df, event_alc_1b, "Adolescents (FE only)")
balance_df <- make_event_df_2(balance_df, event_mjn_1b, "Adolescents (FE only)")
balance_df <- make_event_df_2(balance_df, event_fgh_1b, "Adolescents (FE only)")

# Adolescents (fully adjusted)
balance_df <- make_event_df_2(balance_df, event_sad_2b,
                                "Adolescents (fully adjusted)")
balance_df <- make_event_df_2(balance_df, event_con_2b,
                                "Adolescents (fully adjusted)")
balance_df <- make_event_df_2(balance_df, event_att_2b,
                                "Adolescents (fully adjusted)")
balance_df <- make_event_df_2(balance_df, event_alc_2b,
                                "Adolescents (fully adjusted)")
balance_df <- make_event_df_2(balance_df, event_mjn_2b,
                                "Adolescents (fully adjusted)")
balance_df <- make_event_df_2(balance_df, event_fgh_2b,
                                "Adolescents (fully adjusted)")

# Reorder outcomes
balance_df$outcome <- factor(
  balance_df$outcome, levels=c("Sad or hopeless", "Considered suicide", "Attempted suicide", "Recent alcohol", "Recent marijuana", "Physical fight"))

# Get min. and max. N
balance_n <-
  (c(length(event_sad_1b$residuals), length(event_sad_2b$residuals),
     length(event_con_1b$residuals), length(event_con_2b$residuals),
     length(event_att_1b$residuals), length(event_att_2b$residuals),
     length(event_alc_1b$residuals), length(event_alc_2b$residuals),
     length(event_mjn_1b$residuals), length(event_mjn_2b$residuals),
     length(event_fgh_1b$residuals), length(event_fgh_2b$residuals)))
min(balance_n); max(balance_n)

# Generate event study plot: Balanced panel
event_plot_balance <- print_event_plot(
  balance_df,
  Y_TITLE    = "Effect of raising minimum wage\non indicated outcome by year",
  Y_MIN      = -0.1,
  Y_MAX      =  0.1,
  COLORS     = "Standard"
)

# Export figure
ggsave(plot=event_plot_balance, file="Exhibits/YRBS event studies, balanced.pdf",
       width=6, height=6, units='in', dpi=600)
