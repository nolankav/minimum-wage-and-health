# Minimum wage and children's mental health
# Analyses using the Youth Risk Behavior Surveillance System
# N.M. Kavanagh, M. McConnell, N. Slopen
# June 27, 2023

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

##############################################################################
# Specify complex design
##############################################################################

# Rescale weight so mean is 1
yrbs_all$weight_2 <- yrbs_all$weight/66.61

# Define complex sampling design
# Nest strata within states and apply survey weights
design_all <- svydesign(id=~PSU, strata=~stratum, nest=TRUE,
                        weights=~weight_2, data=yrbs_all)

# Subset to years of interest
design_sub <- subset(design_all, year %in% c(2000:2020))

##############################################################################
# Table 1: Demographic characteristics
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
# This line provides demographic characteristics without survey weights.
summary(tableby(~ age_2 + sex_2 + race_eth_2 + grade_2,
                yrbs_all_model, digits.pct=0), text=T)

# Child characteristics: weighted
# This line provides demographic characteristics with survey weights.
summary(tableby(~ age_2 + sex_2 + race_eth_2 + grade_2,
                yrbs_all_model, digits.pct=0, weights=weight_2), text=T)

##############################################################################
# Figure 1: U.S. maps for minimum wages
##############################################################################

# Generate new state variable
# Lowercase necessary for maps function
min_wage_df$state <- min_wage_df$State

# Generate lagged minimum wage
# Gets minimum wage of 18 years prior in same state
min_wage_df <- min_wage_df %>%
  group_by(state) %>%
  mutate(lag_by_18 = lag(Effective.Minimum.Wage, n=18, default=NA))

# Compute change in minimum wage
# Note: Must use 2019 df when making map
min_wage_df$change <- min_wage_df$Effective.Minimum.Wage - min_wage_df$lag_by_18

# Subset to years of interest
min_wage_2001 <- subset(min_wage_df, year %in% c(2001))
min_wage_2019 <- subset(min_wage_df, year %in% c(2019))

# Descriptives about minimum wage
table(subset(min_wage_2019, fipsst %in% c(0:56))$change)
table(subset(min_wage_2019, fipsst %in% c(0:56))$change == 0)

# Map for minimum wage in 2001
map_2001 <- plot_usmap(regions="states", data=min_wage_2001,
                           values="Effective.Minimum.Wage", size=0.4) +
  ggtitle("2001") +
  theme(legend.position = "right",
        text = element_text(size = 10, face = "bold"),
        plot.title = element_text(size=14, face="bold", hjust=0.5)) +
  viridis::scale_fill_viridis(limits=c(5.15, 13.5), breaks=c(5.15, 6, 8, 10, 12, 13.5), labels=c("\n\n$5.15\n(fed. min.\nin 2001)", "$6", "$8", "$10", "$12", "$13.5"), name="Effective\nmin. wage")

# Map for minimum wage in 2019
map_2019 <- plot_usmap(regions="states", data=min_wage_2019,
                       values="Effective.Minimum.Wage", size=0.4) +
  ggtitle("2019") +
  theme(legend.position = "right",
        text = element_text(size = 10, face = "bold"),
        plot.title = element_text(size=14, face="bold", hjust=0.5)) +
  viridis::scale_fill_viridis(limits=c(5.15, 13.5), breaks=c(5.15, 6, 8, 10, 12, 13.5), labels=c("\n\n$5.15\n(fed. min.\nin 2001)", "$6", "$8", "$10", "$12", "$13.5"), name="Effective\nmin. wage")

# Map for change in minimum wage
map_change <- plot_usmap(regions="states", data=min_wage_2019,
                       values="change", size=0.4) +
  ggtitle("Change") +
  theme(legend.position = "right",
        text = element_text(size = 10, face = "bold"),
        plot.title = element_text(size=14, face="bold", hjust=0.5)) +
  viridis::scale_fill_viridis(limits=c(2.1, 6.85), breaks=c(2.10, 3, 4, 5, 6, 6.85), labels=c("$2.10", "$3", "$4", "$5", "$6", "$6.85"), name="Increase in\nmin. wage", oob=squish)

# library(scales)
# show_col(viridis_pal()(6))

# Compile figures into object
maps_all <- plot_grid(map_2001, map_2019, map_change, nrow=3)

# Export figure
ggsave(plot=maps_all, file="Map of minimum wages, YRBS.pdf", width=5, height=7, units='in', dpi=600)

##############################################################################
# Figure 1: Map of minimum wages
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

# Generate sampling clusters
yrbs_all_model$cluster <- paste0(yrbs_all_model$fipsst, "-", yrbs_all_model$PSU, "-", yrbs_all_model$stratum)

# Generate age-by-year fixed effects
yrbs_all_model$age_year <- paste0(yrbs_all_model$age_2, "-", yrbs_all_model$year)

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
    category <- "School"}
  
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
    coef_df$Category, levels=c("Symptoms", "Substances", "School"))
  
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
      
      # State-level cluster models
      "Adolescents (FE only, nested clust.)",
      "Adolescents (FE only, state clust.)",
      "Adolescents (fully adj., nested clust.)",
      "Adolescents (fully adj., state clust.)"
    ))
  
  # Return df
  return(coef_df)
}

# Function to generate coefficient plots.
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

# Sad and hopeless
model_min_sad_1 <- felm(sad_hopeless ~ Effective.Minimum.Wage |
                          age_year + fipsst | 0 | cluster,
                        data    = yrbs_all_model,
                        weights = yrbs_all_model$weight_2)
model_min_sad_2 <- felm(sad_hopeless ~ Effective.Minimum.Wage +
                          age_2 + sex_2 + race_eth_2 + grade_2 +
                          elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                          age_year + fipsst | 0 | cluster,
                        data    = yrbs_all_model,
                        weights = yrbs_all_model$weight_2)

# Considered suicide
model_min_con_1 <- felm(cons_suicide ~ Effective.Minimum.Wage |
                          age_year + fipsst | 0 | cluster,
                        data    = yrbs_all_model,
                        weights = yrbs_all_model$weight_2)
model_min_con_2 <- felm(cons_suicide ~ Effective.Minimum.Wage +
                          age_2 + sex_2 + race_eth_2 + grade_2 +
                          elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                          age_year + fipsst | 0 | cluster,
                        data    = yrbs_all_model,
                        weights = yrbs_all_model$weight_2)

# Attempted suicide
model_min_att_1 <- felm(suicide_att ~ Effective.Minimum.Wage |
                          age_year + fipsst | 0 | cluster,
                        data    = yrbs_all_model,
                        weights = yrbs_all_model$weight_2)
model_min_att_2 <- felm(suicide_att ~ Effective.Minimum.Wage +
                          age_2 + sex_2 + race_eth_2 + grade_2 +
                          elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                          age_year + fipsst | 0 | cluster,
                        data    = yrbs_all_model,
                        weights = yrbs_all_model$weight_2)

# Recent alcohol
model_min_alc_1 <- felm(alcohol ~ Effective.Minimum.Wage |
                          age_year + fipsst | 0 | cluster,
                        data    = yrbs_all_model,
                        weights = yrbs_all_model$weight_2)
model_min_alc_2 <- felm(alcohol ~ Effective.Minimum.Wage +
                          age_2 + sex_2 + race_eth_2 + grade_2 +
                          elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                          age_year + fipsst | 0 | cluster,
                        data    = yrbs_all_model,
                        weights = yrbs_all_model$weight_2)

# Recent marijuana
model_min_mjn_1 <- felm(marijuana ~ Effective.Minimum.Wage |
                          age_year + fipsst | 0 | cluster,
                        data    = yrbs_all_model,
                        weights = yrbs_all_model$weight_2)
model_min_mjn_2 <- felm(marijuana ~ Effective.Minimum.Wage +
                          age_2 + sex_2 + race_eth_2 + grade_2 +
                          elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                          age_year + fipsst | 0 | cluster,
                        data    = yrbs_all_model,
                        weights = yrbs_all_model$weight_2)

# Physical fight
model_min_fgh_1 <- felm(fight ~ Effective.Minimum.Wage |
                          age_year + fipsst | 0 | cluster,
                        data    = yrbs_all_model,
                        weights = yrbs_all_model$weight_2)
model_min_fgh_2 <- felm(fight ~ Effective.Minimum.Wage +
                          age_2 + sex_2 + race_eth_2 + grade_2 +
                          elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                          age_year + fipsst | 0 | cluster,
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

# Generate coefficient plot: Main
plot_main <- print_coef_plot(
  main_df,
  Y_TITLE    = "Association of $1 increase in min. wage\nwith adolescents' mental health",
  Y_MIN      = -0.045,
  Y_MAX      =  0.045,
  COLORS     = "Standard"
)

# Export figure
ggsave(plot=plot_main, file="Exhibits/YRBS coefficient plot, main.pdf",
       width=5, height=4, units='in', dpi=600)

##############################################################################
# Robustness check: Sub-population
##############################################################################

# Sad and hopeless
model_min_sad_r <- felm(sad_hopeless ~ Effective.Minimum.Wage*race_eth_cat +
                          age_2 + sex_2 + race_eth_2 + grade_2 +
                          elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                          age_year + fipsst | 0 | cluster,
                        data    = yrbs_all_model,
                        weights = yrbs_all_model$weight_2)

# Considered suicide
model_min_con_r <- felm(cons_suicide ~ Effective.Minimum.Wage*race_eth_cat +
                          age_2 + sex_2 + race_eth_2 + grade_2 +
                          elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                          age_year + fipsst | 0 | cluster,
                        data    = yrbs_all_model,
                        weights = yrbs_all_model$weight_2)

# Attempted suicide
model_min_att_r <- felm(suicide_att ~ Effective.Minimum.Wage*race_eth_cat +
                          age_2 + sex_2 + race_eth_2 + grade_2 +
                          elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                          age_year + fipsst | 0 | cluster,
                        data    = yrbs_all_model,
                        weights = yrbs_all_model$weight_2)

# Recent alcohol
model_min_alc_r <- felm(alcohol ~ Effective.Minimum.Wage*race_eth_cat +
                          age_2 + sex_2 + race_eth_2 + grade_2 +
                          elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                          age_year + fipsst | 0 | cluster,
                        data    = yrbs_all_model,
                        weights = yrbs_all_model$weight_2)

# Recent marijuana
model_min_mjn_r <- felm(marijuana ~ Effective.Minimum.Wage*race_eth_cat +
                          age_2 + sex_2 + race_eth_2 + grade_2 +
                          elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                          age_year + fipsst | 0 | cluster,
                        data    = yrbs_all_model,
                        weights = yrbs_all_model$weight_2)

# Physical fight
model_min_fgh_r <- felm(fight ~ Effective.Minimum.Wage*race_eth_cat +
                          age_2 + sex_2 + race_eth_2 + grade_2 +
                          elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                          age_year + fipsst | 0 | cluster,
                        data    = yrbs_all_model,
                        weights = yrbs_all_model$weight_2)

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
# Robustness check: Alternate specifications
##############################################################################

# Sad and hopeless
model_min_sad_x <- felm(sad_hopeless ~ Effective.Minimum.Wage.2020.Dollars +
                          age_2 + sex_2 + race_eth_2 + grade_2 +
                          elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                          age_year + fipsst | 0 | cluster,
                        data    = yrbs_all_model,
                        weights = yrbs_all_model$weight_2)
model_min_sad_y <- felm(sad_hopeless ~ lag_by_1 +
                          age_2 + sex_2 + race_eth_2 + grade_2 +
                          elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                          age_year + fipsst | 0 | cluster,
                        data    = yrbs_all_model,
                        weights = yrbs_all_model$weight_2)

# Considered suicide
model_min_con_x <- felm(cons_suicide ~ Effective.Minimum.Wage.2020.Dollars +
                          age_2 + sex_2 + race_eth_2 + grade_2 +
                          elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                          age_year + fipsst | 0 | cluster,
                        data    = yrbs_all_model,
                        weights = yrbs_all_model$weight_2)
model_min_con_y <- felm(cons_suicide ~ lag_by_1 +
                          age_2 + sex_2 + race_eth_2 + grade_2 +
                          elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                          age_year + fipsst | 0 | cluster,
                        data    = yrbs_all_model,
                        weights = yrbs_all_model$weight_2)

# Attempted suicide
model_min_att_x <- felm(suicide_att ~ Effective.Minimum.Wage.2020.Dollars +
                          age_2 + sex_2 + race_eth_2 + grade_2 +
                          elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                          age_year + fipsst | 0 | cluster,
                        data    = yrbs_all_model,
                        weights = yrbs_all_model$weight_2)
model_min_att_y <- felm(suicide_att ~ lag_by_1 +
                          age_2 + sex_2 + race_eth_2 + grade_2 +
                          elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                          age_year + fipsst | 0 | cluster,
                        data    = yrbs_all_model,
                        weights = yrbs_all_model$weight_2)

# Recent alcohol
model_min_alc_x <- felm(alcohol ~ Effective.Minimum.Wage.2020.Dollars +
                          age_2 + sex_2 + race_eth_2 + grade_2 +
                          elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                          age_year + fipsst | 0 | cluster,
                        data    = yrbs_all_model,
                        weights = yrbs_all_model$weight_2)
model_min_alc_y <- felm(alcohol ~ lag_by_1 +
                          age_2 + sex_2 + race_eth_2 + grade_2 +
                          elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                          age_year + fipsst | 0 | cluster,
                        data    = yrbs_all_model,
                        weights = yrbs_all_model$weight_2)

# Recent marijuana
model_min_mjn_x <- felm(marijuana ~ Effective.Minimum.Wage.2020.Dollars +
                          age_2 + sex_2 + race_eth_2 + grade_2 +
                          elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                          age_year + fipsst | 0 | cluster,
                        data    = yrbs_all_model,
                        weights = yrbs_all_model$weight_2)
model_min_mjn_y <- felm(marijuana ~ lag_by_1 +
                          age_2 + sex_2 + race_eth_2 + grade_2 +
                          elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                          age_year + fipsst | 0 | cluster,
                        data    = yrbs_all_model,
                        weights = yrbs_all_model$weight_2)

# Physical fight
model_min_fgh_x <- felm(fight ~ Effective.Minimum.Wage.2020.Dollars +
                          age_2 + sex_2 + race_eth_2 + grade_2 +
                          elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                          age_year + fipsst | 0 | cluster,
                        data    = yrbs_all_model,
                        weights = yrbs_all_model$weight_2)
model_min_fgh_y <- felm(fight ~ lag_by_1 +
                          age_2 + sex_2 + race_eth_2 + grade_2 +
                          elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                          age_year + fipsst | 0 | cluster,
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
# Robustness check: Logistic regression
##############################################################################

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
        exp(coef(log_min_dep_1))[2],
        exp(confint(log_min_dep_1))[2,1],
        exp(confint(log_min_dep_1))[2,2],
        exp(confint(log_min_dep_1, level=0.997))[2,1],
        exp(confint(log_min_dep_1, level=0.997))[2,2],
        length(log_min_dep_1$residuals)),
  
  cbind("Depression", "Diagnoses", "All children (fully adjusted)",
        exp(coef(log_min_dep_2))[2],
        exp(confint(log_min_dep_2))[2,1],
        exp(confint(log_min_dep_2))[2,2],
        exp(confint(log_min_dep_2, level=0.997))[2,1],
        exp(confint(log_min_dep_2, level=0.997))[2,2],
        length(log_min_dep_2$residuals)),
  
  # Anxiety
  cbind("Anxiety", "Diagnoses", "All children (FE only)",
        exp(coef(log_min_anx_1))[2],
        exp(confint(log_min_anx_1))[2,1],
        exp(confint(log_min_anx_1))[2,2],
        exp(confint(log_min_anx_1, level=0.997))[2,1],
        exp(confint(log_min_anx_1, level=0.997))[2,2],
        length(log_min_anx_1$residuals)),
  
  cbind("Anxiety", "Diagnoses", "All children (fully adjusted)",
        exp(coef(log_min_anx_2))[2],
        exp(confint(log_min_anx_2))[2,1],
        exp(confint(log_min_anx_2))[2,2],
        exp(confint(log_min_anx_2, level=0.997))[2,1],
        exp(confint(log_min_anx_2, level=0.997))[2,2],
        length(log_min_anx_2$residuals)),
  
  # ADD/ADHD
  cbind("ADD/ADHD", "Diagnoses", "All children (FE only)",
        exp(coef(log_min_add_1))[2],
        exp(confint(log_min_add_1))[2,1],
        exp(confint(log_min_add_1))[2,2],
        exp(confint(log_min_add_1, level=0.997))[2,1],
        exp(confint(log_min_add_1, level=0.997))[2,2],
        length(log_min_add_1$residuals)),
  
  cbind("ADD/ADHD", "Diagnoses", "All children (fully adjusted)",
        exp(coef(log_min_add_2))[2],
        exp(confint(log_min_add_2))[2,1],
        exp(confint(log_min_add_2))[2,2],
        exp(confint(log_min_add_2, level=0.997))[2,1],
        exp(confint(log_min_add_2, level=0.997))[2,2],
        length(log_min_add_2$residuals)),
  
  # Behavioral problems
  cbind("Behavioral prob.", "Diagnoses", "All children (FE only)",
        exp(coef(log_min_beh_1))[2],
        exp(confint(log_min_beh_1))[2,1],
        exp(confint(log_min_beh_1))[2,2],
        exp(confint(log_min_beh_1, level=0.997))[2,1],
        exp(confint(log_min_beh_1, level=0.997))[2,2],
        length(log_min_beh_1$residuals)),
  
  cbind("Behavioral prob.", "Diagnoses", "All children (fully adjusted)",
        exp(coef(log_min_beh_2))[2],
        exp(confint(log_min_beh_2))[2,1],
        exp(confint(log_min_beh_2))[2,2],
        exp(confint(log_min_beh_2, level=0.997))[2,1],
        exp(confint(log_min_beh_2, level=0.997))[2,2],
        length(log_min_beh_2$residuals)),
  
  # Digestive issues
  cbind("Digestive issues", "Sx.", "All children (FE only)",
        exp(coef(log_min_dig_1))[2],
        exp(confint(log_min_dig_1))[2,1],
        exp(confint(log_min_dig_1))[2,2],
        exp(confint(log_min_dig_1, level=0.997))[2,1],
        exp(confint(log_min_dig_1, level=0.997))[2,2],
        length(log_min_dig_1$residuals)),
  
  cbind("Digestive issues", "Sx.", "All children (fully adjusted)",
        exp(coef(log_min_dig_2))[2],
        exp(confint(log_min_dig_2))[2,1],
        exp(confint(log_min_dig_2))[2,2],
        exp(confint(log_min_dig_2, level=0.997))[2,1],
        exp(confint(log_min_dig_2, level=0.997))[2,2],
        length(log_min_dig_2$residuals)),
  
  # Any unmet care
  cbind("Any unmet care", "Health care", "All children (FE only)",
        exp(coef(log_min_unm_1))[2],
        exp(confint(log_min_unm_1))[2,1],
        exp(confint(log_min_unm_1))[2,2],
        exp(confint(log_min_unm_1, level=0.997))[2,1],
        exp(confint(log_min_unm_1, level=0.997))[2,2],
        length(log_min_unm_1$residuals)),
  
  cbind("Any unmet care", "Health care", "All children (fully adjusted)",
        exp(coef(log_min_unm_2))[2],
        exp(confint(log_min_unm_2))[2,1],
        exp(confint(log_min_unm_2))[2,2],
        exp(confint(log_min_unm_2, level=0.997))[2,1],
        exp(confint(log_min_unm_2, level=0.997))[2,2],
        length(log_min_unm_2$residuals)),
  
  # Unmet mental care
  cbind("Unmet mental care", "Health care", "All children (FE only)",
        exp(coef(log_min_men_1))[2],
        exp(confint(log_min_men_1))[2,1],
        exp(confint(log_min_men_1))[2,2],
        exp(confint(log_min_men_1, level=0.997))[2,1],
        exp(confint(log_min_men_1, level=0.997))[2,2],
        length(log_min_men_1$residuals)),
  
  cbind("Unmet mental care", "Health care", "All children (fully adjusted)",
        exp(coef(log_min_men_2))[2],
        exp(confint(log_min_men_2))[2,1],
        exp(confint(log_min_men_2))[2,2],
        exp(confint(log_min_men_2, level=0.997))[2,1],
        exp(confint(log_min_men_2, level=0.997))[2,2],
        length(log_min_men_2$residuals)),
  
  # 7+ school absences
  cbind("7+ school absences", "School & Work", "All children (FE only)",
        exp(coef(log_min_sch_1))[2],
        exp(confint(log_min_sch_1))[2,1],
        exp(confint(log_min_sch_1))[2,2],
        exp(confint(log_min_sch_1, level=0.997))[2,1],
        exp(confint(log_min_sch_1, level=0.997))[2,2],
        length(log_min_sch_1$residuals)),
  
  cbind("7+ school absences", "School & Work", "All children (fully adjusted)",
        exp(coef(log_min_sch_2))[2],
        exp(confint(log_min_sch_2))[2,1],
        exp(confint(log_min_sch_2))[2,2],
        exp(confint(log_min_sch_2, level=0.997))[2,1],
        exp(confint(log_min_sch_2, level=0.997))[2,2],
        length(log_min_sch_2$residuals)),
  
  # Child employment
  cbind("Child employment", "School & Work", "All children (FE only)",
        exp(coef(log_min_job_1))[2],
        exp(confint(log_min_job_1))[2,1],
        exp(confint(log_min_job_1))[2,2],
        exp(confint(log_min_job_1, level=0.997))[2,1],
        exp(confint(log_min_job_1, level=0.997))[2,2],
        length(log_min_job_1$residuals)),
  
  cbind("Child employment", "School & Work", "All children (fully adjusted)",
        exp(coef(log_min_job_2))[2],
        exp(confint(log_min_job_2))[2,1],
        exp(confint(log_min_job_2))[2,2],
        exp(confint(log_min_job_2, level=0.997))[2,1],
        exp(confint(log_min_job_2, level=0.997))[2,2],
        length(log_min_job_2$residuals))
))

# Name columns
colnames(log_df) <- c("Outcome", "Category", "Sample", "or",
                      "conf_95_low", "conf_95_high", "conf_997_low", "conf_997_high", "n")

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
log_df$or            <- as.numeric(log_df$or)
log_df$conf_95_low   <- as.numeric(log_df$conf_95_low)
log_df$conf_95_high  <- as.numeric(log_df$conf_95_high)
log_df$conf_997_low  <- as.numeric(log_df$conf_997_low)
log_df$conf_997_high <- as.numeric(log_df$conf_997_high)
log_df$n             <- as.numeric(log_df$n)

# Get min. and max. N
min(log_df$n); max(log_df$n)

# Generate coefficient plot: Logistic
plot_log <- ggplot(log_df, aes(x=Outcome, y=or, group=Sample, color=Sample)) +
  geom_hline(yintercept=0, color="black", linewidth=0.25) +
  geom_point(position = position_dodge(width=0.6), size=1, aes(shape=Sample)) +
  scale_shape_manual(values = 1:nlevels(log_df$Sample)) +
  geom_errorbar(aes(ymin=conf_95_low, ymax=conf_95_high),
                position = position_dodge(width=0.6), width=0, linewidth=0.8) +
  geom_errorbar(aes(ymin=conf_997_low, ymax=conf_997_high),
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
# Robustness check: Lifetime minimum wage
##############################################################################

# Sad and hopeless
life_min_sad_1 <- felm(sad_hopeless ~ wage_life |
                         age_year + fipsst | 0 | cluster,
                       data    = yrbs_all_model,
                       weights = yrbs_all_model$weight_2)
life_min_sad_2 <- felm(sad_hopeless ~ wage_life +
                         age_2 + sex_2 + race_eth_2 + grade_2 +
                         elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                         age_year + fipsst | 0 | cluster,
                       data    = yrbs_all_model,
                       weights = yrbs_all_model$weight_2)

# Considered suicide
life_min_con_1 <- felm(cons_suicide ~ wage_life |
                         age_year + fipsst | 0 | cluster,
                       data    = yrbs_all_model,
                       weights = yrbs_all_model$weight_2)
life_min_con_2 <- felm(cons_suicide ~ wage_life +
                         age_2 + sex_2 + race_eth_2 + grade_2 +
                         elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                         age_year + fipsst | 0 | cluster,
                       data    = yrbs_all_model,
                       weights = yrbs_all_model$weight_2)

# Attempted suicide
life_min_att_1 <- felm(suicide_att ~ wage_life |
                         age_year + fipsst | 0 | cluster,
                       data    = yrbs_all_model,
                       weights = yrbs_all_model$weight_2)
life_min_att_2 <- felm(suicide_att ~ wage_life +
                         age_2 + sex_2 + race_eth_2 + grade_2 +
                         elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                         age_year + fipsst | 0 | cluster,
                       data    = yrbs_all_model,
                       weights = yrbs_all_model$weight_2)

# Recent alcohol
life_min_alc_1 <- felm(alcohol ~ wage_life |
                         age_year + fipsst | 0 | cluster,
                       data    = yrbs_all_model,
                       weights = yrbs_all_model$weight_2)
life_min_alc_2 <- felm(alcohol ~ wage_life +
                         age_2 + sex_2 + race_eth_2 + grade_2 +
                         elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                         age_year + fipsst | 0 | cluster,
                       data    = yrbs_all_model,
                       weights = yrbs_all_model$weight_2)

# Recent marijuana use
life_min_mjn_1 <- felm(marijuana ~ wage_life |
                         age_year + fipsst | 0 | cluster,
                       data    = yrbs_all_model,
                       weights = yrbs_all_model$weight_2)
life_min_mjn_2 <- felm(marijuana ~ wage_life +
                         age_2 + sex_2 + race_eth_2 + grade_2 +
                         elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                         age_year + fipsst | 0 | cluster,
                       data    = yrbs_all_model,
                       weights = yrbs_all_model$weight_2)

# Physical fight
life_min_fgh_1 <- felm(fight ~ wage_life |
                         age_year + fipsst | 0 | cluster,
                       data    = yrbs_all_model,
                       weights = yrbs_all_model$weight_2)
life_min_fgh_2 <- felm(fight ~ wage_life +
                         age_2 + sex_2 + race_eth_2 + grade_2 +
                         elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                         age_year + fipsst | 0 | cluster,
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
  Y_MIN      = -0.06,
  Y_MAX      =  0.06,
  COLORS     = "Standard"
)

# Export figure
ggsave(plot=plot_life, file="Exhibits/YRBS coefficient plot, lifetime.pdf",
       width=5, height=4, units='in', dpi=600)

##############################################################################
# Robustness check: Cluster at state level
##############################################################################

# Sad and hopeless
model_min_sad_1c <- felm(sad_hopeless ~ Effective.Minimum.Wage |
                           age_year + fipsst | 0 | State,
                         data    = yrbs_all_model,
                         weights = yrbs_all_model$weight_2)
model_min_sad_2c <- felm(sad_hopeless ~ Effective.Minimum.Wage +
                           age_2 + sex_2 + race_eth_2 + grade_2 +
                           elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                           age_year + fipsst | 0 | State,
                         data    = yrbs_all_model,
                         weights = yrbs_all_model$weight_2)

# Considered suicide
model_min_con_1c <- felm(cons_suicide ~ Effective.Minimum.Wage |
                           age_year + fipsst | 0 | State,
                         data    = yrbs_all_model,
                         weights = yrbs_all_model$weight_2)
model_min_con_2c <- felm(cons_suicide ~ Effective.Minimum.Wage +
                           age_2 + sex_2 + race_eth_2 + grade_2 +
                           elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                           age_year + fipsst | 0 | State,
                         data    = yrbs_all_model,
                         weights = yrbs_all_model$weight_2)

# Attempted suicide
model_min_att_1c <- felm(suicide_att ~ Effective.Minimum.Wage |
                           age_year + fipsst | 0 | State,
                         data    = yrbs_all_model,
                         weights = yrbs_all_model$weight_2)
model_min_att_2c <- felm(suicide_att ~ Effective.Minimum.Wage +
                           age_2 + sex_2 + race_eth_2 + grade_2 +
                           elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                           age_year + fipsst | 0 | State,
                         data    = yrbs_all_model,
                         weights = yrbs_all_model$weight_2)

# Recent alcohol
model_min_alc_1c <- felm(alcohol ~ Effective.Minimum.Wage |
                           age_year + fipsst | 0 | State,
                         data    = yrbs_all_model,
                         weights = yrbs_all_model$weight_2)
model_min_alc_2c <- felm(alcohol ~ Effective.Minimum.Wage +
                           age_2 + sex_2 + race_eth_2 + grade_2 +
                           elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                           age_year + fipsst | 0 | State,
                         data    = yrbs_all_model,
                         weights = yrbs_all_model$weight_2)

# Recent marijuana
model_min_mjn_1c <- felm(marijuana ~ Effective.Minimum.Wage |
                           age_year + fipsst | 0 | State,
                         data    = yrbs_all_model,
                         weights = yrbs_all_model$weight_2)
model_min_mjn_2c <- felm(marijuana ~ Effective.Minimum.Wage +
                           age_2 + sex_2 + race_eth_2 + grade_2 +
                           elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                           age_year + fipsst | 0 | State,
                         data    = yrbs_all_model,
                         weights = yrbs_all_model$weight_2)

# Physical fight
model_min_fgh_1c <- felm(fight ~ Effective.Minimum.Wage |
                           age_year + fipsst | 0 | State,
                         data    = yrbs_all_model,
                         weights = yrbs_all_model$weight_2)
model_min_fgh_2c <- felm(fight ~ Effective.Minimum.Wage +
                           age_2 + sex_2 + race_eth_2 + grade_2 +
                           elig_1_5 + elig_6_18 + has_eitc + federal_pct + refundable + max_bft_3 |
                           age_year + fipsst | 0 | State,
                         data    = yrbs_all_model,
                         weights = yrbs_all_model$weight_2)

# Get values from models
clust_df <- NULL

# Adolescents (FE only, state clust.)
clust_df <- make_coef_df(clust_df, model_min_sad_1c, "Adolescents (FE only, state clust.)")
clust_df <- make_coef_df(clust_df, model_min_con_1c, "Adolescents (FE only, state clust.)")
clust_df <- make_coef_df(clust_df, model_min_att_1c, "Adolescents (FE only, state clust.)")
clust_df <- make_coef_df(clust_df, model_min_alc_1c, "Adolescents (FE only, state clust.)")
clust_df <- make_coef_df(clust_df, model_min_mjn_1c, "Adolescents (FE only, state clust.)")
clust_df <- make_coef_df(clust_df, model_min_fgh_1c, "Adolescents (FE only, state clust.)")

# Adolescents (fully adj., state clust.)
clust_df <- make_coef_df(clust_df, model_min_sad_2c, "Adolescents (fully adj., state clust.)")
clust_df <- make_coef_df(clust_df, model_min_con_2c, "Adolescents (fully adj., state clust.)")
clust_df <- make_coef_df(clust_df, model_min_att_2c, "Adolescents (fully adj., state clust.)")
clust_df <- make_coef_df(clust_df, model_min_alc_2c, "Adolescents (fully adj., state clust.)")
clust_df <- make_coef_df(clust_df, model_min_mjn_2c, "Adolescents (fully adj., state clust.)")
clust_df <- make_coef_df(clust_df, model_min_fgh_2c, "Adolescents (fully adj., state clust.)")

# Add main models for comparison
# Adolescents (FE only, nested clust.)
clust_df <- make_coef_df(clust_df, model_min_sad_1, "Adolescents (FE only, nested clust.)")
clust_df <- make_coef_df(clust_df, model_min_con_1, "Adolescents (FE only, nested clust.)")
clust_df <- make_coef_df(clust_df, model_min_att_1, "Adolescents (FE only, nested clust.)")
clust_df <- make_coef_df(clust_df, model_min_alc_1, "Adolescents (FE only, nested clust.)")
clust_df <- make_coef_df(clust_df, model_min_mjn_1, "Adolescents (FE only, nested clust.)")
clust_df <- make_coef_df(clust_df, model_min_fgh_1, "Adolescents (FE only, nested clust.)")

# Adolescents (fully adj., nested clust.)
clust_df <- make_coef_df(clust_df, model_min_sad_2, "Adolescents (fully adj., nested clust.)")
clust_df <- make_coef_df(clust_df, model_min_con_2, "Adolescents (fully adj., nested clust.)")
clust_df <- make_coef_df(clust_df, model_min_att_2, "Adolescents (fully adj., nested clust.)")
clust_df <- make_coef_df(clust_df, model_min_alc_2, "Adolescents (fully adj., nested clust.)")
clust_df <- make_coef_df(clust_df, model_min_mjn_2, "Adolescents (fully adj., nested clust.)")
clust_df <- make_coef_df(clust_df, model_min_fgh_2, "Adolescents (fully adj., nested clust.)")

# Clean dataframe of coefficients
clust_df <- clean_coef_df(clust_df)

# Get min. and max. N
min(clust_df$n); max(clust_df$n)

# Generate coefficient plot: State clusters
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
ggsave(plot=plot_clust, file="Exhibits/YRBS coefficient plot, state clusters.pdf",
       width=7, height=4, units='in', dpi=600)

##############################################################################
# Event studies: Minimum wage and mental health
##############################################################################

# Define treated and control states
TREATED <- c("AR","DE","FL","HI","MD","MO","MT","NE","NJ","NY","SD","WV")
CONTROL <- c("AL","GA","IA","ID","KS","KY","LA","MS","NC","ND","NH",
             "NV","OK","PA","SC","TN","TX","UT","VA","WI","WY")
TREATED_2 <- cdlTools::fips(TREATED, to="FIPS")
CONTROL_2 <- cdlTools::fips(CONTROL, to="FIPS")

# Subset minimum wage df to treated state-years
min_wage_event <- subset(min_wage_df, fipsst %in% c(CONTROL_2, TREATED_2) &
                           year %in% c(2011:2019))

# Define treated year
min_wage_event <- min_wage_event %>% mutate(
  treated_year = case_when(
    fipsst %in% c(12) ~ 2012,
    fipsst %in% c(29) ~ 2013,
    fipsst %in% c(10,30,34,36) ~ 2014,
    fipsst %in% c(5,15,24,31,46,54) ~ 2015
  ))

min_wage_event$period <- min_wage_event$year - min_wage_event$treated_year

# Graph minimum wages of treated states
ggplot(subset(min_wage_event, fipsst %in% TREATED_2),
       aes(x=period, y=Effective.Minimum.Wage)) +
  geom_hline(yintercept=7.25, color="black") +
  #geom_vline(xintercept=-0.5, color="black", linetype="dashed") +
  geom_line(aes(group=State)) +
  theme_test() +
  ylab("Effective minimum wage") +
  xlab("Year relative to raise\nabove federal minimum wage") +
  theme(legend.position = "right",
        text = element_text(size = 10, face = "bold"),
        axis.ticks = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color="light gray", linewidth=0.5),
        panel.grid.minor.y = element_line(color="light gray", linewidth=0.25)) +
  scale_y_continuous(limits = c(6,12),
                     breaks = seq(0, 12, 1),
                     minor_breaks = seq(0, 12, 0.5),
                     labels = function(x) paste0("$",x)) +
  scale_x_continuous(limits = c(-4,6),
                     breaks = seq(-10,2019,1)) #+
  facet_wrap(.~treated_year)#+
# scale_x_continuous(breaks = seq(2011,2019,1)) +
# annotate("text", x=-0.25, y=11.75, label="Raised above federal min. wage",
#          fontface=2, size=3, hjust=0, vjust=0.5) +
# annotate("text", x=7, y=7.5, label="Federal minimum wage",
#          fontface=2, size=3, hjust=1, vjust=0.5)





# # Define years in study period
# FIRST_YEAR <- 2011
# LAST_YEAR  <- 2019
# 
# # Subset minimum wage dataset to study years
# # After federal minimum raised in ~2009
# min_wage_sub <- subset(min_wage_df, year %in% c(FIRST_YEAR:LAST_YEAR))
# 
# # Reorder dataset
# min_wage_sub <- min_wage_sub %>% arrange(State, Year)
# 
# # Get first year above federal minimum
# temp_df <- min_wage_sub %>%
#   group_by(fipsst) %>%
#   filter(above_fed == 1) %>%
#   slice_head(n = 1)
# temp_df$first_above <- temp_df$Year
# 
# # Take only state and first_above
# temp_df <- temp_df %>% select(fipsst, first_above)
# 
# # Treat FIPS as factor
# temp_df$fipsst <- as.factor(temp_df$fipsst)
# 
# # Merge back into main dataframe
# yrbs_all_model <- left_join(yrbs_all_model, temp_df, by="fipsst")
# 
# # Subset full dataset to study years
# yrbs_all_sub <- subset(yrbs_all_model, year %in% c(FIRST_YEAR:LAST_YEAR))
# 
# # Coarsen treatment years
# # If no YRBS in that year, change to next year
# yrbs_all_sub <- yrbs_all_sub %>% mutate(
#   first_above_coarse = case_when(
#     
#     # For odd years, leave alone
#     first_above %% 2 == 1 ~ first_above,
#     
#     # For even years, add one
#     # For example, 2012 becomes 2013
#     first_above %% 2 == 0 ~ first_above + 1
#   ))
# 
# 
# table(yrbs_all_sub$st_abbr.x, yrbs_all_sub$first_above, useNA="always")



# # Generate sampling clusters
# # yrbs_all_sub$cluster <- paste0(yrbs_all_sub$fipsst, "-", yrbs_all_sub$PSU, "-", yrbs_all_sub$stratum)
# yrbs_all_sub$cluster <- paste0(yrbs_all_sub$fipsst, "-", yrbs_all_sub$stratum)
# 
# # Generate age-by-year fixed effects
# yrbs_all_sub$age_year <- paste0(yrbs_all_sub$age_2, "-", yrbs_all_sub$year)





# # Treat cluster as numeric
# yrbs_all_sub$cluster_num <- as.numeric(as.factor(yrbs_all_sub$cluster))
# 
# # Define period for Callaway and Sant'Anna
# yrbs_all_sub <- yrbs_all_sub %>% mutate(
#   period_cs = case_when(
#     
#     # Exclude states already above federal minimum
#     first_above == FIRST_YEAR ~ NA_real_,
#     
#     # Code control states as 0
#     is.na(first_above_coarse) ~ 0,
#     
#     # Otherwise use treatment year
#     TRUE ~ first_above_coarse
#   ))
# 
# # Subset to states of interest
# yrbs_all_cs <- subset(yrbs_all_sub, !is.na(yrbs_all_sub$period_cs))








# Load package
library(did)

# Subset to treated or control state-years
yrbs_all_cs <- subset(yrbs_all_model, fipsst %in% c(CONTROL_2, TREATED_2) &
                        year %in% c(2011:2019))

# Define treated period
yrbs_all_cs <- yrbs_all_cs %>% mutate(
  period_cs = case_when(
    fipsst %in% c(12,29) ~ 2013,
    fipsst %in% c(10,30,34,36,5,15,24,31,46,54) ~ 2015,
    fipsst %in% CONTROL_2 ~ 0
  ))

# Treat cluster as numeric
yrbs_all_cs$cluster_num <- as.numeric(as.factor(yrbs_all_cs$cluster))

# Set seed
set.seed(1234)

# Model: Sad and hopeless
# Note: Subset to complete cases
yrbs_all_cs_1 <- yrbs_all_cs %>%
  filter_at(vars(sad_hopeless, Year, period_cs), all_vars(!is.na(.)))
atts_1 <- att_gt(yname = "sad_hopeless",
                 tname = "Year",
                 gname = "period_cs",
                 control_group = "nevertreated",
                 clustervars   = "cluster_num",
                 panel = FALSE,
                 xformla = ~age_2 + sex_2 + race_eth_2 + grade_2,
                 data = yrbs_all_cs_1,
                 weightsname = "weight_2",
                 allow_unbalanced_panel = TRUE,
                 base_period = "universal",
                 pl = TRUE,
                 cores = 4,
                 print_details = TRUE)

# Model: Considered suicide
# Note: Subset to complete cases
yrbs_all_cs_2 <- yrbs_all_cs %>%
  filter_at(vars(cons_suicide, Year, period_cs), all_vars(!is.na(.)))
atts_2 <- att_gt(yname = "cons_suicide",
                 tname = "Year",
                 gname = "period_cs",
                 control_group = "nevertreated",
                 clustervars   = "cluster_num",
                 panel = FALSE,
                 xformla = ~age_2 + sex_2 + race_eth_2 + grade_2,
                 data = yrbs_all_cs_2,
                 weightsname = "weight_2",
                 allow_unbalanced_panel = TRUE,
                 base_period = "universal",
                 pl = TRUE,
                 cores = 4,
                 print_details = TRUE)

# Model: Attempted suicide
# Note: Subset to complete cases
yrbs_all_cs_3 <- yrbs_all_cs %>%
  filter_at(vars(suicide_att, Year, period_cs), all_vars(!is.na(.)))
atts_3 <- att_gt(yname = "suicide_att",
                 tname = "Year",
                 gname = "period_cs",
                 control_group = "nevertreated",
                 clustervars   = "cluster_num",
                 panel = FALSE,
                 xformla = ~age_2 + sex_2 + race_eth_2 + grade_2,
                 data = yrbs_all_cs_3,
                 weightsname = "weight_2",
                 allow_unbalanced_panel = TRUE,
                 base_period = "universal",
                 pl = TRUE,
                 cores = 4,
                 print_details = TRUE)

# Model: Physical fight
# Note: Subset to complete cases
yrbs_all_cs_4 <- yrbs_all_cs %>%
  filter_at(vars(fight, Year, period_cs), all_vars(!is.na(.)))
atts_4 <- att_gt(yname = "fight",
                 tname = "Year",
                 gname = "period_cs",
                 control_group = "nevertreated",
                 clustervars   = "cluster_num",
                 panel = FALSE,
                 xformla = ~age_2 + sex_2 + race_eth_2 + grade_2,
                 data = yrbs_all_cs_4,
                 weightsname = "weight_2",
                 allow_unbalanced_panel = TRUE,
                 base_period = "universal",
                 pl = TRUE,
                 cores = 4,
                 print_details = TRUE)

# Model: Recent alcohol use
# Note: Subset to complete cases
yrbs_all_cs_5 <- yrbs_all_cs %>%
  filter_at(vars(alcohol, Year, period_cs), all_vars(!is.na(.)))
atts_5 <- att_gt(yname = "alcohol",
                 tname = "Year",
                 gname = "period_cs",
                 control_group = "nevertreated",
                 clustervars   = "cluster_num",
                 panel = FALSE,
                 xformla = ~age_2 + sex_2 + race_eth_2 + grade_2,
                 data = yrbs_all_cs_5,
                 weightsname = "weight_2",
                 allow_unbalanced_panel = TRUE,
                 base_period = "universal",
                 pl = TRUE,
                 cores = 4,
                 print_details = TRUE)

# Model: Recent marijuana use
# Note: Subset to complete cases
yrbs_all_cs_6 <- yrbs_all_cs %>%
  filter_at(vars(marijuana, Year, period_cs), all_vars(!is.na(.)))
atts_6 <- att_gt(yname = "marijuana",
                 tname = "Year",
                 gname = "period_cs",
                 control_group = "nevertreated",
                 clustervars   = "cluster_num",
                 panel = FALSE,
                 xformla = ~age_2 + sex_2 + race_eth_2 + grade_2,
                 data = yrbs_all_cs_6,
                 weightsname = "weight_2",
                 allow_unbalanced_panel = TRUE,
                 base_period = "universal",
                 pl = TRUE,
                 cores = 4,
                 print_details = TRUE)

# Generate aggregate event studies
agg_event_1 <- aggte(atts_1, type = "dynamic")
agg_event_2 <- aggte(atts_2, type = "dynamic")
agg_event_3 <- aggte(atts_3, type = "dynamic")
agg_event_4 <- aggte(atts_4, type = "dynamic")
agg_event_5 <- aggte(atts_5, type = "dynamic")
agg_event_6 <- aggte(atts_6, type = "dynamic")
summary(agg_event_1); summary(agg_event_2); summary(agg_event_3)
summary(agg_event_4); summary(agg_event_5); summary(agg_event_6)

# Plot event-study coefficients
event_plot_1 <- ggdid(agg_event_1) +
  geom_vline(xintercept=-1, color="black", linetype="dashed") +
  geom_hline(yintercept=0, color="black") +
  geom_errorbar(width=0.2) +
  geom_point() +
  geom_line() +
  theme_test() +
  theme(plot.title         = element_blank(),
        legend.position    = "none",
        text               = element_text(size = 10, face = "bold"),
        axis.ticks         = element_blank(),
        panel.grid.major.y = element_line(color = "gray", size = 0.25)) +
  xlab("Year relative to raise") +
  ylab("Adj. coefficients for\nsad and hopeless") +
  scale_y_continuous(labels = function(x) paste0(x*100," pp")) +
  scale_x_continuous(breaks=seq(-4,6,2)) +
  coord_cartesian(ylim=c(-0.1,0.1)) +
  annotate("text", x=-0.5, y=0.075, label="Raised above\nfederal min. wage",
           fontface=2, size=3, hjust=0, vjust=0.5)

event_plot_2 <- ggdid(agg_event_2) +
  geom_vline(xintercept=-1, color="black", linetype="dashed") +
  geom_hline(yintercept=0, color="black") +
  geom_errorbar(width=0.2) +
  geom_point() +
  geom_line() +
  theme_test() +
  theme(plot.title         = element_blank(),
        legend.position    = "none",
        text               = element_text(size = 10, face = "bold"),
        axis.ticks         = element_blank(),
        panel.grid.major.y = element_line(color = "gray", size = 0.25)) +
  xlab("Year relative to raise") +
  ylab("Adj. coefficients for\nconsidered suicide") +
  scale_y_continuous(labels = function(x) paste0(x*100," pp")) +
  scale_x_continuous(breaks=seq(-4,6,2)) +
  coord_cartesian(ylim=c(-0.1,0.1))

event_plot_3 <- ggdid(agg_event_3) +
  geom_vline(xintercept=-1, color="black", linetype="dashed") +
  geom_hline(yintercept=0, color="black") +
  geom_errorbar(width=0.2) +
  geom_point() +
  geom_line() +
  theme_test() +
  theme(plot.title         = element_blank(),
        legend.position    = "none",
        text               = element_text(size = 10, face = "bold"),
        axis.ticks         = element_blank(),
        panel.grid.major.y = element_line(color = "gray", size = 0.25)) +
  xlab("Year relative to raise") +
  ylab("Adj. coefficients for\nattempting suicide") +
  scale_y_continuous(labels = function(x) paste0(x*100," pp")) +
  scale_x_continuous(breaks=seq(-4,6,2)) +
  coord_cartesian(ylim=c(-0.1,0.1))

event_plot_4 <- ggdid(agg_event_4) +
  geom_vline(xintercept=-1, color="black", linetype="dashed") +
  geom_hline(yintercept=0, color="black") +
  geom_errorbar(width=0.2) +
  geom_point() +
  geom_line() +
  theme_test() +
  theme(plot.title         = element_blank(),
        legend.position    = "none",
        text               = element_text(size = 10, face = "bold"),
        axis.ticks         = element_blank(),
        panel.grid.major.y = element_line(color = "gray", size = 0.25)) +
  xlab("Year relative to raise") +
  ylab("Adj. coefficients for\nphysical fight") +
  scale_y_continuous(labels = function(x) paste0(x*100," pp")) +
  scale_x_continuous(breaks=seq(-4,6,2)) +
  coord_cartesian(ylim=c(-0.1,0.1))

event_plot_5 <- ggdid(agg_event_5) +
  geom_vline(xintercept=-1, color="black", linetype="dashed") +
  geom_hline(yintercept=0, color="black") +
  geom_errorbar(width=0.2) +
  geom_point() +
  geom_line() +
  theme_test() +
  theme(plot.title         = element_blank(),
        legend.position    = "none",
        text               = element_text(size = 10, face = "bold"),
        axis.ticks         = element_blank(),
        panel.grid.major.y = element_line(color = "gray", size = 0.25)) +
  xlab("Year relative to raise") +
  ylab("Adj. coefficients for\nrecent alcohol use") +
  scale_y_continuous(labels = function(x) paste0(x*100," pp")) +
  scale_x_continuous(breaks=seq(-4,6,2)) +
  coord_cartesian(ylim=c(-0.1,0.1))

event_plot_6 <- ggdid(agg_event_6) +
  geom_vline(xintercept=-1, color="black", linetype="dashed") +
  geom_hline(yintercept=0, color="black") +
  geom_errorbar(width=0.2) +
  geom_point() +
  geom_line() +
  theme_test() +
  theme(plot.title         = element_blank(),
        legend.position    = "none",
        text               = element_text(size = 10, face = "bold"),
        axis.ticks         = element_blank(),
        panel.grid.major.y = element_line(color = "gray", size = 0.25)) +
  xlab("Year relative to raise") +
  ylab("Adj. coefficients for\nrecent marijuana use") +
  scale_y_continuous(labels = function(x) paste0(x*100," pp")) +
  scale_x_continuous(breaks=seq(-4,6,2)) +
  coord_cartesian(ylim=c(-0.1,0.1))

# Compile figures
event_all <- plot_grid(event_plot_1, event_plot_2, event_plot_3,
                       event_plot_4, event_plot_5, event_plot_6, nrow=3)

# Export figure
ggsave(plot=event_all, file="Exhibits/YRBS event studies, nested errors.pdf",
       width=6, height=6, units='in', dpi=600)
