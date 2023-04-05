# Minimum wage and children's health
# N.M. Kavanagh, N. Slopen
# March 31, 2023

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
yrbs_all <- bind_rows(yrbs_st_am, yrbs_st_nz)

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
    lag_by_17_inf = lag(Effective.Minimum.Wage.2020.Dollars, n=17, default=NA),
    lag_by_18_inf = lag(Effective.Minimum.Wage.2020.Dollars, n=18, default=NA)
    )

# Merge minimum wage and NSCH data
merged <- left_join(yrbs_all, min_wage_df, by=c("fipsst", "year"))

# Clear old datasets from R
rm(yrbs_st_am, yrbs_st_nz)

##############################################################################
# Variable preparation
##############################################################################

# Treat fixed effects as factors
merged$year   <- as.factor(merged$year)
merged$fipsst <- as.factor(merged$fipsst)

# Child's age
merged <- merged %>% mutate(
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
merged <- merged %>% mutate(
  sex_2 = case_when(
    sex == 1 ~ "Female",
    sex == 2 ~ "Male"
  ))
merged$sex_2 <- factor(merged$sex_2, levels = c("Male", "Female"))

# Child's race/ethnicity
merged <- merged %>% mutate(
  race_eth_2 = case_when(
    race7 == 1        ~ "American Indian or Alaska Native",
    race7 %in% c(2,5) ~ "Asian, Native Hawaiian, or Pacific Islander",
    race7 == 3        ~ "Black, non-Hispanic/Latino",
    race7 == 4        ~ "Hispanic/Latino",
    race7 == 6        ~ "White, non-Hispanic/Latino",
    race7 == 7        ~ "Other or mixed race"
  ))
merged$race_eth_2 <- factor(merged$race_eth_2, levels = c("White, non-Hispanic/Latino", "Black, non-Hispanic/Latino", "Hispanic/Latino", "American Indian or Alaska Native", "Asian, Native Hawaiian, or Pacific Islander", "Other or mixed race"))

# Dichotomoize race/ethnicity
# Black/Latino vs. other for interaction models
merged <- merged %>% mutate(
  race_eth_cat = case_when(
    race7 %in% c(3:4)     ~ 0, # Black or Hispanic/Latino
    race7 %in% c(1:2,5:7) ~ 1  # Other races
  ))

# Child's educational grade
merged <- merged %>% mutate(
  grade_2 = case_when(
    grade == 1 ~ "9th grade",
    grade == 2 ~ "10th grade",
    grade == 3 ~ "11th grade",
    grade == 4 ~ "12th grade"
  ))
merged$grade_2 <- factor(merged$grade_2, levels = c("9th grade", "10th grade", "11th grade", "12th grade"))

# Sad or hopeless for 2+ weeks
merged <- merged %>% mutate(
  sad_hopeless = case_when(
    q25 == 1 ~ 1,
    q25 == 2 ~ 0
  ))

# Seriously considered suicide
merged <- merged %>% mutate(
  cons_suicide = case_when(
    q26 == 1 ~ 1,
    q26 == 2 ~ 0
  ))

# Plan for suicide
merged <- merged %>% mutate(
  plan_suicide = case_when(
    q27 == 1 ~ 1,
    q27 == 2 ~ 0
  ))

# Made suicide attempt
merged <- merged %>% mutate(
  suicide_att = case_when(
    q28 == 1        ~ 0,
    q28 %in% c(2:5) ~ 1
  ))

# Physical fight
merged <- merged %>% mutate(
  fight = case_when(
    q17 == 1        ~ 0,
    q17 %in% c(2:8) ~ 1
  ))

# Lifetime minimum wage
merged <- merged %>% mutate(
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

##############################################################################
# Specify complex design
##############################################################################

# Rescale weight so mean is 1
merged$weight_2 <- merged$weight/66.61

# Define complex sampling design
# Nest strata within states and apply survey weights
design_all <- svydesign(id=~PSU, strata=~stratum, nest=TRUE,
                        weights=~weight_2, data=merged)

# Subset to years of interest
design_sub <- subset(design_all, year %in% c(2000:2020))

##############################################################################
# Table 1: Demographic characteristics
##############################################################################

# At least 1 outcome
merged <- merged %>% mutate(
  has_outcome = case_when(
    !is.na(sad_hopeless) | !is.na(plan_suicide) | !is.na(suicide_att) | !is.na(fight) ~ 1
  ))

# Complete cases for covariates
merged_demo <- merged %>%
  subset(., year %in% c(2000:2020)) %>%
  filter_at(vars(Effective.Minimum.Wage, has_outcome, age_2, sex_2, race_eth_2,
                 grade_2), all_vars(!is.na(.)))

# Child characteristics: unweighted
# This line provides demographic characteristics without survey weights.
summary(tableby(~ age_2 + sex_2 + race_eth_2 + grade_2,
                merged_demo, digits.pct=0), text=T)

# Child characteristics: weighted
# This line provides demographic characteristics with survey weights.
summary(tableby(~ age_2 + sex_2 + race_eth_2 + grade_2,
                merged_demo, digits.pct=0, weights=weight_2), text=T)

##############################################################################
# Figure 2: Associations between FPL and outcomes
##############################################################################

# Explore unadjusted associations
prop.table(svytable(~sad_hopeless, design=design_sub))
prop.table(svytable(~cons_suicide, design=design_sub))
prop.table(svytable(~suicide_att,  design=design_sub))
prop.table(svytable(~fight,        design=design_sub))

# # Explore adjusted associations
# model_fpl_dep <- svyglm(depression ~
#                           age + sex + race_eth + family_struc + adult_edu + fpl_category + nativity +
#                           year + fipsst,
#                         design = design_sub)
# summary(model_fpl_dep)
# 
# model_fpl_anx <- svyglm(anxiety ~
#                           age + sex + race_eth + family_struc + adult_edu + fpl_category + nativity +
#                           year + fipsst,
#                         design = design_sub)
# summary(model_fpl_anx)
# 
# model_fpl_add <- svyglm(adhd ~
#                           age + sex + race_eth + family_struc + adult_edu + fpl_category + nativity +
#                           year + fipsst,
#                         design = design_sub)
# summary(model_fpl_add)
# 
# model_fpl_beh <- svyglm(behavior ~
#                           age + sex + race_eth + family_struc + adult_edu + fpl_category + nativity +
#                           year + fipsst,
#                         design = design_sub)
# summary(model_fpl_beh)
# 
# model_fpl_unm <- svyglm(unmet_needs ~
#                           age + sex + race_eth + family_struc + adult_edu + fpl_category + nativity +
#                           year + fipsst,
#                         design = design_sub)
# summary(model_fpl_unm)
# 
# model_fpl_men <- svyglm(unmet_mental ~
#                           age + sex + race_eth + family_struc + adult_edu + fpl_category + nativity +
#                           year + fipsst,
#                         design = design_sub)
# summary(model_fpl_men)
# 
# model_fpl_dig <- svyglm(stomach_r ~
#                           age + sex + race_eth + family_struc + adult_edu + fpl_category + nativity +
#                           year + fipsst,
#                         design = design_sub)
# summary(model_fpl_dig)
# 
# model_fpl_sch <- svyglm(missed_school ~
#                           age + sex + race_eth + family_struc + adult_edu + fpl_category + nativity +
#                           year + fipsst,
#                         design = design_sub)
# summary(model_fpl_sch)
# 
# # Generate marginal means
# means_dep <- estimate_means(model_fpl_dep, at="fpl_category")
# means_anx <- estimate_means(model_fpl_anx, at="fpl_category")
# means_add <- estimate_means(model_fpl_add, at="fpl_category")
# means_beh <- estimate_means(model_fpl_beh, at="fpl_category")
# means_dig <- estimate_means(model_fpl_dig, at="fpl_category")
# means_unm <- estimate_means(model_fpl_unm, at="fpl_category")
# means_men <- estimate_means(model_fpl_men, at="fpl_category")
# means_sch <- estimate_means(model_fpl_sch, at="fpl_category")
# 
# # Dataframe of rates
# means_all <- as.data.frame(rbind(
#   # Depression
#   cbind("Depression", "Less than 100%",  means_dep$Mean[1], means_dep$SE[1]),
#   cbind("Depression", "100% to 199%",    means_dep$Mean[2], means_dep$SE[2]),
#   cbind("Depression", "200% to 299%",    means_dep$Mean[3], means_dep$SE[3]),
#   cbind("Depression", "300% to 399%",    means_dep$Mean[4], means_dep$SE[4]),
#   cbind("Depression", "400% or greater", means_dep$Mean[5], means_dep$SE[5]),
#   
#   # Anxiety
#   cbind("Anxiety", "Less than 100%",  means_anx$Mean[1], means_anx$SE[1]),
#   cbind("Anxiety", "100% to 199%",    means_anx$Mean[2], means_anx$SE[2]),
#   cbind("Anxiety", "200% to 299%",    means_anx$Mean[3], means_anx$SE[3]),
#   cbind("Anxiety", "300% to 399%",    means_anx$Mean[4], means_anx$SE[4]),
#   cbind("Anxiety", "400% or greater", means_anx$Mean[5], means_anx$SE[5]),
#   
#   # ADD/ADHD
#   cbind("ADD/ADHD", "Less than 100%",  means_add$Mean[1], means_add$SE[1]),
#   cbind("ADD/ADHD", "100% to 199%",    means_add$Mean[2], means_add$SE[2]),
#   cbind("ADD/ADHD", "200% to 299%",    means_add$Mean[3], means_add$SE[3]),
#   cbind("ADD/ADHD", "300% to 399%",    means_add$Mean[4], means_add$SE[4]),
#   cbind("ADD/ADHD", "400% or greater", means_add$Mean[5], means_add$SE[5]),
#   
#   # Behavioral problems
#   cbind("Behavioral prob.", "Less than 100%",  means_beh$Mean[1], means_beh$SE[1]),
#   cbind("Behavioral prob.", "100% to 199%",    means_beh$Mean[2], means_beh$SE[2]),
#   cbind("Behavioral prob.", "200% to 299%",    means_beh$Mean[3], means_beh$SE[3]),
#   cbind("Behavioral prob.", "300% to 399%",    means_beh$Mean[4], means_beh$SE[4]),
#   cbind("Behavioral prob.", "400% or greater", means_beh$Mean[5], means_beh$SE[5]),
#   
#   # Digestive issues
#   cbind("Digestive issues", "Less than 100%",  means_dig$Mean[1], means_dig$SE[1]),
#   cbind("Digestive issues", "100% to 199%",    means_dig$Mean[2], means_dig$SE[2]),
#   cbind("Digestive issues", "200% to 299%",    means_dig$Mean[3], means_dig$SE[3]),
#   cbind("Digestive issues", "300% to 399%",    means_dig$Mean[4], means_dig$SE[4]),
#   cbind("Digestive issues", "400% or greater", means_dig$Mean[5], means_dig$SE[5]),
#   
#   # Unmet health care needs (any)
#   cbind("Unmet health care\n(of any kind)", "Less than 100%",  means_unm$Mean[1], means_unm$SE[1]),
#   cbind("Unmet health care\n(of any kind)", "100% to 199%",    means_unm$Mean[2], means_unm$SE[2]),
#   cbind("Unmet health care\n(of any kind)", "200% to 299%",    means_unm$Mean[3], means_unm$SE[3]),
#   cbind("Unmet health care\n(of any kind)", "300% to 399%",    means_unm$Mean[4], means_unm$SE[4]),
#   cbind("Unmet health care\n(of any kind)", "400% or greater", means_unm$Mean[5], means_unm$SE[5]),
#   
#   # Unmet health care needs (mental health)
#   cbind("Unmet health care\n(mental health)", "Less than 100%",  means_men$Mean[1], means_men$SE[1]),
#   cbind("Unmet health care\n(mental health)", "100% to 199%",    means_men$Mean[2], means_men$SE[2]),
#   cbind("Unmet health care\n(mental health)", "200% to 299%",    means_men$Mean[3], means_men$SE[3]),
#   cbind("Unmet health care\n(mental health)", "300% to 399%",    means_men$Mean[4], means_men$SE[4]),
#   cbind("Unmet health care\n(mental health)", "400% or greater", means_men$Mean[5], means_men$SE[5]),
#   
#   # Missed school
#   cbind("Missed school", "Less than 100%",  means_sch$Mean[1], means_sch$SE[1]),
#   cbind("Missed school", "100% to 199%",    means_sch$Mean[2], means_sch$SE[2]),
#   cbind("Missed school", "200% to 299%",    means_sch$Mean[3], means_sch$SE[3]),
#   cbind("Missed school", "300% to 399%",    means_sch$Mean[4], means_sch$SE[4]),
#   cbind("Missed school", "400% or greater", means_sch$Mean[5], means_sch$SE[5])
# ))
# colnames(means_all) <- c("Outcome", "FPL level", "value", "se")
# 
# # Reorder factor levels
# means_all$`FPL level` <- factor(means_all$`FPL level`, levels = c("Less than 100%", "100% to 199%", "200% to 299%", "300% to 399%", "400% or greater"))
# means_all$Outcome <- factor(means_all$Outcome, levels = c("Depression", "Anxiety", "ADD/ADHD", "Behavioral prob.", "Digestive issues", "Unmet health care\n(of any kind)", "Unmet health care\n(mental health)", "Missed school"))
# 
# # Treat adjusted means and SEs as numeric
# means_all$value <- as.numeric(as.character(means_all$value))
# means_all$se    <- as.numeric(as.character(means_all$se))
# 
# # Plot means by outcome
# plot_means <- ggplot(means_all, aes(x=`FPL level`, y=value, fill=`FPL level`)) +
#   geom_bar(stat = "identity") +
#   geom_errorbar(aes(ymin=value-1.96*se, ymax=value+1.96*se), width=0.2) +
#   ylab("Rate of mental health outcome,\nadjusted for demographics") +
#   xlab("Household FPL level") +
#   theme_test() +
#   theme(legend.position = "bottom",
#         text = element_text(size=10, face="bold"),
#         axis.ticks = element_blank(),
#         axis.text.x = element_blank(),
#         panel.grid.major.y = element_line(color="gray", size=0.5),
#         panel.grid.minor.y = element_line(color="gray", size=0.25),
#         strip.background = element_blank(),
#         panel.border = element_blank(),
#         panel.spacing = unit(0.5, "cm", data = NULL),
#         strip.text = element_text(size=10)) +
#   scale_y_continuous(labels = scales::percent,
#                      breaks=c(0, 0.05, 0.10, 0.15)) +
#   scale_fill_grey(start=0.4, end=0.8, name="") +
#   facet_grid(~Outcome, nrow=2, scales="free", space="free")
# print(plot_means)
# 
# # Export figure
# ggsave(plot=plot_means, file="Adjusted rates of outcomes.pdf", width=7, height=7, units='in', dpi=600)

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
# Effect of minimum wage on mental health outcomes
##############################################################################

# Models for sad and hopeless
model_min_sad_1 <- svyglm(sad_hopeless ~ Effective.Minimum.Wage +
                            year + fipsst,
                          design = design_sub)
model_min_sad_2 <- svyglm(sad_hopeless ~ Effective.Minimum.Wage +
                            age_2 + sex_2 + race_eth_2 + grade_2 +
                            year + fipsst,
                          design = design_sub)
model_min_sad_4 <- svyglm(sad_hopeless ~ Effective.Minimum.Wage.2020.Dollars +
                            age_2 + sex_2 + race_eth_2 + grade_2 +
                            year + fipsst,
                          design = design_sub)
model_min_sad_5 <- svyglm(sad_hopeless ~ lag_by_1 +
                            age_2 + sex_2 + race_eth_2 + grade_2 +
                            year + fipsst,
                          design = design_sub)
model_min_sad_7 <- svyglm(sad_hopeless ~ Effective.Minimum.Wage*race_eth_cat +
                            age_2 + sex_2 + race_eth_2 + grade_2 +
                            year + fipsst,
                          design = design_sub)

# Models for considered suicide
model_min_con_1 <- svyglm(cons_suicide ~ Effective.Minimum.Wage +
                            year + fipsst,
                          design = design_sub)
model_min_con_2 <- svyglm(cons_suicide ~ Effective.Minimum.Wage +
                            age_2 + sex_2 + race_eth_2 + grade_2 +
                            year + fipsst,
                          design = design_sub)
model_min_con_4 <- svyglm(cons_suicide ~ Effective.Minimum.Wage.2020.Dollars +
                            age_2 + sex_2 + race_eth_2 + grade_2 +
                            year + fipsst,
                          design = design_sub)
model_min_con_5 <- svyglm(cons_suicide ~ lag_by_1 +
                            age_2 + sex_2 + race_eth_2 + grade_2 +
                            year + fipsst,
                          design = design_sub)
model_min_con_7 <- svyglm(cons_suicide ~ Effective.Minimum.Wage*race_eth_cat +
                            age_2 + sex_2 + race_eth_2 + grade_2 +
                            year + fipsst,
                          design = design_sub)

# Models for attempted suicide
model_min_att_1 <- svyglm(suicide_att ~ Effective.Minimum.Wage +
                            year + fipsst,
                          design = design_sub)
model_min_att_2 <- svyglm(suicide_att ~ Effective.Minimum.Wage +
                            age_2 + sex_2 + race_eth_2 + grade_2 +
                            year + fipsst,
                          design = design_sub)
model_min_att_4 <- svyglm(suicide_att ~ Effective.Minimum.Wage.2020.Dollars +
                            age_2 + sex_2 + race_eth_2 + grade_2 +
                            year + fipsst,
                          design = design_sub)
model_min_att_5 <- svyglm(suicide_att ~ lag_by_1 +
                            age_2 + sex_2 + race_eth_2 + grade_2 +
                            year + fipsst,
                          design = design_sub)
model_min_att_7 <- svyglm(suicide_att ~ Effective.Minimum.Wage*race_eth_cat +
                            age_2 + sex_2 + race_eth_2 + grade_2 +
                            year + fipsst,
                          design = design_sub)

# Models for physical fight
model_min_fgh_1 <- svyglm(fight ~ Effective.Minimum.Wage +
                            year + fipsst,
                          design = design_sub)
model_min_fgh_2 <- svyglm(fight ~ Effective.Minimum.Wage +
                            age_2 + sex_2 + race_eth_2 + grade_2 +
                            year + fipsst,
                          design = design_sub)
model_min_fgh_4 <- svyglm(fight ~ Effective.Minimum.Wage.2020.Dollars +
                            age_2 + sex_2 + race_eth_2 + grade_2 +
                            year + fipsst,
                          design = design_sub)
model_min_fgh_5 <- svyglm(fight ~ lag_by_1 +
                            age_2 + sex_2 + race_eth_2 + grade_2 +
                            year + fipsst,
                          design = design_sub)
model_min_fgh_7 <- svyglm(fight ~ Effective.Minimum.Wage*race_eth_cat +
                            age_2 + sex_2 + race_eth_2 + grade_2 +
                            year + fipsst,
                          design = design_sub)

# Get range of sample sizes
list <- cbind(
  # Sad or hopeless
  length(model_min_sad_1$residuals), length(model_min_sad_2$residuals),
  length(model_min_sad_4$residuals), length(model_min_sad_5$residuals),
  length(model_min_sad_7$residuals),
  
  # Considered suicide
  length(model_min_con_1$residuals), length(model_min_con_2$residuals),
  length(model_min_con_4$residuals), length(model_min_con_5$residuals),
  length(model_min_con_7$residuals),
  
  # Attempted suicide
  length(model_min_att_1$residuals), length(model_min_att_2$residuals),
  length(model_min_att_4$residuals), length(model_min_att_5$residuals),
  length(model_min_att_7$residuals),
  
  # Attempted suicide
  length(model_min_fgh_1$residuals), length(model_min_fgh_2$residuals),
  length(model_min_fgh_4$residuals), length(model_min_fgh_5$residuals),
  length(model_min_fgh_7$residuals)
  )
min(list); max(list)

# Extract coefficients of interest
main_vals <- as.data.frame(rbind(
  # Sad or hopeless
  cbind("Sad or hopeless", "All children (unadjusted)",
        model_min_sad_1$coefficients[2], SE(model_min_sad_1)[2]),
  cbind("Sad or hopeless", "All children (adjusted)",
        model_min_sad_2$coefficients[2], SE(model_min_sad_2)[2]),
  cbind("Sad or hopeless", "Black or Hispanic/Latino",
        model_min_sad_7$coefficients[2], SE(model_min_sad_7)[2]),
  cbind("Sad or hopeless", "All children (2020 dollars)",
        model_min_sad_4$coefficients[2], SE(model_min_sad_4)[2]),
  cbind("Sad or hopeless", "All children (lagged wage)",
        model_min_sad_5$coefficients[2], SE(model_min_sad_5)[2]),
  
  # Considered suicide
  cbind("Considered suicide", "All children (unadjusted)",
        model_min_con_1$coefficients[2], SE(model_min_con_1)[2]),
  cbind("Considered suicide", "All children (adjusted)",
        model_min_con_2$coefficients[2], SE(model_min_con_2)[2]),
  cbind("Considered suicide", "Black or Hispanic/Latino",
        model_min_con_7$coefficients[2], SE(model_min_con_7)[2]),
  cbind("Considered suicide", "All children (2020 dollars)",
        model_min_con_4$coefficients[2], SE(model_min_con_4)[2]),
  cbind("Considered suicide", "All children (lagged wage)",
        model_min_con_5$coefficients[2], SE(model_min_con_5)[2]),
  
  # Attempted suicide
  cbind("Attempted suicide", "All children (unadjusted)",
        model_min_att_1$coefficients[2], SE(model_min_att_1)[2]),
  cbind("Attempted suicide", "All children (adjusted)",
        model_min_att_2$coefficients[2], SE(model_min_att_2)[2]),
  cbind("Attempted suicide", "Black or Hispanic/Latino",
        model_min_att_7$coefficients[2], SE(model_min_att_7)[2]),
  cbind("Attempted suicide", "All children (2020 dollars)",
        model_min_att_4$coefficients[2], SE(model_min_att_4)[2]),
  cbind("Attempted suicide", "All children (lagged wage)",
        model_min_att_5$coefficients[2], SE(model_min_att_5)[2]),
  
  # Physical fight
  cbind("Physical fight", "All children (unadjusted)",
        model_min_fgh_1$coefficients[2], SE(model_min_fgh_1)[2]),
  cbind("Physical fight", "All children (adjusted)",
        model_min_fgh_2$coefficients[2], SE(model_min_fgh_2)[2]),
  cbind("Physical fight", "Black or Hispanic/Latino",
        model_min_fgh_7$coefficients[2], SE(model_min_fgh_7)[2]),
  cbind("Physical fight", "All children (2020 dollars)",
        model_min_fgh_4$coefficients[2], SE(model_min_fgh_4)[2]),
  cbind("Physical fight", "All children (lagged wage)",
        model_min_fgh_5$coefficients[2], SE(model_min_fgh_5)[2])
))
colnames(main_vals) <- c("Outcome", "Sample", "effect", "se")

# Reorder factor variables
main_vals$Outcome <- factor(main_vals$Outcome, levels=c("Sad or hopeless", "Considered suicide", "Attempted suicide", "Physical fight"))
main_vals$Sample <- factor(main_vals$Sample, levels=c("All children (unadjusted)", "All children (adjusted)", "Black or Hispanic/Latino", "All children (2020 dollars)", "All children (lagged wage)"))

# Treat columns as numeric
main_vals$effect <- as.numeric(main_vals$effect)
main_vals$se     <- as.numeric(main_vals$se)

# Generate coefficient plot
plot_main <- ggplot(main_vals, aes(x=Outcome, y=effect,
                                         group=Sample, color=Sample)) +
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
  ggtitle("Adolescents (12-18), 2001-2019")

# Export figure
ggsave(plot=plot_main, file="YRBS coefficient plot, main.pdf", width=5, height=4, units='in', dpi=600)

##############################################################################
# Robustness check: Logistic regression
##############################################################################

# # Models for depression
# log_min_dep_1 <- svyglm(depression ~ Effective.Minimum.Wage +
#                           year + fipsst,
#                         design = design_sub, family="quasibinomial")
# log_min_dep_2 <- svyglm(depression ~ Effective.Minimum.Wage +
#                           age + sex + race_eth + family_struc + adult_edu + fpl_category + nativity +
#                           year + fipsst,
#                         design = design_sub, family="quasibinomial")
# 
# # Models for anxiety
# log_min_anx_1 <- svyglm(anxiety ~ Effective.Minimum.Wage +
#                           year + fipsst,
#                         design = design_sub, family="quasibinomial")
# log_min_anx_2 <- svyglm(anxiety ~ Effective.Minimum.Wage +
#                           age + sex + race_eth + family_struc + adult_edu + fpl_category + nativity +
#                           year + fipsst,
#                         design = design_sub, family="quasibinomial")
# 
# # Models for ADD/ADHD
# log_min_add_1 <- svyglm(adhd ~ Effective.Minimum.Wage +
#                           year + fipsst,
#                         design = design_sub, family="quasibinomial")
# log_min_add_2 <- svyglm(adhd ~ Effective.Minimum.Wage +
#                           age + sex + race_eth + family_struc + adult_edu + fpl_category + nativity +
#                           year + fipsst,
#                         design = design_sub, family="quasibinomial")
# 
# # Models for behavioral problems
# log_min_beh_1 <- svyglm(behavior ~ Effective.Minimum.Wage +
#                           year + fipsst,
#                         design = design_sub, family="quasibinomial")
# log_min_beh_2 <- svyglm(behavior ~ Effective.Minimum.Wage +
#                           age + sex + race_eth + family_struc + adult_edu + fpl_category + nativity +
#                           year + fipsst,
#                         design = design_sub, family="quasibinomial")
# 
# # Models for digestive issues
# log_min_dig_1 <- svyglm(stomach_r ~ Effective.Minimum.Wage +
#                           year + fipsst,
#                         design = design_sub, family="quasibinomial")
# log_min_dig_2 <- svyglm(stomach_r ~ Effective.Minimum.Wage +
#                           age + sex + race_eth + family_struc + adult_edu + fpl_category + nativity +
#                           year + fipsst,
#                         design = design_sub, family="quasibinomial")
# 
# # Models for unmet health needs (any)
# log_min_unm_1 <- svyglm(unmet_needs ~ Effective.Minimum.Wage +
#                           year + fipsst,
#                         design = design_sub, family="quasibinomial")
# log_min_unm_2 <- svyglm(unmet_needs ~ Effective.Minimum.Wage +
#                           age + sex + race_eth + family_struc + adult_edu + fpl_category + nativity +
#                           year + fipsst,
#                         design = design_sub, family="quasibinomial")
# 
# # Models for unmet mental health needs
# log_min_men_1 <- svyglm(unmet_mental ~ Effective.Minimum.Wage +
#                           year + fipsst,
#                         design = design_sub, family="quasibinomial")
# log_min_men_2 <- svyglm(unmet_mental ~ Effective.Minimum.Wage +
#                           age + sex + race_eth + family_struc + adult_edu + fpl_category + nativity +
#                           year + fipsst,
#                         design = design_sub, family="quasibinomial")
# 
# # Models for missing school
# log_min_sch_1 <- svyglm(missed_school ~ Effective.Minimum.Wage +
#                           year + fipsst,
#                         design = design_sub, family="quasibinomial")
# log_min_sch_2 <- svyglm(missed_school ~ Effective.Minimum.Wage +
#                           age + sex + race_eth + family_struc + adult_edu + fpl_category + nativity +
#                           year + fipsst,
#                         design = design_sub, family="quasibinomial")
# 
# # Extract coefficients of interest
# logistic_vals <- as.data.frame(rbind(
#   # Depression
#   cbind("Depression",       "All children (unadjusted)",
#         exp(coef(log_min_dep_1))[2],
#         exp(confint(log_min_dep_1))[2,1], exp(confint(log_min_dep_1))[2,2]),
#   cbind("Depression",       "All children (adjusted)",
#         exp(coef(log_min_dep_2))[2],
#         exp(confint(log_min_dep_2))[2,1], exp(confint(log_min_dep_2))[2,2]),
#   
#   # Anxiety
#   cbind("Anxiety",          "All children (unadjusted)",
#         exp(coef(log_min_anx_1))[2],
#         exp(confint(log_min_anx_1))[2,1], exp(confint(log_min_anx_1))[2,2]),
#   cbind("Anxiety",          "All children (adjusted)",
#         exp(coef(log_min_anx_2))[2],
#         exp(confint(log_min_anx_2))[2,1], exp(confint(log_min_anx_2))[2,2]),
#   
#   # ADD/ADHD
#   cbind("ADD/ADHD",          "All children (unadjusted)",
#         exp(coef(log_min_add_1))[2],
#         exp(confint(log_min_add_1))[2,1], exp(confint(log_min_add_1))[2,2]),
#   cbind("ADD/ADHD",          "All children (adjusted)",
#         exp(coef(log_min_add_2))[2],
#         exp(confint(log_min_add_2))[2,1], exp(confint(log_min_add_2))[2,2]),
#   
#   # Behavioral problems
#   cbind("Behavioral prob.", "All children (unadjusted)",
#         exp(coef(log_min_beh_1))[2],
#         exp(confint(log_min_beh_1))[2,1], exp(confint(log_min_beh_1))[2,2]),
#   cbind("Behavioral prob.", "All children (adjusted)",
#         exp(coef(log_min_beh_2))[2],
#         exp(confint(log_min_beh_2))[2,1], exp(confint(log_min_beh_2))[2,2]),
#   
#   # Digestive issues
#   cbind("Digestive issues", "All children (unadjusted)",
#         exp(coef(log_min_dig_1))[2],
#         exp(confint(log_min_dig_1))[2,1], exp(confint(log_min_dig_1))[2,2]),
#   cbind("Digestive issues", "All children (adjusted)",
#         exp(coef(log_min_dig_2))[2],
#         exp(confint(log_min_dig_2))[2,1], exp(confint(log_min_dig_2))[2,2]),
#   
#   # Unmet health care needs (any)
#   cbind("Unmet health care\n(of any kind)", "All children (unadjusted)",
#         exp(coef(log_min_unm_1))[2],
#         exp(confint(log_min_unm_1))[2,1], exp(confint(log_min_unm_1))[2,2]),
#   cbind("Unmet health care\n(of any kind)", "All children (adjusted)",
#         exp(coef(log_min_unm_2))[2],
#         exp(confint(log_min_unm_2))[2,1], exp(confint(log_min_unm_2))[2,2]),
#   
#   # Unmet mental health care
#   cbind("Unmet health care\n(mental health)", "All children (unadjusted)",
#         exp(coef(log_min_men_1))[2],
#         exp(confint(log_min_men_1))[2,1], exp(confint(log_min_men_1))[2,2]),
#   cbind("Unmet health care\n(mental health)", "All children (adjusted)",
#         exp(coef(log_min_men_2))[2],
#         exp(confint(log_min_men_2))[2,1], exp(confint(log_min_men_2))[2,2]),
#   
#   # Missed 1+ week of school
#   cbind("Missed school",    "All children (unadjusted)",
#         exp(coef(log_min_sch_1))[2],
#         exp(confint(log_min_sch_1))[2,1], exp(confint(log_min_sch_1))[2,2]),
#   cbind("Missed school",    "All children (adjusted)",
#         exp(coef(log_min_sch_2))[2],
#         exp(confint(log_min_sch_2))[2,1], exp(confint(log_min_sch_2))[2,2])
# ))
# colnames(logistic_vals) <- c("Outcome", "Sample", "or", "conf_low", "conf_high")
# 
# # Reorder factor variables
# logistic_vals$Outcome <- factor(logistic_vals$Outcome, levels=c("Depression", "Anxiety", "ADD/ADHD", "Behavioral prob.", "Digestive issues", "Unmet health care\n(of any kind)", "Unmet health care\n(mental health)", "Missed school"))
# logistic_vals$Sample <- factor(logistic_vals$Sample, levels=c("All children (unadjusted)", "All children (adjusted)", "Less than 200% FPL", "Black or Hispanic/Latino", "Adolescents, age 13-17", "All children (2020 dollars)", "All children (lagged wage)", "All children (lifetime wage, infl. adj.)"))
# 
# # Treat columns as numeric
# logistic_vals$or        <- as.numeric(logistic_vals$or)
# logistic_vals$conf_low  <- as.numeric(logistic_vals$conf_low)
# logistic_vals$conf_high <- as.numeric(logistic_vals$conf_high)
# 
# # Generate coefficient plot
# plot_int <- ggplot(logistic_vals, aes(x=Outcome, y=or,
#                                          group=Sample, color=Sample)) +
#   geom_point(position = position_dodge(width=0.6), size=1, aes(shape=Sample)) +
#   scale_shape_manual(values = 1:nlevels(logistic_vals$Sample)) +
#   geom_errorbar(aes(ymin=conf_low, ymax=conf_high),
#                 position = position_dodge(width=0.6), width=0.4) +
#   ylab("Effect of $1 increase in minimum wage\non mental health outcomes (odds ratio)") +
#   theme_test() +
#   theme(legend.position = "right",
#         text = element_text(size = 10, face = "bold"),
#         axis.text.x = element_text(angle=45, hjust=1, vjust=1),
#         axis.title.x = element_blank(),
#         axis.ticks = element_blank(),
#         strip.background = element_blank(),
#         legend.title = element_blank(),
#         panel.grid.major.x = element_blank(),
#         panel.grid.major.y = element_line(color="light gray", linewidth=0.5),
#         panel.grid.minor.y = element_line(color="light gray", linewidth=0.25)) +
#   geom_hline(yintercept=1, color="black", linewidth=0.25) +
#   scale_y_continuous(trans = "log10", limits=c(0.25,4),
#                      labels=c(0.25,0.5,1,2,4),
#                      breaks=c(0.25,0.5,1,2,4),
#                      minor_breaks = seq(0.25,10,0.25)) +
#   scale_color_grey(start=0, end=0.7)
# 
# # Export figure
# ggsave(plot=plot_int, file="Coefficient plot, logistic.pdf", width=6, height=4, units='in', dpi=600)

##############################################################################
# Effect of lifetime minimum wage on mental health outcomes
##############################################################################

# # Models for depression
# life_min_dep_1 <- svyglm(depression ~ wage_life +
#                            age + year + fipsst,
#                          design = design_sub)
# life_min_dep_2 <- svyglm(depression ~ wage_life +
#                            age + sex + race_eth + family_struc + adult_edu + fpl_category + nativity +
#                            year + fipsst,
#                          design = design_sub)
# life_min_dep_3 <- svyglm(depression ~ wage_life*low_income +
#                            age + sex + race_eth + family_struc + adult_edu + fpl_category + nativity +
#                            year + fipsst,
#                          design = design_sub)
# life_min_dep_6 <- svyglm(depression ~ wage_life*age_cat +
#                            age + sex + race_eth + family_struc + adult_edu + fpl_category + nativity +
#                            year + fipsst,
#                          design = design_sub)
# life_min_dep_7 <- svyglm(depression ~ wage_life*race_eth_cat +
#                            age + sex + race_eth + family_struc + adult_edu + fpl_category + nativity +
#                            year + fipsst,
#                          design = design_sub)
# life_min_dep_8 <- svyglm(depression ~ wage_life*adult_edu_cat +
#                            age + sex + race_eth + family_struc + adult_edu + fpl_category + nativity +
#                            year + fipsst,
#                          design = design_sub)
# life_min_dep_9 <- svyglm(depression ~ wage_life*nativity_cat +
#                            age + sex + race_eth + family_struc + adult_edu + fpl_category + nativity +
#                            year + fipsst,
#                          design = design_sub)
# 
# # Models for anxiety
# life_min_anx_1 <- svyglm(anxiety ~ wage_life +
#                            age + year + fipsst,
#                          design = design_sub)
# life_min_anx_2 <- svyglm(anxiety ~ wage_life +
#                            age + sex + race_eth + family_struc + adult_edu + fpl_category + nativity +
#                            year + fipsst,
#                          design = design_sub)
# life_min_anx_3 <- svyglm(anxiety ~ wage_life*low_income +
#                            age + sex + race_eth + family_struc + adult_edu + fpl_category + nativity +
#                            year + fipsst,
#                          design = design_sub)
# life_min_anx_6 <- svyglm(anxiety ~ wage_life*age_cat +
#                            age + sex + race_eth + family_struc + adult_edu + fpl_category + nativity +
#                            year + fipsst,
#                          design = design_sub)
# life_min_anx_7 <- svyglm(anxiety ~ wage_life*race_eth_cat +
#                            age + sex + race_eth + family_struc + adult_edu + fpl_category + nativity +
#                            year + fipsst,
#                          design = design_sub)
# life_min_anx_8 <- svyglm(anxiety ~ wage_life*adult_edu_cat +
#                            age + sex + race_eth + family_struc + adult_edu + fpl_category + nativity +
#                            year + fipsst,
#                          design = design_sub)
# life_min_anx_9 <- svyglm(anxiety ~ wage_life*nativity_cat +
#                            age + sex + race_eth + family_struc + adult_edu + fpl_category + nativity +
#                            year + fipsst,
#                          design = design_sub)
# 
# # Models for ADD/ADHD
# life_min_add_1 <- svyglm(adhd ~ wage_life +
#                            age + year + fipsst,
#                          design = design_sub)
# life_min_add_2 <- svyglm(adhd ~ wage_life +
#                            age + sex + race_eth + family_struc + adult_edu + fpl_category + nativity +
#                            year + fipsst,
#                          design = design_sub)
# life_min_add_3 <- svyglm(adhd ~ wage_life*low_income +
#                            age + sex + race_eth + family_struc + adult_edu + fpl_category + nativity +
#                            year + fipsst,
#                          design = design_sub)
# life_min_add_6 <- svyglm(adhd ~ wage_life*age_cat +
#                            age + sex + race_eth + family_struc + adult_edu + fpl_category + nativity +
#                            year + fipsst,
#                          design = design_sub)
# life_min_add_7 <- svyglm(adhd ~ wage_life*race_eth_cat +
#                            age + sex + race_eth + family_struc + adult_edu + fpl_category + nativity +
#                            year + fipsst,
#                          design = design_sub)
# life_min_add_8 <- svyglm(adhd ~ wage_life*adult_edu_cat +
#                            age + sex + race_eth + family_struc + adult_edu + fpl_category + nativity +
#                            year + fipsst,
#                          design = design_sub)
# life_min_add_9 <- svyglm(adhd ~ wage_life*nativity_cat +
#                            age + sex + race_eth + family_struc + adult_edu + fpl_category + nativity +
#                            year + fipsst,
#                          design = design_sub)
# 
# # Models for behavioral problems
# life_min_beh_1 <- svyglm(behavior ~ wage_life +
#                            age + year + fipsst,
#                          design = design_sub)
# life_min_beh_2 <- svyglm(behavior ~ wage_life +
#                            age + sex + race_eth + family_struc + adult_edu + fpl_category + nativity +
#                            year + fipsst,
#                          design = design_sub)
# life_min_beh_3 <- svyglm(behavior ~ wage_life*low_income +
#                            age + sex + race_eth + family_struc + adult_edu + fpl_category + nativity +
#                            year + fipsst,
#                          design = design_sub)
# life_min_beh_6 <- svyglm(behavior ~ wage_life*age_cat +
#                            age + sex + race_eth + family_struc + adult_edu + fpl_category + nativity +
#                            year + fipsst,
#                          design = design_sub)
# life_min_beh_7 <- svyglm(behavior ~ wage_life*race_eth_cat +
#                            age + sex + race_eth + family_struc + adult_edu + fpl_category + nativity +
#                            year + fipsst,
#                          design = design_sub)
# life_min_beh_8 <- svyglm(behavior ~ wage_life*adult_edu_cat +
#                            age + sex + race_eth + family_struc + adult_edu + fpl_category + nativity +
#                            year + fipsst,
#                          design = design_sub)
# life_min_beh_9 <- svyglm(behavior ~ wage_life*nativity_cat +
#                            age + sex + race_eth + family_struc + adult_edu + fpl_category + nativity +
#                            year + fipsst,
#                          design = design_sub)
# 
# # Models for digestive issues
# life_min_dig_1 <- svyglm(stomach_r ~ wage_life +
#                            age + year + fipsst,
#                          design = design_sub)
# life_min_dig_2 <- svyglm(stomach_r ~ wage_life +
#                            age + sex + race_eth + family_struc + adult_edu + fpl_category + nativity +
#                            year + fipsst,
#                          design = design_sub)
# life_min_dig_3 <- svyglm(stomach_r ~ wage_life*low_income +
#                            age + sex + race_eth + family_struc + adult_edu + fpl_category + nativity +
#                            year + fipsst,
#                          design = design_sub)
# life_min_dig_6 <- svyglm(stomach_r ~ wage_life*age_cat +
#                            age + sex + race_eth + family_struc + adult_edu + fpl_category + nativity +
#                            year + fipsst,
#                          design = design_sub)
# life_min_dig_7 <- svyglm(stomach_r ~ wage_life*race_eth_cat +
#                            age + sex + race_eth + family_struc + adult_edu + fpl_category + nativity +
#                            year + fipsst,
#                          design = design_sub)
# life_min_dig_8 <- svyglm(stomach_r ~ wage_life*adult_edu_cat +
#                            age + sex + race_eth + family_struc + adult_edu + fpl_category + nativity +
#                            year + fipsst,
#                          design = design_sub)
# life_min_dig_9 <- svyglm(stomach_r ~ wage_life*nativity_cat +
#                            age + sex + race_eth + family_struc + adult_edu + fpl_category + nativity +
#                            year + fipsst,
#                          design = design_sub)
# 
# # Models for unmet health needs (any)
# life_min_unm_1 <- svyglm(unmet_needs ~ wage_life +
#                            age + year + fipsst,
#                          design = design_sub)
# life_min_unm_2 <- svyglm(unmet_needs ~ wage_life +
#                            age + sex + race_eth + family_struc + adult_edu + fpl_category + nativity +
#                            year + fipsst,
#                          design = design_sub)
# life_min_unm_3 <- svyglm(unmet_needs ~ wage_life*low_income +
#                            age + sex + race_eth + family_struc + adult_edu + fpl_category + nativity +
#                            year + fipsst,
#                          design = design_sub)
# life_min_unm_6 <- svyglm(unmet_needs ~ wage_life*age_cat +
#                            age + sex + race_eth + family_struc + adult_edu + fpl_category + nativity +
#                            year + fipsst,
#                          design = design_sub)
# life_min_unm_7 <- svyglm(unmet_needs ~ wage_life*race_eth_cat +
#                            age + sex + race_eth + family_struc + adult_edu + fpl_category + nativity +
#                            year + fipsst,
#                          design = design_sub)
# life_min_unm_8 <- svyglm(unmet_needs ~ wage_life*adult_edu_cat +
#                            age + sex + race_eth + family_struc + adult_edu + fpl_category + nativity +
#                            year + fipsst,
#                          design = design_sub)
# life_min_unm_9 <- svyglm(unmet_needs ~ wage_life*nativity_cat +
#                            age + sex + race_eth + family_struc + adult_edu + fpl_category + nativity +
#                            year + fipsst,
#                          design = design_sub)
# 
# # Models for unmet mental health needs
# life_min_men_1 <- svyglm(unmet_mental ~ wage_life +
#                            age + year + fipsst,
#                          design = design_sub)
# life_min_men_2 <- svyglm(unmet_mental ~ wage_life +
#                            age + sex + race_eth + family_struc + adult_edu + fpl_category + nativity +
#                            year + fipsst,
#                          design = design_sub)
# life_min_men_3 <- svyglm(unmet_mental ~ wage_life*low_income +
#                            age + sex + race_eth + family_struc + adult_edu + fpl_category + nativity +
#                            year + fipsst,
#                          design = design_sub)
# life_min_men_6 <- svyglm(unmet_mental ~ wage_life*age_cat +
#                            age + sex + race_eth + family_struc + adult_edu + fpl_category + nativity +
#                            year + fipsst,
#                          design = design_sub)
# life_min_men_7 <- svyglm(unmet_mental ~ wage_life*race_eth_cat +
#                            age + sex + race_eth + family_struc + adult_edu + fpl_category + nativity +
#                            year + fipsst,
#                          design = design_sub)
# life_min_men_8 <- svyglm(unmet_mental ~ wage_life*adult_edu_cat +
#                            age + sex + race_eth + family_struc + adult_edu + fpl_category + nativity +
#                            year + fipsst,
#                          design = design_sub)
# life_min_men_9 <- svyglm(unmet_mental ~ wage_life*nativity_cat +
#                            age + sex + race_eth + family_struc + adult_edu + fpl_category + nativity +
#                            year + fipsst,
#                          design = design_sub)
# 
# # Models for missing school
# life_min_sch_1 <- svyglm(missed_school ~ wage_life +
#                            age + year + fipsst,
#                          design = design_sub)
# life_min_sch_2 <- svyglm(missed_school ~ wage_life +
#                            age + sex + race_eth + family_struc + adult_edu + fpl_category + nativity +
#                            year + fipsst,
#                          design = design_sub)
# life_min_sch_3 <- svyglm(missed_school ~ wage_life*low_income +
#                            age + sex + race_eth + family_struc + adult_edu + fpl_category + nativity +
#                            year + fipsst,
#                          design = design_sub)
# life_min_sch_6 <- svyglm(missed_school ~ wage_life*age_cat +
#                            age + sex + race_eth + family_struc + adult_edu + fpl_category + nativity +
#                            year + fipsst,
#                          design = design_sub)
# life_min_sch_7 <- svyglm(missed_school ~ wage_life*race_eth_cat +
#                            age + sex + race_eth + family_struc + adult_edu + fpl_category + nativity +
#                            year + fipsst,
#                          design = design_sub)
# life_min_sch_8 <- svyglm(missed_school ~ wage_life*adult_edu_cat +
#                            age + sex + race_eth + family_struc + adult_edu + fpl_category + nativity +
#                            year + fipsst,
#                          design = design_sub)
# life_min_sch_9 <- svyglm(missed_school ~ wage_life*nativity_cat +
#                            age + sex + race_eth + family_struc + adult_edu + fpl_category + nativity +
#                            year + fipsst,
#                          design = design_sub)
# 
# # Extract coefficients of interest
# life_vals <- as.data.frame(rbind(
#   # Depression
#   cbind("Depression",       "All children (unadjusted)",
#         life_min_dep_1$coefficients[2], SE(life_min_dep_1)[2]),
#   cbind("Depression",       "All children (adjusted)",
#         life_min_dep_2$coefficients[2], SE(life_min_dep_2)[2]),
#   cbind("Depression",       "Less than 200% FPL",
#         life_min_dep_3$coefficients[2], SE(life_min_dep_3)[2]),
#   cbind("Depression",       "Adults with high school or less",
#         life_min_dep_8$coefficients[2], SE(life_min_dep_8)[2]),
#   cbind("Depression",       "Black or Hispanic/Latino",
#         life_min_dep_7$coefficients[2], SE(life_min_dep_7)[2]),
#   cbind("Depression",       "First- or second-generation",
#         life_min_dep_9$coefficients[2], SE(life_min_dep_9)[2]),
#   cbind("Depression",       "Adolescents, age 13-17",
#         life_min_dep_6$coefficients[2], SE(life_min_dep_6)[2]),
#   
#   # Anxiety
#   cbind("Anxiety",          "All children (unadjusted)",
#         life_min_anx_1$coefficients[2], SE(life_min_anx_1)[2]),
#   cbind("Anxiety",          "All children (adjusted)",
#         life_min_anx_2$coefficients[2], SE(life_min_anx_2)[2]),
#   cbind("Anxiety",          "Less than 200% FPL",
#         life_min_anx_3$coefficients[2], SE(life_min_anx_3)[2]),
#   cbind("Anxiety",          "Adults with high school or less",
#         life_min_anx_8$coefficients[2], SE(life_min_anx_8)[2]),
#   cbind("Anxiety",          "Black or Hispanic/Latino",
#         life_min_anx_7$coefficients[2], SE(life_min_anx_7)[2]),
#   cbind("Anxiety",          "First- or second-generation",
#         life_min_anx_9$coefficients[2], SE(life_min_anx_9)[2]),
#   cbind("Anxiety",          "Adolescents, age 13-17",
#         life_min_anx_6$coefficients[2], SE(life_min_anx_6)[2]),
#   
#   # ADD/ADHD
#   cbind("ADD/ADHD",          "All children (unadjusted)",
#         life_min_add_1$coefficients[2], SE(life_min_add_1)[2]),
#   cbind("ADD/ADHD",          "All children (adjusted)",
#         life_min_add_2$coefficients[2], SE(life_min_add_2)[2]),
#   cbind("ADD/ADHD",          "Less than 200% FPL",
#         life_min_add_3$coefficients[2], SE(life_min_add_3)[2]),
#   cbind("ADD/ADHD",          "Adults with high school or less",
#         life_min_add_8$coefficients[2], SE(life_min_add_8)[2]),
#   cbind("ADD/ADHD",          "Black or Hispanic/Latino",
#         life_min_add_7$coefficients[2], SE(life_min_add_7)[2]),
#   cbind("ADD/ADHD",          "First- or second-generation",
#         life_min_add_9$coefficients[2], SE(life_min_add_9)[2]),
#   cbind("ADD/ADHD",          "Adolescents, age 13-17",
#         life_min_add_6$coefficients[2], SE(life_min_add_6)[2]),
#   
#   # Behavioral problems
#   cbind("Behavioral prob.", "All children (unadjusted)",
#         life_min_beh_1$coefficients[2], SE(life_min_beh_1)[2]),
#   cbind("Behavioral prob.", "All children (adjusted)",
#         life_min_beh_2$coefficients[2], SE(life_min_beh_2)[2]),
#   cbind("Behavioral prob.", "Less than 200% FPL",
#         life_min_beh_3$coefficients[2], SE(life_min_beh_3)[2]),
#   cbind("Behavioral prob.", "Adults with high school or less",
#         life_min_beh_8$coefficients[2], SE(life_min_beh_8)[2]),
#   cbind("Behavioral prob.", "Black or Hispanic/Latino",
#         life_min_beh_7$coefficients[2], SE(life_min_beh_7)[2]),
#   cbind("Behavioral prob.", "First- or second-generation",
#         life_min_beh_9$coefficients[2], SE(life_min_beh_9)[2]),
#   cbind("Behavioral prob.", "Adolescents, age 13-17",
#         life_min_beh_6$coefficients[2], SE(life_min_beh_6)[2]),
#   
#   # Digestive issues
#   cbind("Digestive issues", "All children (unadjusted)",
#         life_min_dig_1$coefficients[2], SE(life_min_dig_1)[2]),
#   cbind("Digestive issues", "All children (adjusted)",
#         life_min_dig_2$coefficients[2], SE(life_min_dig_2)[2]),
#   cbind("Digestive issues", "Less than 200% FPL",
#         life_min_dig_3$coefficients[2], SE(life_min_dig_3)[2]),
#   cbind("Digestive issues", "Adults with high school or less",
#         life_min_dig_8$coefficients[2], SE(life_min_dig_8)[2]),
#   cbind("Digestive issues", "Black or Hispanic/Latino",
#         life_min_dig_7$coefficients[2], SE(life_min_dig_7)[2]),
#   cbind("Digestive issues", "First- or second-generation",
#         life_min_dig_9$coefficients[2], SE(life_min_dig_9)[2]),
#   cbind("Digestive issues", "Adolescents, age 13-17",
#         life_min_dig_6$coefficients[2], SE(life_min_dig_6)[2]),
#   
#   # Unmet health care needs (any)
#   cbind("Unmet health care\n(of any kind)", "All children (unadjusted)",
#         life_min_unm_1$coefficients[2], SE(life_min_unm_1)[2]),
#   cbind("Unmet health care\n(of any kind)", "All children (adjusted)",
#         life_min_unm_2$coefficients[2], SE(life_min_unm_2)[2]),
#   cbind("Unmet health care\n(of any kind)", "Less than 200% FPL",
#         life_min_unm_3$coefficients[2], SE(life_min_unm_3)[2]),
#   cbind("Unmet health care\n(of any kind)", "Adults with high school or less",
#         life_min_unm_8$coefficients[2], SE(life_min_unm_8)[2]),
#   cbind("Unmet health care\n(of any kind)", "Black or Hispanic/Latino",
#         life_min_unm_7$coefficients[2], SE(life_min_unm_7)[2]),
#   cbind("Unmet health care\n(of any kind)", "First- or second-generation",
#         life_min_unm_9$coefficients[2], SE(life_min_unm_9)[2]),
#   cbind("Unmet health care\n(of any kind)", "Adolescents, age 13-17",
#         life_min_unm_6$coefficients[2], SE(life_min_unm_6)[2]),
#   
#   # Unmet mental health care
#   cbind("Unmet health care\n(mental health)", "All children (unadjusted)",
#         life_min_men_1$coefficients[2], SE(life_min_men_1)[2]),
#   cbind("Unmet health care\n(mental health)", "All children (adjusted)",
#         life_min_men_2$coefficients[2], SE(life_min_men_2)[2]),
#   cbind("Unmet health care\n(mental health)", "Less than 200% FPL",
#         life_min_men_3$coefficients[2], SE(life_min_men_3)[2]),
#   cbind("Unmet health care\n(mental health)", "Adults with high school or less",
#         life_min_men_8$coefficients[2], SE(life_min_men_8)[2]),
#   cbind("Unmet health care\n(mental health)", "Black or Hispanic/Latino",
#         life_min_men_7$coefficients[2], SE(life_min_men_7)[2]),
#   cbind("Unmet health care\n(mental health)", "First- or second-generation",
#         life_min_men_9$coefficients[2], SE(life_min_men_9)[2]),
#   cbind("Unmet health care\n(mental health)", "Adolescents, age 13-17",
#         life_min_men_6$coefficients[2], SE(life_min_men_6)[2]),
#   
#   # Missed 1+ week of school
#   cbind("Missed school",    "All children (unadjusted)",
#         life_min_sch_1$coefficients[2], SE(life_min_sch_1)[2]),
#   cbind("Missed school",    "All children (adjusted)",
#         life_min_sch_2$coefficients[2], SE(life_min_sch_2)[2]),
#   cbind("Missed school",    "Less than 200% FPL",
#         life_min_sch_3$coefficients[2], SE(life_min_sch_3)[2]),
#   cbind("Missed school",    "Adults with high school or less",
#         life_min_sch_8$coefficients[2], SE(life_min_sch_8)[2]),
#   cbind("Missed school",    "Black or Hispanic/Latino",
#         life_min_sch_7$coefficients[2], SE(life_min_sch_7)[2]),
#   cbind("Missed school",    "First- or second-generation",
#         life_min_sch_9$coefficients[2], SE(life_min_sch_9)[2]),
#   cbind("Missed school",    "Adolescents, age 13-17",
#         life_min_sch_6$coefficients[2], SE(life_min_sch_6)[2])
# ))
# colnames(life_vals) <- c("Outcome", "Sample", "effect", "se")
# 
# # Reorder factor variables
# life_vals$Outcome <- factor(life_vals$Outcome, levels=c("Depression", "Anxiety", "ADD/ADHD", "Behavioral prob.", "Digestive issues", "Unmet health care\n(of any kind)", "Unmet health care\n(mental health)", "Missed school"))
# life_vals$Sample <- factor(life_vals$Sample, levels=c("All children (unadjusted)", "All children (adjusted)", "Less than 200% FPL", "Adults with high school or less", "Black or Hispanic/Latino", "First- or second-generation", "Adolescents, age 13-17"))
# 
# # Treat columns as numeric
# life_vals$effect <- as.numeric(life_vals$effect)
# life_vals$se     <- as.numeric(life_vals$se)
# 
# # Generate coefficient plot
# plot_life <- ggplot(life_vals, aes(x=Outcome, y=effect,
#                                          group=Sample, color=Sample)) +
#   geom_point(position = position_dodge(width=0.6), size=1, aes(shape=Sample)) +
#   scale_shape_manual(values = 1:nlevels(life_vals$Sample)) +
#   geom_errorbar(aes(ymin=effect-1.96*se, ymax=effect+1.96*se),
#                 position = position_dodge(width=0.6), width=0.4) +
#   ylab("Effect of lifetime $1 increase in minimum\nwage on mental health outcomes") +
#   theme_test() +
#   theme(legend.position = "right",
#         text = element_text(size = 10, face = "bold"),
#         axis.text.x = element_text(angle=45, hjust=1, vjust=1),
#         axis.title.x = element_blank(),
#         axis.ticks = element_blank(),
#         strip.background = element_blank(),
#         legend.title = element_blank(),
#         panel.grid.major.x = element_blank(),
#         panel.grid.major.y = element_line(color="light gray", linewidth=0.5),
#         panel.grid.minor.y = element_line(color="light gray", linewidth=0.25)) +
#   geom_hline(yintercept=0, color="black", linewidth=0.25) +
#   scale_y_continuous(limits = c(-0.05, 0.05),
#                      breaks = seq(-0.04, 0.04, 0.02),
#                      minor_breaks = seq(-0.05, 0.05, 0.01),
#                      labels = function(x) paste0(x*100," pp")) +
#   scale_color_grey(start=0, end=0.7)
# 
# # Export figure
# ggsave(plot=plot_life, file="Coefficient plot, lifetime.pdf", width=8, height=4, units='in', dpi=600)
