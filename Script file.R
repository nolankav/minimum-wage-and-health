# Minimum wage and children's health
# N.M. Kavanagh, N. Slopen
# February 25, 2023

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

# Generate lagged minimum wage
min_wage_df <- min_wage_df %>%
  group_by(fipsst) %>%
  mutate(lag_by_1 = lag(Effective.Minimum.Wage, n=1, default=NA))

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
# White vs. non-white for interaction models
merged <- merged %>% mutate(
  race_eth_cat = case_when(
    sc_hispanic_r == 1    ~ "Non-white",
    sc_race_r == 1        ~ "White",
    sc_race_r %in% c(2:7) ~ "Non-white",
  ))
merged$race_eth_cat <- factor(merged$race_eth_cat, levels = c("Non-white", "White"))

# Adults' highest educational attainment
merged <- merged %>% mutate(
  adult_edu = case_when(
    higrade_tvis == 1 ~ "Less than high school",
    higrade_tvis == 2 ~ "High school (including vocational)",
    higrade_tvis == 3 ~ "Some college or associate degree",
    higrade_tvis == 4 ~ "College degree or higher"
  ))
merged$adult_edu <- factor(merged$adult_edu, levels = c("Less than high school", "High school (including vocational)", "Some college or associate degree", "College degree or higher"))

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

##############################################################################
# Specify complex design
##############################################################################

# Rescale weight so mean is 1
merged$weights <- merged$fwc/1946

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
    !is.na(depression) | !is.na(anxiety) | !is.na(behavior) |
      !is.na(stomach_r) | !is.na(missed_school) ~ 1
  ))

# Complete cases for covariates
merged_demo <- merged %>%
  subset(., age %in% c(3:17)) %>%
  filter_at(vars(Effective.Minimum.Wage, has_outcome, age, sex, race_eth,
                 adult_edu, fpl_category), all_vars(!is.na(.)))

# Child characteristics: unweighted
# This line provides demographic characteristics without survey weights.
summary(tableby(~ age + sex + race_eth + adult_edu + fpl_category,
                merged_demo, digits.pct=0), text=T)

# Child characteristics: weighted
# This line provides demographic characteristics with survey weights.
summary(tableby(~ age + sex + race_eth + adult_edu + fpl_category,
                merged_demo, digits.pct=0, weights=fwc/1946), text=T)

##############################################################################
# Figure 2: Associations between FPL and outcomes
##############################################################################

# Explore unadjusted associations
prop.table(svytable(~fpl_category + depression,    design=design_sub), 1)
prop.table(svytable(~fpl_category + anxiety,       design=design_sub), 1)
prop.table(svytable(~fpl_category + behavior,      design=design_sub), 1)
prop.table(svytable(~fpl_category + stomach_r,     design=design_sub), 1)
prop.table(svytable(~fpl_category + unmet_needs,   design=design_sub), 1)
prop.table(svytable(~fpl_category + unmet_mental,  design=design_sub), 1)
prop.table(svytable(~fpl_category + missed_school, design=design_sub), 1)

# Explore adjusted associations
model_fpl_dep <- svyglm(depression ~
                          age + sex + race_eth + adult_edu + fpl_category +
                          year + fipsst,
                        design = design_sub)
summary(model_fpl_dep)

model_fpl_anx <- svyglm(anxiety ~
                          age + sex + race_eth + adult_edu + fpl_category +
                          year + fipsst,
                        design = design_sub)
summary(model_fpl_anx)

model_fpl_beh <- svyglm(behavior ~
                          age + sex + race_eth + adult_edu + fpl_category +
                          year + fipsst,
                        design = design_sub)
summary(model_fpl_beh)

model_fpl_unm <- svyglm(unmet_needs ~
                          age + sex + race_eth + adult_edu + fpl_category +
                          year + fipsst,
                        design = design_sub)
summary(model_fpl_unm)

model_fpl_men <- svyglm(unmet_mental ~
                          age + sex + race_eth + adult_edu + fpl_category +
                          year + fipsst,
                        design = design_sub)
summary(model_fpl_men)

model_fpl_dig <- svyglm(stomach_r ~
                          age + sex + race_eth + adult_edu + fpl_category +
                          year + fipsst,
                        design = design_sub)
summary(model_fpl_dig)

model_fpl_sch <- svyglm(missed_school ~
                          age + sex + race_eth + adult_edu + fpl_category +
                          year + fipsst,
                        design = design_sub)
summary(model_fpl_sch)

# Prepare table for adjusted associations
cov_row <- as.data.frame(
  rbind(cbind("Child demographics",        t(rep("Yes",   4))),
        cbind("State fixed effects",       t(rep("Yes",   4))),
        cbind("Year fixed effects",        t(rep("Yes",   4))),
        cbind("Clustered standard errors", t(rep("State", 4))),
        cbind("Survey weights",            t(rep("Yes",   4)))))

# Compile results into table
modelsummary(list("Depression"       = model_fpl_dep,
                  "Anxiety"          = model_fpl_anx,
                  "Behavioral prob." = model_fpl_beh,
                  "Stomach issues"   = model_fpl_dig,
                  "Missed school"    = model_fpl_sch),
             coef_omit   = "^(?!fpl_category)",
             fmt         = "%.3f",
             statistic   = c("[{conf.low}, {conf.high}]", "P={p.value}"),
             gof_omit    = "Log*|AIC|BIC|F|RMSE|Std",
             coef_map    = c("fpl_categoryLess than 100%"  = "Less than 100%",
                             "fpl_category100% to 199%"    = "100% to 199%",
                             "fpl_category200% to 299%"    = "200% to 299%",
                             "fpl_category300% to 399%"    = "300% to 399%",
                             "fpl_category400% or greater" = "400% or greater"),
             add_rows    = cov_row,
             title       = "Association between household FPL and indicated outcomes (linear probability models).",
             stars       = c("*"=0.05, "**"=0.01, "***"=0.001))

# Generate marginal means
means_dep <- estimate_means(model_fpl_dep, at="fpl_category")
means_anx <- estimate_means(model_fpl_anx, at="fpl_category")
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
  cbind("Unmet health care needs\n(of any kind)", "Less than 100%",  means_unm$Mean[1], means_unm$SE[1]),
  cbind("Unmet health care needs\n(of any kind)", "100% to 199%",    means_unm$Mean[2], means_unm$SE[2]),
  cbind("Unmet health care needs\n(of any kind)", "200% to 299%",    means_unm$Mean[3], means_unm$SE[3]),
  cbind("Unmet health care needs\n(of any kind)", "300% to 399%",    means_unm$Mean[4], means_unm$SE[4]),
  cbind("Unmet health care needs\n(of any kind)", "400% or greater", means_unm$Mean[5], means_unm$SE[5]),
  
  # Unmet health care needs (mental health)
  cbind("Unmet health care needs\n(mental health)", "Less than 100%",  means_men$Mean[1], means_men$SE[1]),
  cbind("Unmet health care needs\n(mental health)", "100% to 199%",    means_men$Mean[2], means_men$SE[2]),
  cbind("Unmet health care needs\n(mental health)", "200% to 299%",    means_men$Mean[3], means_men$SE[3]),
  cbind("Unmet health care needs\n(mental health)", "300% to 399%",    means_men$Mean[4], means_men$SE[4]),
  cbind("Unmet health care needs\n(mental health)", "400% or greater", means_men$Mean[5], means_men$SE[5]),
  
  # Missed school
  cbind("Missed school", "Less than 100%",  means_sch$Mean[1], means_sch$SE[1]),
  cbind("Missed school", "100% to 199%",    means_sch$Mean[2], means_sch$SE[2]),
  cbind("Missed school", "200% to 299%",    means_sch$Mean[3], means_sch$SE[3]),
  cbind("Missed school", "300% to 399%",    means_sch$Mean[4], means_sch$SE[4]),
  cbind("Missed school", "400% or greater", means_sch$Mean[5], means_sch$SE[5])
))
colnames(means_all) <- c("Outcome", "FPL level", "value", "se")

# Reorder factor levels
means_all$`FPL level` <- factor(means_all$`FPL level`, levels = c("Less than 100%", "100% to 199%", "200% to 299%", "300% to 399%", "400% or greater"))
means_all$Outcome <- factor(means_all$Outcome, levels = c("Depression", "Anxiety", "Behavioral prob.", "Digestive issues", "Unmet health care needs\n(of any kind)", "Unmet health care needs\n(mental health)", "Missed school"))

# Treat adjusted means and SEs as numeric
means_all$value <- as.numeric(as.character(means_all$value))
means_all$se    <- as.numeric(as.character(means_all$se))

# Plot means by outcome
plot_1 <- ggplot(means_all, aes(x=`FPL level`, y=value, fill=`FPL level`)) +
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
  facet_grid(~Outcome, scales="free", space="free")
print(plot_1)

# Export figure
ggsave(plot=plot_1, file="Adjusted rates of outcomes.pdf", width=7, height=4, units='in', dpi=600)

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
ggsave(plot=maps_all, file="Map of minimum wages.pdf", width=5, height=7, units='in', dpi=600)

##############################################################################
# Effect of minimum wage on mental health outcomes
##############################################################################

# Models for depression
model_min_dep_1 <- svyglm(depression ~ Effective.Minimum.Wage +
                            year + fipsst,
                          design = design_sub)
model_min_dep_2 <- svyglm(depression ~ Effective.Minimum.Wage +
                            age + sex + race_eth + adult_edu + fpl_category +
                            year + fipsst,
                          design = design_sub)
model_min_dep_3 <- svyglm(depression ~ Effective.Minimum.Wage*low_income +
                            age + sex + race_eth + adult_edu + fpl_category +
                            year + fipsst,
                          design = design_sub)
model_min_dep_4 <- svyglm(depression ~ Effective.Minimum.Wage.2020.Dollars +
                            age + sex + race_eth + adult_edu + fpl_category +
                            year + fipsst,
                          design = design_sub)
model_min_dep_5 <- svyglm(depression ~ lag_by_1 +
                            age + sex + race_eth + adult_edu + fpl_category +
                            year + fipsst,
                          design = design_sub)
model_min_dep_6 <- svyglm(depression ~ Effective.Minimum.Wage*age_cat +
                            age + sex + race_eth + adult_edu + fpl_category +
                            year + fipsst,
                          design = design_sub)
model_min_dep_7 <- svyglm(depression ~ Effective.Minimum.Wage*race_eth_cat +
                            age + sex + race_eth + adult_edu + fpl_category +
                            year + fipsst,
                          design = design_sub)

# Models for anxiety
model_min_anx_1 <- svyglm(anxiety ~ Effective.Minimum.Wage +
                            year + fipsst,
                          design = design_sub)
model_min_anx_2 <- svyglm(anxiety ~ Effective.Minimum.Wage +
                            age + sex + race_eth + adult_edu + fpl_category +
                            year + fipsst,
                          design = design_sub)
model_min_anx_3 <- svyglm(anxiety ~ Effective.Minimum.Wage*low_income +
                            age + sex + race_eth + adult_edu + fpl_category +
                            year + fipsst,
                          design = design_sub)
model_min_anx_4 <- svyglm(anxiety ~ Effective.Minimum.Wage.2020.Dollars +
                            age + sex + race_eth + adult_edu + fpl_category +
                            year + fipsst,
                          design = design_sub)
model_min_anx_5 <- svyglm(anxiety ~ lag_by_1 +
                            age + sex + race_eth + adult_edu + fpl_category +
                            year + fipsst,
                          design = design_sub)
model_min_anx_6 <- svyglm(anxiety ~ Effective.Minimum.Wage*age_cat +
                            age + sex + race_eth + adult_edu + fpl_category +
                            year + fipsst,
                          design = design_sub)
model_min_anx_7 <- svyglm(anxiety ~ Effective.Minimum.Wage*race_eth_cat +
                            age + sex + race_eth + adult_edu + fpl_category +
                            year + fipsst,
                          design = design_sub)

# Models for behavioral problems
model_min_beh_1 <- svyglm(behavior ~ Effective.Minimum.Wage +
                            year + fipsst,
                          design = design_sub)
model_min_beh_2 <- svyglm(behavior ~ Effective.Minimum.Wage +
                            age + sex + race_eth + adult_edu + fpl_category +
                            year + fipsst,
                          design = design_sub)
model_min_beh_3 <- svyglm(behavior ~ Effective.Minimum.Wage*low_income +
                            age + sex + race_eth + adult_edu + fpl_category +
                            year + fipsst,
                          design = design_sub)
model_min_beh_4 <- svyglm(behavior ~ Effective.Minimum.Wage.2020.Dollars +
                            age + sex + race_eth + adult_edu + fpl_category +
                            year + fipsst,
                          design = design_sub)
model_min_beh_5 <- svyglm(behavior ~ lag_by_1 +
                            age + sex + race_eth + adult_edu + fpl_category +
                            year + fipsst,
                          design = design_sub)
model_min_beh_6 <- svyglm(behavior ~ Effective.Minimum.Wage*age_cat +
                            age + sex + race_eth + adult_edu + fpl_category +
                            year + fipsst,
                          design = design_sub)
model_min_beh_7 <- svyglm(behavior ~ Effective.Minimum.Wage*race_eth_cat +
                            age + sex + race_eth + adult_edu + fpl_category +
                            year + fipsst,
                          design = design_sub)

# Models for digestive issues
model_min_dig_1 <- svyglm(stomach_r ~ Effective.Minimum.Wage +
                            year + fipsst,
                          design = design_sub)
model_min_dig_2 <- svyglm(stomach_r ~ Effective.Minimum.Wage +
                            age + sex + race_eth + adult_edu + fpl_category +
                            year + fipsst,
                          design = design_sub)
model_min_dig_3 <- svyglm(stomach_r ~ Effective.Minimum.Wage*low_income +
                            age + sex + race_eth + adult_edu + fpl_category +
                            year + fipsst,
                          design = design_sub)
model_min_dig_4 <- svyglm(stomach_r ~ Effective.Minimum.Wage.2020.Dollars +
                            age + sex + race_eth + adult_edu + fpl_category +
                            year + fipsst,
                          design = design_sub)
model_min_dig_5 <- svyglm(stomach_r ~ lag_by_1 +
                            age + sex + race_eth + adult_edu + fpl_category +
                            year + fipsst,
                          design = design_sub)
model_min_dig_6 <- svyglm(stomach_r ~ Effective.Minimum.Wage*age_cat +
                            age + sex + race_eth + adult_edu + fpl_category +
                            year + fipsst,
                          design = design_sub)
model_min_dig_7 <- svyglm(stomach_r ~ Effective.Minimum.Wage*race_eth_cat +
                            age + sex + race_eth + adult_edu + fpl_category +
                            year + fipsst,
                          design = design_sub)

# Models for unmet health needs (any)
model_min_unm_1 <- svyglm(unmet_needs ~ Effective.Minimum.Wage +
                            year + fipsst,
                          design = design_sub)
model_min_unm_2 <- svyglm(unmet_needs ~ Effective.Minimum.Wage +
                            age + sex + race_eth + adult_edu + fpl_category +
                            year + fipsst,
                          design = design_sub)
model_min_unm_3 <- svyglm(unmet_needs ~ Effective.Minimum.Wage*low_income +
                            age + sex + race_eth + adult_edu + fpl_category +
                            year + fipsst,
                          design = design_sub)
model_min_unm_4 <- svyglm(unmet_needs ~ Effective.Minimum.Wage.2020.Dollars +
                            age + sex + race_eth + adult_edu + fpl_category +
                            year + fipsst,
                          design = design_sub)
model_min_unm_5 <- svyglm(unmet_needs ~ lag_by_1 +
                            age + sex + race_eth + adult_edu + fpl_category +
                            year + fipsst,
                          design = design_sub)
model_min_unm_6 <- svyglm(unmet_needs ~ Effective.Minimum.Wage*age_cat +
                            age + sex + race_eth + adult_edu + fpl_category +
                            year + fipsst,
                          design = design_sub)
model_min_unm_7 <- svyglm(unmet_needs ~ Effective.Minimum.Wage*race_eth_cat +
                            age + sex + race_eth + adult_edu + fpl_category +
                            year + fipsst,
                          design = design_sub)

# Models for unmet mental health needs
model_min_men_1 <- svyglm(unmet_mental ~ Effective.Minimum.Wage +
                            year + fipsst,
                          design = design_sub)
model_min_men_2 <- svyglm(unmet_mental ~ Effective.Minimum.Wage +
                            age + sex + race_eth + adult_edu + fpl_category +
                            year + fipsst,
                          design = design_sub)
model_min_men_3 <- svyglm(unmet_mental ~ Effective.Minimum.Wage*low_income +
                            age + sex + race_eth + adult_edu + fpl_category +
                            year + fipsst,
                          design = design_sub)
model_min_men_4 <- svyglm(unmet_mental ~ Effective.Minimum.Wage.2020.Dollars +
                            age + sex + race_eth + adult_edu + fpl_category +
                            year + fipsst,
                          design = design_sub)
model_min_men_5 <- svyglm(unmet_mental ~ lag_by_1 +
                            age + sex + race_eth + adult_edu + fpl_category +
                            year + fipsst,
                          design = design_sub)
model_min_men_6 <- svyglm(unmet_mental ~ Effective.Minimum.Wage*age_cat +
                            age + sex + race_eth + adult_edu + fpl_category +
                            year + fipsst,
                          design = design_sub)
model_min_men_7 <- svyglm(unmet_mental ~ Effective.Minimum.Wage*race_eth_cat +
                            age + sex + race_eth + adult_edu + fpl_category +
                            year + fipsst,
                          design = design_sub)

# Models for missing school
model_min_sch_1 <- svyglm(missed_school ~ Effective.Minimum.Wage +
                            year + fipsst,
                          design = design_sub)
model_min_sch_2 <- svyglm(missed_school ~ Effective.Minimum.Wage +
                            age + sex + race_eth + adult_edu + fpl_category +
                            year + fipsst,
                          design = design_sub)
model_min_sch_3 <- svyglm(missed_school ~ Effective.Minimum.Wage*low_income +
                            age + sex + race_eth + adult_edu + fpl_category +
                            year + fipsst,
                          design = design_sub)
model_min_sch_4 <- svyglm(missed_school ~ Effective.Minimum.Wage.2020.Dollars +
                            age + sex + race_eth + adult_edu + fpl_category +
                            year + fipsst,
                          design = design_sub)
model_min_sch_5 <- svyglm(missed_school ~ lag_by_1 +
                            age + sex + race_eth + adult_edu + fpl_category +
                            year + fipsst,
                          design = design_sub)
model_min_sch_6 <- svyglm(missed_school ~ Effective.Minimum.Wage*age_cat +
                            age + sex + race_eth + adult_edu + fpl_category +
                            year + fipsst,
                          design = design_sub)
model_min_sch_7 <- svyglm(missed_school ~ Effective.Minimum.Wage*race_eth_cat +
                            age + sex + race_eth + adult_edu + fpl_category +
                            year + fipsst,
                          design = design_sub)

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
  
  # Missed school
  length(model_min_sch_1$residuals), length(model_min_sch_2$residuals),
  length(model_min_sch_3$residuals), length(model_min_sch_4$residuals),
  length(model_min_sch_5$residuals), length(model_min_sch_6$residuals),
  length(model_min_sch_7$residuals)
  )
min(list); max(list)

# Extract coefficients of interest
interaction_vals <- as.data.frame(rbind(
  # Depression
  cbind("Depression",       "All children (unadjusted)",
        model_min_dep_1$coefficients[2], SE(model_min_dep_1)[2]),
  cbind("Depression",       "All children (adjusted)",
        model_min_dep_2$coefficients[2], SE(model_min_dep_2)[2]),
  cbind("Depression",       "Less than 200% FPL",
        model_min_dep_3$coefficients[2], SE(model_min_dep_3)[2]),
  cbind("Depression",       "Non-white children",
        model_min_dep_7$coefficients[2], SE(model_min_dep_7)[2]),
  cbind("Depression",       "Adolescents, age 13-17",
        model_min_dep_6$coefficients[2], SE(model_min_dep_6)[2]),
  cbind("Depression",       "All children (2020 dollars)",
        model_min_dep_4$coefficients[2], SE(model_min_dep_4)[2]),
  cbind("Depression",       "All children (lagged wage)",
        model_min_dep_5$coefficients[2], SE(model_min_dep_5)[2]),
  
  # Anxiety
  cbind("Anxiety",          "All children (unadjusted)",
        model_min_anx_1$coefficients[2], SE(model_min_anx_1)[2]),
  cbind("Anxiety",          "All children (adjusted)",
        model_min_anx_2$coefficients[2], SE(model_min_anx_2)[2]),
  cbind("Anxiety",          "Less than 200% FPL",
        model_min_anx_3$coefficients[2], SE(model_min_anx_3)[2]),
  cbind("Anxiety",          "Non-white children",
        model_min_anx_7$coefficients[2], SE(model_min_anx_7)[2]),
  cbind("Anxiety",          "Adolescents, age 13-17",
        model_min_anx_6$coefficients[2], SE(model_min_anx_6)[2]),
  cbind("Anxiety",          "All children (2020 dollars)",
        model_min_anx_4$coefficients[2], SE(model_min_anx_4)[2]),
  cbind("Anxiety",          "All children (lagged wage)",
        model_min_anx_5$coefficients[2], SE(model_min_anx_5)[2]),
  
  # Behavioral problems
  cbind("Behavioral prob.", "All children (unadjusted)",
        model_min_beh_1$coefficients[2], SE(model_min_beh_1)[2]),
  cbind("Behavioral prob.", "All children (adjusted)",
        model_min_beh_2$coefficients[2], SE(model_min_beh_2)[2]),
  cbind("Behavioral prob.", "Less than 200% FPL",
        model_min_beh_3$coefficients[2], SE(model_min_beh_3)[2]),
  cbind("Behavioral prob.", "Non-white children",
        model_min_beh_7$coefficients[2], SE(model_min_beh_7)[2]),
  cbind("Behavioral prob.", "Adolescents, age 13-17",
        model_min_beh_6$coefficients[2], SE(model_min_beh_6)[2]),
  cbind("Behavioral prob.", "All children (2020 dollars)",
        model_min_beh_4$coefficients[2], SE(model_min_beh_4)[2]),
  cbind("Behavioral prob.", "All children (lagged wage)",
        model_min_beh_5$coefficients[2], SE(model_min_beh_5)[2]),
  
  # Digestive issues
  cbind("Digestive issues", "All children (unadjusted)",
        model_min_dig_1$coefficients[2], SE(model_min_dig_1)[2]),
  cbind("Digestive issues", "All children (adjusted)",
        model_min_dig_2$coefficients[2], SE(model_min_dig_2)[2]),
  cbind("Digestive issues", "Less than 200% FPL",
        model_min_dig_3$coefficients[2], SE(model_min_dig_3)[2]),
  cbind("Digestive issues", "Non-white children",
        model_min_dig_7$coefficients[2], SE(model_min_dig_7)[2]),
  cbind("Digestive issues", "Adolescents, age 13-17",
        model_min_dig_6$coefficients[2], SE(model_min_dig_6)[2]),
  cbind("Digestive issues", "All children (2020 dollars)",
        model_min_dig_4$coefficients[2], SE(model_min_dig_4)[2]),
  cbind("Digestive issues", "All children (lagged wage)",
        model_min_dig_5$coefficients[2], SE(model_min_dig_5)[2]),
  
  # Unmet health care needs (any)
  cbind("Unmet health care needs\n(of any kind)", "All children (unadjusted)",
        model_min_unm_1$coefficients[2], SE(model_min_unm_1)[2]),
  cbind("Unmet health care needs\n(of any kind)", "All children (adjusted)",
        model_min_unm_2$coefficients[2], SE(model_min_unm_2)[2]),
  cbind("Unmet health care needs\n(of any kind)", "Less than 200% FPL",
        model_min_unm_3$coefficients[2], SE(model_min_unm_3)[2]),
  cbind("Unmet health care needs\n(of any kind)", "Non-white children",
        model_min_unm_7$coefficients[2], SE(model_min_unm_7)[2]),
  cbind("Unmet health care needs\n(of any kind)", "Adolescents, age 13-17",
        model_min_unm_6$coefficients[2], SE(model_min_unm_6)[2]),
  cbind("Unmet health care needs\n(of any kind)", "All children (2020 dollars)",
        model_min_unm_4$coefficients[2], SE(model_min_unm_4)[2]),
  cbind("Unmet health care needs\n(of any kind)", "All children (lagged wage)",
        model_min_unm_5$coefficients[2], SE(model_min_unm_5)[2]),
  
  # Unmet mental health care
  cbind("Unmet health care needs\n(mental health)", "All children (unadjusted)",
        model_min_men_1$coefficients[2], SE(model_min_men_1)[2]),
  cbind("Unmet health care needs\n(mental health)", "All children (adjusted)",
        model_min_men_2$coefficients[2], SE(model_min_men_2)[2]),
  cbind("Unmet health care needs\n(mental health)", "Less than 200% FPL",
        model_min_men_3$coefficients[2], SE(model_min_men_3)[2]),
  cbind("Unmet health care needs\n(mental health)", "Non-white children",
        model_min_men_7$coefficients[2], SE(model_min_men_7)[2]),
  cbind("Unmet health care needs\n(mental health)", "Adolescents, age 13-17",
        model_min_men_6$coefficients[2], SE(model_min_men_6)[2]),
  cbind("Unmet health care needs\n(mental health)", "All children (2020 dollars)",
        model_min_men_4$coefficients[2], SE(model_min_men_4)[2]),
  cbind("Unmet health care needs\n(mental health)", "All children (lagged wage)",
        model_min_men_5$coefficients[2], SE(model_min_men_5)[2]),
  
  # Missed 1+ week of school
  cbind("Missed school",    "All children (unadjusted)",
        model_min_sch_1$coefficients[2], SE(model_min_sch_1)[2]),
  cbind("Missed school",    "All children (adjusted)",
        model_min_sch_2$coefficients[2], SE(model_min_sch_2)[2]),
  cbind("Missed school",    "Less than 200% FPL",
        model_min_sch_3$coefficients[2], SE(model_min_sch_3)[2]),
  cbind("Missed school",    "Non-white children",
        model_min_sch_7$coefficients[2], SE(model_min_sch_7)[2]),
  cbind("Missed school",    "Adolescents, age 13-17",
        model_min_sch_6$coefficients[2], SE(model_min_sch_6)[2]),
  cbind("Missed school",    "All children (2020 dollars)",
        model_min_sch_4$coefficients[2], SE(model_min_sch_4)[2]),
  cbind("Missed school",    "All children (lagged wage)",
        model_min_sch_5$coefficients[2], SE(model_min_sch_5)[2])
))
colnames(interaction_vals) <- c("Outcome", "Sample", "effect", "se")

# Reorder factor variables
interaction_vals$Outcome <- factor(interaction_vals$Outcome, levels=c("Depression", "Anxiety", "Behavioral prob.", "Digestive issues", "Unmet health care needs\n(of any kind)", "Unmet health care needs\n(mental health)", "Missed school"))
interaction_vals$Sample <- factor(interaction_vals$Sample, levels=c("All children (unadjusted)", "All children (adjusted)", "Less than 200% FPL", "Non-white children", "Adolescents, age 13-17", "All children (2020 dollars)", "All children (lagged wage)"))

# Treat columns as numeric
interaction_vals$effect <- as.numeric(interaction_vals$effect)
interaction_vals$se     <- as.numeric(interaction_vals$se)

# Generate coefficient plot
plot_int <- ggplot(interaction_vals, aes(x=Outcome, y=effect,
                                         group=Sample, color=Sample)) +
  geom_point(position = position_dodge(width=0.6), size=1, aes(shape=Sample)) +
  scale_shape_manual(values = 1:nlevels(interaction_vals$Sample)) +
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
        panel.grid.major.y = element_line(color="light gray", size=0.5),
        panel.grid.minor.y = element_line(color="light gray", size=0.25)) +
  geom_hline(yintercept=0, color="black", size=0.25) +
  scale_y_continuous(limits = c(-0.025, 0.025),
                     breaks = c(-0.02, -0.01, 0, 0.01, 0.02),
                     minor_breaks = seq(-0.02, 0.02, 0.01),
                     labels = function(x) paste0(x*100," pp")) +
  scale_color_grey(start=0, end=0.7)

# Export figure
ggsave(plot=plot_int, file="Coefficient plot.pdf", width=6.5, height=4, units='in', dpi=600)
