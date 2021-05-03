rm(list=ls())
setwd("C:/Users/tanus/Google Drive/MSSc Peace and Conflict Research/Master Thesis/Data")
csv <- read.csv("thesis_dataset_final.csv")

library(dplyr)
library(tidyr)
library(stargazer)

# considerations
# probably remove countries with fewer than 10 obs or something, e.g. South Sudan
# there are 131 iso3c codes but 135 countries in dataset, figure out why


### DATA WRANGLING

data <- csv %>%
    select(!c(X, nightlight_total, foodsup_var_pcap, oecdcode, fao, gwn, iso2c)) %>%
    relocate(gdp_pcap_c2011, .after=droughtfloods_avg_90_09) %>%
    # create aid dollars, aid per capita and non-ag aid variables
    mutate(
        oecd_aid_nonag = oecd_aid_total-oecd_aid_ag,
        aid_ag_dollars = oecd_aid_ag*1000000,
        aid_ag_pcap = aid_ag_dollars/pop_total,
        aid_total_dollars = oecd_aid_total*1000000,
        aid_total_pcap = aid_total_dollars/pop_total,
        aid_nonag_dollars = oecd_aid_nonag*1000000,
        aid_nonag_pcap = aid_nonag_dollars/pop_total
    ) %>%
    relocate(oecd_aid_nonag, .after="oecd_aid_total") %>%
    # add lead of 1 year and 2 years to food security variables
    # do this before filtering for year, to incorporate lead times
    # repeat drought value by country
    group_by(country) %>%
    mutate(
        ldiet_energysup_3yr = dplyr::lead(diet_energysup_3yr,1),
        l2diet_energysup_3yr = dplyr::lead(diet_energysup_3yr,2),
        l3diet_energysup_3yr = dplyr::lead(diet_energysup_3yr,3),
        lundernourish_3yr = dplyr::lead(undernourish_3yr,1),
        l2undernourish_3yr = dplyr::lead(undernourish_3yr,2),
        l3undernourish_3yr = dplyr::lead(undernourish_3yr,3),
        lsev_foodinsec = dplyr::lead(sev_foodinsec,1),
        l2sev_foodinsec = dplyr::lead(sev_foodinsec,2),
        lmodsev_foodinsec = dplyr::lead(modsev_foodinsec,1),
        l2modsev_foodinsec = dplyr::lead(modsev_foodinsec,2),
        droughtfloods_avg_90_09 = sum(droughtfloods_avg_90_09, na.rm=TRUE)
    ) %>%
    ungroup() %>%
    filter(year<2017) %>% # peace years variable is up to 2017 only
    # log transform monetary variables
    # add 1 to avoid missing values as log(0)=undefined
    # categorise peace years
    # calculate severe conflict based on battle deaths and bd/population
    mutate(
        aid_ag_c2018_log = log1p(aid_ag_dollars + 1),
        aid_ag_pcap_c2018_log = log1p(aid_ag_pcap + 1),
        aid_nonag_c2018_log = log1p(aid_nonag_dollars + 1),
        aid_nonag_pcap_c2018_log = log1p(aid_nonag_pcap + 1),
        gdp_pcap_c2011_log = log1p(gdp_pcap_c2011 + 1),
        bdpop = battle_deaths/pop_total*100,
        sevconflict = case_when(battle_deaths>=500 ~ 1, TRUE ~ 0),
        sevconflict_pop = case_when(bdpop>=0.001 ~ 1, TRUE ~ 0),
        peaceyears_cat = case_when(
            between(peaceyears, 0, 3) ~ 1, 
            between(peaceyears, 4, 14) ~ 2, 
            peaceyears>14 ~ 3
            ),
        peaceyears_0to3 = case_when(peaceyears_cat==1 ~ 1, TRUE ~ 0),
        peaceyears_4to14 = case_when(peaceyears_cat==2 ~ 1, TRUE ~ 0),
        peaceyears_15plus = case_when(peaceyears_cat==3 ~ 1, TRUE ~ 0)
    ) %>%
    # some countries move from one category to another within the timeframe
    # recategorising peace years based on minumum value category
    group_by(country) %>%
    mutate(cpeaceyears_cat_min = min(peaceyears_cat)) %>%
    ungroup() %>%
    mutate(
        cpeaceyears_0to3 = case_when(cpeaceyears_cat_min==1 ~ 1, TRUE ~ 0),
        cpeaceyears_4to14 = case_when(cpeaceyears_cat_min==2 ~ 1, TRUE ~ 0),
        cpeaceyears_15plus = case_when(cpeaceyears_cat_min==3 ~ 1, TRUE ~ 0)
    ) %>%
    # filter out na in country name
    drop_na(country)
    # consider revisiting country codes that don't correspond with a country name

library(stringr)
# data$peaceyears_cat <- str_replace(data$peaceyears_cat, '1', 'conflict')
# data$peaceyears_cat <- str_replace(data$peaceyears_cat, '2', 'postconflict')
# data$peaceyears_cat <- str_replace(data$peaceyears_cat, '3', 'stable')
# data$peaceyears_cat_min <- str_replace(data$peaceyears_cat_min, '1', 'conflict')
# data$peaceyears_cat_min <- str_replace(data$peaceyears_cat_min, '2', 'postconflict')
# data$peaceyears_cat_min <- str_replace(data$peaceyears_cat_min, '3', 'stable')
data$lundernourish_3yr <- str_replace(data$lundernourish_3yr, '<', '')
data$l2undernourish_3yr <- str_replace(data$l2undernourish_3yr, '<', '')
data$l3undernourish_3yr <- str_replace(data$l3undernourish_3yr, '<', '')


# require(foreign)
# write.csv(data, "tanushree_thesisdata.csv")
# write.dta(data, "tanushree_thesisdata.dta")




# sub-samples, could be useful later

peaceyears_0to3 <- data %>%
    filter(peaceyears_cat=="1")

peaceyears_4to14 <- data %>%
    filter(peaceyears_cat=="2")

peaceyears_15plus <- data %>%
    filter(peaceyears_cat=="3")

# sub-samples by country, could be useful later

cpeaceyears_0to3 <- data %>%
    group_by(country) %>%
    filter(cpeaceyears_cat_min=="1")

cpeaceyears_4to14 <- data %>%
    group_by(country) %>%
    filter(cpeaceyears_cat_min=="2")

cpeaceyears_15plus <- data %>%
    group_by(country) %>%
    filter(cpeaceyears_cat_min=="3")



# ### SUMMARY STATS
# 
# unique(data$country) #135
# 
# data %>%
#     count(peaceyears_0to3, peaceyears_4to14, peaceyears_15plus)
#     # 870, 327, 525 obs respectively
# 
# unique(peaceyears_0to3$country) #58 - countries that experienced active conflict / less than 2 years postconflict during the time period
# unique(peaceyears_4to14$country) #22 - countries that experienced 3-15 peace years during the time
# unique(peaceyears_15plus$country) #34 - countries that experienced 16+ peace years for the entirety of the period
# # NA: 24 countries (no peace years data)
# # WHY ARE THERE DIFFERENT NUMBERS OF COUNTRIES EVERY TIME?!!! E.G. when moving the 'drop_na' function to near the beginning
# 
# summary(data)
# summary(data$lundernourish_3yr)
# summary(data$aid_ag_pcap)
# summary(data$aid_ag_pcap_c2018_log)



# correlation matrix

# indicates the correlation coefficient to be computed
# the default is pearson correlation coefficient which measures the linear dependence between two variables
# kendall and spearman correlation methods are non-parametric rank-based correlation test

data$lundernourish_3yr <- as.numeric(data$lundernourish_3yr)
data$l2undernourish_3yr <- as.numeric(data$l2undernourish_3yr)
data$l3undernourish_3yr <- as.numeric(data$l3undernourish_3yr)

corrdata <- data %>%
    select(aid_ag_pcap_c2018_log, aid_nonag_pcap_c2018_log, ldiet_energysup_3yr, lundernourish_3yr, peaceyears_0to3, peaceyears_4to14, peaceyears_15plus, gdp_pcap_c2011_log, sevconflict_pop, fragile_state, droughtfloods_avg_90_09)

corrdata <- cor(corrdata, use ="complete.obs", method = "pearson")
colnames(corrdata) <- c("aid (agricultural) per capita", "aid (non-agricultural) per capita", "dietary energy supply", "undernourishment", "peace status: conflict", "peace status: post-conflict", "peace status: stable", "GDP per capita", "severe conflict", "fragile state", "natural disasters")
rownames(corrdata) <- c("aid (agricultural) per capita", "aid (non-agricultural) per capita", "dietary energy supply", "undernourishment", "peace status: conflict", "peace status: post-conflict", "peace status: stable", "GDP per capita", "severe conflict", "fragile state", "natural disasters")


# stargazer(corrdata, type = "text", title = "Correlation Matrix", digits=2, out="correlation.html")

library(corrplot)
corrplot(corrdata, type = "upper",
         tl.col = "black", tl.srt = 45)

# recordPlot()

# save dataframe
save(data,file="data.Rda")

# density plots...

library(ggplot2)

ggplot(
    data,
    aes(aid_ag_pcap),
    ) +
    geom_density(fill="lavender") +
    xlab("Agricultural aid per capita (value, non-logged)") +
    ylab("Density") +
    theme_light()

ggsave("density_non-logged.png")

ggplot(
    data,
    aes(aid_ag_pcap_c2018_log)
    ) +
    geom_density(fill="lavender") +
    xlab("Agricultural aid per capita (value, log-transformed)") +
    ylab("Density") +
    theme_light()

ggsave("density_logged.png")


hist(
    data$aid_ag_pcap, 
    main = "Agricultural Aid Per Capita",
    xlab = "Value", 
    ylim=c(0, 2000),
    col="lavender"
)

hist(
    data$aid_ag_pcap_c2018_log, 
    main = "Agricultural Aid Per Capita (log-transformed)",
    xlab = "Value", 
    ylim=c(0, 700),
    col="lavender"
)

hist(
    data$lundernourish_3yr,
    main = "Prevalence of Undernourishment",
    xlab = "Value", 
    ylim=c(0, 400),
    col="lavender"
)

# frequency plots


datagroupcountry <- data %>%
    select(c(country, peaceyears_cat)) %>%
    group_by(country) %>%
    summarise(peaceyears = min(peaceyears_cat)) %>%
    ungroup()

ggplot(datagroupcountry, aes(peaceyears)) +
    geom_bar()

ggplot(data, aes(peaceyears_cat)) +
    geom_bar(fill="lavender", col="black") +
    theme_minimal() +
    xlab("Peace status") +
    ylab("Values")
    


# boxplots

ggplot(data) +
    aes(x = "", y = lundernourish_3yr) +
    geom_boxplot(fill = "lavender") +
    theme_minimal()

ggplot(data) +
    aes(x = "", y = aid_ag_pcap_c2018_log) +
    geom_boxplot(fill = "lavender") +
    theme_minimal()


### HAUSMAN TEST (OLS/FE)

# test for model misspecification
# null hypothesis - preferred model is random effects
# alternate hypothesis is that the model is fixed effects
# the tests looks to see if there is a correlation between the unique errors and the regressors in the model
# null hypothesis is that there is no correlation between the two
# interpretation: if the p-value is small (less than 0.05), reject the null hypothesis (and choose fixed effects)

library(plm)

#step 1 : Estimate the FE model
fe <- plm(lundernourish_3yr ~ aid_ag_pcap_c2018_log + sevconflict_pop + gdp_pcap_c2011_log + droughtfloods_avg_90_09 + peaceyears_cat, data=data,model="within")
summary(fe)
#step 2 : Estimate the RE model
re <- pggls(lundernourish_3yr ~ aid_ag_pcap_c2018_log + sevconflict_pop + gdp_pcap_c2011_log + droughtfloods_avg_90_09 + peaceyears_cat, data=data,model="random")
summary(re)
#step 3 : Run Hausman test
phtest(fe, re)

# if I include gdp, fixed effects; otherwise, OLS



### LINEAR MODELS


# OLS, BIVARIATE
aidund_ols1 <- lm(lundernourish_3yr ~ aid_ag_pcap_c2018_log, data=data) #OLS


# ONE-YEAR LEAD, COUNTRY FIXED EFFECTS

# bivariate
aidund_fe1 <- lm(lundernourish_3yr ~ aid_ag_pcap_c2018_log + factor(country) - 1, data=data) #FE

# with controls
aidundc_fe1 <- lm(lundernourish_3yr ~ aid_ag_pcap_c2018_log + gdp_pcap_c2011_log + sevconflict_pop + droughtfloods_avg_90_09 + factor(country) - 1, data=data)

# adding peace years
aidundpy0_fe1 <- lm(lundernourish_3yr ~ aid_ag_pcap_c2018_log + gdp_pcap_c2011_log + sevconflict_pop + droughtfloods_avg_90_09 + peaceyears_0to3 + factor(country) - 1, data=data)
aidundpy1_fe1 <- lm(lundernourish_3yr ~ aid_ag_pcap_c2018_log + gdp_pcap_c2011_log + sevconflict_pop + droughtfloods_avg_90_09 + peaceyears_4to14 + factor(country) - 1, data=data)
aidundpy2_fe1 <- lm(lundernourish_3yr ~ aid_ag_pcap_c2018_log + gdp_pcap_c2011_log + sevconflict_pop + droughtfloods_avg_90_09 + peaceyears_15plus + factor(country) - 1, data=data)
aidundpyall_fe1 <- lm(lundernourish_3yr ~ aid_ag_pcap_c2018_log + gdp_pcap_c2011_log + sevconflict_pop + droughtfloods_avg_90_09 + peaceyears_cat + factor(country) - 1, data=data)

# tables
stargazer(aidund_ols1, aidund_fe1, type = "text", title = "Table", digits=2, out="OLS vs FE.html")
stargazer(aidund_fe1, aidundc_fe1, aidundpyall_fe1, type = "text", title = "Table", digits=2)
stargazer(aidundc_fe1, aidundpy0_fe1, aidundpy1_fe1, aidundpy2_fe1, type = "text", title = "Table", digits=2, no.space=TRUE, out="Stepwise - 1-year lead.html")


# EXCLUDING CONFLICT COUNTRIES

datanoc <- data %>%
    filter(peaceyears_cat!="conflict")

aidundc_noc_fe1 <- lm(lundernourish_3yr ~ aid_ag_pcap_c2018_log + gdp_pcap_c2011_log + sevconflict_pop + droughtfloods_avg_90_09 + factor(country) - 1, data=datanoc)
aidundpy1_noc_fe1 <- lm(lundernourish_3yr ~ aid_ag_pcap_c2018_log + gdp_pcap_c2011_log + sevconflict_pop + droughtfloods_avg_90_09 + peaceyears_4to14 + factor(country) - 1, data=datanoc)
aidundpy2_noc_fe1 <- lm(lundernourish_3yr ~ aid_ag_pcap_c2018_log + gdp_pcap_c2011_log + sevconflict_pop + droughtfloods_avg_90_09 + peaceyears_15plus + factor(country) - 1, data=datanoc)

stargazer(aidundc_noc_fe1, aidundpy1_noc_fe1, aidundpy2_noc_fe1, type = "text", title = "Table", digits=2, no.space=TRUE, out="No conflict - 1-year lead.html")


# 1-YEAR LEAD, YEAR FIXED EFFECTS

# bivariate - 1 yr lead, ols and country fixed effects
aidund_fey1 <- lm(lundernourish_3yr ~ aid_ag_pcap_c2018_log + factor(year) - 1, data=data) #FE

# with controls
aidundc_fey1 <- lm(lundernourish_3yr ~ aid_ag_pcap_c2018_log + gdp_pcap_c2011_log + sevconflict_pop + droughtfloods_avg_90_09 + factor(year) - 1, data=data)

# adding peace years
aidundpy0_fey1 <- lm(lundernourish_3yr ~ aid_ag_pcap_c2018_log + gdp_pcap_c2011_log + sevconflict_pop + droughtfloods_avg_90_09 + peaceyears_0to3 + factor(year) - 1, data=data)
aidundpy1_fey1 <- lm(lundernourish_3yr ~ aid_ag_pcap_c2018_log + gdp_pcap_c2011_log + sevconflict_pop + droughtfloods_avg_90_09 + peaceyears_4to14 + factor(year) - 1, data=data)
aidundpy2_fey1 <- lm(lundernourish_3yr ~ aid_ag_pcap_c2018_log + gdp_pcap_c2011_log + sevconflict_pop + droughtfloods_avg_90_09 + peaceyears_15plus + factor(year) - 1, data=data)
aidundpyall_fey1 <- lm(lundernourish_3yr ~ aid_ag_pcap_c2018_log + gdp_pcap_c2011_log + sevconflict_pop + droughtfloods_avg_90_09 + peaceyears_cat + factor(year) - 1, data=data)

# tables
stargazer(aidundc_fey1, aidundpy0_fey1, aidundpy1_fey1, aidundpy2_fey1, type = "text", title = "Table", digits=2)


# 2 + 3 YEAR LEADS, COUNTRY FIXED EFFECTS

# bivariate
aidund_fe2 <- lm(l2undernourish_3yr ~ aid_ag_pcap_c2018_log + factor(country) - 1, data=data)
aidund_fe3 <- lm(l3undernourish_3yr ~ aid_ag_pcap_c2018_log + factor(country) - 1, data=data)

# with controls
aidundc_fe2 <- lm(l2undernourish_3yr ~ aid_ag_pcap_c2018_log + gdp_pcap_c2011_log + sevconflict + factor(country) - 1, data=data)
aidundc_fe3 <- lm(l3undernourish_3yr ~ aid_ag_pcap_c2018_log + gdp_pcap_c2011_log + sevconflict + factor(country) - 1, data=data)

# adding peace years
aidundpy0_fe2 <- lm(l2undernourish_3yr ~ aid_ag_pcap_c2018_log + gdp_pcap_c2011_log + sevconflict_pop + droughtfloods_avg_90_09 + peaceyears_0to3 + factor(country) - 1, data=data)
aidundpy0_fe3 <- lm(l3undernourish_3yr ~ aid_ag_pcap_c2018_log + gdp_pcap_c2011_log + sevconflict_pop + droughtfloods_avg_90_09 + peaceyears_0to3 + factor(country) - 1, data=data)
aidundpy1_fe2 <- lm(l2undernourish_3yr ~ aid_ag_pcap_c2018_log + gdp_pcap_c2011_log + sevconflict_pop + droughtfloods_avg_90_09 + peaceyears_4to14 + factor(country) - 1, data=data)
aidundpy1_fe3 <- lm(l3undernourish_3yr ~ aid_ag_pcap_c2018_log + gdp_pcap_c2011_log + sevconflict_pop + droughtfloods_avg_90_09 + peaceyears_4to14 + factor(country) - 1, data=data)
aidundpy2_fe2 <- lm(l2undernourish_3yr ~ aid_ag_pcap_c2018_log + gdp_pcap_c2011_log + sevconflict_pop + droughtfloods_avg_90_09 + peaceyears_15plus + factor(country) - 1, data=data)
aidundpy2_fe3 <- lm(l3undernourish_3yr ~ aid_ag_pcap_c2018_log + gdp_pcap_c2011_log + sevconflict_pop + droughtfloods_avg_90_09 + peaceyears_15plus + factor(country) - 1, data=data)
aidundpyall_fe2 <- lm(l2undernourish_3yr ~ aid_ag_pcap_c2018_log + gdp_pcap_c2011_log + sevconflict_pop + droughtfloods_avg_90_09 + peaceyears_cat + factor(country) - 1, data=data)
aidundpyall_fe3 <- lm(l3undernourish_3yr ~ aid_ag_pcap_c2018_log + gdp_pcap_c2011_log + sevconflict_pop + droughtfloods_avg_90_09 + peaceyears_cat + factor(country) - 1, data=data)

# tables
stargazer(aidundc_fe1, aidundc_fe2, aidundc_fe3, type = "text", title = "Table", digits=2)
stargazer(aidundpy0_fe1, aidundpy0_fe2, aidundpy0_fe3, type = "text", title = "Table", digits=2)
stargazer(aidundpy1_fe1, aidundpy1_fe2, aidundpy1_fe3, type = "text", title = "Table", digits=2)
stargazer(aidundpy2_fe1, aidundpy2_fe2, aidundpy2_fe3, type = "text", title = "Table", digits=2)



### ERROR TERM CALCULATION

# root mean square error
sqrt(mean(aidundc_fe1$residuals^2))

# The standard regression assumptions include the following about residuals/errors:
# The error has a normal distribution (normality assumption).
# The errors have mean zero.
# The errors have same but unknown variance (homoscedasticity assumption).
# The error are independent of each other (independent errors assumption).

library(olsrr)

# detects violation of normality assumption
ols_plot_resid_qq(aidundc_fe1) # qq plot
ols_test_normality(aidundc_fe1) # P VALUE IS 0, THAT PROBABLY SUCKS + SOME STATS ARE HUGE
ols_test_correlation(aidundc_fe1)


# Residual vs Fitted Values Plot
# scatter plot of residuals on the y axis and fitted values on the x axis to detect non-linearity, unequal error variances, and outliers.

# Characteristics of a well behaved residual vs fitted plot:
    
# The residuals spread randomly around the 0 line indicating that the relationship is linear.
# The residuals form an approximate horizontal band around the 0 line indicating homogeneity of error variance.
# No one residual is visibly away from the random pattern of the residuals indicating that there are no outliers.

ols_plot_resid_fit(aidundc_fe1) # there are outliers. what to do?
ols_plot_resid_hist(aidundc_fe1) # looks good!



### PLOTS


ggplot(data) +
    aes(x = "", y = aid_ag_pcap) +
    geom_boxplot(fill = "#0c4c8a") +
    theme_minimal()


plot_aiddes <- ggplot(data, 
                 aes(x = aid_ag_pcap, y = lundernourish_3yr, colour = peaceyears_cat)
) +
    geom_point() +
    xlim(0, 80) +
    ylim(50,160) +
    geom_smooth(method=lm)

plot_aiddes

# using log variable (note: excludes outliers)

plot_aiddeslog <- ggplot(data, 
                 aes(x = aid_ag_pcap_c2018_log, y = lundernourish_3yr)
) +
    geom_point() +
    xlim(1, 5) +
    ylim(60,160) +
    geom_smooth(method=lm)

plot_aiddeslog

# using log variable + peace years categories

plot_aiddespy <- ggplot(data, 
                   aes(x = aid_ag_pcap_c2018_log, y = lundernourish_3yr, colour = peaceyears_cat)
) +
    geom_point() +
    xlim(1, 5) +
    ylim(60,160) +
    geom_smooth(method=lm)

plot_aiddespy




### ROBUSTNESS CHECKS

# undernourishment
# non-ag aid

aidund <- ggplot(datato2019, 
                 aes(x = aid_ag_pcap, y = lundernourish_3yr, colour = rspol_stability)
) +
    geom_point() +
    xlim(0, 80) +
    geom_smooth(method=lm)

aidund


aidundpy <- ggplot(datato2017, 
                   aes(x = aid_ag_pcap, y = lundernourish_3yr, colour = peaceyears)
) +
    geom_point() +
    xlim(0, 80) +
    geom_smooth(method=lm)

aidundpy

