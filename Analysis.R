rm(list=ls())
setwd("~/Master-Thesis")
csv <- read.csv("thesis_dataset_final.csv")

library(dplyr)
library(tidyr)
library(stargazer)


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
        sevconflict = case_when(battle_deaths>=1000 ~ 1, TRUE ~ 0),
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


library(stringr)
# data$peaceyears_cat <- str_replace(data$peaceyears_cat, '1', 'conflict')
# data$peaceyears_cat <- str_replace(data$peaceyears_cat, '2', 'postconflict')
# data$peaceyears_cat <- str_replace(data$peaceyears_cat, '3', 'stable')
# data$peaceyears_cat_min <- str_replace(data$peaceyears_cat_min, '1', 'conflict')
# data$peaceyears_cat_min <- str_replace(data$peaceyears_cat_min, '2', 'postconflict')
# data$peaceyears_cat_min <- str_replace(data$peaceyears_cat_min, '3', 'stable')

# converting 'less than' values to actual values - clarify in research output
data$undernourish_3yr <- str_replace(data$undernourish_3yr, '<', '')
data$lundernourish_3yr <- str_replace(data$lundernourish_3yr, '<', '')
data$l2undernourish_3yr <- str_replace(data$l2undernourish_3yr, '<', '')
data$l3undernourish_3yr <- str_replace(data$l3undernourish_3yr, '<', '')


# require(foreign)
# write.csv(data, "tanushree_thesisdata.csv")
# write.dta(data, "tanushree_thesisdata.dta")
# save(data, file = "data.RData")



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
# unique(data$country)
# 
# data %>%
#     count(peaceyears_0to3, peaceyears_4to14, peaceyears_15plus)
# 
# unique(peaceyears_0to3$country) - # countries that experienced active conflict / less than 2 years postconflict during the time period
# unique(peaceyears_4to14$country) - # countries that experienced 3-15 peace years during the time
# unique(peaceyears_15plus$country) - # countries that experienced 16+ peace years for the entirety of the period
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
    select(aid_ag_pcap_c2018_log, aid_nonag_pcap_c2018_log, ldiet_energysup_3yr, lundernourish_3yr, peaceyears_0to3, peaceyears_4to14, gdp_pcap_c2011_log, sevconflict, fragile_state, droughtfloods_avg_90_09)

corrdata <- cor(corrdata, use ="complete.obs", method = "pearson")
colnames(corrdata) <- c("aid (agricultural) per capita", "aid (non-agricultural) per capita", "dietary energy supply", "undernourishment", "conflict", "post-conflict", "GDP per capita", "severe conflict", "fragile state", "natural disasters")
rownames(corrdata) <- c("aid (agricultural) per capita", "aid (non-agricultural) per capita", "dietary energy supply", "undernourishment", "conflict", "post-conflict", "GDP per capita", "severe conflict", "fragile state", "natural disasters")


 stargazer(corrdata, type = "text", title = "Correlation Matrix", digits=2, out="correlation.html")

library(corrplot)
library(RColorBrewer)
corr_plot <- corrplot(corrdata, type = "upper",
         tl.col = "black", tl.srt = 45, tl.cex=1,
         col = brewer.pal(n = 8, name = "RdYlBu"))



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

# ggsave("density_non-logged.png")

ggplot(
    data,
    aes(aid_ag_pcap_c2018_log)
    ) +
    geom_density(fill="lavender") +
    xlab("Agricultural aid per capita (value, log-transformed)") +
    ylab("Density") +
    theme_light()

# ggsave("density_logged.png")


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
    theme_minimal() +
    xlab("Prevalence of Undernourishment (%)") +
    ylab("Values")

# ggsave("Visualisations/boxplot_fs.png")

ggplot(data) +
    aes(x = "", y = aid_ag_pcap_c2018_log) +
    geom_boxplot(fill = "lavender") +
    theme_minimal() +
    xlab("Agricultural Aid Per Capita (log-transformed)") +
    ylab("Values")

# ggsave("Visualisations/boxplot_aid.png")




### HAUSMAN TEST (OLS/FE)

# test for model misspecification
# null hypothesis - preferred model is random effects
# alternate hypothesis is that the model is fixed effects
# the tests looks to see if there is a correlation between the unique errors and the regressors in the model
# null hypothesis is that there is no correlation between the two
# interpretation: if the p-value is small (less than 0.05), reject the null hypothesis (and choose fixed effects)

library(plm)



#step 1 : Estimate the FE model
fe <- plm(lundernourish_3yr ~ aid_ag_pcap_c2018_log + sevconflict + gdp_pcap_c2011_log + droughtfloods_avg_90_09 + peaceyears_0to3 + peaceyears_4to14, data=data,model="within")
summary(fe)
#step 2 : Estimate the RE model
re <- pggls(lundernourish_3yr ~ aid_ag_pcap_c2018_log + sevconflict + gdp_pcap_c2011_log + droughtfloods_avg_90_09 + peaceyears_0to3 + peaceyears_4to14, data=data,model="random")
summary(re)
#step 3 : Run Hausman test
phtest(fe, re)

# if I include gdp, fixed effects; otherwise, OLS



### LINEAR MODELS


# OLS, BIVARIATE
M1 <- lm(lundernourish_3yr ~ aid_ag_pcap_c2018_log, data=data) #OLS
M2 <- lm(l3undernourish_3yr ~ aid_ag_pcap_c2018_log, data=data) #OLS


# FIXED EFFECTS (country, unless otherwise specified)

# bivariate - country fixed
M3 <- lm(lundernourish_3yr ~ aid_ag_pcap_c2018_log + factor(country) - 1, data=data)
M4 <- lm(l3undernourish_3yr ~ aid_ag_pcap_c2018_log + factor(country) - 1, data=data)

# bivariate - year fixed
M5 <- lm(lundernourish_3yr ~ aid_ag_pcap_c2018_log + factor(year) - 1, data=data) 
M6 <- lm(l3undernourish_3yr ~ aid_ag_pcap_c2018_log + factor(year) - 1, data=data)

# with controls
M7 <- lm(lundernourish_3yr ~ aid_ag_pcap_c2018_log + gdp_pcap_c2011_log + sevconflict + droughtfloods_avg_90_09 + factor(country) - 1, data=data)
M8 <- lm(l3undernourish_3yr ~ aid_ag_pcap_c2018_log + gdp_pcap_c2011_log + sevconflict + droughtfloods_avg_90_09 + factor(country) - 1, data=data)
# M7a <- lm(lundernourish_3yr ~ aid_ag_pcap_c2018_log  + gdp_pcap_c2011_log + sevconflict + droughtfloods_avg_90_09 + factor(year) - 1, data=data) 
# M8a <- lm(l3undernourish_3yr ~ aid_ag_pcap_c2018_log +  + gdp_pcap_c2011_log + sevconflict + droughtfloods_avg_90_09 + factor(year) - 1, data=data)

# stargazer(M7, M8, M7a, M8a, type="text")

# adding conflict/post-conflict controls
M9 <- lm(lundernourish_3yr ~ aid_ag_pcap_c2018_log + gdp_pcap_c2011_log + sevconflict + droughtfloods_avg_90_09 + peaceyears_0to3 + peaceyears_4to14 + factor(country) - 1, data=data)
M10 <- lm(l3undernourish_3yr ~ aid_ag_pcap_c2018_log + gdp_pcap_c2011_log + sevconflict + droughtfloods_avg_90_09 + peaceyears_0to3 + peaceyears_4to14 + factor(country) - 1, data=data)

# adding interaction terms
M11 <- lm(lundernourish_3yr ~ aid_ag_pcap_c2018_log + gdp_pcap_c2011_log + sevconflict + droughtfloods_avg_90_09 + peaceyears_0to3 + peaceyears_4to14 + aid_ag_pcap_c2018_log*peaceyears_0to3 + factor(country) - 1, data=data)
M12 <- lm(l3undernourish_3yr ~ aid_ag_pcap_c2018_log + gdp_pcap_c2011_log + sevconflict + droughtfloods_avg_90_09 + peaceyears_0to3 + peaceyears_4to14 + aid_ag_pcap_c2018_log*peaceyears_0to3 + factor(country) - 1, data=data)
M13 <- lm(lundernourish_3yr ~ aid_ag_pcap_c2018_log + gdp_pcap_c2011_log + sevconflict + droughtfloods_avg_90_09 + peaceyears_0to3 + peaceyears_4to14 + aid_ag_pcap_c2018_log*peaceyears_4to14 + factor(country) - 1, data=data)
M14 <- lm(l3undernourish_3yr ~ aid_ag_pcap_c2018_log + gdp_pcap_c2011_log + sevconflict + droughtfloods_avg_90_09 + peaceyears_0to3 + peaceyears_4to14 + aid_ag_pcap_c2018_log*peaceyears_4to14 + factor(country) - 1, data=data)


# tables

stargazer(M1, M2, M3, M4, M5, M6,
          type = "text", 
          title = "Table", 
          digits=2,
          model.numbers=FALSE,
          no.space=TRUE,
          covariate.labels = c("Ag. aid per capita"),
          dep.var.caption =c("Prevalence of undernourishment"),
          column.labels =c("M1", "M2", "M3", "M4", "M5", "M6"),
          dep.var.labels=c("+1", "+3", "+1", "+3", "+1", "+3"),
          out="Bivariate - OLS vs FE.html"
)


stargazer(M7, M8, M9, M10, M11, M12, M13, M14,
          type = "text", 
          title = "Table", 
          digits=2,
          model.numbers=FALSE,
          no.space = TRUE,
          covariate.labels = c("Ag. aid per capita", "GDP per capita", "Severe conflict", "Natural disasters", "Conflict", "Post-Conflict"),
          dep.var.caption =c("Prevalence of undernourishment"),
          column.labels =c("M7", "M8", "M9", "M10", "M11", "M12", "M13", "M14"),
          dep.var.labels=c("+1", "+3", "+1", "+3", "+1", "+3", "+1", "+3"),
          out="Multivariate - Controls and Interaction Models.html"
          )



library("sjPlot")
library("sjmisc")

data(data)

plot_model(M11, 
           type = "int", 
           terms = c("aid_ag_pcap_c2018_log", "peaceyears_0to3")
           ) +
    labs(title="Predicted Values for the Prevalence of Undernourishment") +
    xlab("Agricultural Aid Per Capita (log-transformed)") +
    ylab("Prevalence of Undernourishment") +
    theme_sjplot() +
    scale_color_brewer(palette = "Paired", name = "Conflict Phase",
                       labels = c("No Conflict", "Conflict")) +
    scale_fill_brewer(palette = "Paired")

ggsave("interaction plot M11.png")


plot_model(M13, 
           type = "int", 
           terms = c("aid_ag_pcap_c2018_log", "peaceyears_0to3")
) +
    labs(title="Predicted Values for the Prevalence of Undernourishment") +
    xlab("Agricultural Aid Per Capita (log-transformed)") +
    ylab("Prevalence of Undernourishment") +
    theme_sjplot() +
    scale_color_brewer(palette = "Paired", name = "Conflict Phase",
                       labels = c("Not Post-Conflict", "Post-Conflict")) +
    scale_fill_brewer(palette = "Paired")

ggsave("interaction plot M13.png")


# EXCLUDING CONFLICT COUNTRIES

datanoc <- data %>%
    filter(peaceyears_cat!="conflict")

class(data$peaceyears_4to14)

M15 <- lm(lundernourish_3yr ~ aid_ag_pcap_c2018_log + factor(country) - 1, data=datanoc)
M16 <- lm(l3undernourish_3yr ~ aid_ag_pcap_c2018_log + factor(country) - 1, data=datanoc)
M17 <- lm(lundernourish_3yr ~ aid_ag_pcap_c2018_log + gdp_pcap_c2011_log + sevconflict + droughtfloods_avg_90_09 + factor(country) - 1, data=datanoc)
M18 <- lm(l3undernourish_3yr ~ aid_ag_pcap_c2018_log + gdp_pcap_c2011_log + sevconflict + droughtfloods_avg_90_09 + factor(country) - 1, data=datanoc)
M19 <- lm(lundernourish_3yr ~ aid_ag_pcap_c2018_log + gdp_pcap_c2011_log + sevconflict + droughtfloods_avg_90_09 + peaceyears_4to14 + factor(country) - 1, data=datanoc)
M20 <- lm(l3undernourish_3yr ~ aid_ag_pcap_c2018_log + gdp_pcap_c2011_log + sevconflict + droughtfloods_avg_90_09 + peaceyears_4to14 + factor(country) - 1, data=datanoc)
M21 <- lm(lundernourish_3yr ~ aid_ag_pcap_c2018_log + gdp_pcap_c2011_log + sevconflict + droughtfloods_avg_90_09 + peaceyears_4to14 + aid_ag_pcap_c2018_log*peaceyears_4to14 + factor(country) - 1, data=datanoc)
M22 <- lm(l3undernourish_3yr ~ aid_ag_pcap_c2018_log + gdp_pcap_c2011_log + sevconflict + droughtfloods_avg_90_09 + peaceyears_4to14 + aid_ag_pcap_c2018_log*peaceyears_4to14 + factor(country) - 1, data=datanoc)

stargazer(M15, M16, M17, M18, M19, M20, M21, M22,
          type = "text", 
          title = "Table", 
          digits=2,
          model.numbers=FALSE,
          no.space = TRUE,
          covariate.labels = c("Ag. aid per capita", "GDP per capita", "Severe conflict", "Natural disasters", "Post-conflict"),
          dep.var.caption =c("Prevalence of undernourishment"),
          column.labels =c("M15", "M16", "M17", "M18", "M19", "M20", "M21", "M22"),
          dep.var.labels=c("+1", "+3", "+1", "+3", "+1", "+3", "+1", "+3"),
          out="Multivariate - excluding conflict - Controls and Interaction Models.html"
          )


plot_model(M22, 
           type = "int", 
           terms = c("aid_ag_pcap_c2018_log", "peaceyears_0to3")
) +
    labs(title="Predicted Values for the Prevalence of Undernourishment") +
    xlab("Agricultural Aid Per Capita (log-transformed)") +
    ylab("Prevalence of Undernourishment") +
    theme_sjplot() +
    scale_color_brewer(palette = "Paired", name = "Conflict Phase",
                       labels = c("Not Post-Conflict", "Post-Conflict")) +
    scale_fill_brewer(palette = "Paired")


plot_model(M22) +
    theme_sjplot(base_size = 7, base_family = "")

plot_model(M21, type = "int", terms = c("aid_ag_pcap_c2018_log", "peaceyears_4to14"))
plot_model(M22, type = "int", terms = c("aid_ag_pcap_c2018_log", "peaceyears_4to14"))


anova(M21, M19)




### ERROR TERM CALCULATION

# root mean square error
sqrt(mean(M9$residuals^2))
sqrt(mean(M10$residuals^2))

# The standard regression assumptions include the following about residuals/errors:
# The error has a normal distribution (normality assumption).
# The errors have mean zero.
# The errors have same but unknown variance (homoscedasticity assumption).
# The error are independent of each other (independent errors assumption).

library(olsrr)

# detects violation of normality assumption
ols_plot_resid_qq(M9) # qq plot
ols_test_normality(M9)
ols_test_correlation(M9)

ols_plot_resid_qq(M10) # qq plot
ols_test_normality(M10)
ols_test_correlation(M10)


# Residual vs Fitted Values Plot
# scatter plot of residuals on the y axis and fitted values on the x axis to detect non-linearity, unequal error variances, and outliers.

# Characteristics of a well behaved residual vs fitted plot:
    
# The residuals spread randomly around the 0 line indicating that the relationship is linear.
# The residuals form an approximate horizontal band around the 0 line indicating homogeneity of error variance.
# No one residual is visibly away from the random pattern of the residuals indicating that there are no outliers.

M9_residplot <- ols_plot_resid_fit(M9)
M9_residhist <- ols_plot_resid_hist(M9)

M10_residplot <- ols_plot_resid_fit(M10)
M10_residhist <- ols_plot_resid_hist(M10)

M3_residplot <- ols_plot_resid_fit(M3)
M3_residhist <- ols_plot_resid_hist(M3)

M5_residplot <- ols_plot_resid_fit(M5)
M5_residhist <- ols_plot_resid_hist(M5)


### ROBUSTNESS CHECKS

# dietary energy supply

# with controls
M7r <- lm(ldiet_energysup_3yr ~ aid_ag_pcap_c2018_log + gdp_pcap_c2011_log + sevconflict + droughtfloods_avg_90_09 + factor(country) - 1, data=data)
M8r <- lm(l3diet_energysup_3yr ~ aid_ag_pcap_c2018_log + gdp_pcap_c2011_log + sevconflict + droughtfloods_avg_90_09 + factor(country) - 1, data=data)

# stargazer(M7, M8, M7a, M8a, type="text")

# adding conflict/post-conflict controls
M9r <- lm(ldiet_energysup_3yr ~ aid_ag_pcap_c2018_log + gdp_pcap_c2011_log + sevconflict + droughtfloods_avg_90_09 + peaceyears_0to3 + peaceyears_4to14 + factor(country) - 1, data=data)
M10r <- lm(l3diet_energysup_3yr ~ aid_ag_pcap_c2018_log + gdp_pcap_c2011_log + sevconflict + droughtfloods_avg_90_09 + peaceyears_0to3 + peaceyears_4to14 + factor(country) - 1, data=data)

# adding interaction terms
M11r <- lm(ldiet_energysup_3yr ~ aid_ag_pcap_c2018_log + gdp_pcap_c2011_log + sevconflict + droughtfloods_avg_90_09 + peaceyears_0to3 + peaceyears_4to14 + aid_ag_pcap_c2018_log*peaceyears_0to3 + factor(country) - 1, data=data)
M12r <- lm(l3diet_energysup_3yr ~ aid_ag_pcap_c2018_log + gdp_pcap_c2011_log + sevconflict + droughtfloods_avg_90_09 + peaceyears_0to3 + peaceyears_4to14 + aid_ag_pcap_c2018_log*peaceyears_0to3 + factor(country) - 1, data=data)
M13r <- lm(ldiet_energysup_3yr ~ aid_ag_pcap_c2018_log + gdp_pcap_c2011_log + sevconflict + droughtfloods_avg_90_09 + peaceyears_0to3 + peaceyears_4to14 + aid_ag_pcap_c2018_log*peaceyears_4to14 + factor(country) - 1, data=data)
M14r <- lm(l3diet_energysup_3yr ~ aid_ag_pcap_c2018_log + gdp_pcap_c2011_log + sevconflict + droughtfloods_avg_90_09 + peaceyears_0to3 + peaceyears_4to14 + aid_ag_pcap_c2018_log*peaceyears_4to14 + factor(country) - 1, data=data)


# tables

stargazer(M7r, M8r, M9r, M10r, M11r, M12r, M13r, M14r,
          type = "text", 
          title = "Table", 
          digits=2,
          model.numbers=FALSE,
          no.space = TRUE,
          covariate.labels = c("Ag. aid per capita", "GDP per capita", "Severe conflict", "Natural disasters", "Conflict", "Post-Conflict"),
          dep.var.caption =c("Dietary energy supply"),
          column.labels =c("M7", "M8", "M9", "M10", "M11", "M12", "M13", "M14"),
          dep.var.labels=c("+1", "+3", "+1", "+3", "+1", "+3", "+1", "+3"),
          out="Robustness - Multivariate - Controls and Interaction Models.html"
)


plot_model(M11r, 
           type = "int", 
           terms = c("aid_ag_pcap_c2018_log", "peaceyears_0to3")
) +
    labs(title="Predicted Values for Dietary Energy Supply") +
    xlab("Agricultural Aid Per Capita (log-transformed)") +
    ylab("Dietary Energy Supply (% of MDER)") +
    theme_sjplot() +
    scale_color_brewer(palette = "Paired", name = "Conflict Phase",
                       labels = c("No Conflict", "Conflict")) +
    scale_fill_brewer(palette = "Paired")

ggsave("interaction plot M11 robustness.png")


plot_model(M13r, 
           type = "int", 
           terms = c("aid_ag_pcap_c2018_log", "peaceyears_0to3")
) +
    labs(title="Predicted Values for Dietary Energy Supply") +
    xlab("Agricultural Aid Per Capita (log-transformed)") +
    ylab("Dietary Energy Supply (% of MDER)") +
    theme_sjplot() +
    scale_color_brewer(palette = "Paired", name = "Conflict Phase",
                       labels = c("Not Post-Conflict", "Post-Conflict")) +
    scale_fill_brewer(palette = "Paired")

ggsave("interaction plot M13 robustness.png")


# EXCLUDING CONFLICT COUNTRIES

M15r <- lm(ldiet_energysup_3yr ~ aid_ag_pcap_c2018_log + factor(country) - 1, data=datanoc)
M16r <- lm(l3diet_energysup_3yr ~ aid_ag_pcap_c2018_log + factor(country) - 1, data=datanoc)
M17r <- lm(ldiet_energysup_3yr ~ aid_ag_pcap_c2018_log + gdp_pcap_c2011_log + sevconflict + droughtfloods_avg_90_09 + factor(country) - 1, data=datanoc)
M18r <- lm(l3diet_energysup_3yr ~ aid_ag_pcap_c2018_log + gdp_pcap_c2011_log + sevconflict + droughtfloods_avg_90_09 + factor(country) - 1, data=datanoc)
M19r <- lm(ldiet_energysup_3yr ~ aid_ag_pcap_c2018_log + gdp_pcap_c2011_log + sevconflict + droughtfloods_avg_90_09 + peaceyears_4to14 + factor(country) - 1, data=datanoc)
M20r <- lm(l3diet_energysup_3yr ~ aid_ag_pcap_c2018_log + gdp_pcap_c2011_log + sevconflict + droughtfloods_avg_90_09 + peaceyears_4to14 + factor(country) - 1, data=datanoc)
M21r <- lm(ldiet_energysup_3yr ~ aid_ag_pcap_c2018_log + gdp_pcap_c2011_log + sevconflict + droughtfloods_avg_90_09 + peaceyears_4to14 + aid_ag_pcap_c2018_log*peaceyears_4to14 + factor(country) - 1, data=datanoc)
M22r <- lm(l3diet_energysup_3yr ~ aid_ag_pcap_c2018_log + gdp_pcap_c2011_log + sevconflict + droughtfloods_avg_90_09 + peaceyears_4to14 + aid_ag_pcap_c2018_log*peaceyears_4to14 + factor(country) - 1, data=datanoc)

stargazer(M15r, M16r, M17r, M18r, M19r, M20r, M21r, M22r,
          type = "text", 
          title = "Table", 
          digits=2,
          model.numbers=FALSE,
          no.space = TRUE,
          covariate.labels = c("Ag. aid per capita", "GDP per capita", "Severe conflict", "Natural disasters", "Post-conflict"),
          dep.var.caption =c("Dietary energy supply"),
          column.labels =c("M15", "M16", "M17", "M18", "M19", "M20", "M21", "M22"),
          dep.var.labels=c("+1", "+3", "+1", "+3", "+1", "+3", "+1", "+3"),
          out="Robustness - Multivariate - excluding conflict - Controls and Interaction Models.html"
)


plot_model(M22r, 
           type = "int", 
           terms = c("aid_ag_pcap_c2018_log", "peaceyears_0to3")
) +
    labs(title="Predicted Values for Dietary Energy Supply") +
    xlab("Agricultural Aid Per Capita (log-transformed)") +
    ylab("Dietary Energy Supply") +
    theme_sjplot() +
    scale_color_brewer(palette = "Paired", name = "Conflict Phase",
                       labels = c("Not Post-Conflict", "Post-Conflict")) +
    scale_fill_brewer(palette = "Paired")

ggsave("interaction plot M22 robustness.png")



## robustness with aid

# with controls
M7rr <- lm(lundernourish_3yr ~ aid_nonag_pcap_c2018_log + gdp_pcap_c2011_log + sevconflict + droughtfloods_avg_90_09 + factor(country) - 1, data=data)
M8rr <- lm(l3undernourish_3yr ~ aid_nonag_pcap_c2018_log + gdp_pcap_c2011_log + sevconflict + droughtfloods_avg_90_09 + factor(country) - 1, data=data)

# stargazer(M7, M8, M7a, M8a, type="text")

# adding conflict/post-conflict controls
M9rr <- lm(lundernourish_3yr ~ aid_nonag_pcap_c2018_log + gdp_pcap_c2011_log + sevconflict + droughtfloods_avg_90_09 + peaceyears_0to3 + peaceyears_4to14 + factor(country) - 1, data=data)
M10rr <- lm(l3undernourish_3yr ~ aid_nonag_pcap_c2018_log + gdp_pcap_c2011_log + sevconflict + droughtfloods_avg_90_09 + peaceyears_0to3 + peaceyears_4to14 + factor(country) - 1, data=data)

# adding interaction terms
M11rr <- lm(lundernourish_3yr ~ aid_nonag_pcap_c2018_log + gdp_pcap_c2011_log + sevconflict + droughtfloods_avg_90_09 + peaceyears_0to3 + peaceyears_4to14 + aid_nonag_pcap_c2018_log*peaceyears_0to3 + factor(country) - 1, data=data)
M12rr <- lm(l3undernourish_3yr ~ aid_nonag_pcap_c2018_log + gdp_pcap_c2011_log + sevconflict + droughtfloods_avg_90_09 + peaceyears_0to3 + peaceyears_4to14 + aid_nonag_pcap_c2018_log*peaceyears_0to3 + factor(country) - 1, data=data)
M13rr <- lm(lundernourish_3yr ~ aid_nonag_pcap_c2018_log + gdp_pcap_c2011_log + sevconflict + droughtfloods_avg_90_09 + peaceyears_0to3 + peaceyears_4to14 + aid_nonag_pcap_c2018_log*peaceyears_4to14 + factor(country) - 1, data=data)
M14rr <- lm(l3undernourish_3yr ~ aid_nonag_pcap_c2018_log + gdp_pcap_c2011_log + sevconflict + droughtfloods_avg_90_09 + peaceyears_0to3 + peaceyears_4to14 + aid_nonag_pcap_c2018_log*peaceyears_4to14 + factor(country) - 1, data=data)


# tables

stargazer(M7rr, M8rr, M9rr, M10rr, M11rr, M12rr, M13rr, M14rr,
          type = "text", 
          title = "Table", 
          digits=2,
          model.numbers=FALSE,
          no.space = TRUE,
          covariate.labels = c("Non-ag. aid per capita", "GDP per capita", "Severe conflict", "Natural disasters", "Conflict", "Post-Conflict"),
          dep.var.caption =c("Prevalence of undernourishment"),
          column.labels =c("M7", "M8", "M9", "M10", "M11", "M12", "M13", "M14"),
          dep.var.labels=c("+1", "+3", "+1", "+3", "+1", "+3", "+1", "+3"),
          out="Robustness aid - Multivariate - Controls and Interaction Models.html"
)



plot_model(M11rr, 
           type = "int", 
           terms = c("aid_nonag_pcap_c2018_log", "peaceyears_0to3")
) +
    labs(title="Predicted Values for the Prevalence of Undernourishment") +
    xlab("Non-agricultural Aid Per Capita (log-transformed)") +
    ylab("Prevalence of Undernourishment") +
    theme_sjplot() +
    scale_color_brewer(palette = "Paired", name = "Conflict Phase",
                       labels = c("No Conflict", "Conflict")) +
    scale_fill_brewer(palette = "Paired")

ggsave("interaction plot M11 - robustness aid.png")


plot_model(M13rr, 
           type = "int", 
           terms = c("aid_nonag_pcap_c2018_log", "peaceyears_0to3")
) +
    labs(title="Predicted Values for the Prevalence of Undernourishment") +
    xlab("Non-agricultural Aid Per Capita (log-transformed)") +
    ylab("Prevalence of Undernourishment") +
    theme_sjplot() +
    scale_color_brewer(palette = "Paired", name = "Conflict Phase",
                       labels = c("Not Post-Conflict", "Post-Conflict")) +
    scale_fill_brewer(palette = "Paired")

ggsave("interaction plot M13 - robustness aid.png")


# EXCLUDING CONFLICT COUNTRIES

M15rr <- lm(lundernourish_3yr ~ aid_nonag_pcap_c2018_log + factor(country) - 1, data=datanoc)
M16rr <- lm(l3undernourish_3yr ~ aid_nonag_pcap_c2018_log + factor(country) - 1, data=datanoc)
M17rr <- lm(lundernourish_3yr ~ aid_nonag_pcap_c2018_log + gdp_pcap_c2011_log + sevconflict + droughtfloods_avg_90_09 + factor(country) - 1, data=datanoc)
M18rr <- lm(l3undernourish_3yr ~ aid_nonag_pcap_c2018_log + gdp_pcap_c2011_log + sevconflict + droughtfloods_avg_90_09 + factor(country) - 1, data=datanoc)
M19rr <- lm(lundernourish_3yr ~ aid_nonag_pcap_c2018_log + gdp_pcap_c2011_log + sevconflict + droughtfloods_avg_90_09 + peaceyears_4to14 + factor(country) - 1, data=datanoc)
M20rr <- lm(l3undernourish_3yr ~ aid_nonag_pcap_c2018_log + gdp_pcap_c2011_log + sevconflict + droughtfloods_avg_90_09 + peaceyears_4to14 + factor(country) - 1, data=datanoc)
M21rr <- lm(lundernourish_3yr ~ aid_nonag_pcap_c2018_log + gdp_pcap_c2011_log + sevconflict + droughtfloods_avg_90_09 + peaceyears_4to14 + aid_nonag_pcap_c2018_log*peaceyears_4to14 + factor(country) - 1, data=datanoc)
M22rr <- lm(l3undernourish_3yr ~ aid_nonag_pcap_c2018_log + gdp_pcap_c2011_log + sevconflict + droughtfloods_avg_90_09 + peaceyears_4to14 + aid_nonag_pcap_c2018_log*peaceyears_4to14 + factor(country) - 1, data=datanoc)

stargazer(M15rr, M16rr, M17rr, M18rr, M19rr, M20rr, M21rr, M22rr,
          type = "text", 
          title = "Table", 
          digits=2,
          model.numbers=FALSE,
          no.space = TRUE,
          covariate.labels = c("Non-ag. aid per capita", "GDP per capita", "Severe conflict", "Natural disasters", "Post-conflict"),
          dep.var.caption =c("Prevalence of undernourishment"),
          column.labels =c("M15", "M16", "M17", "M18", "M19", "M20", "M21", "M22"),
          dep.var.labels=c("+1", "+3", "+1", "+3", "+1", "+3", "+1", "+3"),
          out="Robustness aid - Multivariate - excluding conflict - Controls and Interaction Models.html"
)


plot_model(M22, 
           type = "int", 
           terms = c("aid_nonag_pcap_c2018_log", "peaceyears_0to3")
) +
    labs(title="Predicted Values for the Prevalence of Undernourishment") +
    xlab("Non-agricultural Aid Per Capita (log-transformed)") +
    ylab("Prevalence of Undernourishment") +
    theme_sjplot() +
    scale_color_brewer(palette = "Paired", name = "Conflict Phase",
                       labels = c("Not Post-Conflict", "Post-Conflict")) +
    scale_fill_brewer(palette = "Paired")









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

