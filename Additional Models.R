### ADDITIONAL MODELS


# OLS

# bivariate
aiddes_ols2 <- lm(l2undernourish_3yr ~ aid_ag_pcap_c2018_log, data=data)
aiddes_ols2 <- lm(l2undernourish_3yr ~ aid_ag_pcap_c2018_log, data=data)
aiddes_ols3 <- lm(l3undernourish_3yr ~ aid_ag_pcap_c2018_log, data=data)

# with controls - multivariate
aiddesc_ols1 <- lm(lundernourish_3yr ~ aid_ag_pcap_c2018_log + fragile_state + sevconflict, data=data)
aiddesc_ols2 <- lm(l2undernourish_3yr ~ aid_ag_pcap_c2018_log + fragile_state + sevconflict, data=data)
aiddesc_ols3 <- lm(l3undernourish_3yr ~ aid_ag_pcap_c2018_log + gdp_pcap_c2011_log + sevconflict, data=data)

stargazer(aiddes_ols1, aiddesc_ols1, aiddescpy_ols1, type = "text", title = "Table", digits=2)
stargazer(aiddesc_ols1, aiddescpy0_ols1, aiddescpy1_ols1, aiddescpy2_ols1, type = "text", title = "Table", digits=2)
stargazer(aiddescpy_ols1, aiddescpy_ols2, aiddescpy_ols3, type = "text", title = "Table", digits=2)


## adding peace years variables, with controls

# ols, with all peace years variable
aiddescpy_ols1 <- lm(lundernourish_3yr ~ aid_ag_pcap_c2018_log + fragile_state + sevconflict + peaceyears_cat, data=data)
aiddescpy_ols2 <- lm(l2undernourish_3yr ~ aid_ag_pcap_c2018_log + fragile_state + sevconflict + peaceyears_cat, data=data)
aiddescpy_ols3 <- lm(l3undernourish_3yr ~ aid_ag_pcap_c2018_log + fragile_state + sevconflict + peaceyears_cat, data=data)

# ols, stepwise, binary indicators for peace category

# 0-3 peace years
aiddescpy0_ols1 <- lm(lundernourish_3yr ~ aid_ag_pcap_c2018_log + fragile_state + sevconflict + peaceyears_0to3, data=data)
aiddescpy0_ols2 <- lm(l2undernourish_3yr ~ aid_ag_pcap_c2018_log + fragile_state + sevconflict + peaceyears_0to3, data=data)
aiddescpy0_ols3 <- lm(l3undernourish_3yr ~ aid_ag_pcap_c2018_log + fragile_state + sevconflict + peaceyears_0to3, data=data)

# 4 to 15
aiddescpy1_ols1 <- lm(lundernourish_3yr ~ aid_ag_pcap_c2018_log + fragile_state + sevconflict + peaceyears_4to15, data=data)
aiddescpy1_ols2 <- lm(l2undernourish_3yr ~ aid_ag_pcap_c2018_log + fragile_state + sevconflict + peaceyears_4to15, data=data)
aiddescpy1_ols3 <- lm(l3undernourish_3yr ~ aid_ag_pcap_c2018_log + fragile_state + sevconflict + peaceyears_4to15, data=data)

# 16 plus
aiddescpy2_ols1 <- lm(lundernourish_3yr ~ aid_ag_pcap_c2018_log + fragile_state + sevconflict + peaceyears_16plus, data=data)
aiddescpy2_ols2 <- lm(l2undernourish_3yr ~ aid_ag_pcap_c2018_log + fragile_state + sevconflict + peaceyears_16plus, data=data)
aiddescpy2_ols3 <- lm(l3undernourish_3yr ~ aid_ag_pcap_c2018_log + fragile_state + sevconflict + peaceyears_16plus, data=data)


# models



# OLS - excluding conflict


# no controls - bivariate

aiddes_ols_noc1 <- lm(lundernourish_3yr ~ aid_ag_pcap_c2018_log, data=data_noconflict)
aiddes_ols_noc2 <- lm(l2undernourish_3yr ~ aid_ag_pcap_c2018_log, data=data_noconflict)
aiddes_ols_noc3 <- lm(l3undernourish_3yr ~ aid_ag_pcap_c2018_log, data=data_noconflict)


# with controls - multivariate
aiddesc_ols_noc1 <- lm(lundernourish_3yr ~ aid_ag_pcap_c2018_log + gdp_pcap_c2011_log + fragile_state, data=data_noconflict)
aiddesc_ols_noc2 <- lm(l2undernourish_3yr ~ aid_ag_pcap_c2018_log + gdp_pcap_c2011_log + fragile_state, data=data_noconflict)
aiddesc_ols_noc3 <- lm(l3undernourish_3yr ~ aid_ag_pcap_c2018_log + gdp_pcap_c2011_log + fragile_state, data=data_noconflict)


# models

stargazer(aiddes_ols_noc1, aiddesc_ols_noc1, aiddescpy_ols_noc1, type = "text", title = "Table", digits=2)
stargazer(aiddesc_ols_noc1, aiddescpy0_ols_noc1, aiddescpy1_ols_noc1, aiddescpy2_ols_noc1, type = "text", title = "Table", digits=2)
stargazer(aiddescpy_ols_noc1, aiddescpy_ols_noc2, aiddescpy_ols_noc3, type = "text", title = "Table", digits=2)



## adding peace years variables, with controls

# ols, with all peace years variable
aiddescpy_ols_noc1 <- lm(lundernourish_3yr ~ aid_ag_pcap_c2018_log + fragile_state + peaceyears_cat, data=data_noconflict)
aiddescpy_ols_noc2 <- lm(l2undernourish_3yr ~ aid_ag_pcap_c2018_log + fragile_state + peaceyears_cat, data=data_noconflict)
aiddescpy_ols_noc3 <- lm(l3undernourish_3yr ~ aid_ag_pcap_c2018_log + fragile_state + peaceyears_cat, data=data_noconflict)

# ols, stepwise, binary indicators for peace category

# 0-3 peace years
aiddescpy0_ols_noc1 <- lm(lundernourish_3yr ~ aid_ag_pcap_c2018_log + fragile_state + peaceyears_0to3, data=data_noconflict)
aiddescpy0_ols_noc2 <- lm(l2undernourish_3yr ~ aid_ag_pcap_c2018_log + fragile_state + peaceyears_0to3, data=data_noconflict)
aiddescpy0_ols_noc3 <- lm(l3undernourish_3yr ~ aid_ag_pcap_c2018_log + fragile_state + peaceyears_0to3, data=data_noconflict)

# 4 to 15
aiddescpy1_ols_noc1 <- lm(lundernourish_3yr ~ aid_ag_pcap_c2018_log + fragile_state + peaceyears_4to15, data=data_noconflict)
aiddescpy1_ols_noc2 <- lm(l2undernourish_3yr ~ aid_ag_pcap_c2018_log + fragile_state + peaceyears_4to15, data=data_noconflict)
aiddescpy1_ols_noc3 <- lm(l3undernourish_3yr ~ aid_ag_pcap_c2018_log + fragile_state + peaceyears_4to15, data=data_noconflict)

# 16 plus
aiddescpy2_ols_noc1 <- lm(lundernourish_3yr ~ aid_ag_pcap_c2018_log + fragile_state + peaceyears_16plus, data=data_noconflict)
aiddescpy2_ols_noc2 <- lm(l2undernourish_3yr ~ aid_ag_pcap_c2018_log + fragile_state + peaceyears_16plus, data=data_noconflict)
aiddescpy2_ols_noc3 <- lm(l3undernourish_3yr ~ aid_ag_pcap_c2018_log + fragile_state + peaceyears_16plus, data=data_noconflict)
