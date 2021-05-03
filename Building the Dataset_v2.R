rm(list=ls())

library(dplyr)
library(tidyr)
library(reshape2)

# read in files
setwd("C:/Users/tanus/Google Drive/MSSc Peace and Conflict Research/Master Thesis/Data/Building the Dataset")
wdi <- read.csv("WDI/WDI_World Bank.csv", fileEncoding="UTF-8-BOM")

# to add country codes
library(countrycode)
countrycodes <- codelist %>% select(fao, gwn, iso2c, iso3c, wb)


### World Development Indicators

wdi <- wdi %>% 
    rename("1996" = X1996..YR1996.) %>%
    rename("1997" = X1997..YR1997.) %>%
    rename("1998" = X1998..YR1998.) %>%
    rename("1999" = X1999..YR1999.) %>%
    rename("2000" = X2000..YR2000.) %>%
    rename("2001" = X2001..YR2001.) %>%
    rename("2002" = X2002..YR2002.) %>%
    rename("2003" = X2003..YR2003.) %>%
    rename("2004" = X2004..YR2004.) %>%
    rename("2005" = X2005..YR2005.) %>%
    rename("2006" = X2006..YR2006.) %>%
    rename("2007" = X2007..YR2007.) %>%
    rename("2008" = X2008..YR2008.) %>%
    rename("2009" = X2009..YR2009.) %>%
    rename("2010" = X2010..YR2010.) %>%
    rename("2011" = X2011..YR2011.) %>%
    rename("2012" = X2012..YR2012.) %>%
    rename("2013" = X2013..YR2013.) %>%
    rename("2014" = X2014..YR2014.) %>%
    rename("2015" = X2015..YR2015.) %>%
    rename("2016" = X2016..YR2016.) %>%
    rename("2017" = X2017..YR2017.) %>%
    rename("2018" = X2018..YR2018.) %>%
    rename("2019" = X2019..YR2019.) %>%
    rename("2020" = X2020..YR2020.)

wdi2 <- wdi %>%
    pivot_longer(
        cols = 5:29,
        names_to = "Year",
        values_to = ".value"
    )

wdi2 <- wdi2[wdi2$Country.Name != "", ]
wdi2 <- wdi2[wdi2$Series.Name != "", ]
wdi2$Year <- as.numeric(wdi2$Year)

fun <- function(x) x[!is.na(x)]

wdi3 <- wdi2 %>%
    select(!"Series.Code") %>%
    group_by(Country.Name, Country.Code, Year) %>%
    arrange(Country.Name, Year) %>%
    pivot_wider(
        names_from=Series.Name,
        names_repair = "check_unique",
        values_from=.value,
        values_fill = NULL
        ) %>%
    filter(Year>=2000) %>%
    select(c(1:3,65,66,67,17,87)) %>%
    group_by(Country.Name, Country.Code, Year) %>% 
    summarise_all(fun) %>%
    left_join(countrycodes, by=c("Country.Code"="wb"))


### FOOD SECURITY

faostat <- read.csv("FAOSTAT/FAOSTAT.csv")

library(stringr)

faostat$Year.Code <- str_replace(faostat$Year.Code, '19992001', '2001')
faostat$Year.Code <- str_replace(faostat$Year.Code, '20002002', '2002')
faostat$Year.Code <- str_replace(faostat$Year.Code, '20012003', '2003')
faostat$Year.Code <- str_replace(faostat$Year.Code, '20022004', '2004')
faostat$Year.Code <- str_replace(faostat$Year.Code, '20032005', '2005')
faostat$Year.Code <- str_replace(faostat$Year.Code, '20042006', '2006')
faostat$Year.Code <- str_replace(faostat$Year.Code, '20052007', '2007')
faostat$Year.Code <- str_replace(faostat$Year.Code, '20062008', '2008')
faostat$Year.Code <- str_replace(faostat$Year.Code, '20072009', '2009')
faostat$Year.Code <- str_replace(faostat$Year.Code, '20082010', '2010')
faostat$Year.Code <- str_replace(faostat$Year.Code, '20092011', '2011')
faostat$Year.Code <- str_replace(faostat$Year.Code, '20102012', '2012')
faostat$Year.Code <- str_replace(faostat$Year.Code, '20112013', '2013')
faostat$Year.Code <- str_replace(faostat$Year.Code, '20122014', '2014')
faostat$Year.Code <- str_replace(faostat$Year.Code, '20132015', '2015')
faostat$Year.Code <- str_replace(faostat$Year.Code, '20142016', '2016')
faostat$Year.Code <- str_replace(faostat$Year.Code, '20152017', '2017')
faostat$Year.Code <- str_replace(faostat$Year.Code, '20162018', '2018')
faostat$Year.Code <- str_replace(faostat$Year.Code, '20172019', '2019')
faostat$Year.Code <- as.numeric(faostat$Year.Code)

faostat2 <- faostat %>%
    select(c("Area", "Area.Code", "Item", "Year.Code", "Value")) %>%
    arrange(Area, Year.Code) %>%
    filter() %>%
    pivot_wider(
        names_from=Item,
        values_from=Value,
        values_fill = NULL
        ) %>%
    rename("fao"="Area.Code") %>%
    relocate(fao, .after=last_col()) %>%
    left_join(countrycodes, by="fao") %>%
    select(c(1,2,4,10,16,17,26,27,37))


# OECD AID

# install.packages("OECD")
# library(OECD)
# 
# dataset_list <- get_datasets()
# search_dataset("CRS", data = dataset_list)
# 
# oecdaid <- "CRS1"
# dstruc <- get_data_structure(oecdaid)
# 
# dstruc$VAR_DESC
# dstruc$RECIPIENT
# dstruc$SECTOR
# dstruc$YEAR
# dstruc$AMOUNTTYPE
# dstruc$FLOWTYPE
# 
# oecdyears <- list("2000")
# 
# oecdaid <- get_dataset("CRS1",
#                              filter = "20005.10100+10010+71+86+64+62+30+35+57+45+93+65+66+63+61+88+55+85+89+10001+10002+130+142+133+136+139+189+10003+10050+228+233+274+271+238+248+252+253+257+258+259+266+270+273+279+278+282+285+288+265+1027+10051+225+229+231+232+234+235+245+239+268+1028+10052+227+280+249+275+218+1029+10053+236+287+230+247+240+241+243+244+251+255+256+260+261+276+269+272+283+1030+289+298+10004+10005+10054+376+377+373+328+329+388+386+338+378+340+381+349+354+385+361+382+383+384+375+387+1031+10055+352+336+342+347+351+358+364+366+1032+389+10006+425+428+431+434+437+440+446+451+454+457+460+463+489+10056+331+498+10007+10008+725+728+730+740+735+738+742+745+748+751+753+755+761+732+764+765+769+789+10009+625+610+611+666+630+612+645+613+614+655+635+660+665+640+615+616+617+619+679+689+10011+530+540+543+546+549+552+555+558+561+566+573+576+550+580+589+798+10012+10057+832+850+862+866+854+1033+10058+836+859+860+845+858+861+10059+831+840+856+880+868+870+872+876+1035+889+9998+228+233+274+271+238+252+253+259+266+273+279+278+282+285+288+225+231+232+235+268+249+236+287+240+243+244+251+255+256+260+269+272+283+349+728+745+765+625+666+630+635+660+580+866+854+836+872+265+740+57+93+85+142+136+139+248+229+234+280+230+247+241+261+342+347+351+364+428+738+753+755+769+610+612+645+614+665+640+615+617+549+573+550+862+860+868+71+86+64+65+66+63+55+130+133+257+245+239+227+275+218+276+377+338+378+340+381+354+385+383+384+352+336+358+366+425+431+437+440+446+451+454+457+463+730+751+764+611+613+655+616+540+543+555+832+859+845+861+831+856+880+870+876+62+30+35+45+61+258+270+376+373+328+329+388+386+361+382+375+387+434+460+331+725+735+742+748+761+732+530+546+552+558+561+566+576+850+858+840+88+228+233+271+238+252+253+259+266+273+278+282+285+288+229+231+232+234+235+268+236+287+247+240+241+243+244+251+255+256+260+269+272+283+349+351+364+428+446+625+93+66+228+238+253+266+279+285+288+265+231+232+227+280+249+287+255+260+428+451+745+753+625+610+611+630+613+614+660+615+616+617+233+257+270+268+230+244+376+377+373+328+329+388+386+338+378+340+381+349+354+385+361+382+383+384+375+387+352+446+457+331+761+765+655+530+832+850+862+866+854+836+859+860+845+858+861+831+840+856+880+870+872+133+228+233+274+271+238+248+252+259+273+279+278+282+285+288+265+225+229+231+232+234+235+245+280+249+287+247+240+243+244+251+255+256+260+261+272+283+349+347+351+364+463+728+740+745+625+666+635+665+615+540+543+573+550+580+862+866+228+271+238+252+253+259+266+273+279+278+285+231+232+235+287+240+243+244+251+255+260+272+283+349+740+625+615+573+580+93+85+130+142+136+139+233+274+248+282+288+265+225+229+234+268+280+249+236+230+247+241+256+261+269+342+351+364+428+728+745+753+755+765+769+666+630+645+614+635+660+665+640+617+550+862+866+854+836+860+71+86+64+57+65+66+63+55+133+245+239+227+275+218+338+378+340+381+354+383+384+352+336+347+358+425+431+437+440+446+451+454+457+463+730+738+751+764+610+611+612+613+655+616+540+543+549+555+832+859+880+870+872+62+30+35+45+61+257+270+377+373+328+329+388+386+382+375+387+366+434+460+331+725+735+742+748+761+732+530+546+552+558+561+566+576+850+845+858+861+840+258+276+376+385+361+831+856+868+876+71+86+93+85+610+611+612+613+614+615+616+617+71+86+93+85+610+611+612+613+614+615+616+617+228+233+274+271+238+248+252+253+257+259+266+270+273+278+282+285+288+265+225+229+231+232+234+235+245+239+268+227+280+249+275+236+287+230+247+240+241+243+244+251+255+256+260+261+269+272+283+377+328+329+378+340+381+349+354+382+383+384+375+352+446+457+832+862+866+854+836+880+870+872+232+287+230+240+244+255+256+260+269.1000+311.100.100.D.112.100", 
#                              pre_formatted = TRUE)
# 
# oecdaid$SECTOR <- str_replace(oecdaid$SECTOR, '311', 'oecd_aid_ag_c2018')
# oecdaid$SECTOR <- str_replace(oecdaid$SECTOR, '1000', 'oecd_aid_total_c2018')
# 
# oecdaid1 <- oecdaid %>%
#     select(2,3,13,14) %>%
#     pivot_wider(
#         names_from = "SECTOR",
#         values_from = "obsValue"
#     ) %>%
#     rename("oecdcode" = "RECIPIENT") %>%
#     rename("year" = "obsTime")
# 
# oecdaid1$oecdcode <- as.numeric(oecdaid1$oecdcode)
# oecdaid1$year <- as.numeric(oecdaid1$year)
# 
# oecdcountries <- read.csv("OECD/DAC-CRS-CODES.csv", fileEncoding="UTF-8-BOM", sep=";")
# 
# oecdaid2 <- oecdaid1 %>%
#     left_join(oecdcountries, by=c("oecdcode"="Recipient.code")) %>%
#     select(!c(5,6,8:12)) %>%
#     drop_na(ISOcode) %>%
#     rename("iso3c"="ISOcode") %>%
#     rename("fragile_state"="FragileState") %>%
#     arrange(oecdcode,year)
# 
# write.csv(oecdaid2, "oecd_aid.csv")

oecd <- read.csv("OECD/oecd_aid.csv",fileEncoding="UTF-8-BOM")
oecd <- oecd %>%
    select(!X)

### EPR

epr <- read.csv("EPR/data.csv",fileEncoding="UTF-8-BOM", sep=";")
epr$countries_gwid <- as.numeric(epr$countries_gwid)

eprfinal <- epr %>%
    filter(year>=2000) %>%
    left_join(countrycodes, by=c("countries_gwid"="gwn")) %>%
    select(c("year", "countryname", "incidence_flag", "peaceyears", "nightlight_total", "iso3c")) %>%
    arrange(countryname, year)


# MERGE allll the things

# join FAOSTAT with WDI

wdi3$Year <- as.numeric(wdi3$Year)
faostat2$Year.Code <- as.numeric(faostat2$Year.Code)
eprfinal$year <- as.numeric(eprfinal$year)


faowdi <- faostat2 %>%
    rename("year"="Year.Code") %>%
    full_join(wdi3, by=c("iso3c", "year"="Year"), na.rm=TRUE) %>%
    relocate("iso3c", .after=last_col()) %>%
    relocate("Country.Name", .before=1) %>%
    select(!c("Country.Code", "Area")) %>%
    arrange("Country.Name", "year") %>%
    unique()

# merge with IATI

oecdfaowdi <- faowdi %>%
    full_join(oecd, by=c("iso3c", "year")) %>%
    relocate(14:18, .after=last_col()) %>%
    relocate(9:12, .after=2)

oecdfaowdi <- rename(oecdfaowdi, country = 1)
oecdfaowdi <- rename(oecdfaowdi, pop_total = 3)
oecdfaowdi <- rename(oecdfaowdi, pop_female = 4)
oecdfaowdi <- rename(oecdfaowdi, pop_male = 5)
oecdfaowdi <- rename(oecdfaowdi, battle_deaths = 6)
oecdfaowdi <- rename(oecdfaowdi, foodsup_var_pcap = 7)
oecdfaowdi <- rename(oecdfaowdi, diet_energysup_3yr = 8)
oecdfaowdi <- rename(oecdfaowdi, gdp_pcap_c2011 = 9)
oecdfaowdi <- rename(oecdfaowdi, undernourish_3yr = 10)
oecdfaowdi <- rename(oecdfaowdi, sev_foodinsec = 11)
oecdfaowdi <- rename(oecdfaowdi, modsev_foodinsec = 12)
oecdfaowdi <- rename(oecdfaowdi, droughtfloods_avg_90_09 = 13)


# merge with EPR

eproecdfaowdi <- oecdfaowdi %>%
    full_join(eprfinal, by=c("iso3c", "year")) %>%
    select(!c("countryname")) %>%
    relocate("iso3c", "iso2c", "fao", "gwn", "oecdcode", .after = last_col()) %>%
    arrange(country, year)


# clean up the data
    
dataset <- eproecdfaowdi %>%
    na_if("..") %>%
    na_if("") %>%
    drop_na(iso3c, oecdcode)

write.csv(dataset, "thesis_dataset_final.csv")
