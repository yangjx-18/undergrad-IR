library(dplyr)
library(tidyr)
library(countrycode)
library(states)
library(lubridate)
library(stargazer)

test1 <- summary(latin_base)
stargazer(latin_base, title = "描述性统计分析", type = "html",
          no.space = TRUE, summary.stat = c('median','mean','sd','min','max'),
          out = "/Users/yangjingxi/Desktop/国关毕设/IR_Dissertation/123.html")



# Latino
## 2020
summary(latin_2020$age)
latin_2020 <- Latinobarometro_2020_Eng %>% 
  select(numinves, numentre, p30st.c, idenpa, edad, sexo, 
         s24.a, s24.a, s16, s15, s30, p4stgbs, p18st) %>% 
  filter(p30st.c >= 1 & p30st.c <=4) %>% 
  mutate(ccode = countrycode(idenpa, "iso3n", "cown"), 
         nation = countrycode(idenpa, "iso3n", "country.name")) %>% 
  rename("age" = "edad", "employ"="s24.a", "edu_year"="s16", "edu_age"="s15",
         "wealth" = "s30", "econ" = "p4stgbs", "left"= "p18st", "on_china" = "p30st.c")

## 2018
summary(Latinobarometro_2018_Esp$P40ST.C)
latin_2018 <- Latinobarometro_2018_Esp %>% 
  select(NUMINVES, IDENPA, NUMENTRE, P40ST.C, EDAD, SEXO, 
         S14A, S14A, S10, S26, P6STGBSC, P22ST) %>% 
  filter(P40ST.C >= 1 & P40ST.C <=4)%>% 
  mutate(ccode = countrycode(IDENPA, "iso3n", "cown"), 
         nation = countrycode(IDENPA, "iso3n", "country.name")) %>% 
  rename("age" = "EDAD", "sexo" ="SEXO", "employ"="S14A", "edu_year"="S10", 
         "wealth" = "S26", "econ" = "P6STGBSC", "left"= "P22ST", "on_china" = "P40ST.C",
         "numinves"="NUMINVES", "numentre"="NUMENTRE", "idenpa"="IDENPA") %>% 
  mutate(edu_age=edu_year) %>% as.data.frame() %>% 
  mutate(numinves=as.numeric(numinves), idenpa=as.numeric(idenpa),
                                                          numentre=as.numeric(numentre), on_china=as.numeric(on_china),
                                                          age=as.numeric(age), sexo=as.numeric(sexo),
                                                          employ=as.numeric(employ), edu_year=as.numeric(edu_year),
                                                          edu_age=as.numeric(edu_age), wealth=as.numeric(wealth),
                                                          econ=as.numeric(econ), left=as.numeric(left))

## 2017
summary(Latinobarometro2017Eng_v20180117$P45ST.C)
latin_2017 <- Latinobarometro2017Eng_v20180117 %>% 
  select(numinves, idenpa, numentre, P45ST.C, edad, sexo, 
         S18.A, S18.A, S14, S13, S22, P4STGBSC, P19STC)%>% 
  filter(P45ST.C >= 1 & P45ST.C <=4)%>% 
  mutate(ccode = countrycode(idenpa, "iso3n", "cown"), 
         nation = countrycode(idenpa, "iso3n", "country.name")) %>% 
  rename("age" = "edad", "employ"="S18.A", "edu_year"="S14", "edu_age"="S13",
         "wealth" = "S22", "econ" = "P4STGBSC", "left"= "P19STC", "on_china" = "P45ST.C")


## 2016
summary(Latinobarometro2016Eng_v20170205$P46STC)
latin_2016 <- Latinobarometro2016Eng_v20170205 %>% 
  select(numinves, idenpa, numentre, P46STC, edad, sexo, 
         S18A, S18A, S13, S12, S22, P4STGBS, P17ST) %>% 
  filter(P46STC >= 1 & P46STC <=4) %>% 
  mutate(ccode = countrycode(idenpa, "iso3n", "cown"), 
         nation = countrycode(idenpa, "iso3n", "country.name")) %>% 
  rename("age" = "edad",  "employ"="S18A", "edu_year"="S13", "edu_age"="S12",
         "wealth" = "S22", "econ" = "P4STGBS", "left"= "P17ST", "on_china" = "P46STC")

## 2015
summary(Latinobarometro_2015_Eng$P35ST.C)
latin_2015 <- Latinobarometro_2015_Eng %>% 
  select(numinves, idenpa, numentre, P35ST.C, S13, S12, 
         S21.A, S21.A, S19, S18, S29, P3STGBS, P27ST) %>% 
  filter(P35ST.C >= 1 & P35ST.C <=4) %>% 
  mutate(ccode = countrycode(idenpa, "iso3n", "cown"), 
         nation = countrycode(idenpa, "iso3n", "country.name"))  %>% 
  rename("age" = "S13", "sexo" ="S12", "employ"="S21.A", "edu_year"="S19", "edu_age"="S18",
         "wealth" = "S29", "econ" = "P3STGBS", "left"= "P27ST", "on_china" = "P35ST.C") %>% 
  mutate(numinves = 2015)

## 2013
summary(Latinobarometro2013Eng$P48ST.C)
latin_2013 <- Latinobarometro2013Eng %>% 
  select(numinves, idenpa, numentre, P48ST.C, S11, S10, 
         S19.A, S19.A, S17, S16, S27, P3STGBS, P41ST) %>% 
  filter(P48ST.C >= 1 & P48ST.C <=4) %>% 
  mutate(ccode = countrycode(idenpa, "iso3n", "cown"), 
         nation = countrycode(idenpa, "iso3n", "country.name"))  %>% 
  rename("age" = "S11", "sexo" ="S10", "employ"="S19.A", "edu_year"="S17", "edu_age"="S16",
         "wealth" = "S27", "econ" = "P3STGBS", "left"= "P41ST", "on_china" = "P48ST.C") %>% 
  mutate(numinves = 2013) 

## 2011
summary(Latinobarometro_2011_eng$P44ST_C)
latin_2011 <- Latinobarometro_2011_eng %>% 
  select(numinves, idenpa, numentre, P44ST_C, S17, S16, 
         S23A, S23A, S21, S20, S34, P3ST_A, P76ST) %>% 
  filter(P44ST_C >= 1 & P44ST_C <=4) %>% 
  mutate(ccode = countrycode(idenpa, "iso3n", "cown"), 
         nation = countrycode(idenpa, "iso3n", "country.name"))  %>% 
  rename("age" = "S17", "sexo" ="S16", "employ"="S23A", "edu_year"="S21", "edu_age"="S20",
         "wealth" = "S34", "econ" = "P3ST_A", "left"= "P76ST", "on_china" = "P44ST_C") %>% 
  mutate(numinves = 2011) %>% mutate(numinves=as.numeric(numinves), idenpa=as.numeric(idenpa),
                                     numentre=as.numeric(numentre), on_china=as.numeric(on_china),
                                     age=as.numeric(age), sexo=as.numeric(sexo),
                                     employ=as.numeric(employ), edu_year=as.numeric(edu_year),
                                     edu_age=as.numeric(edu_age), wealth=as.numeric(wealth),
                                     econ=as.numeric(econ), left=as.numeric(left))
  
## combine
latin_base <- rbind(latin_2011,latin_2013,latin_2015,latin_2016,latin_2017,latin_2018,latin_2020)
latin_base <- latin_base %>% filter(on_china>=0 & employ>=0 & edu_year>0 & 
                                      wealth>0 & econ>0 & left>=0 & left <=10)

latin_base <- left_join(latin_base, basic_frame, by = c("ccode" = "cowcode", "numinves" = "year"))
latin_base <- latin_base %>% mutate(year_1 = numinves -1)
### on_china_binomize
latin_base <- latin_base %>% mutate(on_china_bin = 0)
latin_base$on_china_bin[which(latin_base$on_china==1 | latin_base$on_china==2)] <- 1

# UNVOTE
dfAgree <- dfAgree %>% filter(year >= 2010 & year <= 2020 & ccode1==710) %>% 
  select(ccode2, agree, year, IdealPointDistance)
latin_base <- left_join(latin_base, dfAgree, by = c("ccode" = "ccode2", "year_1" = "year"))

# V-DEM
vdem_base <- vdem %>% filter(year  >= 2010 & year <= 2020) %>% 
  select(country_text_id, COWcode, country_name, year, v2x_polyarchy, v2x_libdem,
         v2x_partipdem, v2x_delibdem, v2x_egaldem)
latin_base <- left_join(latin_base, vdem_base, by = c("ccode" = "COWcode", "year_1" = "year"))

# Econ National data
## trade
trade <- rbind(commodity2010_2014,commodity2015_2019)
trade <- trade %>% rename("iso"="Reporter ISO", "import"="Trade Value (US$)")
latin_base <- left_join(latin_base, trade, by = c("iso" = "iso", "year_1" = "Year"))

trade_all <- rbind(trade_all_1014, trade_all_1519)
trade_all <- trade_all %>% rename("iso"="Reporter ISO", "import"="Trade Value (US$)")
latin_base <- left_join(latin_base, trade_all,by = c("iso" = "iso", "year_1" = "Year"))

latin_base <- latin_base %>% mutate(chn_econ_depend=chn_import/all_import)

## FDI

## Econ Cooperation

turnover_latin <- turnover_latin %>% 
  filter(ccode!=is.na(ccode)) 
econ_coop_value <- gather(turnover_latin, key = "year", value= "turnover", `2010`: `2020`)
econ_coop_value <- econ_coop_value %>% mutate(year=as.numeric(year))
latin_base <- left_join(latin_base, econ_coop_value,by = c("ccode" = "ccode", "year_1" = "year"))

## BRI project
bri_project <- projectList %>% mutate(year=year(`Announced Date`)) %>% 
  filter( Region=="Americas") %>% 
  mutate(cowcode=countrycode(`Project Country/Region`, "country.name", "cown")) %>% 
  select(cowcode,year,`Announced Date`,`Project Country/Region`,
                                      Type,Status,`Total Cost (USD, M)`,`Initiative Type`,
                                      )

bri_project <- bri_project %>% group_by(cowcode,year) %>%
  arrange(cowcode,year) %>% summarise(value=sum(`Total Cost (USD, M)`)) %>% ungroup

bri_project <- bri_project %>% group_by(cowcode) %>% 
  mutate(cum_num = cumsum(value)) %>% ungroup

## basic frame
basic_frame <- latin_base %>% group_by(ccode,year,nation) %>% 
  arrange(ccode,year) %>% summarise(test=n())
basic_frame <- left_join(basic_frame, bri_project,
                        by = c("ccode" = "cowcode", "year" = "year"))
### reload basic frame
basic_frame <- basic_frame %>% mutate(cum_num= ifelse(is.na(cum_num),0,cum_num))
latin_base <- left_join(latin_base, basic_frame,by = c("ccode" = "ccode", "numinves" = "numinves"))


## did recode
latin_base <- latin_base %>% mutate(first.treat=0)
latin_base$first.treat[which(latin_base$ccode)]

# delete redundant
latin_base <- latin_base %>% select(-country_name.x, -Reporter.x, -session.x,
                                    -ccode1, -country_name.x.x, -country_name.y,
                                    -Reporter.y, -test,-project_num.y,-country_name,
                                    -iso.y, -bri_dum.y, -countryname, -region)
latin_base <- latin_base %>% select(-country_text_id) %>% rename("treat_year"="year", "year"="numinves", "iso"="iso.x", "bri_dum"="bri_dum.x", "chn_import"="import.x", "all_import"="import.y")
latin_base <- latin_base %>% rename("id"="numentre")
latin_base <- latin_base %>% select(-first.treat)
