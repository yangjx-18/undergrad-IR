library(plm)
library(did)
library(panelView)
library(dplyr)
library(ggplot2)
library(cowplot)
library(dotwhisker)
library(mgcv)
library(lme4)
library(stargazer)
library(broom)
library(multiwayvcov,arm)
source("Replication/model_setup.R")


latin_base <- latin_base %>% as.data.frame()
latin_base$year <- as.integer(latin_base$year )

latin_42 <- latin_base %>% filter(ccode==42)
latin_70 <- latin_base %>% filter(ccode==70)
latin_90 <- latin_base %>% filter(ccode==90)
latin_91 <- latin_base %>% filter(ccode==91)
latin_92 <- latin_base %>% filter(ccode==92)
latin_93 <- latin_base %>% filter(ccode==93)
latin_94 <- latin_base %>% filter(ccode==94)
latin_95 <- latin_base %>% filter(ccode==95)

# model setup
form_bri_interact <- as.formula("on_china ~ bri_dum +
                             bri_dum:wealth + bri_dum:edu_year+
                             log(e_gdppc) +log(e_pop) +chn_econ_depend +
                            age + sexo + employ + edu_year + wealth +
                            econ + left + IdealPointDistance + v2x_polyarchy" )


form_cum_interact <- as.formula("on_china ~ cum_num +
                             cum_num:wealth + cum_num:edu_year+
                             log(e_gdppc) + log(e_pop) +chn_econ_depend + 
                            age + sexo + employ + edu_year + wealth +
                            econ + left + IdealPointDistance + v2x_polyarchy" )

form_val_interact <- as.formula("on_china ~ log(turnover) +
                             log(turnover):wealth + log(turnover):edu_year+
                             log(e_gdppc) +log(e_pop) +chn_econ_depend +
                            age + sexo + employ + edu_year + wealth +
                            econ + left + IdealPointDistance + v2x_polyarchy" )


# linear regress
## fix effect
fix_within_bri <- plm(on_china ~ bri_dum +
                        bri_dum:wealth + bri_dum:edu_year+
                        log(e_gdppc) +log(e_pop) +chn_econ_depend +
                        age + sexo + employ + edu_year + wealth +
                        econ + left + IdealPointDistance + v2x_polyarchy, data = latin_base, index = c("year"), 
               model = "within", effect = "time")
summary(fix_within_bri)
fix_within_cum <- plm(form_cum_interact, data = latin_base, index = c("year"), 
                      model = "within", effect = "time")
summary(fix_within_cum)
fix_within_val <- plm(form_val_interact, data = latin_base, index = c("year"), 
                      model = "within", effect = "time")
summary(fix_within_val)


fix_plot <- dwplot(list(fix_within_bri,fix_within_cum,fix_within_val)) + theme_bw()
ggsave(fix_plot, filename = "fix_flot_nonse.png")


## random effect
fix_random_bri <- plm(form_bri_interact, data = latin_base, index = c("year"), 
                           model = "random", effect = "time")
summary(fix_random_bri)
fix_random_cum <- plm(form_cum_interact, data = latin_base, index = c("year"), 
                      model = "random", effect = "time")
summary(fix_random_cum)
fix_random_val <- plm(form_val_interact, data = latin_base, index = c("year"), 
                      model = "random", effect = "time")
summary(fix_random_val)



random_result <- stargazer(fix_random_bri, fix_random_cum, fix_random_val,
                        digits = 3, type = "text",  
                        title = "Random Effect", 
                        dep.var.labels = c("bri","cum", "val"))
write.table(random_result, file = "random_result.txt", 
            row.names = FALSE, sep = ",")

# logic fixed effect model
library(lme4)
library(bife)
library(texreg)

logit_fix_bri <- bife(on_china_bin ~ log(e_gdppc) +log(e_pop) +chn_econ_depend +
                        age + sexo + employ + edu_year + wealth +
                        econ + left + IdealPointDistance + v2x_polyarchy+
                        bri_dum + bri_dum:wealth + bri_dum:edu_year | year , 
             data = latin_base, "logit")

summary(logit_fix_bri)
summary(get_APEs(logit_fix_bri))

logit_fix_cum <- bife(on_china_bin ~ log(e_gdppc) +log(e_pop) +chn_econ_depend +
                        age + sexo + employ + edu_year + wealth +
                        econ + left + IdealPointDistance + v2x_polyarchy+
                        cum_num + cum_num:wealth + cum_num:edu_year | year , 
                      data = latin_base, "logit")
summary(logit_fix_cum)
summary(get_APEs(logit_fix_cum))

logit_fix_val <- bife(on_china_bin ~ log(e_gdppc) +log(e_pop) +chn_econ_depend +
                        age + sexo + employ + edu_year + wealth +
                        econ + left + IdealPointDistance + v2x_polyarchy+
                        log(turnover) + log(turnover):wealth + log(turnover):edu_year | year , 
                      data = latin_base, "logit")
summary(logit_fix_val)
summary(get_APEs(logit_fix_val))


logit_result <- stargazer(logit_fix_bri, logit_fix_cum, logit_fix_val,
                           digits = 3, type = "text",  
                           title = "Logit Fixed Effect Model", 
                           dep.var.labels = c("bri","cum", "val"))
write.table(logit_result, file = "logit_result.txt", 
            row.names = FALSE, sep = ",")


#  probit fixed effect model
probit_fix_bri <- bife(on_china_bin ~ log(e_gdppc) +log(e_pop) +chn_econ_depend +
                        age + sexo + employ + edu_year + wealth +
                        econ + left + IdealPointDistance + v2x_polyarchy+
                        bri_dum + bri_dum:edu_year + bri_dum:wealth | year , 
                      data = latin_base, "probit")
summary(probit_fix_bri)
summary(get_APEs(probit_fix_bri))

probit_fix_cum <- bife(on_china_bin ~ log(e_gdppc) +log(e_pop) +chn_econ_depend +
                         age + sexo + employ + edu_year + wealth +
                         econ + left + IdealPointDistance + v2x_polyarchy+
                         cum_num + cum_num:edu_year + cum_num:wealth | year , 
                       data = latin_base, "probit")
summary(probit_fix_cum)
summary(get_APEs(probit_fix_cum))

probit_fix_val <- bife(on_china_bin ~ log(e_gdppc) +log(e_pop) +chn_econ_depend +
                        age + sexo + employ + edu_year + wealth +
                        econ + left + IdealPointDistance + v2x_polyarchy+
                         log(turnover) + log(turnover):wealth + log(turnover):edu_year | year , 
                      data = latin_base, "probit")
summary(probit_fix_val)
summary(get_APEs(probit_fix_val))

binomial_result <- htmlreg(list(logit_fix_bri, logit_fix_cum, probit_fix_bri, probit_fix_cum
             ))


# DID
## generate first_treat_year
basic_frame <- basic_frame %>% group_by(cowcode) %>% 
  mutate(treat = cumsum(bri_dum)) %>% ungroup
latin_base <- left_join(latin_base, basic_frame,by = c("ccode" = "cowcode", "numinves" = "year"))

## model
library(did)
example_attgt <- att_gt(yname = "on_china_bin",
                        tname = "year",
                        idname = "id",
                        gname = "treat_year",
                        xformla = ~ age + sexo + employ + edu_year + wealth +
                          econ + left,
                        panel = FALSE,
                        allow_unbalanced_panel = TRUE,
                        data = latin_base)
ggdid(example_attgt, ylim = c(-.3,.3))
summary(example_attgt)
mw.dyn <- aggte(example_attgt, type = "dynamic")
summary(mw.dyn)
ggdid(mw.dyn, ylim = c(-.3,.3))

test1 <- tidy(example_attgt)
test2 <- tidy(summary(mw.dyn))
write.csv(test2, file = "test2.csv")
