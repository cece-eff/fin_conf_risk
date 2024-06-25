## CCF 5.31.24
##fin_risk_conf overconfidence analyses

## Load libraries
library(haven)
library(tidyverse)
library(knitr)
library(here)
library(cowplot)

##run fin_conf_risk data means

##compare overconfidence to 0
t.test(rush1wave_mod$new_overconfidence)
t.test(rush1wave_mod$overconfidence)
t.test(rush1wave_mod$overconfidence_match)


##linear and quadratic effects of age on NEW overconfidence
lm.age.overconfid <- lm(z_new_overconfidence ~ z_age + age2, data = rush1wave_mod)
summary(lm.age.overconfid)
confint(lm.age.overconfid, c('z_age', 'age2'), 0.95)
##controlling for cognition
lm.age.overconfid.ctrl <- lm(z_new_overconfidence ~ z_age + age2+ z_cogn_global, data = rush1wave_mod)
summary(lm.age.overconfid.ctrl)
confint(lm.age.overconfid.ctrl, c('z_age', 'age2', 'z_cogn_global'), 0.95)
#graph
ggplot(rush1wave_mod, 
       aes(
         x=z_age, 
         y=z_new_overconfidence)) +
  geom_point(color = "grey57") +
  geom_smooth(method = "lm", color = "black") +
  labs(x = "Age", y = "Overconfidence") +
  theme_bw()

##relationship between risk aversion and financial risk tolerance
# controlling for age:
lm.ra.fr <- lm(z_risk2 ~ z_gamma + z_age, data = rush1wave_mod)
summary(lm.ra.fr)
confint(lm.ra.fr, c('z_gamma'), 0.95)

# not controlling for age: 
lm.ra.fr.noctrl <- lm(z_risk2 ~ z_gamma, data = rush1wave_mod)
summary(lm.ra.fr.noctrl)
confint(lm.ra.fr.noctrl, 'z_gamma', 0.95)

  #graph
ggplot(rush1wave_mod, 
       aes(x=gamma, y=risk2_trim)) +
  geom_point(color = "grey57") +
  geom_smooth(method = "lm", color = "black") +
  labs(x = "Risk aversion", y = "Financial risk taking") +
  theme_bw()

##CORRECTED OVERCONFIDENCE EFFECTS
##relationship between risk aversion and overconfidence
lm.overconfid.raalt <- lm(z_gamma ~ z_new_overconfidence + z_age, data = rush1wave_mod)
summary(lm.overconfid.raalt)
confint(lm.overconfid.raalt, 'z_new_overconfidence', 0.95)

lm.overconfid.raalt2 <- lm(z_gamma ~ z_new_overconfidence + z_age + z_cogn_global, data = rush1wave_mod)
summary(lm.overconfid.raalt2)
confint(lm.overconfid.raalt2, 'z_new_overconfidence', 0.95)

ggplot(rush1wave_mod, 
       aes(x=z_new_overconfidence, y=z_gamma)) +
  geom_jitter(color = "grey57") +
  geom_smooth(method = "lm", color = "black") +
  labs(x = "Overconfidence", y = "Financial risk aversion") +
  theme_bw()

##relationship between risk tolerance and overconfidence
##remove AD participants
no_ad<-rush1wave_mod %>% filter(healthy_mci_ad != 3)

lm.overconfid.risk.alt <- lm(z_risk2 ~ z_new_overconfidence + z_age, data=no_ad)
summary(lm.overconfid.risk.alt)
confint(lm.overconfid.risk.alt, 'z_new_overconfidence', 0.95)

lm.overconfid.risk.alt2 <- lm(z_risk2 ~ z_new_overconfidence + z_age + z_cogn_global, data=no_ad)
summary(lm.overconfid.risk.alt2)
confint(lm.overconfid.risk.alt2, 'z_new_overconfidence', 0.95)

all<-ggplot(no_ad, 
       aes(x=z_new_overconfidence, y=z_risk2)) + xlim(-3, 3)+
  geom_jitter(color = "grey45") +
  geom_smooth(method = "lm", color = "grey45") +
  labs(x = "Overconfidence", y = "Financial Risk Tolerance") +
  theme_bw(base_size = 18) + theme(plot.title = element_text(vjust = 3)) + ggtitle("NCI + MCI") + coord_fixed()

##overconfidence and risk tolerance by diagnosis group
##NCI (healthy controls)
hc_risktol<-lm(z_risk2 ~ z_new_overconfidence + z_age + z_cogn_global, 
               data=rush1wave_mod %>% filter(healthy_mci_ad == 1))
summary(hc_risktol)
confint(hc_risktol, 'z_new_overconfidence', 0.95)
  #graph
nci<-ggplot(subset(rush1wave_mod, healthy_mci_ad %in% 1), 
       aes(x=z_new_overconfidence, y=z_risk2)) + xlim(-3, 3)+
  geom_jitter(color = "grey45") +
  geom_smooth(method = "lm", color = "grey45") +
  labs(x = "Overconfidence", y = "Financial Risk Tolerance") +
  theme_bw(base_size = 18) + theme(plot.title = element_text(vjust = 3)) + ggtitle("NCI")+ coord_fixed()

#MCI 
mci_risktol<-lm(z_risk2 ~ z_new_overconfidence + z_age + z_cogn_global, 
                data=rush1wave_mod %>% filter(healthy_mci_ad == 2))
summary(mci_risktol)
confint(mci_risktol, 'z_new_overconfidence', 0.95)
  #graph
mci<-ggplot(subset(rush1wave_mod, healthy_mci_ad %in% 2), 
       aes(x=z_new_overconfidence, y=z_risk2)) +  xlim(-3, 3)+
  geom_jitter(color = "grey45") +
  geom_smooth(method = "lm", color = "grey45") +
  labs(x = "Overconfidence", y = "Financial Risk Tolerance") +
  theme_bw(base_size =18)+ theme(plot.title = element_text(vjust = 3)) +ggtitle("MCI") + coord_fixed()

plot_grid(all, nci, mci, ncol =3)
ggsave("figure1.png", width= 10, height= 5)

#AGE EFFECTS
#### Linear and quadratic effects of age on financial risk tolerance
lm.age.rt <- lm(z_risk2 ~ z_age + age2, data = rush1wave_mod)
summary(lm.age.rt)
confint(lm.age.rt, c('z_age', 'age2'), 0.95)
#graph
ggplot(rush1wave_mod, 
       aes(
         x=age_at_visit, 
         y=risk2_trim)) +
  geom_point(color = "grey57") +
  geom_smooth(method = "lm", color = "black") +
  labs(x = "Age", y = "Financial risk tolerance") +
  theme_bw() 