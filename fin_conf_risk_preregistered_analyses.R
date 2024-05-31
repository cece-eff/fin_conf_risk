##fin_conf_risk preregistered analyses
##CCF 5.31.24 

## Load libraries
library(haven)
library(tidyverse)
library(knitr)
library(here)

## Data importing
finlit1wave <- read_dta("fin_literacy_firstwave.dta") 
# individual responses to indv items from the questionnaire
rush1wave <- read_dta("rush_singlewave.dta") # subject level data

### Pre-Registered Analyses
#Linear and quadratic effects of age on risk aversion
lm.age.risk <- lm(z_gamma ~ z_age + age2, data = rush1wave_mod) 

summary(lm.age.risk)
confint(lm.age.risk, c('z_age', 'age2'), 0.95)

ggplot(rush1wave_mod, 
       aes(
         x=age_at_visit, 
         y=gamma)) +
  geom_point(color = "grey57") +
  geom_smooth(method = "lm", color = "black") +
  labs(x = "Age", y="Risk aversion") +
  theme_bw()

##The plot above shows the raw `age_at_visit` by `gamma` values, since the transformed age and gamma values do not provide much interpretability.

#Linear and quadratic effects of age on time discounting:
lm.age.timedisct <- lm(z_alpha ~ z_age + age2, data = rush1wave_mod)

summary(lm.age.timedisct)
confint(lm.age.timedisct, c('z_age', 'age2'), 0.95)

ggplot(rush1wave_mod, 
       aes(
         x=age_at_visit, 
         y=alpha)) +
  geom_point(color = "grey57") +
  geom_smooth(method = "lm", color = "black") +
  labs(x = "Age", y = "Financial risk tolerance") +
  theme_bw()

#Linear and quadratic effects of age on financial literacy: 
lm.age.finlit <- lm(z_fin_literacy_pct ~ z_age + age2, data = rush1wave_mod)

summary(lm.age.finlit)
confint(lm.age.finlit, c('z_age', 'age2'), 0.95)

ggplot(rush1wave_mod, 
       aes(
         x=age_at_visit, 
         y=fin_literacy_pct)) +
  geom_point(color = "grey57") +
  geom_smooth(method = "lm", color = "black") +
  labs(x = "Age", y="Financial literacy") +
  theme_bw()

#Linear and quadratic effects of age on confidence in financial knowledge:
lm.age.finconf <- lm(z_confid_instit ~ z_age + age2, data = rush1wave_mod)

summary(lm.age.finconf)
confint(lm.age.finconf, c('z_age', 'age2'), 0.95)

ggplot(rush1wave_mod, 
       aes(
         x=age_at_visit, 
         y=confid_instit)) +
  geom_point(color = "grey57") +
  geom_smooth(method = "lm", color = "black") +
  labs(x = "Age", y="Confidence") +
  theme_bw()

#Effects of literacy and age on confidence:
lm.litage.conf <- lm(z_confid_instit ~ z_fin_literacy_pct + z_age, data = rush1wave_mod)

summary(lm.litage.conf)
confint(lm.litage.conf, c('z_fin_literacy_pct', 'z_age'), 0.95)

car::avPlots(lm.litage.conf)
#The plot of the left shows the effect of literacy on confidence, controlling for the effects of age. The plot on the right shows the effects of age on confidence, controlling for literacy. 



### Interaction effects: confidence by literacy
#Main and interaction effects of confidence, literacy and age on risk aversion:
lm.interrisk <- lm(z_gamma ~ z_confid_instit*z_fin_literacy_pct*z_age, data = rush1wave_mod)
summary(lm.interrisk)
car::avPlots(lm.interrisk)

rush1wave_new <- rush1wave_mod %>%
  mutate(confinstXage = z_confid_instit * z_age,
         finlitXage = z_fin_literacy_pct * z_age,
         confinstXfinlit = z_confid_instit * z_fin_literacy_pct,
         confinstXfinlitXage = z_confid_instit * z_fin_literacy_pct * z_age)

lm(z_gamma ~ z_confid_instit + z_fin_literacy_pct + confinstXfinlit + z_age + confinstXage+ finlitXage + confinstXfinlitXage, data=rush1wave_new) %>%
  summary()

#Main and interaction effects of confidence, literacy, and age on time discounting:
lm.intertimedisc <- lm(z_alpha ~ z_confid_instit*z_fin_literacy_pct*z_age, data = rush1wave_mod)
summary(lm.intertimedisc)
car::avPlots(lm.intertimedisc)

#### Run both linear regressions above on each level of the healthy group
#Risk aversion
lm(z_gamma ~ z_confid_instit*z_fin_literacy_pct*z_age, 
   data = rush1wave_mod%>%filter(healthy_mci_ad == 1)) %>%
  summary()

lm(z_gamma ~ z_confid_instit*z_fin_literacy_pct*z_age, 
   data = rush1wave_mod%>%filter(healthy_mci_ad == 2))%>%
  summary()

lm(z_gamma ~ z_confid_instit*z_fin_literacy_pct*z_age, 
   data = rush1wave_mod%>%filter(healthy_mci_ad == 3))%>%
  summary()

#Time discounting
lm(z_alpha ~ z_confid_instit*z_fin_literacy_pct*z_age, 
   data = rush1wave_mod%>%filter(healthy_mci_ad == 1))%>%
  summary()

lm(z_alpha ~ z_confid_instit*z_fin_literacy_pct*z_age, 
   data = rush1wave_mod%>%filter(healthy_mci_ad == 2))%>%
  summary()

lm(z_alpha ~ z_confid_instit*z_fin_literacy_pct*z_age, 
   data = rush1wave_mod%>%filter(healthy_mci_ad == 3))%>%
  summary()

