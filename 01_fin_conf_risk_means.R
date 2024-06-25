### fin_conf_risk import data, compute additional variables, and calculate means 
### CCF 5.31.24

## Load libraries
library(haven)
library(tidyverse)
library(knitr)
library(here)

## Data importing
finlit1wave <- read_dta("fin_literacy_firstwave.dta") 
# individual responses to indv items from the questionnaire
rush1wave <- read_dta("rush_singlewave.dta") # subject level data

#Note that there are NA values in each row, so directly omitting NA will delete the entire dataset. We can resolve this issue by removing NA values within each of the following analyses, as necessary.
##computed variables
  ####clinical diagnosis
#- `healthy_mci_ad`: 3-level categorical variable for cognitive diagnosis from 5 dcfdx categories. We ignore dcfdx category 6, which signifies non-AD other dementia category participants
#- `healthy_mci_ad` = 1 when dcfdx = 1
#- `healthy_mci_ad` = 2 when dcfdx = 2 or 3 (combine MCI and MCI+)
#- `healthy_mci_ad` = 1 when dcfdx = 4 or 5 (combine AD and AD+)
#- `label_healthy_mci_ad`: label for the 3 categories of `healthy_mci_ad`
#- no diagnosis for `healthy_mci_ad` = 1
#- MCI/MCI+ for `healthy_mci_ad` = 2
#- AD/AD+ for `healthy_mci_ad` = 3
rush1wave_mod <- rush1wave %>%
  mutate(
    alpha = (small_alpha + large_alpha)/2, 
    healthy_mci_ad = case_when(
      dcfdx == 1 ~ 1,
      dcfdx %in% c(2,3) ~ 2,
      dcfdx %in% c(4,5) ~ 3
    ),
    label_healthy_mci_ad = case_when(
      healthy_mci_ad == 1 ~ "no diagnosis",
      healthy_mci_ad == 2 ~ "MCI/MCI+",
      healthy_mci_ad == 3 ~ "AD/AD+"
    )
  )
  ####knowledge about financial information and concepts (called institutional knowledge)
    ##replace NAs with 0 because R thinks 
    finlit1wave<- finlit1wave %>% replace(is.na(.), 0)
    ##calculate score as sum of 12 items about financial info and concepts
   finlit1wave <- finlit1wave %>%
    mutate(new_instkw_score = instkw2_corr + instkw4_corr + instkw6_corr + instkw7_corr +instkw8_corr + instkw9_corr+ instkw11_corr+ instkw12_corr + instkw13_corr +instkw14_corr + instkw15_corr +instkw16_corr)
    ##take z score of new variable
    finlit1wave<- finlit1wave %>% mutate(z_new_instkw_score = scale(new_instkw_score))

    ##select variables to merge with main dataframe
    instkw_score_id<-finlit1wave %>% select(projid, new_instkw_score, z_new_instkw_score)

    #merge dataframes 
    rush1wave_mod<-right_join(rush1wave_mod, instkw_score_id, by= 'projid')

  ####calculate overconfidence with new instkw score variable
    ##overconfidence is the standardized difference between the confidence and financial knowledge score
    rush1wave_mod<- rush1wave_mod %>% mutate(new_overconfidence = z_confid_instit - z_new_instkw_score, z_new_overconfidence= scale(new_overconfidence))

### Below are the summary statistics of demographic variables
##age at visit
rush1wave_mod %>%
  filter(healthy_mci_ad != 'NA') %>%
  group_by(healthy_mci_ad) %>%
  summarize(
    Min = min(age_at_visit),
    "1st Qu." = quantile(age_at_visit, 0.25),
    Median = median(age_at_visit),
    Mean = mean(age_at_visit),
    "3rd Qu." = quantile(age_at_visit, 0.75),
    Max = max(age_at_visit),
    iqr = IQR(age_at_visit)
  ) %>%
  kable()

##sex
rush1wave_mod %>%
  filter(healthy_mci_ad != 'NA') %>%
  group_by(healthy_mci_ad) %>%
  summarize(
    Min = min(msex),
    "1st Qu." = quantile(msex, 0.25),
    Median = median(msex),
    Mean = mean(msex),
    "3rd Qu." = quantile(msex, 0.75),
    Max = max(msex),
    iqr = IQR(msex)
  ) %>%
  kable()

##global cognition
rush1wave_mod %>%
  filter(healthy_mci_ad != 'NA') %>%
  group_by(healthy_mci_ad) %>%
  summarize(
    Min = min(cogn_global),
    "1st Qu." = quantile(cogn_global, 0.25, na.rm = T),
    Median = median(cogn_global),
    Mean = mean(cogn_global),
    "3rd Qu." = quantile(cogn_global, 0.75, na.rm = T),
    Max = max(cogn_global),
    iqr = IQR(cogn_global, na.rm = T)
  ) %>%
  kable()

##education level
rush1wave_mod %>%
  filter(healthy_mci_ad != 'NA') %>%
  group_by(healthy_mci_ad) %>%
  summarize(
    Min = min(educ),
    "1st Qu." = quantile(educ, 0.25, na.rm = T),
    Median = median(educ),
    Mean = mean(educ),
    "3rd Qu." = quantile(educ, 0.75, na.rm = T),
    Max = max(educ),
    iqr = IQR(educ, na.rm = T)
  ) %>%
  kable()

##income
rush1wave_mod %>%
  filter(healthy_mci_ad != 'NA') %>%
  group_by(healthy_mci_ad) %>%
  summarize(
    Min = min(income_bl),
    "1st Qu." = quantile(income_bl, 0.25, na.rm = T),
    Median = median(income_bl),
    Mean = mean(income_bl),
    "3rd Qu." = quantile(income_bl, 0.75, na.rm = T),
    Max = max(income_bl),
    iqr = IQR(income_bl, na.rm = T)
  ) %>%
  kable()

###Below are the summary statistics of decision making variables

##risk aversion--gamma
rush1wave_mod %>%
  filter(healthy_mci_ad != 'NA') %>%
  group_by(healthy_mci_ad) %>%
  summarize(
    Min = min(gamma),
    "1st Qu." = quantile(gamma, 0.25, na.rm = T),
    Median = median(gamma),
    Mean = mean(gamma),
    "3rd Qu." = quantile(gamma, 0.75, na.rm = T),
    Max = max(gamma),
    iqr = IQR(gamma, na.rm = T)) %>% 
  kable()

##financial risk tolerance
rush1wave_mod %>%
  filter(healthy_mci_ad != 'NA', risk2_trim != 'NA') %>%
  group_by(healthy_mci_ad) %>%
  summarize(
    Min = min(risk2_trim),
    "1st Qu." = quantile(risk2_trim, 0.25, na.rm = T),
    Median = median(risk2_trim),
    Mean = mean(risk2_trim),
    "3rd Qu." = quantile(risk2_trim, 0.75, na.rm = T),
    Max = max(risk2_trim),
    iqr = IQR(risk2_trim, na.rm = T)) %>% 
  kable()

## time discount rate
rush1wave_mod %>%
  filter(healthy_mci_ad != 'NA') %>%
  group_by(healthy_mci_ad) %>%
  summarize(
    Min = min(alpha),
    "1st Qu." = quantile(alpha, 0.25, na.rm = T),
    Median = median(alpha),
    Mean = mean(alpha),
    "3rd Qu." = quantile(alpha, 0.75, na.rm = T),
    Max = max(alpha),
    iqr = IQR(alpha, na.rm = T)) %>% 
  kable()

##total financial literacy
rush1wave_mod %>%
  filter(healthy_mci_ad != 'NA', fin_literacy_pct != 'NA') %>%
  group_by(healthy_mci_ad) %>%
  summarize(
    Min = min(fin_literacy_pct),
    "1st Qu." = quantile(fin_literacy_pct, 0.25, na.rm = T),
    Median = median(fin_literacy_pct),
    Mean = mean(fin_literacy_pct),
    "3rd Qu." = quantile(fin_literacy_pct, 0.75, na.rm = T),
    Max = max(fin_literacy_pct),
    iqr = IQR(fin_literacy_pct, na.rm = T)) %>% 
  kable()

##numeracy score
rush1wave_mod %>%
  filter(healthy_mci_ad != 'NA', finknw_score != 'NA') %>%
  group_by(healthy_mci_ad) %>%
  summarize(
    Min = min(finknw_score),
    "1st Qu." = quantile(finknw_score, 0.25, na.rm = T),
    Median = median(finknw_score),
    Mean = mean(finknw_score),
    "3rd Qu." = quantile(finknw_score, 0.75, na.rm = T),
    Max = max(finknw_score),
    iqr = IQR(finknw_score, na.rm = T)) %>% 
  kable()

##financial knowledge score
rush1wave_mod2 %>%
  filter(healthy_mci_ad != 'NA', new_instkw_score != 'NA') %>%
  group_by(healthy_mci_ad) %>%
  summarize(
    Min = min(new_instkw_score),
    "1st Qu." = quantile(new_instkw_score, 0.25, na.rm = T),
    Median = median(new_instkw_score),
    Mean = mean(new_instkw_score),
    "3rd Qu." = quantile(new_instkw_score, 0.75, na.rm = T),
    Max = max(new_instkw_score),
    iqr = IQR(new_instkw_score, na.rm = T)) %>% 
  kable()

##confidence in financial knowledge questions
rush1wave_mod %>%
  filter(healthy_mci_ad != 'NA', confid_instit != 'NA') %>%
  group_by(healthy_mci_ad) %>%
  summarize(
    Min = min(confid_instit),
    "1st Qu." = quantile(confid_instit, 0.25, na.rm = T),
    Median = median(confid_instit),
    Mean = mean(confid_instit),
    "3rd Qu." = quantile(confid_instit, 0.75, na.rm = T),
    Max = max(confid_instit),
    iqr = IQR(confid_instit, na.rm = T)) %>% 
  kable()

##overconfidence-- difference between confidence and financial literacy overall (financial knowledge questions + numeracy questions)
rush1wave_mod %>%
  filter(healthy_mci_ad != 'NA', overconfidence != 'NA') %>%
  group_by(healthy_mci_ad) %>%
  summarize(
    Min = min(overconfidence),
    "1st Qu." = quantile(overconfidence, 0.25, na.rm = T),
    Median = median(overconfidence),
    Mean = mean(overconfidence),
    "3rd Qu." = quantile(overconfidence, 0.75, na.rm = T),
    Max = max(overconfidence),
    iqr = IQR(overconfidence, na.rm = T)) %>% 
  kable()

##new_overconfidence-- difference between confidence about financial knowledge and financial knowledge score using corrected overconfidence variable--do NOT use overconfidence_match
rush1wave_mod %>%
  filter(healthy_mci_ad != 'NA', new_overconfidence != 'NA') %>%
  group_by(healthy_mci_ad) %>%
  summarize(
    Min = min(new_overconfidence),
    "1st Qu." = quantile(new_overconfidence, 0.25, na.rm = T),
    Median = median(new_overconfidence),
    Mean = mean(new_overconfidence),
    "3rd Qu." = quantile(new_overconfidence, 0.75, na.rm = T),
    Max = max(new_overconfidence),
    iqr = IQR(new_overconfidence, na.rm = T)) %>% 
  kable()