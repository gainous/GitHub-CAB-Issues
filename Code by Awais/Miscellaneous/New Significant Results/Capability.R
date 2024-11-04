Domestic_Issues_SysCapable <- glm(Domestic_Issues ~ system_capable_n_sc, 
                                data = CAB_data_Recoded, 
                                family = binomial)
summary(Domestic_Issues_SysCapable)


#####
Political_Violence_SysCapable <- glm(Political_Violence ~ system_capable_n_sc, 
                                   data = CAB_data_Recoded, 
                                   family = binomial)
summary(Political_Violence_SysCapable)


#####
Economic_Issues_SysCapable <- glm(Economic_Issues ~ system_capable_n_sc, 
                                data = CAB_data_Recoded, 
                                family = binomial)
summary(Economic_Issues_SysCapable)

####################################################################

Domestic_Issues_SysProud <- glm(Domestic_Issues ~ system_proud_n_sc, 
                                  data = CAB_data_Recoded, 
                                  family = binomial)
summary(Domestic_Issues_SysProud)


#####
Political_Violence_SysProud <- glm(Political_Violence ~ system_proud_n_sc, 
                                     data = CAB_data_Recoded, 
                                     family = binomial)
summary(Political_Violence_SysProud)


#####
Economic_Issues_SysProud <- glm(Economic_Issues ~ system_proud_n_sc, 
                                  data = CAB_data_Recoded, 
                                  family = binomial)
summary(Economic_Issues_SysProud)


####################################################################


Domestic_Issues_SysDeserves <- glm(Domestic_Issues ~ system_deserves_n_sc, 
                                data = CAB_data_Recoded, 
                                family = binomial)
summary(Domestic_Issues_SysDeserves)


#####
Political_Violence_SysDeserves <- glm(Political_Violence ~ system_deserves_n_sc, 
                                   data = CAB_data_Recoded, 
                                   family = binomial)
summary(Political_Violence_SysDeserves)


#####
Economic_Issues_SysDeserves <- glm(Economic_Issues ~ system_deserves_n_sc, 
                                data = CAB_data_Recoded, 
                                family = binomial)
summary(Economic_Issues_SysDeserves)

####################################################################

Domestic_Issues_SysLive <- glm(Domestic_Issues ~ system_live_n_sc, 
                                   data = CAB_data_Recoded, 
                                   family = binomial)
summary(Domestic_Issues_SysLive)


#####
Political_Violence_SysLive <- glm(Political_Violence ~ system_live_n_sc, 
                                      data = CAB_data_Recoded, 
                                      family = binomial)
summary(Political_Violence_SysLive)


#####
Economic_Issues_SysLive <- glm(Economic_Issues ~ system_live_n_sc, 
                                   data = CAB_data_Recoded, 
                                   family = binomial)
summary(Economic_Issues_SysLive)

####################################################################

names(CAB_data_Recoded)
table(CAB_data_Recoded$country)

# Ensure the 'createdummies' package is installed
install.packages("createdummies")

# Load the createdummies library
library(createdummies)

# Create dummy variables for the 'country' variable
CAB_data_Recoded <- createdummies(CAB_data_Recoded, "country", sep = "_")

# View the updated dataset with the dummy variables
head(CAB_data_Recoded)

library(car)
CAB_data_Recoded$KZ = recode(CAB_data_Recoded$country, "'KZ' = 1; else = 0")
CAB_data_Recoded$KG = recode(CAB_data_Recoded$country, "'KG' = 1; else = 0")
CAB_data_Recoded$GA = recode(CAB_data_Recoded$country, "'GA' = 1; else = 0")

dom_issues_mod <- glm(Domestic_Issues ~  sm_critical_index + tv_critical_index + homophilous_index +
                            + age_n_sc + edu_n_sc + inc_n_sc + pol_interest_n_sc + KZ + KG, 
                               data = CAB_data_Recoded, 
                               family = binomial)
summary(dom_issues_mod)


pv_issues_mod <- glm(Political_Violence ~  sm_critical_index + tv_critical_index + homophilous_index +
                      + age_n_sc + edu_n_sc + inc_n_sc + pol_interest_n_sc + KZ + KG, 
                      data = CAB_data_Recoded, 
                      family = binomial)
summary(pv_issues_mod)

econ_issues_mod <- glm(Economic_Issues ~  sm_critical_index + tv_critical_index + homophilous_index +
                      + age_n_sc + edu_n_sc + inc_n_sc + pol_interest_n_sc + KZ + KG, 
                      data = CAB_data_Recoded, 
                      family = binomial)
summary(econ_issues_mod)

#Variable Construction###############################################
#Avoidance Index
library(psy)
avoidance_matrix = cbind(
  CAB_data_Recoded$avoidance_blocking_n_sc,
  CAB_data_Recoded$avoidance_unfriending_n_sc,
  CAB_data_Recoded$avoidance_leaving_group_n_sc,
  CAB_data_Recoded$avoidance_unsubscribing_n_sc)
cronbach(avoidance_matrix)

library(scales)
CAB_data_Recoded$avoidance_index = rescale((
  CAB_data_Recoded$avoidance_blocking_n_sc +
    CAB_data_Recoded$avoidance_unfriending_n_sc +
    CAB_data_Recoded$avoidance_leaving_group_n_sc +
    CAB_data_Recoded$avoidance_unsubscribing_n_sc), to = c(0, 1))
summary(CAB_data_Recoded$avoidance_index)

#Exposure to Disagreement Index
sm_disagreement_matrix = cbind(
  CAB_data_Recoded$sm_disagreement_politics_n_sc,
  CAB_data_Recoded$sm_disagreement_issues_n_sc,
  CAB_data_Recoded$sm_disagreement_news_n_sc)
cronbach(sm_disagreement_matrix)

CAB_data_Recoded$sm_disagreement_index = rescale((
  CAB_data_Recoded$sm_disagreement_politics_n_sc +
    CAB_data_Recoded$sm_disagreement_issues_n_sc +
    CAB_data_Recoded$sm_disagreement_news_n_sc), to = c(0, 1))
summary(CAB_data_Recoded$sm_disagreement_index)

#Social Media Critical Exposure/Engagement Index
sm_critical_matrix = cbind(
  CAB_data_Recoded$sm_critical_local_n_sc,
  CAB_data_Recoded$sm_critical_central_n_sc,
  CAB_data_Recoded$sm_engage_critical_n_sc)
cronbach(sm_critical_matrix)

CAB_data_Recoded$sm_critical_index = rescale((
  CAB_data_Recoded$sm_critical_local_n_sc +
    CAB_data_Recoded$sm_critical_central_n_sc +
    CAB_data_Recoded$sm_engage_critical_n_sc), to = c(0, 1))
summary(CAB_data_Recoded$sm_critical_index)

#Television Critical Exposure Index
tv_critical_matrix = cbind(
  CAB_data_Recoded$tv_critical_local_n_sc,
  CAB_data_Recoded$tv_critical_central_n_sc)
cronbach(tv_critical_matrix)

CAB_data_Recoded$tv_critical_index = rescale((
  CAB_data_Recoded$tv_critical_local_n_sc +
    CAB_data_Recoded$tv_critical_central_n_sc), to = c(0, 1))
summary(CAB_data_Recoded$tv_critical_index)

#Pro Russia Digital Exposure Index
dig_pro_russia_matrix = cbind(
  CAB_data_Recoded$sm_positive_local_n_sc,
  CAB_data_Recoded$sm_positive_central_n_sc,
  CAB_data_Recoded$sm_engage_supportive_sc,
  CAB_data_Recoded$clickable_state_n_sc,
  CAB_data_Recoded$clickable_russian_n_sc,
  CAB_data_Recoded$paid_posters_percentage_n_sc)
cronbach(dig_pro_russia_matrix)

CAB_data_Recoded$dig_pro_russia_index = rescale((
  CAB_data_Recoded$sm_positive_local_n_sc +
    CAB_data_Recoded$sm_positive_central_n_sc +
    CAB_data_Recoded$sm_engage_supportive_sc +
    CAB_data_Recoded$clickable_state_n_sc +
    CAB_data_Recoded$clickable_russian_n_sc +
    CAB_data_Recoded$paid_posters_percentage_n_sc), to = c(0, 1))
summary(CAB_data_Recoded$dig_pro_russia_index)

CAB_data_Recoded$sm_disagreement_politics_n_sc = rescale(CAB_data_Recoded$sm_disagreement_politics_n, to = c(1, 0))
CAB_data_Recoded$sm_disagreement_news_n_sc = rescale(CAB_data_Recoded$sm_disagreement_news_n, to = c(1, 0))
CAB_data_Recoded$sm_disagreement_issues_n_sc = rescale(CAB_data_Recoded$sm_disagreement_issues_n, to = c(1, 0))

#Homophilous Network
homophilous_matrix = cbind(
  CAB_data_Recoded$avoidance_blocking_n_sc,
  CAB_data_Recoded$avoidance_unfriending_n_sc,
  CAB_data_Recoded$avoidance_leaving_group_n_sc,
  CAB_data_Recoded$avoidance_unsubscribing_n_sc,
  CAB_data_Recoded$sm_disagreement_politics_n_sc,
  CAB_data_Recoded$sm_disagreement_issues_n_sc,
  CAB_data_Recoded$sm_disagreement_news_n_sc,
  CAB_data_Recoded$echo_chamber_n_sc)
cronbach(homophilous_matrix)

library(scales)
CAB_data_Recoded$homophilous_index = rescale((
  CAB_data_Recoded$avoidance_blocking_n_sc +
  CAB_data_Recoded$avoidance_unfriending_n_sc +
  CAB_data_Recoded$avoidance_leaving_group_n_sc +
  CAB_data_Recoded$avoidance_unsubscribing_n_sc +
  CAB_data_Recoded$sm_disagreement_politics_n_sc +
  CAB_data_Recoded$sm_disagreement_issues_n_sc +
  CAB_data_Recoded$sm_disagreement_news_n_sc +
  CAB_data_Recoded$echo_chamber_n_sc), to = c(0, 1))
summary(CAB_data_Recoded$homophilous_index)

hist(CAB_data_Recoded$homophilous_index)
#Exposure to Disagreement Index
sm_disagreement_matrix = cbind(
  CAB_data_Recoded$sm_disagreement_politics_n_sc,
  CAB_data_Recoded$sm_disagreement_issues_n_sc,
  CAB_data_Recoded$sm_disagreement_news_n_sc)
cronbach(sm_disagreement_matrix)

CAB_data_Recoded$sm_disagreement_index = rescale((
  CAB_data_Recoded$sm_disagreement_politics_n_sc +
    CAB_data_Recoded$sm_disagreement_issues_n_sc +
    CAB_data_Recoded$sm_disagreement_news_n_sc), to = c(0, 1))
summary(CAB_data_Recoded$sm_disagreement_index)

