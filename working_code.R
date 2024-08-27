names(CAB_data)

library(data.table)
CAB_data_dem = rbindlist(list(GA_data,KG_data,KZ_data), fill = TRUE)
table(CAB_data_dem$country)


mod_econ = glm(General_economic_situation ~ tiktok_n_sc + facebook_n_sc + instagram_n_sc + twitter_n_sc + vkontakte_n_sc + 
                 age_n_sc + urbanicity_Village + gender_Male + edu_n_sc + inc_n_sc, data = CAB_data, family = binomial)
summary(mod_econ)

ai_model_1 = lm(AI_n_sc ~ tiktok_n_sc + facebook_n_sc + instagram_n_sc + twitter_n_sc + vkontakte_n_sc + media_trust_index + tracking_index +
                  age_n_sc + urbanicity_Village + gender_Male + edu_n_sc + inc_n_sc + country_KZ + country_KG, data = CAB_data_merge, weights = weight)
summary(ai_model_1)

mod_econ = glm(General_economic_situation ~ tiktok_n_sc + 
                 age_n_sc + urbanicity_Village + gender_Male + edu_n_sc + inc_n_sc, data = CAB_data, family = binomial)
summary(mod_econ)

mod_econ = glm(General_economic_situation_Combined ~ news_balance_n_sc + sm_critical_central_n_sc + sm_critical_local_n_sc + 
               + sm_positive_local_n_sc + sm_positive_central_n_sc + tv_critical_local_n_sc + tv_critical_central_n_sc + echo_chamber_n_sc + trust_western_n_sc + trust_state_n_sc
                + pol_interest_n_sc + age_n_sc + urbanicity_Village + gender_Male + edu_n_sc + inc_n_sc 
               + countryKG + countryKZ + countryTJ + countryUZ, data = CAB_data, family = binomial)
summary(mod_econ)

mod_corrupt = glm(Terrorism_Combined ~ avoidance_index + sm_disagreement_index
                   + gender_Male + age_n_sc + age_n_sc + urbanicity_Village + edu_n_sc + inc_n_sc
               , data = CAB_data, family = binomial)
summary(mod_corrupt)

summary(lm(CAB_data$avoidance_index ~ CAB_data$inc_n_sc))

#Avoidance Index
library(psy)
avoidance_matrix = cbind(
  CAB_data$avoidance_blocking_n_sc,
  CAB_data$avoidance_unfriending_n_sc,
  CAB_data$avoidance_leaving_group_n_sc,
  CAB_data$avoidance_unsubscribing_n_sc)
cronbach(avoidance_matrix)

CAB_data$avoidance_index = rescale((
  CAB_data$avoidance_blocking_n_sc +
  CAB_data$avoidance_unfriending_n_sc +
  CAB_data$avoidance_leaving_group_n_sc +
  CAB_data$avoidance_unsubscribing_n_sc), to = c(0, 1))
summary(CAB_data$avoidance_index)

sm_disagreement_matrix = cbind(
  CAB_data$sm_disagreement_politics_n_sc,
  CAB_data$sm_disagreement_issues_n_sc,
  CAB_data$sm_disagreement_news_n_sc)
cronbach(sm_disagreement_matrix)

CAB_data$sm_disagreement_index = rescale((
  CAB_data$sm_disagreement_politics_n_sc +
  CAB_data$sm_disagreement_issues_n_sc +
  CAB_data$sm_disagreement_news_n_sc), to = c(0, 1))
summary(CAB_data$sm_disagreement_index)


table(CAB_data$trust_vkontakte)

sm_disagreement_politics_n_sc <.0001                                            <.0001                    <.0001                     
sm_disagreement_news_n_sc     <.0001              <.0001                                                  <.0001                     
sm_disagreement_issues_n_sc   <.0001              <.0001                        <.0001                                               
avoidance_blocking_n_sc       <.0001              <.0001                        <.0001                    <.0001                     
avoidance_leaving_group_n_sc  <.0001              <.0001                        <.0001                    <.0001                     
avoidance_unfriending_n_sc    <.0001              <.0001                        <.0001                    <.0001                     
avoidance_unsubscribing_n_sc 


library(interactions)
# Plot the interaction effect with sm_critical_central_n_sc on the x-axis
interact_plot(mod_econ, pred = sm_critical_central_n_sc, modx = news_balance_n_sc, 
              plot.points = TRUE, interval = TRUE, int.width = 0.95, 
              x.label = "SM Critical Central", y.label = "Predicted Probability of General Economic Situation",
              main.title = "Interaction Effect of SM Critical Central and News Balance")


names(CAB_data)

# Create dummy variables
dummies <- model.matrix(~ country - 1, data = CAB_data)

# Convert to a data frame and add to CAB_data
dummy_data <- as.data.frame(dummies)
CAB_data <- cbind(CAB_data, dummy_data)
names(CAB_data)

library(Rcmdr)

library(Hmisc)
rcorr.adjust(CAB_data[,c("Corruption_Combined","sm_disagreement_politics_n_sc","sm_disagreement_news_n_sc",                                                       
                         "sm_disagreement_issues_n_sc", "avoidance_blocking_n_sc","avoidance_leaving_group_n_sc",
                         "avoidance_unfriending_n_sc","avoidance_unsubscribing_n_sc",
                         "echo_chamber_n_sc")], type="pearson", use="complete")

names(CAB_data)
