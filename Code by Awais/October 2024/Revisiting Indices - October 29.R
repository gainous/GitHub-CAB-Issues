save.image("~/GitHub/GitHub-CAB-Issues/CAB_data.RData")

# Political instability, Corruption, Discrimination/ethnic or religious tensions####
pol_issues <- ifelse(CAB_data_dem$q13_a %in% c(1, 13, 15) | CAB_data_dem$q13_b %in% c(1, 13, 15), 1, 0)

# Unemployment, Inflation/Prices, Wages/Pensions, Lack of opportunities, General economic situation, Access to basic needs (food, electricity, medicine, etc)####
eco_issues <- ifelse(CAB_data_dem$q13_a %in% c(2, 3, 4, 6, 7, 14) | CAB_data_dem$q13_b %in% c(2, 3, 4, 6, 7, 14), 1, 0)

# Deterioration of the quality and access to education, Increase in crime/violence/concerns about personal safety, Emigration####
soc_issues <- ifelse(CAB_data_dem$q13_a %in% c(8, 12, 19) | CAB_data_dem$q13_b %in% c(8, 12, 19), 1, 0)

# Terrorism, War/conflict in other countries, Unresolved territorial conflicts####
sec_issues <- ifelse(CAB_data_dem$q13_a %in% c(10, 11, 18) | CAB_data_dem$q13_b %in% c(10, 11, 18), 1, 0)

# Deterioration of the environment, Infrastructure issues###
env_issues <- ifelse(CAB_data_dem$q13_a %in% c(8, 16) | CAB_data_dem$q13_b %in% c(8,16), 1, 0)

table(pol_issues)
table(eco_issues)
table(soc_issues)
table(sec_issues)
table(sec_issues)
table(env_issues)


#Critical Social Media Index
critical_social_media = cbind(
  CAB_data_dem$sm_critical_local_n_sc,
  CAB_data_dem$sm_critical_central_n_sc)

library(psy)
cronbach(critical_social_media)

library(scales)
critical_social_media = rescale(
  CAB_data_dem$sm_critical_local_n_sc+
  CAB_data_dem$sm_critical_central_n_sc,
  to = c(1, 0)
)



#Critical TV Index
critical_tv_index = cbind(
  CAB_data_dem$tv_critical_local_n_sc,
  CAB_data_dem$tv_critical_central_n_sc)
cronbach(critical_tv_index)

critical_tv_index = rescale(
  CAB_data_dem$tv_critical_local_n_sc +
  CAB_data_dem$tv_critical_central_n_sc,
  to = c(1, 0)
)

#Legacy Media Exposure for Political News
CAB_data_dem$pol_news_tv_n #as (1,0)

#Social Media Exposure for Political News
sm_pol_news = rescale(
  CAB_data_dem$pol_news_facebook_n_sc +
  CAB_data_dem$pol_news_vkontakte_n_sc +
  CAB_data_dem$pol_news_tiktok_n_sc +
  CAB_data_dem$pol_news_twitter_n_sc +
  CAB_data_dem$pol_news_odnoklassniki_n_sc,
  to = c(1, 0)
)

#Positive SM Media
sm_positive = rescale(
  CAB_data_dem$sm_positive_local_n_sc +
  CAB_data_dem$sm_positive_central_n_sc,
  to = c(1, 0)
)


#Social Media Engagement Index##
sm_engage_index = cbind(
  CAB_data_dem$sm_engage_friends_n_sc,
  CAB_data_dem$sm_engage_groups_n_sc,
  CAB_data_dem$sm_engage_post_n_sc,
  CAB_data_dem$sm_engage_critical_n_sc,
  CAB_data_dem$sm_engage_supportive_sc,
  CAB_data_dem$sm_engage_offline_sc)
cronbach(sm_engage_index)

sm_engage_index = rescale(
  CAB_data_dem$sm_engage_friends_n_sc + 
  CAB_data_dem$sm_engage_groups_n_sc +
  CAB_data_dem$sm_engage_post_n_sc +
  CAB_data_dem$sm_engage_critical_n_sc +
  CAB_data_dem$sm_engage_supportive_sc +
  CAB_data_dem$sm_engage_offline_sc,
  to = c(1, 0)
)

#Digital Sources for Political News###
digital_src_pol_news <- rescale(
  CAB_data_dem$pol_news_facebook_n_sc +
    CAB_data_dem$pol_news_twitter_n_sc + 
    CAB_data_dem$pol_news_tiktok_n_sc + 
    CAB_data_dem$pol_news_vkontakte_n_sc +
    CAB_data_dem$pol_news_odnoklassniki_n_sc,
  to = c(1, 0)
)

#General Socail Media Index
gen_sm_index = cbind(
  CAB_data_dem$facebook_n_sc,
  CAB_data_dem$vkontakte_n_sc,
  CAB_data_dem$instagram_n_sc,
  CAB_data_dem$tiktok_n_sc,
  CAB_data_dem$twitter_n_sc)
cronbach(gen_sm_index)

gen_sm_index <- rescale(
  CAB_data_dem$facebook_n_sc +
  CAB_data_dem$vkontakte_n_sc +
  CAB_data_dem$instagram_n_sc +
  CAB_data_dem$tiktok_n_sc +
  CAB_data_dem$twitter_n_sc,
  to = c(0, 1)
)



#Trust on Legacy Media#
trust_legacy <- rescale(
  CAB_data_dem$trust_state_n_sc +
    CAB_data_dem$trust_russian_media_n_sc,
  to = c(1, 0)
)

#Trust on Western Media#
trust_western_n_sc = CAB_data_dem$trust_western_n_sc

#Trust on Russian Media#
trust_russia_both <- rescale(
  CAB_data_dem$trust_vkontakte_n_sc +
    CAB_data_dem$trust_russian_media_n_sc,
  to = c(1, 0)
)



#Algorithm-Driven Social Media Index ###
algdriven_sm_index <- rescale(
  CAB_data_dem$instagram_n_sc +
    CAB_data_dem$tiktok_n_sc +
    CAB_data_dem$twitter_n_sc,
  to = c(0, 1)
)


#Social Network Driven SM Index###
CAB_data_dem$socnetdriven_sm_index <- rescale(
  CAB_data_dem$facebook_n_sc +
    CAB_data_dem$vkontakte_n_sc,
  to = c(0, 1)
)


west_src_pol_events = cbind(
  CAB_data_dem$pol_news_facebook_n_sc,
  CAB_data_dem$pol_news_twitter_n_sc)
# CAB_data_dem$pol_news_tiktok_n_sc)
cronbach(west_src_pol_events)

west_src_pol_events <- rescale(
  CAB_data_dem$pol_news_facebook_n_sc +
    CAB_data_dem$pol_news_twitter_n_sc, to = c(1, 0)
)

##############################################
national_defenders = cbind(CAB_data_dem$system_capable_n_sc,
                           CAB_data_dem$system_proud_n_sc,
                           CAB_data_dem$system_deserves_n_sc,
                           CAB_data_dem$system_live_n_sc,
                           CAB_data_dem$trust_central_n_sc,
                           CAB_data_dem$trust_local_n_sc,
                           CAB_data_dem$democracy_protests_n_sc,
                           CAB_data_dem$participate_rally_nat_def,
                           CAB_data_dem$participate_meeting_nat_def,
                           CAB_data_dem$participate_member_nat_def)

cronbach(national_defenders)

CAB_data_dem$participate_rally_nat_def = rescale(CAB_data_dem$participate_rally_n, to = c(1, 0)) # 1 means yes
CAB_data_dem$participate_meeting_nat_def = rescale(CAB_data_dem$participate_meeting_n, to = c(1, 0)) # 1 means yes
CAB_data_dem$participate_member_nat_def = rescale(CAB_data_dem$participate_member_n, to = c(1, 0)) # 1 means not at all

national_defenders = rescale(CAB_data_dem$system_capable_n_sc +
                           CAB_data_dem$system_proud_n_sc +
                           CAB_data_dem$system_deserves_n_sc +
                           CAB_data_dem$system_live_n_sc +
                           CAB_data_dem$trust_central_n_sc +
                           CAB_data_dem$trust_local_n_sc +
                           CAB_data_dem$democracy_protests_n_sc +
                           CAB_data_dem$participate_rally_nat_def +
                           CAB_data_dem$participate_meeting_nat_def +
                           CAB_data_dem$participate_member_nat_def,
                           to = c(1, 0)
)

# Ideologically Consistent Conservatives Index ####
CAB_data_dem$trust_US_negative =  rescale(CAB_data_dem$trust_US_n, to = c(0, 1)) # 1 means not at all
CAB_data_dem$trust_EU_negative =  rescale(CAB_data_dem$trust_EU_n, to = c(0, 1)) # 1 means not at all

idconsisten_conserv = cbind(
  CAB_data_dem$trust_central_n_sc,
  CAB_data_dem$trust_local_n_sc,
  CAB_data_dem$system_capable_n_sc,
  CAB_data_dem$system_proud_n_sc,
  CAB_data_dem$trust_state_n_sc,
  CAB_data_dem$system_deserves_n_sc,
  CAB_data_dem$system_live_n_sc,
  CAB_data_dem$trust_US_negative,
  CAB_data_dem$trust_china_n_sc,
  CAB_data_dem$trust_russia_n_sc,
  CAB_data_dem$trust_EU_negative)
  # CAB_data_dem$pol_news_tv_n_sc)
cronbach(idconsisten_conserv)


idconsisten_conserv = rescale(
  CAB_data_dem$trust_central_n_sc +
  CAB_data_dem$trust_local_n_sc +
  CAB_data_dem$system_capable_n_sc +
  CAB_data_dem$system_proud_n_sc +
  CAB_data_dem$trust_state_n_sc +
  CAB_data_dem$system_deserves_n_sc +
  CAB_data_dem$system_live_n_sc +
  CAB_data_dem$trust_US_negative +
  CAB_data_dem$trust_china_n_sc +
  CAB_data_dem$trust_russia_n_sc +
  CAB_data_dem$trust_EU_negative,
to = c(1, 0)
)




CAB_data_dem$tracking_central_sur_ave_lib =  rescale(CAB_data_dem$tracking_central_n, to = c(1, 0)) #1 means not comfortable at all
CAB_data_dem$tracking_local_sur_ave_lib =  rescale(CAB_data_dem$tracking_local_n, to = c(1, 0)) #1 means not comfortable at all
CAB_data_dem$tracking_companies_sur_ave_lib =  rescale(CAB_data_dem$tracking_companies_n, to = c(1, 0)) #1 means not comfortable at all
CAB_data_dem$sm_engage_friends_sur_ave_lib =  rescale(CAB_data_dem$sm_engage_friends_n, to = c(0, 1)) # 1 means less engagement
CAB_data_dem$sm_engage_groups_sur_ave_lib = rescale(CAB_data_dem$sm_engage_groups_n, to = c(0, 1)) # 1 means less engagement


surv_averse_lib = cbind(
  CAB_data_dem$tracking_central_sur_ave_lib,
  CAB_data_dem$tracking_local_sur_ave_lib,
  # CAB_data_dem$vpn_use_ser_ave_lib,
  CAB_data_dem$tracking_companies_sur_ave_lib,
  CAB_data_dem$sm_engage_friends_sur_ave_lib,
  CAB_data_dem$sm_engage_groups_sur_ave_lib)
# CAB_data_dem$news_balance_n_sc,
# CAB_data_dem$trust_western_n_sc)
cronbach(surv_averse_lib)



#Creating an index of Surveillance-Averse Libertarian###
CAB_data_dem$surv_averse_lib_index <- rescale(
  CAB_data_dem$tracking_central_sur_ave_lib +
    CAB_data_dem$tracking_local_sur_ave_lib +
    # CAB_data_dem$vpn_use_ser_ave_lib +
    CAB_data_dem$tracking_companies_sur_ave_lib +
    CAB_data_dem$sm_engage_friends_sur_ave_lib,
  # CAB_data_dem$sm_engage_groups_sur_ave_lib +
  # CAB_data_dem$trust_western_n_sc,
  to = c(0, 1)
)




for (dep_var in dependent_vars) {
  formula <- as.formula(paste(dep_var, "~ facebook_n_sc + instagram_n_sc + twitter_n_sc + vkontakte_n_sc + 
                               age_n_sc + gender_Male + urbanicity_City + edu_n_sc + inc_n_sc"))
  models[[dep_var]] <- glm(formula, data = CAB_data_dem, family = binomial)
}
