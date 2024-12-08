library(scales)

#Recording Issues into Five Overarching Categories####

# Create the 'Political Violence' column

# Political instability, Corruption, Discrimination/ethnic or religious tensions####
CAB_data_dem$pol_issues <- ifelse(CAB_data_dem$q13_a %in% c(1, 13, 15) | CAB_data_dem$q13_b %in% c(1, 13, 15), 1, 0)

# Unemployment, Inflation/Prices, Wages/Pensions, Lack of opportunities, General economic situation, Access to basic needs (food, electricity, medicine, etc)####
CAB_data_dem$eco_issues <- ifelse(CAB_data_dem$q13_a %in% c(2, 3, 4, 6, 7, 14) | CAB_data_dem$q13_b %in% c(2, 3, 4, 6, 7, 14), 1, 0)

# Deterioration of the quality and access to education, Increase in crime/violence/concerns about personal safety, Emigration####
CAB_data_dem$soc_issues <- ifelse(CAB_data_dem$q13_a %in% c(8, 12, 19) | CAB_data_dem$q13_b %in% c(8, 12, 19), 1, 0)

# Terrorism, War/conflict in other countries, Unresolved territorial conflicts####
CAB_data_dem$sec_issues <- ifelse(CAB_data_dem$q13_a %in% c(10, 11, 18) | CAB_data_dem$q13_b %in% c(10, 11, 18), 1, 0)

# Deterioration of the environment, Infrastructure issues###
CAB_data_dem$env_issues <- ifelse(CAB_data_dem$q13_a %in% c(8, 16) | CAB_data_dem$q13_b %in% c(8,16), 1, 0)


table(CAB_data_dem$pol_issues)
table(CAB_data_dem$eco_issues)
table(CAB_data_dem$soc_issues)
table(CAB_data_dem$sec_issues)
table(CAB_data_dem$sec_issues)
table(CAB_data_dem$env_issues)



#Nationalistic Defenders ####
library(scales)
CAB_data_dem$participate_rally_nat_def = rescale(CAB_data_dem$participate_rally_n, to = c(1, 0)) # 1 means yes
CAB_data_dem$participate_meeting_nat_def = rescale(CAB_data_dem$participate_meeting_n, to = c(1, 0)) # 1 means yes
CAB_data_dem$participate_member_nat_def = rescale(CAB_data_dem$participate_member_n, to = c(1, 0)) # 1 means not at all

#nat_def <- CAB_data_dem[
#  CAB_data_dem$participate_rally_n == 1 , CAB_data_dem$participate_meeting == 1, CAB_data_dem$participate_member ==1,
#]
national_defenders = cbind(CAB_data_dem$system_capable_n_sc,
                           CAB_data_dem$system_proud_n_sc,
                           CAB_data_dem$system_deserves_n_sc,
                           CAB_data_dem$system_live_n_sc,
                           CAB_data_dem$sm_engage_supportive_sc,
                           CAB_data_dem$sm_critical_local_n_sc,
                           CAB_data_dem$sm_critical_central_n_sc,
                           CAB_data_dem$sm_positive_local_n_sc,
                           CAB_data_dem$sm_positive_central_n_sc,
                           CAB_data_dem$trust_central_n_sc,
                           CAB_data_dem$trust_local_n_sc,
                           CAB_data_dem$democracy_protests_n_sc,
                           CAB_data_dem$participate_rally_nat_def,
                           CAB_data_dem$participate_meeting_nat_def,
                           CAB_data_dem$participate_member_nat_def)
library(psy)
cronbach(national_defenders)


library(scales)
#creating an index of national defenders####
CAB_data_dem$national_defenders_index <- rescale(
  CAB_data_dem$system_capable_n_sc +
    CAB_data_dem$system_proud_n_sc +
    CAB_data_dem$system_deserves_n_sc +
    CAB_data_dem$system_live_n_sc +
    CAB_data_dem$sm_engage_supportive_sc +
    CAB_data_dem$sm_critical_local_n_sc +
    CAB_data_dem$sm_critical_central_n_sc +
    CAB_data_dem$sm_positive_local_n_sc +
    CAB_data_dem$sm_positive_central_n_sc +
    CAB_data_dem$trust_central_n_sc +
    CAB_data_dem$trust_local_n_sc +
    CAB_data_dem$democracy_protests_n_sc +
    CAB_data_dem$participate_rally_nat_def +
    CAB_data_dem$participate_meeting_nat_def +
    CAB_data_dem$participate_member_nat_def,
  to = c(0, 1)
)
summary(CAB_data_dem$national_defenders_index)
hist(CAB_data_dem$national_defenders_index)

# Recoding Political Disucssion with Family and Friends #
CAB_data_dem$pol_dis <- as.numeric(CAB_data_dem$pol_discuss)


library(scales)
# Step 5: Rescale the numeric values to the desired direction
CAB_data_dem$pol_dis_silob <- rescale(CAB_data_dem$pol_discuss_n, to = c(0, 1)) #1 means less
CAB_data_dem$trust_central_silob = rescale(CAB_data_dem$trust_central_n_sc, to = c(0, 1)) #1 means less
CAB_data_dem$trust_local_silob = rescale(CAB_data_dem$trust_local_n_sc, to = c(0, 1)) #1 means less
CAB_data_dem$sm_disagreement_politics_silob = rescale(CAB_data_dem$sm_disagreement_politics_n_sc, to = c(1, 0)) #see disagreement? #1 means very often
CAB_data_dem$sm_engage_friends_silob <- rescale(CAB_data_dem$sm_engage_friends_n, to = c(0, 1)) # 1 meas less
CAB_data_dem$sm_engage_groups_silob = rescale(CAB_data_dem$sm_engage_groups_n, to = c(0, 1)) # 1 meas less
CAB_data_dem$sm_engage_post_silob = rescale(CAB_data_dem$sm_engage_post_n, to = c(0, 1)) # 1 meas less
CAB_data_dem$sm_engage_critical_silob = rescale(CAB_data_dem$sm_engage_critical_n, to = c(0, 1)) # 1 meas less
CAB_data_dem$sm_engage_supportive_silob = rescale(CAB_data_dem$sm_engage_supportive_n, to = c(0, 1)) # 1 meas less
CAB_data_dem$sm_engage_offline_silob = rescale(CAB_data_dem$sm_engage_offline_n, to = c(0, 1)) # 1 meas less
CAB_data_dem$participate_rally_sil_obs = rescale(CAB_data_dem$participate_rally_n, to = c(0, 1))   # 1 means No

library(psy)
sil_obs <- cbind(
  CAB_data_dem$pol_dis_silob,
  CAB_data_dem$sm_engage_friends_silob,
  CAB_data_dem$sm_engage_groups_silob,
  CAB_data_dem$sm_engage_post_silob,
  CAB_data_dem$sm_engage_supportive_silob,
  CAB_data_dem$sm_engage_offline_silob,
  CAB_data_dem$participate_rally_sil_obs)

cronbach(sil_obs)

#sclae if item deleted
#install.packages('psych')



#if some items are negatively correlated
#alpha(sil_obs, check.keys = TRUE)



library(scales)
###Creating an index of "silent observers"##
CAB_data_dem$sil_obs_index <- scales::rescale(
  CAB_data_dem$pol_dis_silob +
    # CAB_data_dem$trust_central_silob +
    # CAB_data_dem$trust_local_silob +
    # CAB_data_dem$sm_disagreement_politics_silob +
    CAB_data_dem$sm_engage_friends_silob +
    CAB_data_dem$sm_engage_groups_silob +
    CAB_data_dem$sm_engage_post_silob +
    CAB_data_dem$sm_engage_critical_silob +
    CAB_data_dem$sm_engage_supportive_silob +
    CAB_data_dem$sm_engage_offline_silob +
    CAB_data_dem$participate_rally_sil_obs,
  to = c(0, 1)
)
summary(CAB_data_dem$sil_obs_index)
hist(CAB_data_dem$sil_obs_index)

library(scales)
#Recoding for SM Warriors#
CAB_data_dem$participate_rally_smw =  rescale(CAB_data_dem$participate_rally_n, to = c(0, 1))   # 1 means No
table(CAB_data_dem$participate_rally_smw)

CAB_data_dem$participate_meeting_smw =  rescale(CAB_data_dem$participate_meeting_n, to = c(0, 1)) # 1 means No
table(CAB_data_dem$participate_meeting_smw)

CAB_data_dem$participate_volunteer_smw =  rescale(CAB_data_dem$participate_volunteer_n, to = c(0, 1)) # 1 means No
table(CAB_data_dem$participate_volunteer_smw)

CAB_data_dem$participate_member_smw =  rescale(CAB_data_dem$participate_member_n, to = c(0, 1)) # 1 means No
table(CAB_data_dem$participate_member_smw)

CAB_data_dem$participate_community_smw =   rescale(CAB_data_dem$participate_community_n, to = c(0, 1)) # 1 means No
table(CAB_data_dem$participate_community_smw)

CAB_data_dem$participate_contact_smw =  rescale(CAB_data_dem$participate_contact_n, to = c(0, 1)) # 1 means No
table(CAB_data_dem$participate_contact_smw)

CAB_data_dem$participate_protest_smw =  rescale(CAB_data_dem$participate_protest_n, to = c(0, 1)) # 1 means No
table(CAB_data_dem$participate_protest_smw)

CAB_data_dem$participate_vote_smw =  rescale(CAB_data_dem$participate_vote_n, to = c(0, 1)) # 1 means No
table(CAB_data_dem$participate_vote_smw)

CAB_data_dem$sm_engage_critical_smw =  rescale(CAB_data_dem$sm_engage_critical_n, to = c(1, 0)) # 1 means more
CAB_data_dem$sm_engage_friends_smw =  rescale(CAB_data_dem$sm_engage_friends_n, to = c(1, 0)) # 1 means more
CAB_data_dem$sm_engage_groups_smw =  rescale(CAB_data_dem$sm_engage_groups_n, to = c(1, 0)) # 1 means more
CAB_data_dem$sm_engage_post_smw = rescale(CAB_data_dem$sm_engage_post_n, to = c(1, 0)) # 1 means more

#SM Warriors
sm_warriors <- cbind(
  CAB_data_dem$sm_engage_friends_smw,
  CAB_data_dem$sm_engage_groups_smw,
  CAB_data_dem$sm_engage_post_smw)

cronbach(sm_warriors)





library(scales)
# Creating an Index
CAB_data_dem$smwar_index = rescale(
  CAB_data_dem$sm_engage_critical_smw +
    CAB_data_dem$sm_engage_friends_smw + 
    CAB_data_dem$sm_engage_groups_smw + 
    CAB_data_dem$sm_engage_post_smw + 
    # CAB_data_dem$participate_rally_smw + 
    CAB_data_dem$participate_protest_smw, 
  # CAB_data_dem$participate_meeting_smw + 
  # CAB_data_dem$participate_volunteer_smw + 
  # CAB_data_dem$participate_member_smw + 
  # CAB_data_dem$participate_contact_smw + 
  # CAB_data_dem$participate_vote_smw, 
  to = c(0, 1)  # Rescale to range [0, 1]
)

summary(CAB_data_dem$smwar_index)
hist(CAB_data_dem$smwar_index)

# # Dillusioned Reformists Index ######
# library(scales)
CAB_data_dem$system_capable_rescaled =  rescale(CAB_data_dem$system_capable_n_sc, to = c(0, 1)) #1 towards disagreement
CAB_data_dem$participate_rally_dilref =  rescale(CAB_data_dem$participate_rally_n, to = c(1, 0)) #1 Yes

dil_ref = cbind(
CAB_data_dem$system_capable_rescaled,
CAB_data_dem$system_hurdles_participate_n_sc,
CAB_data_dem$trust_western_n_sc,
CAB_data_dem$participate_rally_dilref)
cronbach(dil_ref)
# 
# alpha(dil_ref)
# alpha(dil_ref, check.keys = TRUE)
# table(dil_ref)
# 
# 
# #creating an index of dillusioned reformists index ###
# CAB_data_dem$dilref_index <- rescale(
#   CAB_data_dem$sm_engage_critical_n_sc +
#     CAB_data_dem$system_capable_rescaled +
#     CAB_data_dem$system_hurdles_participate_n_sc +
#     CAB_data_dem$trust_western_n_sc +
#     CAB_data_dem$participate_rally_dilref,
#   to = c(0, 1)
# )


library(scales)
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
  CAB_data_dem$trust_EU_negative,
  CAB_data_dem$pol_news_tv_n_sc)
cronbach(idconsisten_conserv)


library(scales)
#Creating an index of ideologically consistent conservative index###
CAB_data_dem$idconsisten_conserv_index <- rescale(
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
    CAB_data_dem$trust_EU_negative +
    CAB_data_dem$pol_news_tv_n_sc,
  to = c(0, 1)
)

summary(CAB_data_dem$idconsisten_conserv_index)
hist(CAB_data_dem$idconsisten_conserv_index)

#Surveillance-Averse Libertarians####
CAB_data_dem$tracking_central_sur_ave_lib =  rescale(CAB_data_dem$tracking_central_n, to = c(1, 0)) #1 means not comfortable at all
CAB_data_dem$tracking_local_sur_ave_lib =  rescale(CAB_data_dem$tracking_local_n, to = c(1, 0)) #1 means not comfortable at all
CAB_data_dem$tracking_companies_sur_ave_lib =  rescale(CAB_data_dem$tracking_companies_n, to = c(1, 0)) #1 means not comfortable at all
CAB_data_dem$sm_engage_friends_sur_ave_lib =  rescale(CAB_data_dem$sm_engage_friends_n, to = c(0, 1)) # 1 means less engagement
CAB_data_dem$sm_engage_groups_sur_ave_lib = rescale(CAB_data_dem$sm_engage_groups_n, to = c(0, 1)) # 1 means less engagement
CAB_data_dem$vpn_use_ser_ave_lib <- as.numeric(CAB_data_dem$vpn_use_Yes)

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
summary(CAB_data_dem$surv_averse_lib_index)
hist(CAB_data_dem$surv_averse_lib_index)

# The Selective Avoiders ####
selective_avoiders = cbind(
  CAB_data_dem$avoidance_unfriending_n_sc,
  CAB_data_dem$avoidance_blocking_n_sc,
  CAB_data_dem$sm_disagreement_politics_n_sc,
  CAB_data_dem$sm_disagreement_news_n_sc,
  CAB_data_dem$sm_disagreement_issues_n_sc)
cronbach(selective_avoiders)


library(Rcmdr)



#create an index of  Polarized Echo Chamber#
CAB_data_dem$polarized_echo_chamber_index <- rescale(
  CAB_data_dem$avoidance_unfriending_n_sc +
    CAB_data_dem$avoidance_blocking_n_sc +
    # CAB_data_dem$echo_chamber_n_sc +
    CAB_data_dem$sm_disagreement_politics_n_sc +
    CAB_data_dem$sm_disagreement_news_n_sc +
    CAB_data_dem$sm_disagreement_issues_n_sc,
  to = c(0, 1)
)


#Political Social Media Index##
pol_sm_index = cbind(
  CAB_data_dem$sm_engage_friends_n_sc,
  CAB_data_dem$sm_engage_groups_n_sc,
  CAB_data_dem$sm_engage_post_n_sc,
  CAB_data_dem$sm_engage_critical_n_sc,
  CAB_data_dem$sm_engage_supportive_sc,
  CAB_data_dem$sm_engage_offline_sc)
cronbach(pol_sm_index)

CAB_data_dem$pol_sm_index <- rescale(
  CAB_data_dem$sm_engage_friends_n_sc +
  CAB_data_dem$sm_engage_groups_n_sc +
  CAB_data_dem$sm_engage_post_n_sc +
  CAB_data_dem$sm_engage_critical_n_sc +
  CAB_data_dem$sm_engage_supportive_sc +
  CAB_data_dem$sm_engage_offline_sc,
  to = c(1, 0)
)


#Social Media Use (Platforms)####
#General Index
sm_platform_index = cbind(
  CAB_data_dem$facebook_n_sc,
  CAB_data_dem$vkontakte_n_sc,
  CAB_data_dem$instagram_n_sc,
  CAB_data_dem$tiktok_n_sc,
  CAB_data_dem$twitter_n_sc,
  CAB_data_dem$youtube_n_sc,
  CAB_data_dem$whatsapp_n_sc, 
  CAB_data_dem$telegram_n_sc)
cronbach(sm_platform_index)


CAB_data_dem$sm_platform_index <- rescale(
  CAB_data_dem$facebook_n_sc +
  CAB_data_dem$vkontakte_n_sc +
  CAB_data_dem$instagram_n_sc +
  CAB_data_dem$tiktok_n_sc +
  CAB_data_dem$twitter_n_sc +
  CAB_data_dem$youtube_n_sc +
  CAB_data_dem$whatsapp_n_sc + 
  CAB_data_dem$telegram_n_sc,
  to = c(0, 1)
)


#Algorithm-Driven Social Media Index ###
CAB_data_dem$algdriven_sm_index <- rescale(
  CAB_data_dem$facebook_n_sc +
    CAB_data_dem$vkontakte_n_sc +
    CAB_data_dem$instagram_n_sc +
    CAB_data_dem$tiktok_n_sc +
    CAB_data_dem$twitter_n_sc +
    CAB_data_dem$youtube_n_sc +
    CAB_data_dem$whatsapp_n_sc + 
    CAB_data_dem$telegram_n_sc,
  to = c(0, 1)
)


#Social Network Driven SM Index###
CAB_data_dem$socnetdriven_sm_index <- rescale(
  CAB_data_dem$facebook_n_sc +
    CAB_data_dem$vkontakte_n_sc +
    CAB_data_dem$instagram_n_sc +
    CAB_data_dem$tiktok_n_sc +
    CAB_data_dem$twitter_n_sc +
    CAB_data_dem$youtube_n_sc +
    CAB_data_dem$whatsapp_n_sc + 
    CAB_data_dem$telegram_n_sc,
  to = c(0, 1)
)


#############################################################
#Western Sources for News about Political Event###

west_src_pol_events = cbind(
  CAB_data_dem$pol_news_facebook_n_sc,
  CAB_data_dem$pol_news_twitter_n_sc)
  # CAB_data_dem$pol_news_tiktok_n_sc)
cronbach(west_src_pol_events)

CAB_data_dem$west_src_pol_events <- rescale(
  CAB_data_dem$pol_news_facebook_n_sc +
  CAB_data_dem$pol_news_twitter_n_sc, to = c(1, 0)
)



#Legacy Sources for News about Political Event###

#Digital Sources for News about Political Event###
digital_src_pol_news <- rescale(
  CAB_data_dem$pol_news_facebook_n_sc +
    CAB_data_dem$pol_news_twitter_n_sc + 
    CAB_data_dem$pol_news_tiktok_n_sc + 
    CAB_data_dem$pol_news_vkontakte_n_sc +
    CAB_data_dem$pol_news_odnoklassniki_n_sc,
  to = c(1, 0)
)


library(scales)
#Trust on Legacy Media#
trust_legacy <- rescale(
    CAB_data_dem$trust_state_n_sc +
    CAB_data_dem$trust_russian_media_n_sc,
  to = c(1, 0)
)

#Trust on Western Media#
trust_western_n_sc = CAB_data_dem$trust_western_n_sc

#Trust on Russian Media#
CAB_data_dem$trust_russia_both <- rescale(
  CAB_data_dem$trust_vkontakte_n_sc +
    CAB_data_dem$trust_russian_media_n_sc,
  to = c(1, 0)
)




##################################################
# Engaged Extroverts Index (High political engagement)
# engaged_extroverts <- cbind(
#   CAB_data_dem$participate_rally_nat_def,
#   CAB_data_dem$participate_meeting_nat_def,
#   CAB_data_dem$sm_engage_post_n_sc,
#   CAB_data_dem$sm_engage_groups_n_sc)
# 
# # Check reliability
# cronbach(engaged_extroverts)


#Cynics Index (Low trust)
cynics <- cbind(
  1 - CAB_data_dem$trust_central_n_sc,  # Reversed trust scores
  1 - CAB_data_dem$trust_local_n_sc,
  1 - CAB_data_dem$trust_state_n_sc
)
cronbach(cynics)
# Institutional Believers Index (High trust) - Reverse of Cynics






# Optimists Index (Positive future outlook)
optimists <- cbind(
  CAB_data_dem$trust_central_n_sc,
  CAB_data_dem$sm_positive_local_n_sc,
  CAB_data_dem$sm_positive_central_n_sc,
  CAB_data_dem$sm_engage_supportive_sc,
  CAB_data_dem$trust_state_n_sc)       # Positive social media outlook

cronbach(optimists)

cor(CAB_data_dem[,c("pol_issues","national_defenders_index","sil_obs_index","surv_averse_lib_index","smwar_index",
                    "idconsisten_conserv_index")], 
    use="complete",)

library(Hmisc, pos=19)
rcorr.adjust(CAB_data_dem[,c("env_issues","national_defenders_index","sil_obs_index","surv_averse_lib_index","smwar_index",
                             "idconsisten_conserv_index")], type="pearson", use="complete")

table(CAB_data_dem$pol_issues)
table(CAB_data_dem$eco_issues)
table(CAB_data_dem$soc_issues)
table(CAB_data_dem$sec_issues)
table(CAB_data_dem$sec_issues)
table(CAB_data_dem$env_issues)

