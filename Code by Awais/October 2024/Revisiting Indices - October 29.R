#Critical Social Media Index
critical_social_media = cbind(
  CAB_data_dem$sm_critical_local_n_sc,
  CAB_data_dem$sm_critical_central_n_sc)
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

#Positive SM Media Flow
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
  CAB_data_dem$twitter_n_sc,
  CAB_data_dem$youtube_n_sc,
  CAB_data_dem$whatsapp_n_sc, 
  CAB_data_dem$telegram_n_sc)
cronbach(sm_platform_index)

gen_sm_index <- rescale(
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
library(psy)
cronbach(national_defenders)


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
