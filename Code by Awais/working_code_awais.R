#Script File = GitHub-CAB-Issues\Code by Awais\working_code_awais
#Data File = > save.image("~/GitHub/GitHub-CAB-Issues/CAB_data.RData")



#Recording Issues into Five Overarching Categories####

# Create the 'Political Violence' column

##Political instability, Corruption, Discrimination/ethnic or religious tensions####
CAB_data_dem$pol_issues <- ifelse(CAB_data_dem$q13_a %in% c(1, 13, 15) | CAB_data_dem$q13_b %in% c(1, 13, 15), 1, 0)
table(CAB_data_dem$pol_issues)

##Unemployment, Inflation/Prices, Wages/Pensions, Lack of opportunities, General economic situation, Access to basic needs (food, electricity, medicine, etc)####
CAB_data_dem$eco_issues <- ifelse(CAB_data_dem$q13_a %in% c(2, 3, 4, 6, 7, 14) | CAB_data_dem$q13_b %in% c(2, 3, 4, 6, 7, 14), 1, 0)
table(CAB_data_dem$eco_issues)

##Deterioration of the quality and access to education, Increase in crime/violence/concerns about personal safety, Emigration####
CAB_data_dem$soc_issues <- ifelse(CAB_data_dem$q13_a %in% c(8, 12, 19) | CAB_data_dem$q13_b %in% c(8, 12, 19), 1, 0)
table(CAB_data_dem$soc_issues)

##Terrorism, War/conflict in other countries, Unresolved territorial conflicts####
CAB_data_dem$sec_issues <- ifelse(CAB_data_dem$q13_a %in% c(10, 11, 18) | CAB_data_dem$q13_b %in% c(10, 11, 18), 1, 0)
table(CAB_data_dem$sec_issues)

##Deterioration of the environment, Infrastructure issues###
CAB_data_dem$env_issues <- ifelse(CAB_data_dem$q13_a %in% c(8, 16) | CAB_data_dem$q13_b %in% c(8,16), 1, 0)
table(CAB_data_dem$env_issues)


##National Deferenders ####
CAB_data_dem$participate_rally_nat_def = rescale(CAB_data_dem$participate_rally_n, to = c(1, 0)) # 1 means yes
CAB_data_dem$participate_meeting_nat_def = rescale(CAB_data_dem$participate_meeting_n, to = c(1, 0)) # 1 means yes
CAB_data_dem$participate_member_nat_def = rescale(CAB_data_dem$participate_member_n, to = c(1, 0)) # 1 means not at all


national_defenders= cbind(CAB_data_dem$system_capable_n_sc,
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


#creating an index of national defenders#
CAB_data_dem$national_defenders_index <- rescale(
  CAB_data_dem$system_capable_n_sc +
    CAB_data_dem$system_proud_n_sc +
    CAB_data_dem$system_deserves_n_sc +
    CAB_data_dem$system_live_n_sc +
    CAB_data_dem$trust_central_n_sc +
    CAB_data_dem$trust_local_n_sc +
    CAB_data_dem$democracy_protests_n_sc +
    CAB_data_dem$participate_rally_nat_def +
    CAB_data_dem$participate_meeting_nat_def +
    CAB_data_dem$participate_member_nat_def,
  to = c(0, 1)
)

summary(CAB_data_dem$national_defenders)
hist(CAB_data_dem$national_defenders_index)


##Ideologically Consistent Conservatives Index ####
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

CAB_data_dem$idconsisten_conserv <- rescale(
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

summary(CAB_data_dem$idconsisten_conserv)
hist(CAB_data_dem$idconsisten_conserv)


##Surveillance-Averse Libertarians####
CAB_data_dem$tracking_central_sur_ave_lib =  rescale(CAB_data_dem$tracking_central_n, to = c(1, 0)) #1 means not comfortable at all
CAB_data_dem$tracking_local_sur_ave_lib =  rescale(CAB_data_dem$tracking_local_n, to = c(1, 0)) #1 means not comfortable at all
CAB_data_dem$tracking_companies_sur_ave_lib =  rescale(CAB_data_dem$tracking_companies_n, to = c(1, 0)) #1 means not comfortable at all

surv_averse_lib = cbind(
  CAB_data_dem$tracking_central_sur_ave_lib,
  CAB_data_dem$tracking_local_sur_ave_lib,
  CAB_data_dem$tracking_companies_sur_ave_lib,
  CAB_data_dem$trust_western_n_sc)
cronbach(surv_averse_lib) #.50

CAB_data_dem$surv_averse_lib_index <- rescale(
  CAB_data_dem$tracking_central_sur_ave_lib+
  CAB_data_dem$tracking_local_sur_ave_lib+
  CAB_data_dem$tracking_companies_sur_ave_lib+
  CAB_data_dem$trust_western_n_sc,
  to = c(0, 1)
)
summary(CAB_data_dem$surv_averse_lib_index)
hist(CAB_data_dem$surv_averse_lib_index)



names(CAB_data_dem)

# List of social media platforms and corresponding variables
platforms <- c("facebook_n_sc", "vkontakte_n_sc", "instagram_n_sc", "tiktok_n_sc", "twitter_n_sc", "youtube_n_sc", "whatsapp_n_sc", "telegram_n_sc")
variables <- c("facebook_n", "vkontakte_n", "instagram_n", "tiktok_n", "twitter_n", "youtube_n", "whatsapp_n", "telegram_n")

library(dplyr)
# Loop over each platform and create numeric versions of each social media usage variable
for (i in seq_along(variables)) {
  CAB_data_dem <- CAB_data_dem %>%
    mutate(
      !!paste0(platforms[i], "_1sm_no") := case_when(
        q22 == 2 ~ 0,       # Set to 0 if q22 is 2
        TRUE ~ as.numeric(.data[[variables[i]]])  # Use the corresponding q23_ variable
      )
    )
}

# Rescale the new numeric columns to range 0-1
CAB_data_dem <- CAB_data_dem %>%
  mutate(
    across(ends_with("_1sm_no"), ~ scales::rescale(., to = c(0, 1)))
  )

# Display summaries for verification
for (platform in platforms) {
  print(paste0("Summary for ", platform, " (numeric):"))
  print(summary(CAB_data_dem[[paste0(platform, "_1sm_no")]]))
}

table(CAB_data_dem$q23_a)
table(CAB_data_dem$q22)
table(CAB_data_dem$facebook_n_sc_1sm_no)
table(CAB_data_dem$facebook_n)

library(scales)
CAB_data_dem$xxx = rescale(CAB_data_dem$q, to = c(0,1))
table(CAB_data_dem$xxx)

library(psy)
##General Social Media Index####
gen_sm_index = cbind(
  CAB_data_dem$facebook_n_sc_1sm_no,
  CAB_data_dem$vkontakte_n_sc_1sm_no,
  CAB_data_dem$instagram_n_sc_1sm_no,
  CAB_data_dem$tiktok_n_sc_1sm_no,
  CAB_data_dem$twitter_n_sc_1sm_no)
cronbach(gen_sm_index) #.53

CAB_data_dem$gen_sm_index <-  rescale(
  CAB_data_dem$facebook_n_sc_1sm_no +
  CAB_data_dem$vkontakte_n_sc_1sm_no +
  CAB_data_dem$instagram_n_sc_1sm_no +
  CAB_data_dem$tiktok_n_sc_1sm_no +
  CAB_data_dem$twitter_n_sc_1sm_no, 
  to = c(0, 1)
)
hist(CAB_data_dem$gen_sm_index)

##Algorithm-Driven Social Media Index ####
CAB_data_dem$algdriven_sm_index <- rescale(
  CAB_data_dem$instagram_n_sc_1sm_no +
    CAB_data_dem$tiktok_n_sc_1sm_no +
    CAB_data_dem$twitter_n_sc_1sm_no,
  to = c(0, 1)
)
hist(CAB_data_dem$algdriven_sm_index)


##Social Network Driven SM Index###
CAB_data_dem$socnetdriven_sm_index <- rescale(
  CAB_data_dem$facebook_n_sc_1sm_no +
    CAB_data_dem$vkontakte_n_sc_1sm_no,
  to = c(0, 1)
)
hist(CAB_data_dem$socnetdriven_sm_index)


##Trust on Legacy Media####
CAB_data_dem$trust_legacy <- rescale(
  CAB_data_dem$trust_state_n_sc +
    CAB_data_dem$trust_russian_media_n_sc,
  to = c(0, 1)
)
table(CAB_data_dem$trust_legacy)

##Trust on Western Media####
table(CAB_data_dem$trust_western_n_sc)


##Trust on Russian Media####
CAB_data_dem$trust_russia_both <- rescale(
  CAB_data_dem$trust_vkontakte_n_sc +
    CAB_data_dem$trust_russian_media_n_sc,
  to = c(1, 0)
)

table(CAB_data_dem$trust_russia_both)





##Western Political News####
west_src_pol_events = cbind(
  CAB_data_dem$pol_news_facebook_n_sc,
  CAB_data_dem$pol_news_twitter_n_sc)
cronbach(west_src_pol_events)

CAB_data_dem$west_src_pol_events <- rescale(
  CAB_data_dem$pol_news_facebook_n_sc +
    CAB_data_dem$pol_news_twitter_n_sc, to = c(0, 1)
)

table(CAB_data_dem$west_src_pol_events)





##Critical Social Media Index####
platforms <- c("sm_critical_local_n_sc", "sm_critical_central_n_sc", "sm_positive_local_n_sc", "sm_positive_central_n_sc")
variables <- c("sm_critical_local_n_sc", "sm_critical_central_n_sc", "sm_positive_local_n_sc", "sm_positive_central_n_sc")

library(dplyr)
# Loop over each platform and create numeric versions of each social media usage variable
for (i in seq_along(variables)) {
  CAB_data_dem <- CAB_data_dem %>%
    mutate(
      !!paste0(platforms[i], "_2sm_no") := case_when(
        q22 == 2 ~ 0,       # Set to 0 if q22 is 2
        TRUE ~ as.numeric(.data[[variables[i]]])  # Use the corresponding q23_ variable
      )
    )
}

# Rescale the new numeric columns to range 0-1
CAB_data_dem <- CAB_data_dem %>%
  mutate(
    across(ends_with("_2sm_no"), ~ scales::rescale(., to = c(0, 1)))
  )

# Display summaries for verification
for (platform in platforms) {
  print(paste0("Summary for ", platform, " (numeric):"))
  print(summary(CAB_data_dem[[paste0(platform, "_1sm_no")]]))
}




CAB_data_dem$critical_social_media = cbind(
  CAB_data_dem$sm_critical_local_n_sc_2sm_no,
  CAB_data_dem$sm_critical_central_n_sc_2sm_no)
library(psy)
cronbach(CAB_data_dem$critical_social_media)

library(scales)
CAB_data_dem$critical_social_media = rescale(
  CAB_data_dem$sm_critical_local_n_sc_2sm_no +
    CAB_data_dem$sm_critical_central_n_sc_2sm_no,
  to = c(0,1)
)
summary(CAB_data_dem$critical_social_media)
hist(CAB_data_dem$critical_social_media)





##Socail Media Engagement####
platforms <- c("sm_engage_friends_n_sc", "sm_engage_groups_n_sc", "sm_engage_post_n_sc", "sm_engage_critical_n_sc", "sm_engage_supportive_sc", "sm_engage_offline_sc")
variables <- c("q38_a", "q38_b", "q38_c", "q38_d", "q38_e")

library(dplyr)
# Loop over each platform and create numeric versions of each social media usage variable
for (i in seq_along(variables)) {
  CAB_data_dem <- CAB_data_dem %>%
    mutate(
      !!paste0(platforms[i], "_3sm_no") := case_when(
        q22 == 2 ~ 0,       # Set to 0 if q22 is 2
        TRUE ~ as.numeric(.data[[variables[i]]])  # Use the corresponding q23_ variable
      )
    )
}


#This is the Code that Works

# Create new variables combining the 0 values from set 1 and set 2
CAB_data_dem <- CAB_data_dem %>%
  mutate(
    pol_news_facebook_n_sc_combined = ifelse(facebook_n_sc_1sm_no == 0, 0, pol_news_facebook_n_sc),
    pol_news_vkontakte_n_sc_combined = ifelse(vkontakte_n_sc_1sm_no == 0, 0, pol_news_vkontakte_n_sc),
    pol_news_tiktok_n_sc_combined = ifelse(tiktok_n_sc_1sm_no == 0, 0, pol_news_tiktok_n_sc),
    pol_news_twitter_n_sc_combined = ifelse(twitter_n_sc_1sm_no == 0, 0, pol_news_twitter_n_sc)
  )

table(CAB_data_dem$facebook_n_sc_combined)

table(CAB_data_dem$trust_facebook_n_sc)
CAB_data_dem <- CAB_data_dem %>%
  mutate(
    trust_facebook_n_sc_combined = ifelse(facebook_n_sc_1sm_no == 0, 0, trust_facebook_n_sc),
    trust_vkontakte_n_sc_combined = ifelse(vkontakte_n_sc_1sm_no == 0, 0, trust_vkontakte_n_sc)
  )

table(CAB_data_dem$trust_facebook_n_sc_combined)

# List of social media platforms and corresponding variables
table(CAB_data_dem$sm_critical_central_n_sc)
platforms1 <- c("sm_critical_central_n_sc", "sm_critical_local_n_sc")
variables1 <- c("sm_critical_central_n_sc", "sm_critical_local_n_sc")

library(dplyr)
# Loop over each platform and create numeric versions of each social media usage variable
for (i in seq_along(variables1)) {
  CAB_data_dem <- CAB_data_dem %>%
    mutate(
      !!paste0(platforms1[i], "_sm_no") := case_when(
        q22 == 2 ~ 0,       # Set to 0 if q22 is 2
        TRUE ~ as.numeric(.data[[variables1[i]]])  # Use the corresponding variable
      )
    )
}

table(CAB_data_dem$sm_critical_central_n_sc_sm_no)
table(CAB_data_dem$sm_critical_local_n_sc_sm_no)

# List of social media platforms and corresponding variables
table(CAB_data_dem$sm_positive_central_n_sc)
platforms2 <- c("sm_positive_central_n_sc", "sm_positive_local_n_sc")
variables2 <- c("sm_positive_central_n_sc", "sm_positive_local_n_sc")

library(dplyr)
# Loop over each platform and create numeric versions of each social media usage variable
for (i in seq_along(variables2)) {
  CAB_data_dem <- CAB_data_dem %>%
    mutate(
      !!paste0(platforms2[i], "_sm_no") := case_when(
        q22 == 2 ~ 0,       # Set to 0 if q22 is 2
        TRUE ~ as.numeric(.data[[variables2[i]]])  # Use the corresponding variable
      )
    )
}

table(CAB_data_dem$sm_positive_central_n_sc_sm_no)
table(CAB_data_dem$sm_positive_local_n_sc_sm_no)



