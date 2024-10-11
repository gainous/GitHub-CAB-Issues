#Recording Issues into Three Overarching Categories####

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


#Social Media Warriors Index####
# Subsetting the data to include only the rows where participate_rally_n and participate_protest_n etc. are equal to 2
sm_war_filter <- CAB_data_dem[
  CAB_data_dem$participate_rally_n == 2 & CAB_data_dem$participate_protest_n == 2 & CAB_data_dem$participate_meeting_n == 2 &
    CAB_data_dem$participate_volunteer_n == 2 & CAB_data_dem$participate_member_n == 2 & CAB_data_dem$participate_contact_n == 2 & 
    CAB_data_dem$participate_vote_n == 2,
]

# Count the number of rows in sm_war_filter
num_entries <- nrow(sm_war_filter)

# Display the result
print(num_entries)

# Now create the sm_warriors matrix using the filtered data
sm_warriors <- cbind(
  sm_war_filter$sm_engage_critical_n_sc,
  #sm_war_filter$sm_engage_friends_n_sc,
  sm_war_filter$sm_engage_groups_n_sc,
  sm_war_filter$sm_engage_post_n_sc
)

library(psy)
# Calculating the Cronbach's alpha for sm_warriors
cronbach(sm_warriors)



## without filtered data#

sm_warriors2 <- cbind(
  CAB_data_dem$sm_engage_critical_n_sc,
  #CAB_data_dem$sm_engage_friends_n_sc,
  CAB_data_dem$sm_engage_groups_n_sc,
  CAB_data_dem$sm_engage_post_n_sc
)
cronbach(sm_warriors2)

## without filter but taking into account those variables #

CAB_data_dem$participate_protest_for_smw = factor(CAB_data_dem$participate_protest, levels = c('Yes', 'No'), ordered = TRUE)
CAB_data_dem$participate_protest_for_smw = as.numeric(CAB_data_dem$participate_protest_for_smw)
CAB_data_dem$participate_protest_for_smw = scales::rescale(CAB_data_dem$participate_protest_for_smw, to = c(0, 1))

CAB_data_dem$participate_rally_for_smw = factor(CAB_data_dem$participate_rally, levels = c('Yes', 'No'), ordered = TRUE)
CAB_data_dem$participate_rally_for_smw = as.numeric(CAB_data_dem$participate_rally_for_smw)
CAB_data_dem$participate_rally_for_smw = scales::rescale(CAB_data_dem$participate_rally_for_smw, to = c(0, 1))

CAB_data_dem$participate_meetin_for_smw = factor(CAB_data_dem$participate_meeting, levels = c('Yes', 'No'), ordered = TRUE)
CAB_data_dem$participate_meetin_for_smw = as.numeric(CAB_data_dem$participate_meetin_for_smw)
CAB_data_dem$participate_meetin_for_smw = scales::rescale(CAB_data_dem$participate_meetin_for_smw, to = c(0, 1))
table(CAB_data_dem$participate_meetin_for_smw)

CAB_data_dem$participate_volunteer_for_smw = factor(CAB_data_dem$participate_volunteer, levels = c('Yes', 'No'), ordered = TRUE)
CAB_data_dem$participate_volunteer_for_smw = as.numeric(CAB_data_dem$participate_volunteer_for_smw)
CAB_data_dem$participate_volunteer_for_smw = scales::rescale(CAB_data_dem$participate_volunteer_for_smw, to = c(0, 1))
table(CAB_data_dem$participate_volunteer_for_smw)

CAB_data_dem$participate_member_for_smw = factor(CAB_data_dem$participate_member, levels = c('Yes', 'No'), ordered = TRUE)
CAB_data_dem$participate_member_for_smw = as.numeric(CAB_data_dem$participate_member_for_smw)
CAB_data_dem$participate_member_for_smw = scales::rescale(CAB_data_dem$participate_member_for_smw, to = c(0, 1))
table(CAB_data_dem$participate_member_for_smw)

CAB_data_dem$participate_contact_for_smw = factor(CAB_data_dem$participate_contact, levels = c('Yes', 'No'), ordered = TRUE)
CAB_data_dem$participate_contact_for_smw = as.numeric(CAB_data_dem$participate_contact_for_smw)
CAB_data_dem$participate_contact_for_smw = scales::rescale(CAB_data_dem$participate_contact_for_smw, to = c(0, 1))
table(CAB_data_dem$participate_contact_for_smw)


CAB_data_dem$participate_vote_for_smw = factor(CAB_data_dem$participate_vote, levels = c('Yes', 'No'), ordered = TRUE)
CAB_data_dem$participate_vote_for_smw = as.numeric(CAB_data_dem$participate_vote_for_smw)
CAB_data_dem$participate_vote_for_smw = scales::rescale(CAB_data_dem$participate_vote_for_smw, to = c(0, 1))
table(CAB_data_dem$participate_vote_for_smw)


# Now create the sm_warriors matrix using the filtered data
sm_warriors3 <- cbind(
  # CAB_data_dem$sm_engage_critical_n_sc,
  CAB_data_dem$sm_engage_friends_n_sc,
  # CAB_data_dem$sm_engage_groups_n_sc,
  # CAB_data_dem$sm_engage_post_n_sc,
  CAB_data_dem$participate_rally_for_smw,
  CAB_data_dem$participate_protest_for_smw,
  CAB_data_dem$participate_meetin_for_smw,
  CAB_data_dem$participate_volunteer_for_smw,
  CAB_data_dem$participate_member_for_smw,
  # CAB_data_dem$participate_vote_for_smw,
  CAB_data_dem$participate_contact_for_smw)

cronbach(sm_warriors3)














# Dillusioned Reformists Index ######
library(scales)
CAB_data_dem$system_capable_rescaled = scales::rescale(CAB_data_dem$system_capable_n_sc, to = c(0, 1)) #1=towards disagreement

dil_ref_filter <- CAB_data_dem[
  CAB_data_dem$participate_rally_n == 1 ,
]

# Count the number of rows in sm_war_filter
num_entries_1 <- nrow(dil_ref_filter)

# Display the result
print(num_entries_1)

dil_ref = cbind(
  dil_ref_filter$sm_engage_critical_n_sc,
  dil_ref_filter$system_capable_rescaled,
  dil_ref_filter$system_hurdles_participate_n_sc,
  dil_ref_filter$trust_western_n_sc)
cronbach(dil_ref)

#without filtered data##
dil_ref_2 = cbind(
  CAB_data_dem$sm_engage_critical_n_sc,
  CAB_data_dem$system_capable_rescaled,
  CAB_data_dem$system_hurdles_participate_n_sc,
  CAB_data_dem$trust_western_n_sc)
cronbach(dil_ref_2)



library(psych)

# Silent Observers #####
library(dplyr)
library(car)
library(scales)

# Recoding Political Disucssion with Family and Friends #
CAB_data_dem$pol_dis <- as.numeric(CAB_data_dem$pol_discuss)

library(scales)
# Step 5: Rescale the numeric values to the desired direction
CAB_data_dem$pol_dis <- scales::rescale(CAB_data_dem$pol_dis, to = c(0, 1))
CAB_data_dem$trust_central_silob = scales::rescale(CAB_data_dem$trust_central_n_sc, to = c(0, 1))
CAB_data_dem$trust_local_silob = scales::rescale(CAB_data_dem$trust_local_n_sc, to = c(0, 1))
CAB_data_dem$sm_disagreement_politics_silob = scales::rescale(CAB_data_dem$sm_disagreement_politics_n_sc, to = c(1, 0))
CAB_data_dem$sm_engage_friends_n_sc <- scales::rescale(CAB_data_dem$sm_engage_friends_n, to = c(0, 1))
CAB_data_dem$sm_engage_groups_silob = scales::rescale(CAB_data_dem$sm_engage_groups_n, to = c(0, 1))
CAB_data_dem$sm_engage_post_silob = scales::rescale(CAB_data_dem$sm_engage_post_n, to = c(0, 1))
CAB_data_dem$sm_engage_critical_silob = scales::rescale(CAB_data_dem$sm_engage_critical_n, to = c(0, 1))
CAB_data_dem$sm_engage_supportive_silob = scales::rescale(CAB_data_dem$sm_engage_supportive_n, to = c(0, 1))
CAB_data_dem$sm_engage_offline_silob = scales::rescale(CAB_data_dem$sm_engage_offline_n, to = c(0, 1))

# Filter cases who didn't participate in any political activity/rally in last 12 months 
sil_obs_filter <- CAB_data_dem[
  CAB_data_dem$participate_rally_n == 2 ,  # 2= not participated in a rally in last 12 months ####
]


# Count the number of rows in sm_war_filter
num_entries_2 <- nrow(sil_obs_filter)

# Display the result
print(num_entries_2)


library(psy)
sil_obs <- cbind(
  sil_obs_filter$pol_dis,
  sil_obs_filter$trust_central_silob,
  sil_obs_filter$trust_local_silob,
  # sil_obs_filter$sm_critical_local_n_sc,
  # sil_obs_filter$sm_critical_central_n_sc,
  # sil_obs_filter$sm_disagreement_politics_silob,
  # sil_obs_filter$sm_engage_friends_silob,
  sil_obs_filter$sm_engage_groups_silob,
  sil_obs_filter$sm_engage_post_silob,
  sil_obs_filter$sm_engage_critical_silob,
  sil_obs_filter$sm_engage_supportive_silob,
  sil_obs_filter$sm_engage_offline_silob)

cronbach(sil_obs)

# if we want all negatively correlated item to reverse their directions#
#library(psych)
#alpha(sil_obs)
#alpha(sil_obs_removethis, check.keys = TRUE)



# Ideologically Consistent Conservatives Index ####
ide_con_filter <- CAB_data_dem[
  CAB_data_dem$participate_rally_n == 1 ,
]

ide_con_filter$trust_US_negative = scales::rescale(ide_con_filter$trust_US_n, to = c(0, 1))
ide_con_filter$trust_EU_negative = scales::rescale(ide_con_filter$trust_EU_n, to = c(0, 1))

num_entries_3 <- nrow(ide_con_filter)

# Display the result
print(num_entries_3)


idconsisten_conserv = cbind(
  ide_con_filter$trust_central_n_sc,
  ide_con_filter$trust_local_n_sc,
  ide_con_filter$system_capable_n_sc,
  ide_con_filter$system_proud_n_sc,
  ide_con_filter$trust_state_n_sc,
  ide_con_filter$system_deserves_n_sc,
  ide_con_filter$system_live_n_sc,
  ide_con_filter$trust_US_negative,
  ide_con_filter$trust_china_n_sc,
  ide_con_filter$trust_russia_n_sc,
  ide_con_filter$trust_EU_negative,
  ide_con_filter$pol_news_tv_n_sc)
cronbach(idconsisten_conserv)


#without filtered data#
idconsisten_conserv_1 = cbind(
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
cronbach(idconsisten_conserv_1)

#without filered and taking into account participation rally#
CAB_data_dem$participate_rally_for_smw = factor(CAB_data_dem$participate_rally, levels = c('Yes', 'No'), ordered = TRUE)
CAB_data_dem$participate_rally_for_smw = as.numeric(CAB_data_dem$participate_rally_for_smw)
CAB_data_dem$participate_rally_for_smw = scales::rescale(CAB_data_dem$participate_rally_for_smw, to = c(1, 0))

idconsisten_conserv_2 = cbind(
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
  CAB_data_dem$participate_rally_for_smw,
  CAB_data_dem$pol_news_tv_n_sc)
cronbach(idconsisten_conserv_2)

Nationalistic Defenders Index=(Reversed_System_Support)+(Reversed_Supportive_Engagement)+(Critical_Local_Exposure)+(Critical_Central_Exposure)+(Reversed_Trust_Central_Gov)+(Reversed_Pride_Government)+(Political_Activity)+(Voted_Recent_Election)+(Reversed_Freedom_Political_Views)+(Supportive_Stories_Social_Media)

#Nationalistic Defenders ####

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
                           CAB_data_dem$democracy_protests_n_sc)
cronbach(national_defenders)

library(scales)
CAB_data_dem$national_defenders = rescale(cbind(CAB_data_dem$system_capable_n_sc +
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
                           CAB_data_dem$democracy_protests_n_sc), to = c(0, 1))
hist(CAB_data_dem$national_defenders)
CAB_data_dem$national_defenders = rescale(cbind(CAB_data_dem$system_capable_n_sc +
hist(asian.barometer$q9)

#Index Construction##########################################
#Algorithm Driven Index
library(psy)
algorithm_matrix = cbind(
  CAB_data_merge$tiktok_n_sc,
  CAB_data_merge$twitter_n_sc,
  CAB_data_merge$instagram_n_sc)
cronbach(algorithm_matrix)

CAB_data_merge$algorithm_index = rescale((
  CAB_data_merge$tiktok_n_sc +
    CAB_data_merge$twitter_n_sc +
    CAB_data_merge$instagram_n_sc), to = c(0, 1))
summary(CAB_data_merge$algorithm_index)

#For footnote highlighting why Instagram can be counted as algorithm-driven
cor.test(CAB_data_merge$instagram_n_sc,CAB_data_merge$tiktok_n_sc)

social_network_matrix = cbind(
  CAB_data_merge$facebook_n_sc,
  CAB_data_merge$vkontakte_n_sc)
cronbach(social_network_matrix)

#Use cor.test here instead because it's only two variables
cor.test(CAB_data_merge$facebook_n_sc,CAB_data_merge$vkontakte_n_sc)

CAB_data_merge$social_network_index = rescale((
  CAB_data_merge$facebook_n_sc +
    CAB_data_merge$vkontakte_n_sc), to = c(0, 1))
summary(CAB_data_merge$social_network_index)
