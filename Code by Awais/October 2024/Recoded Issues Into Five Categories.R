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

library(scales)
# Dillusioned Reformists Index ######
CAB_data_dem$system_capable_rescaled = scales::rescale(CAB_data_dem$system_capable_n_sc, to = c(0, 1))
CAB_data_dem$sm_engage_critical_n_sc = scales::rescale(CAB_data_dem$sm_engage_critical_n_sc, to = c(0, 1))

dil_ref_filter <- CAB_data_dem[
  CAB_data_dem$participate_rally_n == 1 ,
]

dil_ref = cbind(
  dil_ref_filter$sm_engage_critical_n_sc,
  dil_ref_filter$system_capable_rescaled,
  dil_ref_filter$system_hurdles_participate_n_sc,
  dil_ref_filter$trust_western_n_sc)
cronbach(dil_ref)


library(psych)
alpha(dil_ref)

# Silent Observers #####
library(dplyr)
library(car)
library(scales)

# Recoding Political Disucssion with Family and Friends #

CAB_data_dem$pol_discuss <- recode(CAB_data_dem$pol_discuss ,
                                   "1" = 'A few times a day',
                                   "2" = 'Once a day',
                                   "3" = 'Three to five days a week',
                                   "4" = 'Once a week',
                                   "5" = 'Less often than once a week',
                                   "6" = 'Never')
# Step 3: Convert the recoded variable to an ordered factor
CAB_data_dem$pol_discuss <- factor(CAB_data_dem$pol_discuss, 
                                   levels = c('A few times a day', 'Once a day', 
                                              'Three to five days a week', 'Once a week', 
                                              'Less often than once a week', 'Never'), 
                                   ordered = TRUE)

# Step 4: Convert the ordered factor to numeric for scaling
CAB_data_dem$pol_dis <- as.numeric(CAB_data_dem$pol_discuss)

# Step 5: Rescale the numeric values to the desired direction
CAB_data_dem$pol_dis <- rescale(CAB_data_dem$pol_dis, to = c(0, 1))
CAB_data_dem$trust_central_silob = scales::rescale(CAB_data_dem$trust_central_n_sc, to = c(0, 1))
CAB_data_dem$trust_local_silob = scales::rescale(CAB_data_dem$trust_local_n_sc, to = c(0, 1))
CAB_data_dem$sm_disagreement_politics_silob = scales::rescale(CAB_data_dem$sm_disagreement_politics_n_sc, to = c(1, 0))
CAB_data_dem$sm_engage_friends_n_sc <- scales::rescale(CAB_data_dem$sm_engage_friends_n, to = c(0, 1))
CAB_data_dem$sm_engage_groups_silob = scales::rescale(CAB_data_dem$sm_engage_groups_n, to = c(0, 1))
CAB_data_dem$sm_engage_post_silob = scales::rescale(CAB_data_dem$sm_engage_post_n, to = c(0, 1))
CAB_data_dem$sm_engage_critical_silob = scales::rescale(CAB_data_dem$sm_engage_critical_n, to = c(0, 1))
CAB_data_dem$sm_engage_supportive_silob = scales::rescale(CAB_data_dem$sm_engage_supportive_n, to = c(0, 1))
CAB_data_dem$sm_engage_offline_silob = scales::rescale(CAB_data_dem$sm_engage_offline_n, to = c(0, 1))

# Filter cases who did'nt participate in any political activity/rally in last 12 months 
sil_obs_filter <- CAB_data_dem[
  CAB_data_dem$participate_rally_n == 2 ,
]

library(psy)
sil_obs <- cbind(
  sil_obs_filter$pol_dis,
  sil_obs_filter$trust_central_silob,
  sil_obs_filter$trust_local_silob,
  # #sil_obs_filter$sm_critical_local_n_sc,
  # sil_obs_filter$sm_critical_central_n_sc,
  # sil_obs_filter$sm_disagreement_politics_silob,
  # sil_obs_filter$sm_engage_friends_silob,
  sil_obs_filter$sm_engage_groups_silob,
  sil_obs_filter$sm_engage_post_silob,
  sil_obs_filter$sm_engage_critical_silob,
  sil_obs_filter$sm_engage_supportive_silob,
  sil_obs_filter$sm_engage_offline_silob)

cronbach(sil_obs)

library(psych)
alpha(sil_obs)
# if we want all negatively correlated item to reverse their directions#
alpha(sil_obs_removethis, check.keys = TRUE)


# Ideologically Consistent Conservatives Index ####
ide_con_filter$trust_US_negative = scales::rescale(ide_con_filter$trust_US_n, to = c(0, 1))
ide_con_filter$trust_EU_negative = scales::rescale(ide_con_filter$trust_EU_n, to = c(0, 1))



ide_con_filter <- CAB_data_dem[
  CAB_data_dem$participate_rally_n == 1 ,
]
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
