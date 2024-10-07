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



# Creating Social Media Warriors Index ####
# Subsetting the data to include only the rows where participate_rally_n and participate_protest_n are equal to 1
sm_war_filter <- CAB_data_dem[
  CAB_data_dem$participate_rally_n == 2 & CAB_data_dem$participate_protest_n == 2 & CAB_data_dem$participate_meeting_n == 2 &
    CAB_data_dem$participate_volunteer_n == 2 & CAB_data_dem$participate_member_n == 2 & CAB_data_dem$participate_contact_n == 2 & 
    CAB_data_dem$participate_vote_n == 2,
]

# Now create the sm_warriors matrix using the filtered data
sm_warriors <- cbind(
  sm_war_filter$sm_engage_critical_n_sc,
  sm_war_filter$sm_engage_friends_n_sc,
  sm_war_filter$sm_engage_groups_n_sc,
  sm_war_filter$sm_engage_post_n_sc
)

library(psy)
# Calculating the Cronbach's alpha for sm_warriors
cronbach(sm_warriors)




