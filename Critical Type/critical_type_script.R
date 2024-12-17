save.image("C:/Users/User/Dropbox/Research/Internet Politics/Central Asia/Central Asia Grant Survey Project/GitHub-CAB-Issues/Critical Type/critical_type_data.RData")
names(CAB_data)


#Pre-imputation Recoding###############################################################################################
####################################################################################################################
#################################################################################################################
library(dplyr)
library(scales)

# Combine the data frames into a single data frame
CAB_data_dem <- bind_rows(GA_data, KG_data, KZ_data)
table(CAB_data_dem$country)
names(CAB_data_dem)

#Adding "Do not Use Internet" as 0 to the Do you Use Social media Questions #########################
table(CAB_data_dem$internet_use_No)
table(CAB_data_dem$sm_use_Yes)

# Recode sm_use_Yes_internet_use_No based on the given conditions
CAB_data_dem <- CAB_data_dem %>%
  mutate(sm_use_Yes_internet_use_No = case_when(
    internet_use_No == 1 ~ 0,  # If internet_use_No = 1, set to 0
    sm_use_Yes == 0 ~ 0,      # If sm_use_Yes = 0, set to 0
    sm_use_Yes == 1 ~ 1       # If sm_use_Yes = 1, set to 1
  ))
table(CAB_data_dem$sm_use_Yes_internet_use_No)

#Create Rescaled Political Discussion
CAB_data_dem$pol_discuss_n_sc = rescale(CAB_data_dem$pol_discuss_n, to = c(1, 0))

#Fix name of SM Supportive and Offline
CAB_data_dem$sm_engage_supportive_n_sc = CAB_data_dem$sm_engage_supportive_sc
CAB_data_dem$sm_engage_offline_n_sc = CAB_data_dem$sm_engage_offline_sc

#Platforms###################
sm_variables <- c("facebook_n_sc", "vkontakte_n_sc", "instagram_n_sc", "tiktok_n_sc", "twitter_n_sc", "sm_critical_local_n_sc",                                                          
                  "sm_critical_central_n_sc", "sm_positive_local_n_sc", "sm_positive_central_n_sc", "echo_chamber_n_sc",
                  "avoidance_blocking_n_sc", "avoidance_unfriending_n_sc", "avoidance_leaving_group_n_sc", "avoidance_unsubscribing_n_sc",                                                    
                  "sm_disagreement_politics_n_sc", "sm_disagreement_news_n_sc", "sm_disagreement_issues_n_sc", "sm_engage_friends_n_sc",                                                          
                  "sm_engage_groups_n_sc", "sm_engage_post_n_sc", "sm_engage_critical_n_sc", "sm_engage_supportive_n_sc",                                                         
                  "sm_engage_offline_n_sc", "pol_news_tv_n_sc", "pol_news_facebook_n_sc", "pol_news_vkontakte_n_sc", "pol_news_tiktok_n_sc",                                                           
                  "pol_news_twitter_n_sc", "pol_news_odnoklassniki_n_sc")

variables_sm <- c("facebook_n_sc", "vkontakte_n_sc", "instagram_n_sc", "tiktok_n_sc", "twitter_n_sc", "sm_critical_local_n_sc",                                                          
                  "sm_critical_central_n_sc", "sm_positive_local_n_sc", "sm_positive_central_n_sc", "echo_chamber_n_sc",
                  "avoidance_blocking_n_sc", "avoidance_unfriending_n_sc", "avoidance_leaving_group_n_sc", "avoidance_unsubscribing_n_sc",                                                    
                  "sm_disagreement_politics_n_sc", "sm_disagreement_news_n_sc", "sm_disagreement_issues_n_sc", "sm_engage_friends_n_sc",                                                          
                  "sm_engage_groups_n_sc", "sm_engage_post_n_sc", "sm_engage_critical_n_sc", "sm_engage_supportive_n_sc",                                                         
                  "sm_engage_offline_n_sc", "pol_news_tv_n_sc", "pol_news_facebook_n_sc", "pol_news_vkontakte_n_sc", "pol_news_tiktok_n_sc",                                                           
                  "pol_news_twitter_n_sc", "pol_news_odnoklassniki_n_sc")

library(dplyr)
# Loop over each platform and create numeric versions of each social media usage variable
for (i in seq_along(variables_sm)) {
  CAB_data_dem <- CAB_data_dem %>%
    mutate(
      !!paste0(sm_variables[i], "_no") := case_when(
        sm_use_Yes_internet_use_No == 0 ~ 0,       # Set to 0 if sm_use_Yes_internet_use_No is 0
        TRUE ~ as.numeric(.data[[variables_sm[i]]])  # Use the corresponding sm_ variable
      )
    )
}

# Display summaries for verification
for (sm_variables in sm_variables) {
  print(paste0("Summary for ", sm_variables, " (numeric):"))
  print(summary(CAB_data_dem[[paste0(sm_variables, "_no")]]))
}

#Factor Variables######################################################################################################
#Adding "Do not Use Internet" as No to the Do you Use Social media Questions #########################
table(CAB_data_dem$internet_use)
table(CAB_data_dem$sm_use)

# Recode sm_use_Yes_internet_use_No_f based on the given conditions
CAB_data_dem <- CAB_data_dem %>%
  mutate(sm_use_Yes_internet_use_No_f = case_when(
    internet_use == "No" ~ "No",  # If internet_use = No, set to No
    sm_use == "No" ~ "No",      # If sm_use = No, set to No
    sm_use == "Yes" ~ "Yes"       # If sm_use = yes, set to Yes
  ))
table(CAB_data_dem$sm_use_Yes_internet_use_No_f)

#Platforms###################
sm_variables_f <- c("facebook", "vkontakte", "instagram", "tiktok", "twitter", "sm_critical_local",                                                          
                    "sm_critical_central", "sm_positive_local", "sm_positive_central", 
                    "avoidance_blocking", "avoidance_unfriending", "avoidance_leaving_group", "avoidance_unsubscribing",                                                    
                    "sm_disagreement_politics", "sm_disagreement_news", "sm_disagreement_issues", "sm_engage_friends",                                                          
                    "sm_engage_groups", "sm_engage_post", "sm_engage_critical", "sm_engage_supportive",                                                         
                    "sm_engage_offline", "pol_news_tv", "pol_news_facebook", "pol_news_vkontakte", "pol_news_tiktok",                                                           
                    "pol_news_twitter", "pol_news_odnoklassniki")

variables_sm_f <- c("facebook", "vkontakte", "instagram", "tiktok", "twitter", "sm_critical_local",                                                          
                    "sm_critical_central", "sm_positive_local", "sm_positive_central", 
                    "avoidance_blocking", "avoidance_unfriending", "avoidance_leaving_group", "avoidance_unsubscribing",                                                    
                    "sm_disagreement_politics", "sm_disagreement_news", "sm_disagreement_issues", "sm_engage_friends",                                                          
                    "sm_engage_groups", "sm_engage_post", "sm_engage_critical", "sm_engage_supportive",                                                         
                    "sm_engage_offline", "pol_news_tv", "pol_news_facebook", "pol_news_vkontakte", "pol_news_tiktok",                                                           
                    "pol_news_twitter", "pol_news_odnoklassniki")

# Loop over each platform and create factor versions of each social media usage variable
for (i in seq_along(variables_sm_f)) {
  CAB_data_dem <- CAB_data_dem %>%
    mutate(
      !!paste0(sm_variables_f[i], "_no") := case_when(
        sm_use_Yes_internet_use_No_f == "No" ~ "No",  # Set to "No" if sm_use_Yes_internet_use_No_f is "No"
        TRUE ~ as.character(.data[[variables_sm_f[i]]])  # Convert the corresponding variable to character
      )
    )
}


names(CAB_data_dem)

save.image("C:/Users/User/Dropbox/Research/Internet Politics/Central Asia/Central Asia Grant Survey Project/GitHub-CAB-Issues/Critical Type/critical_type_data.RData")

#Multiple Imputation#############################################################################################################
##########################################################################################################################

#Imputation - All Used Numeric Variables for All Countries
for_imp = c("country", 
            "facebook_n_sc_no", "vkontakte_n_sc_no", "instagram_n_sc_no", "tiktok_n_sc_no", "twitter_n_sc_no", "sm_critical_local_n_sc_no",                                                          
            "sm_critical_central_n_sc_no", "sm_positive_local_n_sc_no", "sm_positive_central_n_sc_no", "echo_chamber_n_sc_no",
            "avoidance_blocking_n_sc_no", "avoidance_unfriending_n_sc_no", "avoidance_leaving_group_n_sc_no", "avoidance_unsubscribing_n_sc_no",                                                    
            "sm_disagreement_politics_n_sc_no", "sm_disagreement_news_n_sc_no", "sm_disagreement_issues_n_sc_no", "sm_engage_friends_n_sc_no",                                                          
            "sm_engage_groups_n_sc_no", "sm_engage_post_n_sc_no", "sm_engage_critical_n_sc_no", "sm_engage_supportive_n_sc_no",                                                         
            "sm_engage_offline_n_sc_no","system_capable_n_sc", "system_proud_n_sc", "system_deserves_n_sc", "system_live_n_sc",
            "trust_central_n_sc",  "trust_local_n_sc", "democracy_protests_n_sc", "democracy_elections_n_sc", "democracy_speech_n_sc",                                                           
            "democracy_oversight_n_sc", "democracy_organize_n_sc", "democracy_press_n_sc", "democracy_parties_n_sc",                                                         
            "democracy_courts_n_sc", "pol_news_tv_n_sc", "pol_news_facebook_n_sc", "pol_news_vkontakte_n_sc", "pol_news_tiktok_n_sc",                                                           
            "pol_news_twitter_n_sc", "pol_news_odnoklassniki_n_sc", "participate_meeting_n",
            "participate_member_n", "trust_state_n_sc", "trust_US_n_sc", "trust_china_n_sc", "trust_russia_n_sc",
            "trust_EU_n_sc", "news_balance_n_sc", "participate_rally_n", "pol_discuss_n_sc",
            "tv_critical_local_n_sc", "tv_critical_central_n_sc","pol_interest_n_sc", "age_n_sc", "urbanicity_Village", "gender_Male",
            "edu_n_sc", "inc_n_sc","weight")

imp_data = CAB_data_dem[for_imp]

names(imp_data)

# Load required libraries
library(mice)
library(sjmisc)
library(dplyr)

# Perform multiple imputation for numeric variables
CAB_data_dem_imputed_numeric <- mice(imp_data, m=30, maxit=10, method='pmm', seed=42)

# Merge imputations into one frame
CAB_data_dem_merge_numeric = merge_imputations(imp_data, CAB_data_dem_imputed_numeric)

# Create ID
CAB_data_dem_merge_numeric = mutate(CAB_data_dem_merge_numeric, id = rownames(CAB_data_dem_merge_numeric))

# Identify variables that were not imputed due to no missing values
complete_vars_numeric <- names(imp_data)[colSums(is.na(imp_data)) == 0]

# Create dataframes of these complete variables
missing_df_numeric = imp_data[complete_vars_numeric]

# Create id for merging
missing_df_numeric = mutate(missing_df_numeric, id = rownames(missing_df_numeric))

# Bind/merge the complete variables back to the imputed data
CAB_data_dem_merge_numeric = merge(missing_df_numeric, CAB_data_dem_merge_numeric, by = "id", all.y = TRUE)

names(CAB_data_dem_merge_numeric)

# Imputation - All Used Factor Variables for All Countries
for_imp_f = c("sm_critical_local_no", "sm_critical_central_no", "sm_positive_local_no", "sm_positive_central_no")

imp_data_f = CAB_data_dem[for_imp_f]

# Ensure variables are converted to factors
imp_data_f[] <- lapply(imp_data_f, as.factor)

# Perform multiple imputation for factor variables using polyreg
CAB_data_dem_imputed_factors <- mice(imp_data_f, m = 30, maxit = 10, method = 'polyreg', seed = 42)

# Merge imputations into one frame
CAB_data_dem_merge_factors = merge_imputations(imp_data_f, CAB_data_dem_imputed_factors)

# Create an ID for merging
CAB_data_dem_merge_factors <- mutate(CAB_data_dem_merge_factors, id = rownames(CAB_data_dem_merge_factors))

# Merge the numeric and factor imputed datasets
CAB_data_dem_final_merge <- merge(CAB_data_dem_merge_factors, CAB_data_dem_merge_numeric, by = "id")

# Rename final imputed dataset
CAB_data_dem_imputed <- CAB_data_dem_final_merge

#Check dataset
names(CAB_data_dem_imputed)

# Clean up workspace
rm(CAB_data_dem_final_merge)
rm(CAB_data_dem_merge_factors)
rm(CAB_data_dem_merge_numeric)
rm(imp_data)
rm(imp_data_f)
rm(complete_vars_numeric)
rm(missing_df_numeric)
rm(for_imp)
rm(for_imp_f)

save.image("C:/Users/User/Dropbox/Research/Internet Politics/Central Asia/Central Asia Grant Survey Project/GitHub-CAB-Issues/Critical Type/critical_type_data.RData")

#Variable Construction############################################################################################
####################################################################################################################
#####################################################################################################################

library(scales)
##National Deferenders ####
CAB_data_dem_imputed$participate_rally_n_i = rescale(CAB_data_dem_imputed$participate_rally_n, to = c(1, 0)) # 1 means yes
CAB_data_dem_imputed$participate_meeting_n_i = rescale(CAB_data_dem_imputed$participate_meeting_n, to = c(1, 0)) # 1 means yes
CAB_data_dem_imputed$participate_member_n_i = rescale(CAB_data_dem_imputed$participate_member_n, to = c(1, 0)) # 1 means not at all


national_defenders= cbind(CAB_data_dem_imputed$system_capable_n_sc,
                          CAB_data_dem_imputed$system_proud_n_sc,
                          CAB_data_dem_imputed$system_deserves_n_sc,
                          CAB_data_dem_imputed$system_live_n_sc,
                          CAB_data_dem_imputed$trust_central_n_sc,
                          CAB_data_dem_imputed$trust_local_n_sc,
                          CAB_data_dem_imputed$democracy_protests_n_sc,
                          CAB_data_dem_imputed$participate_rally_n_i,
                          CAB_data_dem_imputed$participate_meeting_n_i,
                          CAB_data_dem_imputed$participate_member_n_i)
library(psy)
cronbach(national_defenders)

#creating an index of national defenders#
CAB_data_dem_imputed$national_defenders_index <- rescale(
  CAB_data_dem_imputed$system_capable_n_sc +
    CAB_data_dem_imputed$system_proud_n_sc +
    CAB_data_dem_imputed$system_deserves_n_sc +
    CAB_data_dem_imputed$system_live_n_sc +
    CAB_data_dem_imputed$trust_central_n_sc +
    CAB_data_dem_imputed$trust_local_n_sc +
    CAB_data_dem_imputed$democracy_protests_n_sc +
    CAB_data_dem_imputed$participate_rally_n_i +
    CAB_data_dem_imputed$participate_meeting_n_i +
    CAB_data_dem_imputed$participate_member_n_i,
  to = c(0, 1)
)
summary(CAB_data_dem_imputed$national_defenders_index)
hist(CAB_data_dem_imputed$national_defenders_index)


##Ideologically Consistent Conservatives Index ####
CAB_data_dem_imputed$trust_US_n_i=  rescale(CAB_data_dem_imputed$trust_US_n, to = c(0, 1)) # 1 means not at all
CAB_data_dem_imputed$trust_EU_n_i =  rescale(CAB_data_dem_imputed$trust_EU_n, to = c(0, 1)) # 1 means not at all

idconsisten_conserv = cbind(
  CAB_data_dem_imputed$trust_central_n_sc,
  CAB_data_dem_imputed$trust_local_n_sc,
  CAB_data_dem_imputed$system_capable_n_sc,
  CAB_data_dem_imputed$system_proud_n_sc,
  CAB_data_dem_imputed$trust_state_n_sc,
  CAB_data_dem_imputed$system_deserves_n_sc,
  CAB_data_dem_imputed$system_live_n_sc,
  CAB_data_dem_imputed$trust_US_n_i,
  CAB_data_dem_imputed$trust_china_n_sc,
  CAB_data_dem_imputed$trust_russia_n_sc,
  CAB_data_dem_imputed$trust_EU_n_i,
  CAB_data_dem_imputed$pol_news_tv_n_sc)
cronbach(idconsisten_conserv)

CAB_data_dem_imputed$idconsisten_conserv_index <- rescale(
  CAB_data_dem_imputed$trust_central_n_sc +
    CAB_data_dem_imputed$trust_local_n_sc +
    CAB_data_dem_imputed$system_capable_n_sc +
    CAB_data_dem_imputed$system_proud_n_sc +
    CAB_data_dem_imputed$trust_state_n_sc +
    CAB_data_dem_imputed$system_deserves_n_sc +
    CAB_data_dem_imputed$system_live_n_sc +
    CAB_data_dem_imputed$trust_US_n_i +
    CAB_data_dem_imputed$trust_china_n_sc +
    CAB_data_dem_imputed$trust_russia_n_sc +
    CAB_data_dem_imputed$trust_EU_n_i +
    CAB_data_dem_imputed$pol_news_tv_n_sc,
  to = c(0, 1)
)
summary(CAB_data_dem_imputed$idconsisten_conserv_index)
hist(CAB_data_dem_imputed$idconsisten_conserv_index)


#Silent Observers Index
library(scales)
names(CAB_data_dem_imputed)

#  Rescale the numeric values to the desired direction
CAB_data_dem_imputed$pol_discuss_n_sc_i <- rescale(CAB_data_dem_imputed$pol_discuss_n_sc, to = c(1, 0)) #1 means less
CAB_data_dem_imputed$sm_engage_friends_n_sc_no_i <- rescale(CAB_data_dem_imputed$sm_engage_friends_n_sc_no, to = c(1, 0)) # 1 meas less
CAB_data_dem_imputed$sm_engage_groups_n_sc_no_i = rescale(CAB_data_dem_imputed$sm_engage_groups_n_sc_no, to = c(1, 0)) # 1 meas less
CAB_data_dem_imputed$sm_engage_post_n_sc_no_i = rescale(CAB_data_dem_imputed$sm_engage_post_n_sc_no, to = c(1, 0)) # 1 meas less
CAB_data_dem_imputed$sm_engage_offline_n_sc_no_i = rescale(CAB_data_dem_imputed$sm_engage_offline_n_sc_no, to = c(1, 0)) # 1 meas less
CAB_data_dem_imputed$participate_meeting_n_sc = rescale(CAB_data_dem_imputed$participate_meeting_n, to = c(0, 1)) # 1 meas less
CAB_data_dem_imputed$participate_rally_n_sc = rescale(CAB_data_dem_imputed$participate_rally_n, to = c(0, 1)) # 1 meas less
CAB_data_dem_imputed$participate_member_n_sc = rescale(CAB_data_dem_imputed$participate_member_n, to = c(0, 1)) # 1 meas less


summary(CAB_data_dem_imputed$sm_engage_offline_n_sc_no)

table(CAB_data_dem$participate_rally_n_sc)

library(psy)
sil_obs <- cbind(
  CAB_data_dem_imputed$participate_meeting_n_sc_no,
  CAB_data_dem_imputed$participate_rally_n_sc,
  CAB_data_dem_imputed$participate_member_n_sc
)

cronbach(sil_obs)

CAB_data_dem_imputed$sil_obs_index <- rescale(
  CAB_data_dem_imputed$pol_dis_silob +
    CAB_data_dem_imputed$sm_engage_friends_silob +
    CAB_data_dem_imputed$sm_engage_groups_silob +
    CAB_data_dem_imputed$sm_engage_post_silob +
    CAB_data_dem_imputed$sm_engage_supportive_silob +
    CAB_data_dem_imputed$sm_engage_offline_silob +
    CAB_data_dem_imputed$participate_rally_sil_obs,
  to = c(0, 1)
)
table(CAB_data_dem_imputed$sil_obs_index)

CAB_data_dem_imputed$sm_engage_friends_smw =  rescale(CAB_data_dem_imputed$sm_engage_friends_n_sc_sm_no, to = c(0, 1)) # 1 means more
CAB_data_dem_imputed$sm_engage_groups_smw =  rescale(CAB_data_dem_imputed$sm_engage_groups_n_sc_sm_no, to = c(0, 1)) # 1 means more
CAB_data_dem_imputed$sm_engage_post_smw = rescale(CAB_data_dem_imputed$sm_engage_post_n_sc_sm_no, to = c(0, 1)) # 1 means more


#Digitally Politically Engaged
dig_pol_engage <- cbind(
  CAB_data_dem_imputed$sm_engage_friends_sm,
  CAB_data_dem_imputed$sm_engage_groups_smw,
  CAB_data_dem_imputed$sm_engage_post_smw)

cronbach(dig_pol_engage)

CAB_data_dem_imputed$dig_pol_engage_index <- rescale(
  CAB_data_dem_imputed$sm_engage_friends_smw+
    CAB_data_dem_imputed$sm_engage_groups_smw+
    CAB_data_dem_imputed$sm_engage_post_smw,
  to = c(0, 1)
)

# The Selective Avoiders ####
selective_avoiders = cbind(
  CAB_data_dem_imputed$avoidance_unfriending_n_sc_sm_no,
  CAB_data_dem_imputed$avoidance_blocking_n_sc_sm_no,
  CAB_data_dem_imputed$sm_disagreement_politics_n_sc_sm_no,
  CAB_data_dem_imputed$sm_disagreement_news_n_sc_sm_no,
  CAB_data_dem_imputed$sm_disagreement_issues_n_sc_sm_no)
cronbach(selective_avoiders)

CAB_data_dem_imputed$selective_avoiders_index <- rescale(
  CAB_data_dem_imputed$avoidance_unfriending_n_sc_sm_no + 
    CAB_data_dem_imputed$avoidance_blocking_n_sc_sm_no + 
    CAB_data_dem_imputed$sm_disagreement_politics_n_sc_sm_no + 
    CAB_data_dem_imputed$sm_disagreement_news_n_sc_sm_no + 
    CAB_data_dem_imputed$sm_disagreement_issues_n_sc_sm_no, 
  to = c(0, 1)
)


# List of social media platforms and corresponding variables
platforms <- c("facebook_n_sc", "vkontakte_n_sc", "instagram_n_sc", "tiktok_n_sc", "twitter_n_sc", "youtube_n_sc", "whatsapp_n_sc", "telegram_n_sc")
variables <- c("facebook_n", "vkontakte_n", "instagram_n", "tiktok_n", "twitter_n", "youtube_n", "whatsapp_n", "telegram_n")

library(dplyr)
# Loop over each platform and create numeric versions of each social media usage variable
for (i in seq_along(variables)) {
  CAB_data_dem_imputed <- CAB_data_dem_imputed %>%
    mutate(
      !!paste0(platforms[i], "_1sm_no") := case_when(
        q22 == 2 ~ 0,       # Set to 0 if q22 is 2
        TRUE ~ as.numeric(.data[[variables[i]]])  # Use the corresponding q23_ variable
      )
    )
}

# Rescale the new numeric columns to range 0-1
CAB_data_dem_imputed <- CAB_data_dem_imputed %>%
  mutate(
    across(ends_with("_1sm_no"), ~ scales::rescale(., to = c(0, 1)))
  )

# Display summaries for verification
for (platform in platforms) {
  print(paste0("Summary for ", platform, " (numeric):"))
  print(summary(CAB_data_dem_imputed[[paste0(platform, "_1sm_no")]]))
}

table(CAB_data_dem_imputed$q23_a)
table(CAB_data_dem_imputed$q22)
table(CAB_data_dem_imputed$facebook_n_sc_1sm_no)
table(CAB_data_dem_imputed$facebook_n)

library(scales)
CAB_data_dem_imputed$xxx = rescale(CAB_data_dem_imputed$q, to = c(0,1))
table(CAB_data_dem_imputed$xxx)





library(psy)
##General Social Media Index####
gen_sm_index = cbind(
  CAB_data_dem_imputed$facebook_n_sc_no,
  CAB_data_dem_imputed$vkontakte_n_sc_no,
  CAB_data_dem_imputed$instagram_n_sc_no,
  CAB_data_dem_imputed$tiktok_n_sc_no,
  CAB_data_dem_imputed$twitter_n_sc_no)
cronbach(gen_sm_index)

CAB_data_dem_imputed$gen_sm_index <-  rescale(
  CAB_data_dem_imputed$facebook_n_sc_no +
    CAB_data_dem_imputed$vkontakte_n_sc_no +
    CAB_data_dem_imputed$instagram_n_sc_no +
    CAB_data_dem_imputed$tiktok_n_sc_no +
    CAB_data_dem_imputed$twitter_n_sc_no, 
  to = c(0, 1)
)
hist(CAB_data_dem_imputed$gen_sm_index)

##Algorithm-Driven Social Media Index ####
CAB_data_dem_imputed$algdriven_sm_index <- rescale(
  CAB_data_dem_imputed$instagram_n_sc_1sm_no +
    CAB_data_dem_imputed$tiktok_n_sc_1sm_no +
    CAB_data_dem_imputed$twitter_n_sc_1sm_no,
  to = c(0, 1)
)
hist(CAB_data_dem_imputed$algdriven_sm_index)


##Social Network Driven SM Index###
CAB_data_dem_imputed$socnet_driven_sm_index <- rescale(
  CAB_data_dem_imputed$facebook_n_sc_1sm_no +
    CAB_data_dem_imputed$vkontakte_n_sc_1sm_no,
  to = c(0, 1)
)

table(CAB_data_dem_imputed$socnet_driven_sm_index)
hist(CAB_data_dem_imputed$socnet_driven_sm_index)

##Trust on Legacy Media####
CAB_data_dem_imputed$trust_legacy <- rescale(
  CAB_data_dem_imputed$trust_state_n_sc +
    CAB_data_dem_imputed$trust_russian_media_n_sc,
  to = c(0, 1)
)

table(CAB_data_dem_imputed$trust_state_n_sc)
table(CAB_data_dem_imputed$trust_russian_media_n_sc)
table(CAB_data_dem_imputed$trust_legacy)


##Trust on Western Media####
table(CAB_data_dem_imputed$trust_western_n_sc)


##Trust on Russian Media####
CAB_data_dem_imputed$trust_russia_both <- rescale(
  CAB_data_dem_imputed$trust_vkontakte_n_sc_combined +
    CAB_data_dem_imputed$trust_russian_media_n_sc,
  to = c(1, 0)
)

table(CAB_data_dem_imputed$trust_russia_both)


names(CAB_data_dem_imputed)


##Western Political News####
west_src_pol_events = cbind(
  CAB_data_dem_imputed$pol_news_facebook_n_sc_combined,
  CAB_data_dem_imputed$pol_news_twitter_n_sc_combined)
cronbach(west_src_pol_events) #.16

CAB_data_dem_imputed$west_src_pol_events <- rescale(
  CAB_data_dem_imputed$pol_news_facebook_n_sc +
    CAB_data_dem_imputed$pol_news_twitter_n_sc, to = c(0, 1)
)

table(CAB_data_dem_imputed$west_src_pol_events)
hist(CAB_data_dem_imputed$west_src_pol_events)

names(CAB_data_dem_imputed)


##Critical Social Media Index####
platforms <- c("sm_critical_local_n_sc", "sm_critical_central_n_sc", "sm_positive_local_n_sc", "sm_positive_central_n_sc")
variables <- c("sm_critical_local_n_sc", "sm_critical_central_n_sc", "sm_positive_local_n_sc", "sm_positive_central_n_sc")

library(dplyr)
# Loop over each platform and create numeric versions of each social media usage variable
for (i in seq_along(variables)) {
  CAB_data_dem_imputed <- CAB_data_dem_imputed %>%
    mutate(
      !!paste0(platforms[i], "_2sm_no") := case_when(
        q22 == 2 ~ 0,       # Set to 0 if q22 is 2
        TRUE ~ as.numeric(.data[[variables[i]]])  # Use the corresponding q23_ variable
      )
    )
}

# Rescale the new numeric columns to range 0-1
CAB_data_dem_imputed <- CAB_data_dem_imputed %>%
  mutate(
    across(ends_with("_2sm_no"), ~ scales::rescale(., to = c(0, 1)))
  )

# Display summaries for verification
for (platform in platforms) {
  print(paste0("Summary for ", platform, " (numeric):"))
  print(summary(CAB_data_dem_imputed[[paste0(platform, "_1sm_no")]]))
}



#Critical Social Media Index
CAB_data_dem_imputed$critical_social_media = cbind(
  CAB_data_dem_imputed$sm_critical_local_n_sc_no,
  CAB_data_dem_imputed$sm_critical_central_n_sc_no)
cronbach(CAB_data_dem_imputed$critical_social_media)

library(scales)
CAB_data_dem_imputed$critical_social_media = rescale(
  CAB_data_dem_imputed$sm_critical_local_n_sc_no +
    CAB_data_dem_imputed$sm_critical_central_n_sc_no,
  to = c(0,1)
)
hist(CAB_data_dem_imputed$critical_social_media)

CAB_data_dem_imputed$positive_social_media = cbind(
  CAB_data_dem_imputed$sm_positive_local_n_sc_sm_no,
  CAB_data_dem_imputed$sm_positive_central_n_sc_sm_no)
library(psy)
cronbach(CAB_data_dem_imputed$positive_social_media)

library(scales)
CAB_data_dem_imputed$positive_social_media = rescale(
  CAB_data_dem_imputed$sm_positive_local_n_sc_sm_no +
    CAB_data_dem_imputed$sm_positive_central_n_sc_sm_no,
  to = c(0,1)
)
summary(CAB_data_dem_imputed$positive_social_media)
hist(CAB_data_dem_imputed$positive_social_media)



##Social Media Engagement####
platforms <- c("sm_engage_friends_n_sc", "sm_engage_groups_n_sc", "sm_engage_post_n_sc", "sm_engage_critical_n_sc", "sm_engage_supportive_n_sc", "sm_engage_offline_n_sc")
variables <- c("q38_a", "q38_b", "q38_c", "q38_d", "q38_e")

library(dplyr)
# Loop over each platform and create numeric versions of each social media usage variable
for (i in seq_along(variables)) {
  CAB_data_dem_imputed <- CAB_data_dem_imputed %>%
    mutate(
      !!paste0(platforms[i], "_3sm_no") := case_when(
        q22 == 2 ~ 0,       # Set to 0 if q22 is 2
        TRUE ~ as.numeric(.data[[variables[i]]])  # Use the corresponding q23_ variable
      )
    )
}


#This is the Code that Works
library(dplyr)
# Create new variables combining the 0 values from set 1 and set 2
CAB_data_dem_imputed <- CAB_data_dem_imputed %>%
  mutate(
    pol_news_facebook_n_sc_combined = ifelse(facebook_n_sc_1sm_no == 0, 0, pol_news_facebook_n_sc),
    pol_news_vkontakte_n_sc_combined = ifelse(vkontakte_n_sc_1sm_no == 0, 0, pol_news_vkontakte_n_sc),
    pol_news_tiktok_n_sc_combined = ifelse(tiktok_n_sc_1sm_no == 0, 0, pol_news_tiktok_n_sc),
    pol_news_twitter_n_sc_combined = ifelse(twitter_n_sc_1sm_no == 0, 0, pol_news_twitter_n_sc)
  )


table(CAB_data_dem_imputed$trust_facebook_n_sc)
CAB_data_dem_imputed <- CAB_data_dem_imputed %>%
  mutate(
    trust_facebook_n_sc_combined = ifelse(facebook_n_sc_1sm_no == 0, 0, trust_facebook_n_sc),
    trust_vkontakte_n_sc_combined = ifelse(vkontakte_n_sc_1sm_no == 0, 0, trust_vkontakte_n_sc)
  )

table(CAB_data_dem_imputed$trust_vkontakte_n_sc_combined)

# List of social media platforms and corresponding variables
table(CAB_data_dem_imputed$sm_critical_central_n_sc)
platforms1 <- c("sm_critical_central_n_sc", "sm_critical_local_n_sc")
variables1 <- c("sm_critical_central_n_sc", "sm_critical_local_n_sc")

library(dplyr)
# Loop over each platform and create numeric versions of each social media usage variable
for (i in seq_along(variables1)) {
  CAB_data_dem_imputed <- CAB_data_dem_imputed %>%
    mutate(
      !!paste0(platforms1[i], "_sm_no") := case_when(
        q22 == 2 ~ 0,       # Set to 0 if q22 is 2
        TRUE ~ as.numeric(.data[[variables1[i]]])  # Use the corresponding variable
      )
    )
}


# List of social media platforms and corresponding variables
table(CAB_data_dem_imputed$sm_positive_central_n_sc)
platformsG <- c("sm_engage_friends_n_sc","sm_engage_groups_n_sc","sm_engage_post_n_sc","sm_engage_critical_n_sc","sm_engage_supportive_n_sc", "sm_engage_offline_n_sc")
variablesG <- c("sm_engage_friends_n_sc","sm_engage_groups_n_sc","sm_engage_post_n_sc","sm_engage_critical_n_sc","sm_engage_supportive_n_sc", "sm_engage_offline_n_sc")

library(dplyr)
# Loop over each platform and create numeric versions of each social media usage variable
for (i in seq_along(variablesG)) {
  CAB_data_dem_imputed <- CAB_data_dem_imputed %>%
    mutate(
      !!paste0(platformsG[i], "_sm_no") := case_when(
        q22 == 2 ~ 0,       # Set to 0 if q22 is 2
        TRUE ~ as.numeric(.data[[variablesG[i]]])  # Use the corresponding variable
      )
    )
}

# Display summaries for verification
for (platform in platformsG) {
  print(paste0("Summary for ", platform, " (numeric):"))
  print(summary(CAB_data_dem_imputed[[paste0(platform, "_sm_no")]]))
}

table(CAB_data_dem_imputed$sm_engage_friends_n_sc)
table(CAB_data_dem_imputed$sm_engage_friends_n_sc_sm_no)




#Social Media Engagement Index##
sm_engage_index = cbind(
  CAB_data_dem_imputed$sm_engage_friends_n_sc_sm_no,
  CAB_data_dem_imputed$sm_engage_groups_n_sc_sm_no,
  CAB_data_dem_imputed$sm_engage_post_n_sc_sm_no,
  CAB_data_dem_imputed$sm_engage_critical_n_sc_sm_no,
  CAB_data_dem_imputed$sm_engage_supportive_n_sc_sm_no,
  CAB_data_dem_imputed$sm_engage_offline_n_sc_sm_no)
cronbach(sm_engage_index)

CAB_data_dem_imputed$sm_engage_index = rescale(
  CAB_data_dem_imputed$sm_engage_friends_n_sc_sm_no +
    CAB_data_dem_imputed$sm_engage_groups_n_sc_sm_no +
    CAB_data_dem_imputed$sm_engage_post_n_sc_sm_no +
    CAB_data_dem_imputed$sm_engage_critical_n_sc_sm_no +
    CAB_data_dem_imputed$sm_engage_supportive_n_sc_sm_no +
    CAB_data_dem_imputed$sm_engage_offline_n_sc_sm_no,
  to = c(0, 1)
)

table(CAB_data_dem_imputed$sm_engage_index)
hist(CAB_data_dem_imputed$sm_engage_friends_n_sc_sm_no)

#Critical TV Index
CAB_data_dem_imputed$critical_tv_index = cbind(
  CAB_data_dem_imputed$tv_critical_local_n_sc,
  CAB_data_dem_imputed$tv_critical_central_n_sc)
cronbach(CAB_data_dem_imputed$critical_tv_index)

CAB_data_dem_imputed$critical_tv_index = rescale(
  CAB_data_dem_imputed$tv_critical_local_n_sc +
    CAB_data_dem_imputed$tv_critical_central_n_sc,
  to = c(1, 0)
)




#Critical TV Index
CAB_data_dem_imputed$critical_tv_index = cbind(
  CAB_data_dem_imputed$tv_critical_local_n_sc,
  CAB_data_dem_imputed$tv_critical_central_n_sc)
cronbach(CAB_data_dem_imputed$critical_tv_index)

CAB_data_dem_imputed$critical_tv_index = rescale(
  CAB_data_dem_imputed$tv_critical_local_n_sc +
    CAB_data_dem_imputed$tv_critical_central_n_sc,
  to = c(1, 0)
)



hist(CAB_data_dem_imputed$national_defenders_index)
hist(CAB_data_dem_imputed$idconsisten_conserv_index)
hist(CAB_data_dem_imputed$surv_averse_lib_index)
hist(CAB_data_dem_imputed$gen_sm_index)
hist(CAB_data_dem_imputed$algdriven_sm_index)
hist(CAB_data_dem_imputed$socnet_driven_sm_index)
hist(CAB_data_dem_imputed$sm_engage_index)
hist(CAB_data_dem_imputed$critical_tv_index)
hist(CAB_data_dem_imputed$trust_legacy)
hist(CAB_data_dem_imputed$trust_russia_both)
hist(CAB_data_dem_imputed$critical_social_media)

################################################################################3
#Create country dummies
# Generating dummy variables using model.matrix
dummy_vars <- model.matrix(~ country - 1, data = CAB_data_dem_imputed)

# Renaming the dummy variables
# Replace spaces with underscores and prepend with the variable name for clarity
colnames(dummy_vars) <- gsub(" ", "_", colnames(dummy_vars))
colnames(dummy_vars) <- gsub("country", "country_", colnames(dummy_vars))

# Binding the dummy variables back to the original data frame
CAB_data_dem_imputed <- cbind(CAB_data_dem_imputed, dummy_vars)
rm(dummy_vars)

# Frequency tables for dummy variables of Country
table(CAB_data_dem_imputed$country_KZ)
table(CAB_data_dem_imputed$country_KG)
table(CAB_data_dem_imputed$country_GA)

summary(lm(critical_social_media ~ national_defenders_index + gen_sm_index + age_n_sc + urbanicity_Village + gender_Male + edu_n_sc + inc_n_sc + country_KZ + country_KG, data = CAB_data_dem_imputed))
summary(lm(critical_social_media ~ idconsisten_conserv_index + gen_sm_index + age_n_sc + urbanicity_Village + gender_Male + edu_n_sc + inc_n_sc + country_KZ + country_KG, data = CAB_data_dem_imputed))
summary(lm(critical_social_media ~ sil_obs_index + gen_sm_index + age_n_sc + urbanicity_Village + gender_Male + edu_n_sc + inc_n_sc + country_KZ + country_KG, data = CAB_data_dem_imputed))
summary(lm(critical_social_media ~ selective_avoiders_index + gen_sm_index + age_n_sc + urbanicity_Village + gender_Male + edu_n_sc + inc_n_sc + country_KZ + country_KG, data = CAB_data_dem_imputed))


# Load necessary package
library(lavaan)

# Define the SEM model
model_nd <- '
  national_defenders_index ~ a * critical_social_media + gen_sm_index + age_n_sc + urbanicity_Village +
                              gender_Male + edu_n_sc + inc_n_sc + country_KZ + country_KG

  critical_social_media ~ a * national_defenders_index + gen_sm_index + age_n_sc + urbanicity_Village +
                          gender_Male + edu_n_sc + inc_n_sc + country_KZ + country_KG
'



# Fit the model
fit_nd <- sem(model_nd, data = CAB_data_dem_imputed)

# Summarize the results
summary(fit_nd, standardized = TRUE, fit.measures = TRUE)

library(lavaan)

# Define the SEM model
model_id <- '
  idconsisten_conserv_index ~ a * critical_social_media + gen_sm_index + age_n_sc + urbanicity_Village +
                              gender_Male + edu_n_sc + inc_n_sc + country_KZ + country_KG

  critical_social_media ~ a * idconsisten_conserv_index + gen_sm_index + age_n_sc + urbanicity_Village +
                          gender_Male + edu_n_sc + inc_n_sc + country_KZ + country_KG
'



# Fit the model
fit_id <- sem(model_id, data = CAB_data_dem_imputed)

# Summarize the results
summary(fit_id, standardized = TRUE, fit.measures = TRUE)

summary(lm(positive_social_media ~ national_defenders_index + gen_sm_index + age_n_sc + urbanicity_Village + gender_Male + edu_n_sc + inc_n_sc + country_KZ + country_KG, data = CAB_data_dem_imputed))
summary(lm(positive_social_media ~ idconsisten_conserv_index + gen_sm_index + age_n_sc + urbanicity_Village + gender_Male + edu_n_sc + inc_n_sc + country_KZ + country_KG, data = CAB_data_dem_imputed))
summary(lm(positive_social_media ~ sil_obs_index + gen_sm_index + age_n_sc + urbanicity_Village + gender_Male + edu_n_sc + inc_n_sc + country_KZ + country_KG, data = CAB_data_dem_imputed))
summary(lm(positive_social_media ~ selective_avoiders_index + gen_sm_index + age_n_sc + urbanicity_Village + gender_Male + edu_n_sc + inc_n_sc + country_KZ + country_KG, data = CAB_data_dem_imputed))

summary(lm(critical_tv_index ~ national_defenders_index + gen_sm_index + age_n_sc + urbanicity_Village + gender_Male + edu_n_sc + inc_n_sc + country_KZ + country_KG, data = CAB_data_dem_imputed))
summary(lm(critical_tv_index ~ idconsisten_conserv_index + gen_sm_index + age_n_sc + urbanicity_Village + gender_Male + edu_n_sc + inc_n_sc + country_KZ + country_KG, data = CAB_data_dem_imputed))
summary(lm(critical_tv_index ~ sil_obs_index + gen_sm_index + age_n_sc + urbanicity_Village + gender_Male + edu_n_sc + inc_n_sc + country_KZ + country_KG, data = CAB_data_dem_imputed))
summary(lm(critical_tv_index ~ selective_avoiders_index + gen_sm_index + age_n_sc + urbanicity_Village + gender_Male + edu_n_sc + inc_n_sc + country_KZ + country_KG, data = CAB_data_dem_imputed))



#THIS IS THE BIG BANG AT THE END

summary(lm(news_balance_n_sc ~ national_defenders_index + gen_sm_index + age_n_sc + urbanicity_Village + gender_Male + edu_n_sc + inc_n_sc + country_KZ + country_KG, data = CAB_data_dem_imputed))
summary(lm(news_balance_n_sc  ~ idconsisten_conserv_index + gen_sm_index + age_n_sc + urbanicity_Village + gender_Male + edu_n_sc + inc_n_sc + country_KZ + country_KG, data = CAB_data_dem_imputed))
summary(lm(news_balance_n_sc  ~ sil_obs_index + gen_sm_index + age_n_sc + urbanicity_Village + gender_Male + edu_n_sc + inc_n_sc + country_KZ + country_KG, data = CAB_data_dem_imputed))
summary(lm(news_balance_n_sc  ~ dig_pol_engage_index + gen_sm_index + age_n_sc + urbanicity_Village + gender_Male + edu_n_sc + inc_n_sc + country_KZ + country_KG, data = CAB_data_dem_imputed))
summary(lm(news_balance_n_sc  ~ selective_avoiders_index + gen_sm_index + age_n_sc + urbanicity_Village + gender_Male + edu_n_sc + inc_n_sc + country_KZ + country_KG, data = CAB_data_dem_imputed))


write.csv(CAB_data_dem_imputed, "cab_data2ls.csv")
