library(dplyr)
library(scales)


#SM Clickable Links about State and Russian - With Q22=2####
#for q29 - sm clickable links about state media or russian media

# List of social media platforms and corresponding variables
platforms <- c("clickable_state_", "clickable_russian_")
variables <- c("q29_a", "q29_b")

# Loop over each platform and create numeric versions of each social media usage variable
for (i in seq_along(variables)) {
  CAB_data_dem <- CAB_data_dem %>%
    mutate(
      !!paste0(platforms[i], "from_sm_no_n") := case_when(
        q22 == 2 ~ 5,       # Set to 0 if q22 is 2
        TRUE ~ as.numeric(.data[[variables[i]]])  # Use the corresponding q29_ variable
      )
    )
}


# Rescale the new numeric columns to range 1-0
CAB_data_dem <- CAB_data_dem %>%
  mutate(
    across(ends_with("from_sm_no_n"), ~ rescale(., to = c(1, 0)))
  )

# Display summaries for verification
for (platform in platforms) {
  print(paste0("Summary for ", platform, " (numeric):"))
  print(summary(CAB_data_dem[[paste0(platform, "from_sm_no_n")]]))
}
names(CAB_data_dem)










#Echo Chamber With q22=2####
#for q31 - sm clickable links about state media or russian media

# List of social media platforms and corresponding variables
platforms <- c("avoidance_blocking_", "avoidance_unfriending_", "avoidance_leaving_group_", "avoidance_unsubscribing_")
variables <- c("q35_a", "q35_b", "q35_c", "q35_d")

# Loop over each platform and create numeric versions of each social media usage variable
for (i in seq_along(variables)) {
  CAB_data_dem <- CAB_data_dem %>%
    mutate(
      !!paste0(platforms[i], "from_sm_no_n") := case_when(
        q22 == 2 ~ 4,       # Set to 0 if q22 is 2
        TRUE ~ as.numeric(.data[[variables[i]]])  # Use the corresponding q35_ variable
      )
    )
}


# Rescale the new numeric columns to range 0-1
CAB_data_dem <- CAB_data_dem %>%
  mutate(
    across(ends_with("from_sm_no_n"), ~ rescale(., to = c(1, 0)))
  )

# Display summaries for verification
for (platform in platforms) {
  print(paste0("Summary for ", platform, " (numeric):"))
  print(summary(CAB_data_dem[[paste0(platform, "from_sm_no_n")]]))
}
names(CAB_data_dem)




##SM Disagreement With q22=2####

# List of social media platforms and corresponding variables
platforms <- c("sm_disagreement_politics_", "sm_disagreement_news_", "sm_disagreement_issues_", "avoidance_unsubscribing_")
variables <- c("q36_a", "q36_b", "q36_c")

# Loop over each platform and create numeric versions of each social media usage variable
for (i in seq_along(variables)) {
  CAB_data_dem <- CAB_data_dem %>%
    mutate(
      !!paste0(platforms[i], "from_sm_no_n") := case_when(
        q22 == 2 ~ 4,       # Set to 0 if q22 is 2
        TRUE ~ as.numeric(.data[[variables[i]]])  # Use the corresponding q36_ variable
      )
    )
}


# Rescale the new numeric columns to range 0-1
CAB_data_dem <- CAB_data_dem %>%
  mutate(
    across(ends_with("from_sm_no_n"), ~ rescale(., to = c(0, 1)))
  )

# Display summaries for verification
for (platform in platforms) {
  print(paste0("Summary for ", platform, " (numeric):"))
  print(summary(CAB_data_dem[[paste0(platform, "from_sm_no_n")]]))
}
names(CAB_data_dem)



##network_breadth With q22=2####

# List of social media platforms and corresponding variables
platforms <- c("network_breadth")
variables <- c("q37")

# Loop over each platform and create numeric versions of each social media usage variable
for (i in seq_along(variables)) {
  CAB_data_dem <- CAB_data_dem %>%
    mutate(
      !!paste0(platforms[i], "from_sm_no_n") := case_when(
        q22 == 2 ~ 6,       # Set to 0 if q22 is 2
        TRUE ~ as.numeric(.data[[variables[i]]])  # Use the corresponding q37_ variable
      )
    )
}


# Rescale the new numeric columns to range 0-1
CAB_data_dem <- CAB_data_dem %>%
  mutate(
    across(ends_with("from_sm_no_n"), ~ rescale(., to = c(0, 1)))
  )

# Display summaries for verification
for (platform in platforms) {
  print(paste0("Summary for ", platform, " (numeric):"))
  print(summary(CAB_data_dem[[paste0(platform, "from_sm_no_n")]]))
}
names(CAB_data_dem)





##Social Media Political Activity With q22=2####

# List of social media platforms and corresponding variables
platforms <- c("sm_engage_friends_", "sm_engage_groups_", "sm_engage_post_", "sm_engage_critical_", "sm_engage_supportive_", "sm_engage_offline_")
variables <- c("q38_a", "q38_b", "q38_c", "q38_d", "q38_e", "q38_f")

# Loop over each platform and create numeric versions of each social media usage variable
for (i in seq_along(variables)) {
  CAB_data_dem <- CAB_data_dem %>%
    mutate(
      !!paste0(platforms[i], "from_sm_no_n") := case_when(
        q22 == 2 ~ 6,       # Set to 0 if q22 is 2
        TRUE ~ as.numeric(.data[[variables[i]]])  # Use the corresponding q38_ variable
      )
    )
}


# Rescale the new numeric columns to range 1-0
CAB_data_dem <- CAB_data_dem %>%
  mutate(
    across(ends_with("from_sm_no_n"), ~ rescale(., to = c(1, 0)))
  )

# Display summaries for verification
for (platform in platforms) {
  print(paste0("Summary for ", platform, " (numeric):"))
  print(summary(CAB_data_dem[[paste0(platform, "from_sm_no_n")]]))
}
names(CAB_data_dem)








####SM Positive local and central With q22=2####

# List of social media platforms and corresponding variables
platforms <- c("sm_positive_local_", "sm_positive_central_")
variables <- c("q27_c", "q27_d")

# Loop over each platform and create numeric versions of each social media usage variable
for (i in seq_along(variables)) {
  CAB_data_dem <- CAB_data_dem %>%
    mutate(
      !!paste0(platforms[i], "from_sm_no_n") := case_when(
        q22 == 2 ~ 4,       # Set to 0 if q22 is 2
        TRUE ~ as.numeric(.data[[variables[i]]])  # Use the corresponding q27_ variable
      )
    )
}


# Rescale the new numeric columns to range 1-0
CAB_data_dem <- CAB_data_dem %>%
  mutate(
    across(ends_with("no_n"), ~ rescale(., to = c(1, 0)))
  )

# Display summaries for verification
for (platform in platforms) {
  print(paste0("Summary for ", platform, " (numeric):"))
  print(summary(CAB_data_dem[[paste0(platform, "from_sm_no_n")]]))
}
names(CAB_data_dem)





