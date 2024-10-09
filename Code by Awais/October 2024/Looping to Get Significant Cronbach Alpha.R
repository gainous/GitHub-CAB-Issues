# Load required libraries
library(psy)
library(combinat)

# Define the function to find all combinations with Cronbach's alpha greater than a given threshold
find_all_alpha_above_threshold <- function(data, threshold = 0.5) {
  # Get all possible combinations of items
  item_names <- colnames(data)
  qualifying_combinations <- list()
  
  # Loop through different combinations of items
  for (n in 2:length(item_names)) {  # Starting from 2 items to avoid single-item combinations
    combinations <- combn(item_names, n, simplify = FALSE)
    for (comb in combinations) {
      current_data <- data[, comb, drop = FALSE]
      alpha_value <- cronbach(current_data)$alpha
      
      # Check if the combination's alpha value meets the threshold
      if (alpha_value > threshold) {
        qualifying_combinations <- append(qualifying_combinations, list(list(items = comb, alpha = alpha_value)))
      }
    }
  }
  
  # Return all combinations and their Cronbach's alpha values that meet the threshold
  if (length(qualifying_combinations) > 0) {
    return(qualifying_combinations)
  } else {
    return("No combinations met the threshold for Cronbach's alpha.")
  }
}

# Create your data frame with the relevant columns
test <- data.frame(
    a = CAB_data_dem$pol_dis,
    # b = CAB_data_dem$trust_central_silob,
    c = CAB_data_dem$trust_local_silob,
    d = CAB_data_dem$sm_critical_local_n_sc,
    e = CAB_data_dem$sm_critical_central_n_sc,
    f = CAB_data_dem$sm_disagreement_politics_silob,
    g = CAB_data_dem$sm_engage_friends_silob,
    h = CAB_data_dem$sm_engage_groups_silob,
    i = CAB_data_dem$sm_engage_post_silob,
    j = CAB_data_dem$sm_engage_critical_silob,
    k = CAB_data_dem$sm_engage_supportive_silob,
    l = CAB_data_dem$sm_engage_offline_silob)
library(psy)
  # Calculating the Cronbach's alpha for sm_warriors
  cronbach(test)


# Run the function to find all combinations with Cronbach's alpha greater than 0.5
result <- find_all_alpha_above_threshold(test, threshold = 0.5)

# Display all qualifying combinations and their Cronbach's alpha values
if (is.list(result)) {
  for (i in seq_along(result)) {
    cat("Combination", i, ":\n")
    cat("Items:", paste(result[[i]]$items, collapse = ", "), "\n")
    cat("Cronbach's Alpha:", result[[i]]$alpha, "\n\n")
  }
} else {
  cat(result, "\n")  # Display the message if no combination met the threshold
}

