#looping all variables####
dependent_vars <- c(dependent_vars <- c("Political_Instability", "Political_Instability_Combined", "Unemployment","Unemployment_Combined", "Inflation_or_Prices", "Inflation_or_Prices_Combined", "Wages_or_pensions", "Wages_or_pensions_Combined", "Taxes", "Taxes_Combined", "Access_to_basic_needs", "Access_to_basic_needs_Combined", "General_economic_situation", "General_economic_situation_Combined", "education", "education_Combined", "Housing", "Housing_Combined", "Terrorism", "Terrorism_Combined", "War_or_conflict_abroad", "War_or_conflict_abroad_Combined", "Personal_safety", "Personal_safety_Combined", "Corruption", "Corruption_Combined", "Lack_of_opportunities", "Lack_of_opportunities_Combined", "ethnic_or_religious_tensions", "ethnic_or_religious_tensions_Combined", "Infrastructure", "Infrastructure_Combined", "Environmental_change", "Environmental_change_Combined", "Unresolved_territorial_conflicts", "Unresolved_territorial_conflicts_Combined", "Emigration", "Emigration_Combined"))

models <- list()

#checking binominal regression - with loop####
for (dep_var in dependent_vars) {
  formula <- as.formula(paste(dep_var, "~ facebook_n_sc + instagram_n_sc + twitter_n_sc + vkontakte_n_sc + 
                               age_n_sc + gender_Male + urbanicity_City + edu_n_sc + inc_n_sc"))
  models[[dep_var]] <- glm(formula, data = CAB_data_dem, family = binomial)
}

#to check which dependent varibales are causing error####
models <- list()
warning_vars <- c()

# Loop through dependent variables and fit models
for (dep_var in dependent_vars) {
  # Construct the formula for each dependent variable
  formula <- as.formula(paste(dep_var, "~ instagram_n_sc + twitter_n_sc + vkontakte_n_sc + 
                               age_n_sc + gender_Male + urbanicity_City + edu_n_sc"))
  
  # Try to fit the model, catch warnings
  model_result <- tryCatch({
    glm(formula, data = CAB_data_dem, family = binomial)
  }, warning = function(w) {
    # If a warning occurs, log the dependent variable and show the warning message
    cat("Warning for variable:", dep_var, "\n", w$message, "\n")
    warning_vars <<- c(warning_vars, dep_var)  # Log the variable causing the warning
    return(NULL)  # Return NULL so it doesn't save the model with warnings
  }, error = function(e) {
    # If an error occurs, log it as well
    cat("Error for variable:", dep_var, "\n", e$message, "\n")
    return(NULL)
  })
  
  # If the model ran without warnings or errors, save it
  if (!is.null(model_result)) {
    models[[dep_var]] <- model_result
  }
}

# Output the variables that caused warnings
#cat("Variables that caused warnings:\n")
#print(warning_vars)


#for (dep_var in dependent_vars) {
#  cat("Summary of model for", dep_var, "\n")
#  print(summary(models[[dep_var]]))
#}

table(CAB_data_dem$Terrorism)
table(CAB_data_dem$Terrorism_Combined)
table(CAB_data_dem$Lack_of_opportunities)
table(CAB_data_dem$Lack_of_opportunities_Combined)

#the code from line 18 gave use the variables causing trouble####
#taking out the error causing varibles from the loop (terrorism and lack of opportunities)
dependent_vars <- c(dependent_vars <- c("Political_Instability", "Political_Instability_Combined", "Unemployment","Unemployment_Combined", "Inflation_or_Prices", "Inflation_or_Prices_Combined", "Wages_or_pensions", "Wages_or_pensions_Combined", "Taxes", "Taxes_Combined", "Access_to_basic_needs", "Access_to_basic_needs_Combined", "General_economic_situation", "General_economic_situation_Combined", "education", "education_Combined", "Housing", "Housing_Combined", "War_or_conflict_abroad", "War_or_conflict_abroad_Combined", "Personal_safety", "Personal_safety_Combined", "Corruption", "Corruption_Combined", "ethnic_or_religious_tensions", "ethnic_or_religious_tensions_Combined", "Infrastructure", "Infrastructure_Combined", "Environmental_change", "Environmental_change_Combined", "Unresolved_territorial_conflicts", "Unresolved_territorial_conflicts_Combined", "Emigration", "Emigration_Combined"))


#checking the binominal regression without the error causing variables####
for (dep_var in dependent_vars) {
  formula <- as.formula(paste(dep_var, "~ facebook_n_sc + instagram_n_sc + twitter_n_sc + vkontakte_n_sc + 
                               age_n_sc + gender_Male + urbanicity_City + edu_n_sc + inc_n_sc"))
  models[[dep_var]] <- glm(formula, data = CAB_data_dem, family = binomial)
}

#Checking summary of each variable
for (dep_var in dependent_vars) {
  cat("Summary of model for", dep_var, "\n")
  print(summary(models[[dep_var]]))
}

#Checking only those variables whcih have significant p-value####

