significant_models <- list()   # To store models with significant predictors
significance_level <- 0.05     # Define your threshold for p-value significance

model <- glm(formula, data = CAB_data_dem, family = binomial)
models[[dep_var]] <- model

# Extract the p-values from the model summary
p_values <- summary(model)$coefficients[, "Pr(>|z|)"]

# Check if any p-value is below the significance threshold
if (any(p_values < significance_level)) {
  significant_models[[dep_var]] <- model  # Store the model if any p-value is significant
}
}

# Output the names of the dependent variables with significant results
cat("Models with significant predictors (p-value < 0.05):\n")
print(names(significant_models))


significant_results <- list()  # To store significant results (p-values < 0.05)


model_summary <- summary(model)$coefficients
p_values <- model_summary[, "Pr(>|z|)"]
significant <- model_summary[p_values < significance_level, , drop = FALSE]











# Store significant results if any exist
if (nrow(significant) > 0) {
  significant_results[[dep_var]] <- significant
}
}

# Prepare a data frame to hold the final results
significant_df <- data.frame(
  Dependent_Variable = character(),
  Predictor = character(),
  Estimate = numeric(),
  Std_Error = numeric(),
  Z_value = numeric(),
  P_value = numeric(),
  Significance = character(),
  stringsAsFactors = FALSE
)

# Loop through the significant results and add them to the data frame
for (dep_var in names(significant_results)) {
  sig_data <- significant_results[[dep_var]]
  for (i in 1:nrow(sig_data)) {
    sig_symbol <- ifelse(sig_data[i, "Pr(>|z|)"] < 0.001, "***",
                         ifelse(sig_data[i, "Pr(>|z|)"] < 0.01, "**",
                                ifelse(sig_data[i, "Pr(>|z|)"] < 0.05, "*", "")))
    
    significant_df <- rbind(
      significant_df,
      data.frame(
        Dependent_Variable = dep_var,
        Predictor = rownames(sig_data)[i],
        Estimate = sig_data[i, "Estimate"],
        Std_Error = sig_data[i, "Std. Error"],
        Z_value = sig_data[i, "z value"],
        P_value = sig_data[i, "Pr(>|z|)"],
        Significance = sig_symbol,
        stringsAsFactors = FALSE
      )
    )
  }
}

install.packages("officer")
install.packages("flextable")
library(flextable)
install.packages("magrittr")  # Install magrittr if not already installed
library(magrittr)  # Load magrittr for the pipe operator
library("magrittr")

library(officer)
# Create a Word document and add the table
doc <- read_docx()  # Initialize a new Word document
doc <- doc %>%
  body_add_par("Significant Predictors with p-values < 0.05", style = "heading 1") %>%
  body_add_flextable(flextable(significant_df))  # Add the table to the document

# Save the Word document
print(doc, target = "significant_results.docx")

cat("Word document with significant results has been created: 'significant_results.docx'")
