setwd("C:/Users/muham/Desktop/New folder (3)")

install.packages("readxl")
install.packages("dplyr")

library(readxl)
library(dplyr)

# Import KG_Data Excel File
KG_data <- read_excel("KG_data.xlsx")

# Create the new column - Political Instability as the most important issue or not
table(KG_data$q13_a)
KG_data <- KG_data %>%
  mutate(Political_Instability = ifelse(q13_a == 1, 1, 0))
table(KG_data$Political_Instability)

KG_data$Political_Instability_f = recode(KG_data$Political_Instability, "0 = 'Not Most Important Issue'; 1 = 'Most Important Issue'")
table(KG_data$Political_Instability_f)

# Create the new column - Political Instability as an important (first or second) issue or not 
KG_data <- KG_data %>%
  mutate(Political_Instability_Combined = ifelse(q13_a == 1 | q13_b == 1, 1, 0))

# Create the new column - Unemployment as the most important issue or not
KG_data <- KG_data %>%
  mutate(Unemployment = ifelse(q13_a == 2, 1, 0))

# Create the new column - Unemployment as an important (first or second) issue or not 
KG_data <- KG_data %>%
  mutate(Unemployment_Combined = ifelse(q13_a == 2 | q13_b == 2, 1, 0))

# Create the new column - Inflation_or_Prices as the most important issue or not
KG_data <- KG_data %>%
  mutate(Inflation_or_Prices = ifelse(q13_a == 3, 1, 0))

# Create the new column - Inflation_or_Prices as an important (first or second) issue or not 
KG_data <- KG_data %>%
  mutate(Inflation_or_Prices_Combined = ifelse(q13_a == 3 | q13_b == 3, 1, 0))

# Create the new column - Wages_or_pensions as the most important issue or not
KG_data <- KG_data %>%
  mutate(Wages_or_pensions = ifelse(q13_a == 4, 1, 0))

# Create the new column - Wages_or_pensions as an important (first or second) issue or not 
KG_data <- KG_data %>%
  mutate(Wages_or_pensions_Combined = ifelse(q13_a == 4 | q13_b == 4, 1, 0))

# Create the new column - Taxes as the most important issue or not
KG_data <- KG_data %>%
  mutate(Taxes = ifelse(q13_a == 5, 1, 0))

# Create the new column - Taxes as an important (first or second) issue or not 
KG_data <- KG_data %>%
  mutate(Taxes_Combined = ifelse(q13_a == 5 | q13_b == 5, 1, 0))

# Create the new column - Access_to_basic_needs as the most important issue or not
KG_data <- KG_data %>%
  mutate(Access_to_basic_needs = ifelse(q13_a == 6, 1, 0))

# Create the new column - Access_to_basic_needs as an important (first or second) issue or not 
KG_data <- KG_data %>%
  mutate(Access_to_basic_needs_Combined = ifelse(q13_a == 6 | q13_b == 6, 1, 0))

# Create the new column - General_economic_situation as the most important issue or not
KG_data <- KG_data %>%
  mutate(General_economic_situation = ifelse(q13_a == 7, 1, 0))

# Create the new column - General_economic_situation as an important (first or second) issue or not 
KG_data <- KG_data %>%
  mutate(General_economic_situation_Combined = ifelse(q13_a == 7 | q13_b == 7, 1, 0))

# Create the new column - Education as the most important issue or not
KG_data <- KG_data %>%
  mutate(education = ifelse(q13_a == 8, 1, 0))

# Create the new column - education as an important (first or second) issue or not 
KG_data <- KG_data %>%
  mutate(education_Combined = ifelse(q13_a == 8 | q13_b == 8, 1, 0))

# Create the new column - Housing as the most important issue or not
KG_data <- KG_data %>%
  mutate(Housing = ifelse(q13_a == 9, 1, 0))

# Create the new column - Housing as an important (first or second) issue or not 
KG_data <- KG_data %>%
  mutate(Housing_Combined = ifelse(q13_a == 9 | q13_b == 9, 1, 0))

# Create the new column - Terrorism as the most important issue or not
KG_data <- KG_data %>%
  mutate(Terrorism = ifelse(q13_a == 10, 1, 0))

# Create the new column - Terrorism as an important (first or second) issue or not 
KG_data <- KG_data %>%
  mutate(Terrorism_Combined = ifelse(q13_a == 10 | q13_b == 10, 1, 0))

# Create the new column - War_or_conflict_abroad as the most important issue or not
KG_data <- KG_data %>%
  mutate(War_or_conflict_abroad = ifelse(q13_a == 11, 1, 0))

# Create the new column - War_or_conflict_abroad as an important (first or second) issue or not 
KG_data <- KG_data %>%
  mutate(War_or_conflict_abroad_Combined = ifelse(q13_a == 11 | q13_b == 11, 1, 0))

# Create the new column - Personal_safety as the most important issue or not
KG_data <- KG_data %>%
  mutate(Personal_safety = ifelse(q13_a == 12, 1, 0))

# Create the new column - Personal_safety as an important (first or second) issue or not 
KG_data <- KG_data %>%
  mutate(Personal_safety_Combined = ifelse(q13_a == 12 | q13_b == 12, 1, 0))

# Create the new column - Corruption as the most important issue or not
KG_data <- KG_data %>%
  mutate(Corruption = ifelse(q13_a == 13, 1, 0))

# Create the new column - Corruption as an important (first or second) issue or not 
KG_data <- KG_data %>%
  mutate(Corruption_Combined = ifelse(q13_a == 13 | q13_b == 13, 1, 0))

# Create the new column - Lack_of_opportunities as the most important issue or not
KG_data <- KG_data %>%
  mutate(Lack_of_opportunities = ifelse(q13_a == 14, 1, 0))

# Create the new column - Lack_of_opportunities as an important (first or second) issue or not 
KG_data <- KG_data %>%
  mutate(Lack_of_opportunities_Combined = ifelse(q13_a == 14 | q13_b == 14, 1, 0))

# Create the new column - ethnic_or_religious_tensions as the most important issue or not
KG_data <- KG_data %>%
  mutate(ethnic_or_religious_tensions = ifelse(q13_a == 15, 1, 0))

# Create the new column - ethnic_or_religious_tensions as an important (first or second) issue or not 
KG_data <- KG_data %>%
  mutate(ethnic_or_religious_tensions_Combined = ifelse(q13_a == 15 | q13_b == 15, 1, 0))

# Create the new column - Infrastructure as the most important issue or not
KG_data <- KG_data %>%
  mutate(Infrastructure = ifelse(q13_a == 16, 1, 0))

# Create the new column - Infrastructure as an important (first or second) issue or not 
KG_data <- KG_data %>%
  mutate(Infrastructure_Combined = ifelse(q13_a == 16 | q13_b == 16, 1, 0))

# Create the new column - Environmental_change as the most important issue or not
KG_data <- KG_data %>%
  mutate(Environmental_change = ifelse(q13_a == 17, 1, 0))

# Create the new column - Environmental_change as an important (first or second) issue or not 
KG_data <- KG_data %>%
  mutate(Environmental_change_Combined = ifelse(q13_a == 17 | q13_b == 17, 1, 0))

# Create the new column - Unresolved_territorial_conflicts as the most important issue or not
KG_data <- KG_data %>%
  mutate(Unresolved_territorial_conflicts = ifelse(q13_a == 18, 1, 0))

# Create the new column - Unresolved_territorial_conflicts as an important (first or second) issue or not 
KG_data <- KG_data %>%
  mutate(Unresolved_territorial_conflicts_Combined = ifelse(q13_a == 18 | q13_b == 18, 1, 0))

# Create the new column - Emigration as the most important issue or not
KG_data <- KG_data %>%
  mutate(Emigration = ifelse(q13_a == 19, 1, 0))

# Create the new column - Emigration as an important (first or second) issue or not 
KG_data <- KG_data %>%
  mutate(Emigration_Combined = ifelse(q13_a == 19 | q13_b == 19, 1, 0))












# Import KZ_Data Excel File
KZ_data <- read_excel("KZ_data.xlsx")

# Create the new column - Political Instability as the most important issue or not
KZ_data <- KZ_data %>%
  mutate(Political_Instability = ifelse(q13_a == 1, 1, 0))

# Create the new column - Political Instability as an important (first or second) issue or not 
KZ_data <- KZ_data %>%
  mutate(Political_Instability_Combined = ifelse(q13_a == 1 | q13_b == 1, 1, 0))

# Create the new column - Unemployment as the most important issue or not
KZ_data <- KZ_data %>%
  mutate(Unemployment = ifelse(q13_a == 2, 1, 0))

# Create the new column - Unemployment as an important (first or second) issue or not 
KZ_data <- KZ_data %>%
  mutate(Unemployment_Combined = ifelse(q13_a == 2 | q13_b == 2, 1, 0))

# Create the new column - Inflation_or_Prices as the most important issue or not
KZ_data <- KZ_data %>%
  mutate(Inflation_or_Prices = ifelse(q13_a == 3, 1, 0))

# Create the new column - Inflation_or_Prices as an important (first or second) issue or not 
KZ_data <- KZ_data %>%
  mutate(Inflation_or_Prices_Combined = ifelse(q13_a == 3 | q13_b == 3, 1, 0))

# Create the new column - Wages_or_pensions as the most important issue or not
KZ_data <- KZ_data %>%
  mutate(Wages_or_pensions = ifelse(q13_a == 4, 1, 0))

# Create the new column - Wages_or_pensions as an important (first or second) issue or not 
KZ_data <- KZ_data %>%
  mutate(Wages_or_pensions_Combined = ifelse(q13_a == 4 | q13_b == 4, 1, 0))

# Create the new column - Taxes as the most important issue or not
KZ_data <- KZ_data %>%
  mutate(Taxes = ifelse(q13_a == 5, 1, 0))

# Create the new column - Taxes as an important (first or second) issue or not 
KZ_data <- KZ_data %>%
  mutate(Taxes_Combined = ifelse(q13_a == 5 | q13_b == 5, 1, 0))

# Create the new column - Access_to_basic_needs as the most important issue or not
KZ_data <- KZ_data %>%
  mutate(Access_to_basic_needs = ifelse(q13_a == 6, 1, 0))

# Create the new column - Access_to_basic_needs as an important (first or second) issue or not 
KZ_data <- KZ_data %>%
  mutate(Access_to_basic_needs_Combined = ifelse(q13_a == 6 | q13_b == 6, 1, 0))

# Create the new column - General_economic_situation as the most important issue or not
KZ_data <- KZ_data %>%
  mutate(General_economic_situation = ifelse(q13_a == 7, 1, 0))

# Create the new column - General_economic_situation as an important (first or second) issue or not 
KZ_data <- KZ_data %>%
  mutate(General_economic_situation_Combined = ifelse(q13_a == 7 | q13_b == 7, 1, 0))

# Create the new column - Education as the most important issue or not
KZ_data <- KZ_data %>%
  mutate(education = ifelse(q13_a == 8, 1, 0))

# Create the new column - education as an important (first or second) issue or not 
KZ_data <- KZ_data %>%
  mutate(education_Combined = ifelse(q13_a == 8 | q13_b == 8, 1, 0))

# Create the new column - Housing as the most important issue or not
KZ_data <- KZ_data %>%
  mutate(Housing = ifelse(q13_a == 9, 1, 0))

# Create the new column - Housing as an important (first or second) issue or not 
KZ_data <- KZ_data %>%
  mutate(Housing_Combined = ifelse(q13_a == 9 | q13_b == 9, 1, 0))

# Create the new column - Terrorism as the most important issue or not
KZ_data <- KZ_data %>%
  mutate(Terrorism = ifelse(q13_a == 10, 1, 0))

# Create the new column - Terrorism as an important (first or second) issue or not 
KZ_data <- KZ_data %>%
  mutate(Terrorism_Combined = ifelse(q13_a == 10 | q13_b == 10, 1, 0))

# Create the new column - War_or_conflict_abroad as the most important issue or not
KZ_data <- KZ_data %>%
  mutate(War_or_conflict_abroad = ifelse(q13_a == 11, 1, 0))

# Create the new column - War_or_conflict_abroad as an important (first or second) issue or not 
KZ_data <- KZ_data %>%
  mutate(War_or_conflict_abroad_Combined = ifelse(q13_a == 11 | q13_b == 11, 1, 0))

# Create the new column - Personal_safety as the most important issue or not
KZ_data <- KZ_data %>%
  mutate(Personal_safety = ifelse(q13_a == 12, 1, 0))

# Create the new column - Personal_safety as an important (first or second) issue or not 
KZ_data <- KZ_data %>%
  mutate(Personal_safety_Combined = ifelse(q13_a == 12 | q13_b == 12, 1, 0))

# Create the new column - Corruption as the most important issue or not
KZ_data <- KZ_data %>%
  mutate(Corruption = ifelse(q13_a == 13, 1, 0))

# Create the new column - Corruption as an important (first or second) issue or not 
KZ_data <- KZ_data %>%
  mutate(Corruption_Combined = ifelse(q13_a == 13 | q13_b == 13, 1, 0))

# Create the new column - Lack_of_opportunities as the most important issue or not
KZ_data <- KZ_data %>%
  mutate(Lack_of_opportunities = ifelse(q13_a == 14, 1, 0))

# Create the new column - Lack_of_opportunities as an important (first or second) issue or not 
KZ_data <- KZ_data %>%
  mutate(Lack_of_opportunities_Combined = ifelse(q13_a == 14 | q13_b == 14, 1, 0))

# Create the new column - ethnic_or_religious_tensions as the most important issue or not
KZ_data <- KZ_data %>%
  mutate(ethnic_or_religious_tensions = ifelse(q13_a == 15, 1, 0))

# Create the new column - ethnic_or_religious_tensions as an important (first or second) issue or not 
KZ_data <- KZ_data %>%
  mutate(ethnic_or_religious_tensions_Combined = ifelse(q13_a == 15 | q13_b == 15, 1, 0))

# Create the new column - Infrastructure as the most important issue or not
KZ_data <- KZ_data %>%
  mutate(Infrastructure = ifelse(q13_a == 16, 1, 0))

# Create the new column - Infrastructure as an important (first or second) issue or not 
KZ_data <- KZ_data %>%
  mutate(Infrastructure_Combined = ifelse(q13_a == 16 | q13_b == 16, 1, 0))

# Create the new column - Environmental_change as the most important issue or not
KZ_data <- KZ_data %>%
  mutate(Environmental_change = ifelse(q13_a == 17, 1, 0))

# Create the new column - Environmental_change as an important (first or second) issue or not 
KZ_data <- KZ_data %>%
  mutate(Environmental_change_Combined = ifelse(q13_a == 17 | q13_b == 17, 1, 0))

# Create the new column - Unresolved_territorial_conflicts as the most important issue or not
KZ_data <- KZ_data %>%
  mutate(Unresolved_territorial_conflicts = ifelse(q13_a == 18, 1, 0))

# Create the new column - Unresolved_territorial_conflicts as an important (first or second) issue or not 
KZ_data <- KZ_data %>%
  mutate(Unresolved_territorial_conflicts_Combined = ifelse(q13_a == 18 | q13_b == 18, 1, 0))

# Create the new column - Emigration as the most important issue or not
KZ_data <- KZ_data %>%
  mutate(Emigration = ifelse(q13_a == 19, 1, 0))

# Create the new column - Emigration as an important (first or second) issue or not 
KZ_data <- KZ_data %>%
  mutate(Emigration_Combined = ifelse(q13_a == 19 | q13_b == 19, 1, 0))









# Import TJ_Data Excel File
TJ_data <- read_excel("TJ_data.xlsx")

# Create the new column - Political Instability as the most important issue or not
TJ_data <- TJ_data %>%
  mutate(Political_Instability = ifelse(q13_a == 1, 1, 0))

# Create the new column - Political Instability as an important (first or second) issue or not 
TJ_data <- TJ_data %>%
  mutate(Political_Instability_Combined = ifelse(q13_a == 1 | q13_b == 1, 1, 0))

# Create the new column - Unemployment as the most important issue or not
TJ_data <- TJ_data %>%
  mutate(Unemployment = ifelse(q13_a == 2, 1, 0))

# Create the new column - Unemployment as an important (first or second) issue or not 
TJ_data <- TJ_data %>%
  mutate(Unemployment_Combined = ifelse(q13_a == 2 | q13_b == 2, 1, 0))

# Create the new column - Inflation_or_Prices as the most important issue or not
TJ_data <- TJ_data %>%
  mutate(Inflation_or_Prices = ifelse(q13_a == 3, 1, 0))

# Create the new column - Inflation_or_Prices as an important (first or second) issue or not 
TJ_data <- TJ_data %>%
  mutate(Inflation_or_Prices_Combined = ifelse(q13_a == 3 | q13_b == 3, 1, 0))

# Create the new column - Wages_or_pensions as the most important issue or not
TJ_data <- TJ_data %>%
  mutate(Wages_or_pensions = ifelse(q13_a == 4, 1, 0))

# Create the new column - Wages_or_pensions as an important (first or second) issue or not 
TJ_data <- TJ_data %>%
  mutate(Wages_or_pensions_Combined = ifelse(q13_a == 4 | q13_b == 4, 1, 0))

# Create the new column - Taxes as the most important issue or not
TJ_data <- TJ_data %>%
  mutate(Taxes = ifelse(q13_a == 5, 1, 0))

# Create the new column - Taxes as an important (first or second) issue or not 
TJ_data <- TJ_data %>%
  mutate(Taxes_Combined = ifelse(q13_a == 5 | q13_b == 5, 1, 0))

# Create the new column - Access_to_basic_needs as the most important issue or not
TJ_data <- TJ_data %>%
  mutate(Access_to_basic_needs = ifelse(q13_a == 6, 1, 0))

# Create the new column - Access_to_basic_needs as an important (first or second) issue or not 
TJ_data <- TJ_data %>%
  mutate(Access_to_basic_needs_Combined = ifelse(q13_a == 6 | q13_b == 6, 1, 0))

# Create the new column - General_economic_situation as the most important issue or not
TJ_data <- TJ_data %>%
  mutate(General_economic_situation = ifelse(q13_a == 7, 1, 0))

# Create the new column - General_economic_situation as an important (first or second) issue or not 
TJ_data <- TJ_data %>%
  mutate(General_economic_situation_Combined = ifelse(q13_a == 7 | q13_b == 7, 1, 0))

# Create the new column - Education as the most important issue or not
TJ_data <- TJ_data %>%
  mutate(education = ifelse(q13_a == 8, 1, 0))

# Create the new column - education as an important (first or second) issue or not 
TJ_data <- TJ_data %>%
  mutate(education_Combined = ifelse(q13_a == 8 | q13_b == 8, 1, 0))

# Create the new column - Housing as the most important issue or not
TJ_data <- TJ_data %>%
  mutate(Housing = ifelse(q13_a == 9, 1, 0))

# Create the new column - Housing as an important (first or second) issue or not 
TJ_data <- TJ_data %>%
  mutate(Housing_Combined = ifelse(q13_a == 9 | q13_b == 9, 1, 0))

# Create the new column - Terrorism as the most important issue or not
TJ_data <- TJ_data %>%
  mutate(Terrorism = ifelse(q13_a == 10, 1, 0))

# Create the new column - Terrorism as an important (first or second) issue or not 
TJ_data <- TJ_data %>%
  mutate(Terrorism_Combined = ifelse(q13_a == 10 | q13_b == 10, 1, 0))

# Create the new column - War_or_conflict_abroad as the most important issue or not
TJ_data <- TJ_data %>%
  mutate(War_or_conflict_abroad = ifelse(q13_a == 11, 1, 0))

# Create the new column - War_or_conflict_abroad as an important (first or second) issue or not 
TJ_data <- TJ_data %>%
  mutate(War_or_conflict_abroad_Combined = ifelse(q13_a == 11 | q13_b == 11, 1, 0))

# Create the new column - Personal_safety as the most important issue or not
TJ_data <- TJ_data %>%
  mutate(Personal_safety = ifelse(q13_a == 12, 1, 0))

# Create the new column - Personal_safety as an important (first or second) issue or not 
TJ_data <- TJ_data %>%
  mutate(Personal_safety_Combined = ifelse(q13_a == 12 | q13_b == 12, 1, 0))

# Create the new column - Corruption as the most important issue or not
TJ_data <- TJ_data %>%
  mutate(Corruption = ifelse(q13_a == 13, 1, 0))

# Create the new column - Corruption as an important (first or second) issue or not 
TJ_data <- TJ_data %>%
  mutate(Corruption_Combined = ifelse(q13_a == 13 | q13_b == 13, 1, 0))

# Create the new column - Lack_of_opportunities as the most important issue or not
TJ_data <- TJ_data %>%
  mutate(Lack_of_opportunities = ifelse(q13_a == 14, 1, 0))

# Create the new column - Lack_of_opportunities as an important (first or second) issue or not 
TJ_data <- TJ_data %>%
  mutate(Lack_of_opportunities_Combined = ifelse(q13_a == 14 | q13_b == 14, 1, 0))

# Create the new column - ethnic_or_religious_tensions as the most important issue or not
TJ_data <- TJ_data %>%
  mutate(ethnic_or_religious_tensions = ifelse(q13_a == 15, 1, 0))

# Create the new column - ethnic_or_religious_tensions as an important (first or second) issue or not 
TJ_data <- TJ_data %>%
  mutate(ethnic_or_religious_tensions_Combined = ifelse(q13_a == 15 | q13_b == 15, 1, 0))

# Create the new column - Infrastructure as the most important issue or not
TJ_data <- TJ_data %>%
  mutate(Infrastructure = ifelse(q13_a == 16, 1, 0))

# Create the new column - Infrastructure as an important (first or second) issue or not 
TJ_data <- TJ_data %>%
  mutate(Infrastructure_Combined = ifelse(q13_a == 16 | q13_b == 16, 1, 0))

# Create the new column - Environmental_change as the most important issue or not
TJ_data <- TJ_data %>%
  mutate(Environmental_change = ifelse(q13_a == 17, 1, 0))

# Create the new column - Environmental_change as an important (first or second) issue or not 
TJ_data <- TJ_data %>%
  mutate(Environmental_change_Combined = ifelse(q13_a == 17 | q13_b == 17, 1, 0))

# Create the new column - Unresolved_territorial_conflicts as the most important issue or not
TJ_data <- TJ_data %>%
  mutate(Unresolved_territorial_conflicts = ifelse(q13_a == 18, 1, 0))

# Create the new column - Unresolved_territorial_conflicts as an important (first or second) issue or not 
TJ_data <- TJ_data %>%
  mutate(Unresolved_territorial_conflicts_Combined = ifelse(q13_a == 18 | q13_b == 18, 1, 0))

# Create the new column - Emigration as the most important issue or not
TJ_data <- TJ_data %>%
  mutate(Emigration = ifelse(q13_a == 19, 1, 0))

# Create the new column - Emigration as an important (first or second) issue or not 
TJ_data <- TJ_data %>%
  mutate(Emigration_Combined = ifelse(q13_a == 19 | q13_b == 19, 1, 0))
















# Import UZ_Data Excel File
UZ_data <- read_excel("UZ_data.xlsx")

# Create the new column - Political Instability as the most important issue or not
UZ_data <- UZ_data %>%
  mutate(Political_Instability = ifelse(q13_a == 1, 1, 0))

# Create the new column - Political Instability as an important (first or second) issue or not 
UZ_data <- UZ_data %>%
  mutate(Political_Instability_Combined = ifelse(q13_a == 1 | q13_b == 1, 1, 0))

# Create the new column - Unemployment as the most important issue or not
UZ_data <- UZ_data %>%
  mutate(Unemployment = ifelse(q13_a == 2, 1, 0))

# Create the new column - Unemployment as an important (first or second) issue or not 
UZ_data <- UZ_data %>%
  mutate(Unemployment_Combined = ifelse(q13_a == 2 | q13_b == 2, 1, 0))

# Create the new column - Inflation_or_Prices as the most important issue or not
UZ_data <- UZ_data %>%
  mutate(Inflation_or_Prices = ifelse(q13_a == 3, 1, 0))

# Create the new column - Inflation_or_Prices as an important (first or second) issue or not 
UZ_data <- UZ_data %>%
  mutate(Inflation_or_Prices_Combined = ifelse(q13_a == 3 | q13_b == 3, 1, 0))

# Create the new column - Wages_or_pensions as the most important issue or not
UZ_data <- UZ_data %>%
  mutate(Wages_or_pensions = ifelse(q13_a == 4, 1, 0))

# Create the new column - Wages_or_pensions as an important (first or second) issue or not 
UZ_data <- UZ_data %>%
  mutate(Wages_or_pensions_Combined = ifelse(q13_a == 4 | q13_b == 4, 1, 0))

# Create the new column - Taxes as the most important issue or not
UZ_data <- UZ_data %>%
  mutate(Taxes = ifelse(q13_a == 5, 1, 0))

# Create the new column - Taxes as an important (first or second) issue or not 
UZ_data <- UZ_data %>%
  mutate(Taxes_Combined = ifelse(q13_a == 5 | q13_b == 5, 1, 0))

# Create the new column - Access_to_basic_needs as the most important issue or not
UZ_data <- UZ_data %>%
  mutate(Access_to_basic_needs = ifelse(q13_a == 6, 1, 0))

# Create the new column - Access_to_basic_needs as an important (first or second) issue or not 
UZ_data <- UZ_data %>%
  mutate(Access_to_basic_needs_Combined = ifelse(q13_a == 6 | q13_b == 6, 1, 0))

# Create the new column - General_economic_situation as the most important issue or not
UZ_data <- UZ_data %>%
  mutate(General_economic_situation = ifelse(q13_a == 7, 1, 0))

# Create the new column - General_economic_situation as an important (first or second) issue or not 
UZ_data <- UZ_data %>%
  mutate(General_economic_situation_Combined = ifelse(q13_a == 7 | q13_b == 7, 1, 0))

# Create the new column - Education as the most important issue or not
UZ_data <- UZ_data %>%
  mutate(education = ifelse(q13_a == 8, 1, 0))

# Create the new column - education as an important (first or second) issue or not 
UZ_data <- UZ_data %>%
  mutate(education_Combined = ifelse(q13_a == 8 | q13_b == 8, 1, 0))

# Create the new column - Housing as the most important issue or not
UZ_data <- UZ_data %>%
  mutate(Housing = ifelse(q13_a == 9, 1, 0))

# Create the new column - Housing as an important (first or second) issue or not 
UZ_data <- UZ_data %>%
  mutate(Housing_Combined = ifelse(q13_a == 9 | q13_b == 9, 1, 0))

# Create the new column - Terrorism as the most important issue or not
UZ_data <- UZ_data %>%
  mutate(Terrorism = ifelse(q13_a == 10, 1, 0))

# Create the new column - Terrorism as an important (first or second) issue or not 
UZ_data <- UZ_data %>%
  mutate(Terrorism_Combined = ifelse(q13_a == 10 | q13_b == 10, 1, 0))

# Create the new column - War_or_conflict_abroad as the most important issue or not
UZ_data <- UZ_data %>%
  mutate(War_or_conflict_abroad = ifelse(q13_a == 11, 1, 0))

# Create the new column - War_or_conflict_abroad as an important (first or second) issue or not 
UZ_data <- UZ_data %>%
  mutate(War_or_conflict_abroad_Combined = ifelse(q13_a == 11 | q13_b == 11, 1, 0))

# Create the new column - Personal_safety as the most important issue or not
UZ_data <- UZ_data %>%
  mutate(Personal_safety = ifelse(q13_a == 12, 1, 0))

# Create the new column - Personal_safety as an important (first or second) issue or not 
UZ_data <- UZ_data %>%
  mutate(Personal_safety_Combined = ifelse(q13_a == 12 | q13_b == 12, 1, 0))

# Create the new column - Corruption as the most important issue or not
UZ_data <- UZ_data %>%
  mutate(Corruption = ifelse(q13_a == 13, 1, 0))

# Create the new column - Corruption as an important (first or second) issue or not 
UZ_data <- UZ_data %>%
  mutate(Corruption_Combined = ifelse(q13_a == 13 | q13_b == 13, 1, 0))

# Create the new column - Lack_of_opportunities as the most important issue or not
UZ_data <- UZ_data %>%
  mutate(Lack_of_opportunities = ifelse(q13_a == 14, 1, 0))

# Create the new column - Lack_of_opportunities as an important (first or second) issue or not 
UZ_data <- UZ_data %>%
  mutate(Lack_of_opportunities_Combined = ifelse(q13_a == 14 | q13_b == 14, 1, 0))

# Create the new column - ethnic_or_religious_tensions as the most important issue or not
UZ_data <- UZ_data %>%
  mutate(ethnic_or_religious_tensions = ifelse(q13_a == 15, 1, 0))

# Create the new column - ethnic_or_religious_tensions as an important (first or second) issue or not 
UZ_data <- UZ_data %>%
  mutate(ethnic_or_religious_tensions_Combined = ifelse(q13_a == 15 | q13_b == 15, 1, 0))

# Create the new column - Infrastructure as the most important issue or not
UZ_data <- UZ_data %>%
  mutate(Infrastructure = ifelse(q13_a == 16, 1, 0))

# Create the new column - Infrastructure as an important (first or second) issue or not 
UZ_data <- UZ_data %>%
  mutate(Infrastructure_Combined = ifelse(q13_a == 16 | q13_b == 16, 1, 0))

# Create the new column - Environmental_change as the most important issue or not
UZ_data <- UZ_data %>%
  mutate(Environmental_change = ifelse(q13_a == 17, 1, 0))

# Create the new column - Environmental_change as an important (first or second) issue or not 
UZ_data <- UZ_data %>%
  mutate(Environmental_change_Combined = ifelse(q13_a == 17 | q13_b == 17, 1, 0))

# Create the new column - Unresolved_territorial_conflicts as the most important issue or not
UZ_data <- UZ_data %>%
  mutate(Unresolved_territorial_conflicts = ifelse(q13_a == 18, 1, 0))

# Create the new column - Unresolved_territorial_conflicts as an important (first or second) issue or not 
UZ_data <- UZ_data %>%
  mutate(Unresolved_territorial_conflicts_Combined = ifelse(q13_a == 18 | q13_b == 18, 1, 0))

# Create the new column - Emigration as the most important issue or not
UZ_data <- UZ_data %>%
  mutate(Emigration = ifelse(q13_a == 19, 1, 0))

# Create the new column - Emigration as an important (first or second) issue or not 
UZ_data <- UZ_data %>%
  mutate(Emigration_Combined = ifelse(q13_a == 19 | q13_b == 19, 1, 0))

# Create the new column - Political Instability as the most important issue or not
GA_data <- GA_data %>%
  mutate(Political_Instability = ifelse(q13_a == 1, 1, 0))

# Create the new column - Political Instability as an important (first or second) issue or not 
GA_data <- GA_data %>%
  mutate(Political_Instability_Combined = ifelse(q13_a == 1 | q13_b == 1, 1, 0))

# Create the new column - Unemployment as the most important issue or not
GA_data <- GA_data %>%
  mutate(Unemployment = ifelse(q13_a == 2, 1, 0))

# Create the new column - Unemployment as an important (first or second) issue or not 
GA_data <- GA_data %>%
  mutate(Unemployment_Combined = ifelse(q13_a == 2 | q13_b == 2, 1, 0))

# Create the new column - Inflation_or_Prices as the most important issue or not
GA_data <- GA_data %>%
  mutate(Inflation_or_Prices = ifelse(q13_a == 3, 1, 0))

# Create the new column - Inflation_or_Prices as an important (first or second) issue or not 
GA_data <- GA_data %>%
  mutate(Inflation_or_Prices_Combined = ifelse(q13_a == 3 | q13_b == 3, 1, 0))

# Create the new column - Wages_or_pensions as the most important issue or not
GA_data <- GA_data %>%
  mutate(Wages_or_pensions = ifelse(q13_a == 4, 1, 0))

# Create the new column - Wages_or_pensions as an important (first or second) issue or not 
GA_data <- GA_data %>%
  mutate(Wages_or_pensions_Combined = ifelse(q13_a == 4 | q13_b == 4, 1, 0))

# Create the new column - Taxes as the most important issue or not
GA_data <- GA_data %>%
  mutate(Taxes = ifelse(q13_a == 5, 1, 0))

# Create the new column - Taxes as an important (first or second) issue or not 
GA_data <- GA_data %>%
  mutate(Taxes_Combined = ifelse(q13_a == 5 | q13_b == 5, 1, 0))

# Create the new column - Access_to_basic_needs as the most important issue or not
GA_data <- GA_data %>%
  mutate(Access_to_basic_needs = ifelse(q13_a == 6, 1, 0))

# Create the new column - Access_to_basic_needs as an important (first or second) issue or not 
GA_data <- GA_data %>%
  mutate(Access_to_basic_needs_Combined = ifelse(q13_a == 6 | q13_b == 6, 1, 0))

# Create the new column - General_economic_situation as the most important issue or not
GA_data <- GA_data %>%
  mutate(General_economic_situation = ifelse(q13_a == 7, 1, 0))

# Create the new column - General_economic_situation as an important (first or second) issue or not 
GA_data <- GA_data %>%
  mutate(General_economic_situation_Combined = ifelse(q13_a == 7 | q13_b == 7, 1, 0))

# Create the new column - Education as the most important issue or not
GA_data <- GA_data %>%
  mutate(education = ifelse(q13_a == 8, 1, 0))

# Create the new column - education as an important (first or second) issue or not 
GA_data <- GA_data %>%
  mutate(education_Combined = ifelse(q13_a == 8 | q13_b == 8, 1, 0))

# Create the new column - Housing as the most important issue or not
GA_data <- GA_data %>%
  mutate(Housing = ifelse(q13_a == 9, 1, 0))

# Create the new column - Housing as an important (first or second) issue or not 
GA_data <- GA_data %>%
  mutate(Housing_Combined = ifelse(q13_a == 9 | q13_b == 9, 1, 0))

# Create the new column - Terrorism as the most important issue or not
GA_data <- GA_data %>%
  mutate(Terrorism = ifelse(q13_a == 10, 1, 0))

# Create the new column - Terrorism as an important (first or second) issue or not 
GA_data <- GA_data %>%
  mutate(Terrorism_Combined = ifelse(q13_a == 10 | q13_b == 10, 1, 0))

# Create the new column - War_or_conflict_abroad as the most important issue or not
GA_data <- GA_data %>%
  mutate(War_or_conflict_abroad = ifelse(q13_a == 11, 1, 0))

# Create the new column - War_or_conflict_abroad as an important (first or second) issue or not 
GA_data <- GA_data %>%
  mutate(War_or_conflict_abroad_Combined = ifelse(q13_a == 11 | q13_b == 11, 1, 0))

# Create the new column - Personal_safety as the most important issue or not
GA_data <- GA_data %>%
  mutate(Personal_safety = ifelse(q13_a == 12, 1, 0))

# Create the new column - Personal_safety as an important (first or second) issue or not 
GA_data <- GA_data %>%
  mutate(Personal_safety_Combined = ifelse(q13_a == 12 | q13_b == 12, 1, 0))

# Create the new column - Corruption as the most important issue or not
GA_data <- GA_data %>%
  mutate(Corruption = ifelse(q13_a == 13, 1, 0))

# Create the new column - Corruption as an important (first or second) issue or not 
GA_data <- GA_data %>%
  mutate(Corruption_Combined = ifelse(q13_a == 13 | q13_b == 13, 1, 0))

# Create the new column - Lack_of_opportunities as the most important issue or not
GA_data <- GA_data %>%
  mutate(Lack_of_opportunities = ifelse(q13_a == 14, 1, 0))

# Create the new column - Lack_of_opportunities as an important (first or second) issue or not 
GA_data <- GA_data %>%
  mutate(Lack_of_opportunities_Combined = ifelse(q13_a == 14 | q13_b == 14, 1, 0))

# Create the new column - ethnic_or_religious_tensions as the most important issue or not
GA_data <- GA_data %>%
  mutate(ethnic_or_religious_tensions = ifelse(q13_a == 15, 1, 0))

# Create the new column - ethnic_or_religious_tensions as an important (first or second) issue or not 
GA_data <- GA_data %>%
  mutate(ethnic_or_religious_tensions_Combined = ifelse(q13_a == 15 | q13_b == 15, 1, 0))

# Create the new column - Infrastructure as the most important issue or not
GA_data <- GA_data %>%
  mutate(Infrastructure = ifelse(q13_a == 16, 1, 0))

# Create the new column - Infrastructure as an important (first or second) issue or not 
GA_data <- GA_data %>%
  mutate(Infrastructure_Combined = ifelse(q13_a == 16 | q13_b == 16, 1, 0))

# Create the new column - Environmental_change as the most important issue or not
GA_data <- GA_data %>%
  mutate(Environmental_change = ifelse(q13_a == 17, 1, 0))

# Create the new column - Environmental_change as an important (first or second) issue or not 
GA_data <- GA_data %>%
  mutate(Environmental_change_Combined = ifelse(q13_a == 17 | q13_b == 17, 1, 0))

# Create the new column - Unresolved_territorial_conflicts as the most important issue or not
GA_data <- GA_data %>%
  mutate(Unresolved_territorial_conflicts = ifelse(q13_a == 18, 1, 0))

# Create the new column - Unresolved_territorial_conflicts as an important (first or second) issue or not 
GA_data <- GA_data %>%
  mutate(Unresolved_territorial_conflicts_Combined = ifelse(q13_a == 18 | q13_b == 18, 1, 0))

# Create the new column - Emigration as the most important issue or not
GA_data <- GA_data %>%
  mutate(Emigration = ifelse(q13_a == 19, 1, 0))

# Create the new column - Emigration as an important (first or second) issue or not 
GA_data <- GA_data %>%
  mutate(Emigration_Combined = ifelse(q13_a == 19 | q13_b == 19, 1, 0))

#Single Dataframe - All Countries############################################################################################
library(data.table)
CAB_data = rbindlist(list(GA_data,KG_data,KZ_data,TJ_data,UZ_data), fill = TRUE)
table(CAB_data$country)


