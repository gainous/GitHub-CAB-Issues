#Recording Issues into Three Overarching Categories####

CAB_data_oct=CAB_data_dem

# Create the 'Political Violence' column

# Political instability, Corruption, Discrimination/ethnic or religious tensions####
CAB_data_oct$pol_issues <- ifelse(CAB_data_oct$q13_a %in% c(1, 13, 15) | CAB_data_oct$q13_b %in% c(1, 13, 15), 1, 0)

# Unemployment, Inflation/Prices, Wages/Pensions, Lack of opportunities, General economic situation, Access to basic needs (food, electricity, medicine, etc)####
CAB_data_oct$eco_issues <- ifelse(CAB_data_oct$q13_a %in% c(2, 3, 4, 6, 7, 14) | CAB_data_oct$q13_b %in% c(2, 3, 4, 6, 7, 14), 1, 0)

# Deterioration of the quality and access to education, Increase in crime/violence/concerns about personal safety, Emigration####
CAB_data_oct$soc_issues <- ifelse(CAB_data_oct$q13_a %in% c(8, 12, 19) | CAB_data_oct$q13_b %in% c(8, 12, 19), 1, 0)

# Terrorism, War/conflict in other countries, Unresolved territorial conflicts####
CAB_data_oct$sec_issues <- ifelse(CAB_data_oct$q13_a %in% c(10, 11, 18) | CAB_data_oct$q13_b %in% c(10, 11, 18), 1, 0)

# Deterioration of the environment, Infrastructure issues###
CAB_data_oct$env_issues <- ifelse(CAB_data_oct$q13_a %in% c(8, 16) | CAB_data_oct$q13_b %in% c(8,16), 1, 0)


table(CAB_data_oct$pol_issues)
table(CAB_data_oct$eco_issues)
table(CAB_data_oct$soc_issues)
table(CAB_data_oct$sec_issues)
table(CAB_data_oct$sec_issues)
table(CAB_data_oct$env_issues)


library(writexl)
write_xlsx(CAB_data_oct, "CAB_data_oct.xlsx")
