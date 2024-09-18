#Recording Issues into Three Overarching Categories####

CAB_data_Recoded=CAB_data_dem

# Create the 'Political Violence' column
CAB_data_Recoded$Political_Violence <- ifelse(CAB_data_Recoded$q13_a %in% c(10, 11, 18) | CAB_data_Recoded$q13_b %in% c(10, 11, 18), 1, 0)
CAB_data_Recoded$Domestic_Issues <- ifelse(CAB_data_Recoded$q13_a %in% c(1, 8, 9, 12, 13, 15, 16, 19) | CAB_data_Recoded$q13_b %in% c(1, 8, 9, 12, 13, 15, 16, 19), 1, 0)
CAB_data_Recoded$Economic_Issues <- ifelse(CAB_data_Recoded$q13_a %in% c(2, 3, 4, 5, 6, 7, 14) | CAB_data_Recoded$q13_b %in% c(2, 3, 4, 5, 6, 7, 14), 1, 0)

table(CAB_data_Recoded$Political_Violence)
table(CAB_data_Recoded$Domestic_Issues)
table(CAB_data_Recoded$Economic_Issues)

library(writexl)
write_xlsx(CAB_data_Recoded, "CAB_data_Recoded.xlsx")
