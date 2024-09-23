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

CAB_data_Recoded$Unresolved_territorial_conflicts_Combined

library(Rcmdr)

names(CAB_data_Recoded)
library(Hmisc, pos=19)
rcorr.adjust(CAB_data_Recoded[,c("tv_critical_index",
                                 "Political_Instability_Combined",                                                  
                                 "Unemployment_Combined",                                                           
                                 "Inflation_or_Prices_Combined" ,                                                   
                                 "Wages_or_pensions_Combined"  ,                                                    
                                 "Taxes_Combined"        ,                                                          
                                 "Access_to_basic_needs_Combined" ,                                                 
                                 "General_economic_situation_Combined"   ,                                          
                                 "education_Combined"   ,                                                           
                                 "Housing_Combined"   ,                                                             
                                 "Terrorism_Combined"     ,                                                         
                                 "War_or_conflict_abroad_Combined"   ,                                              
                                 "Personal_safety_Combined"  ,                                                      
                                 "Corruption_Combined" ,                                                            
                                 "Lack_of_opportunities_Combined"  ,                                                
                                 "ethnic_or_religious_tensions_Combined" ,                                          
                                 "Infrastructure_Combined"     ,                                                    
                                 "Environmental_change_Combined"  ,                                                 
                                 "Unresolved_territorial_conflicts_Combined"  ,                                     
                                 "Emigration_Combined"    )], 
             type="pearson", use="complete")

     