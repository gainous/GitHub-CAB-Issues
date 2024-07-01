# Load necessary libraries
library(dplyr)
library(car)

#Import from SPSS, then Export Numeric, and Import - This deals with all of the SPSS labeling issues.
library(haven)
KG_Dataset_Checked_22_02_24 <- read_sav("C:/Users/102142/Dropbox/Research/Internet Politics/Central Asia/Central Asia Grant Survey Project/Data Files/KG Dataset Checked 22.02.24.sav")
KG_data = KG_Dataset_Checked_22_02_24
rm(KG_Dataset_Checked_22_02_24)

KZ_Dataset_Checked_22_02_24 <- read_sav("C:/Users/102142/Dropbox/Research/Internet Politics/Central Asia/Central Asia Grant Survey Project/Data Files/KZ Dataset Checked 22.02.24.sav")
KZ_data = KZ_Dataset_Checked_22_02_24
rm(KZ_Dataset_Checked_22_02_24)

UZ_Dataset_Checked_22_02_24 <- read_sav("C:/Users/102142/Dropbox/Research/Internet Politics/Central Asia/Central Asia Grant Survey Project/Data Files/UZ Dataset Checked 22.02.24.sav")
UZ_data = UZ_Dataset_Checked_22_02_24
rm(UZ_Dataset_Checked_22_02_24)

GE_Dataset_Checked_22_02_24 <- read_sav("C:/Users/102142/Dropbox/Research/Internet Politics/Central Asia/Central Asia Grant Survey Project/Data Files/GE Dataset Checked 22.02.24.sav")
GA_data = GE_Dataset_Checked_22_02_24
rm(GE_Dataset_Checked_22_02_24)

TJ_Dataset_Checked_22_02_24 <- read_sav("C:/Users/102142/Dropbox/Research/Internet Politics/Central Asia/Central Asia Grant Survey Project/Data Files/TJ Dataset Checked 22.02.24.sav")
TJ_data = TJ_Dataset_Checked_22_02_24
rm(TJ_Dataset_Checked_22_02_24)

#Export as CSV and then Manually change to Excel
write.csv(KZ_data, "KZ_data.csv")
rm(KZ_data)

write.csv(UZ_data, "UZ_data.csv")
rm(UZ_data)

write.csv(KG_data, "KG_data.csv")
rm(KG_data)

write.csv(GA_data, "GA_data.csv")
rm(GA_data)

write.csv(TJ_data, "TJ_data.csv")
rm(TJ_data)

library(readxl)
UZ_data <- read_excel("UZ_data.xlsx")
TJ_data <- read_excel("TJ_data.xlsx")
KZ_data <- read_excel("KZ_data.xlsx")
KG_data <- read_excel("KG_data.xlsx")
GA_data <- read_excel("GA_data.xlsx")

#Making Weight variable same name
names(KZ_data)
KZ_data$weight = KZ_data$Weight
KZ_data$Weight = NULL

names(KG_data)
KG_data$weight = KG_data$Weight
KG_data$Weight = NULL

#Fixing all the Social Media Codes (they were coded in different ways across countries)
table(KZ_data$q23_a)
KZ_data$q23_a = recode(KZ_data$q23_a, '1=0;2=1;3=2;4=3;5=4;6=5;7=6;8=7;9=8;10=9;11=10;-7=-7;98=98;99=99')
table(KZ_data$q23_a)

table(KZ_data$q23_b)
KZ_data$q23_b = recode(KZ_data$q23_b, '1=0;2=1;3=2;4=3;5=4;6=5;7=6;8=7;9=8;10=9;11=10;-7=-7;98=98;99=99')
table(KZ_data$q23_b)

table(KZ_data$q23_c)
KZ_data$q23_c = recode(KZ_data$q23_c, '1=0;2=1;3=2;4=3;5=4;6=5;7=6;8=7;9=8;10=9;11=10;-7=-7;98=98;99=99')
table(KZ_data$q23_c)

table(KZ_data$q23_d)
KZ_data$q23_d = recode(KZ_data$q23_d, '1=0;2=1;3=2;4=3;5=4;6=5;7=6;8=7;9=8;10=9;11=10;-7=-7;98=98;99=99')
table(KZ_data$q23_d)

table(KZ_data$q23_e)
KZ_data$q23_e = recode(KZ_data$q23_e, '1=0;2=1;3=2;4=3;5=4;6=5;7=6;8=7;9=8;10=9;11=10;-7=-7;98=98;99=99')
table(KZ_data$q23_e)

table(KZ_data$q23_f)
KZ_data$q23_f = recode(KZ_data$q23_f, '1=0;2=1;3=2;4=3;5=4;6=5;7=6;8=7;9=8;10=9;11=10;-7=-7;98=98;99=99')
table(KZ_data$q23_f)

table(KZ_data$q23_g)
KZ_data$q23_g = recode(KZ_data$q23_g, '1=0;2=1;3=2;4=3;5=4;6=5;7=6;8=7;9=8;10=9;11=10;-7=-7;98=98;99=99')
table(KZ_data$q23_g)

table(KZ_data$q23_h)
KZ_data$q23_h = recode(KZ_data$q23_h, '1=0;2=1;3=2;4=3;5=4;6=5;7=6;8=7;9=8;10=9;11=10;-7=-7;98=98;99=99')
table(KZ_data$q23_h)

table(KG_data$q23_a)
KG_data$q23_a = recode(KG_data$q23_a, '1=0;2=1;3=2;4=3;5=4;6=5;7=6;8=7;9=8;10=9;11=10;-7=-7;98=98;99=99')
table(KG_data$q23_a)

table(KG_data$q23_b)
KG_data$q23_b = recode(KG_data$q23_b, '1=0;2=1;3=2;4=3;5=4;6=5;7=6;8=7;9=8;10=9;11=10;-7=-7;98=98;99=99')
table(KG_data$q23_b)

table(KG_data$q23_c)
KG_data$q23_c = recode(KG_data$q23_c, '1=0;2=1;3=2;4=3;5=4;6=5;7=6;8=7;9=8;10=9;11=10;-7=-7;98=98;99=99')
table(KG_data$q23_c)

table(KG_data$q23_d)
KG_data$q23_d = recode(KG_data$q23_d, '1=0;2=1;3=2;4=3;5=4;6=5;7=6;8=7;9=8;10=9;11=10;-7=-7;98=98;99=99')
table(KG_data$q23_d)

table(KG_data$q23_e)
KG_data$q23_e = recode(KG_data$q23_e, '1=0;2=1;3=2;4=3;5=4;6=5;7=6;8=7;9=8;10=9;11=10;-7=-7;98=98;99=99')
table(KG_data$q23_e)

table(KG_data$q23_f)
KG_data$q23_f = recode(KG_data$q23_f, '1=0;2=1;3=2;4=3;5=4;6=5;7=6;8=7;9=8;10=9;11=10;-7=-7;98=98;99=99')
table(KG_data$q23_f)

table(KG_data$q23_g)
KG_data$q23_g = recode(KG_data$q23_g, '1=0;2=1;3=2;4=3;5=4;6=5;7=6;8=7;9=8;10=9;11=10;-7=-7;98=98;99=99')
table(KG_data$q23_g)

table(KG_data$q23_h)
KG_data$q23_h = recode(KG_data$q23_h, '1=0;2=1;3=2;4=3;5=4;6=5;7=6;8=7;9=8;10=9;11=10;-7=-7;98=98;99=99')
table(KG_data$q23_h)

table(GA_data$q23_a)
GA_data$q23_a = recode(GA_data$q23_a, '1=0;2=1;3=2;4=3;5=4;6=5;7=6;8=7;9=8;10=9;11=10;-7=-7;-9=-9;98=98;99=99')
table(GA_data$q23_a)

table(GA_data$q23_b)
GA_data$q23_b = recode(GA_data$q23_b, '1=0;2=1;3=2;4=3;5=4;6=5;7=6;8=7;9=8;10=9;11=10;-7=-7;-9=-9;98=98;99=99')
table(GA_data$q23_b)

table(GA_data$q23_c)
GA_data$q23_c = recode(GA_data$q23_c, '1=0;2=1;3=2;4=3;5=4;6=5;7=6;8=7;9=8;10=9;11=10;-7=-7;-9=-9;98=98;99=99')
table(GA_data$q23_c)

table(GA_data$q23_d)
GA_data$q23_d = recode(GA_data$q23_d, '1=0;2=1;3=2;4=3;5=4;6=5;7=6;8=7;9=8;10=9;11=10;-7=-7;-9=-9;98=98;99=99')
table(GA_data$q23_d)

table(GA_data$q23_e)
GA_data$q23_e = recode(GA_data$q23_e, '1=0;2=1;3=2;4=3;5=4;6=5;7=6;8=7;9=8;10=9;11=10;-7=-7;-9=-9;98=98;99=99')
table(GA_data$q23_e)

table(GA_data$q23_f)
GA_data$q23_f = recode(GA_data$q23_f, '1=0;2=1;3=2;4=3;5=4;6=5;7=6;8=7;9=8;10=9;11=10;-7=-7;-9=-9;98=98;99=99')
table(GA_data$q23_f)

table(GA_data$q23_g)
GA_data$q23_g = recode(GA_data$q23_g, '1=0;2=1;3=2;4=3;5=4;6=5;7=6;8=7;9=8;10=9;11=10;-7=-7;-9=-9;98=98;99=99')
table(GA_data$q23_g)

table(GA_data$q23_h)
GA_data$q23_h = recode(GA_data$q23_h, '1=0;2=1;3=2;4=3;5=4;6=5;7=6;8=7;9=8;10=9;11=10;-7=-7;-9=-9;98=98;99=99')
table(GA_data$q23_h)

table(TJ_data$q23_a)
TJ_data$q23_a = recode(TJ_data$q23_a, '1=0;2=1;3=2;4=3;5=4;6=5;7=6;8=7;9=8;10=9;11=10;-7=-7;98=98;99=99')
table(TJ_data$q23_a)

table(TJ_data$q23_b)
TJ_data$q23_b = recode(TJ_data$q23_b, '1=0;2=1;3=2;4=3;5=4;6=5;7=6;8=7;9=8;10=9;11=10;-7=-7;98=98;99=99')
table(TJ_data$q23_b)

table(TJ_data$q23_c)
TJ_data$q23_c = recode(TJ_data$q23_c, '1=0;2=1;3=2;4=3;5=4;6=5;7=6;8=7;9=8;10=9;11=10;-7=-7;98=98;99=99')
table(TJ_data$q23_c)

table(TJ_data$q23_d)
TJ_data$q23_d = recode(TJ_data$q23_d, '1=0;2=1;3=2;4=3;5=4;6=5;7=6;8=7;9=8;10=9;11=10;-7=-7;98=98;99=99')
table(TJ_data$q23_d)

table(TJ_data$q23_e)
TJ_data$q23_e = recode(TJ_data$q23_e, '1=0;2=1;3=2;4=3;5=4;6=5;7=6;8=7;9=8;10=9;11=10;-7=-7;98=98;99=99')
table(TJ_data$q23_e)

table(TJ_data$q23_f)
TJ_data$q23_f = recode(TJ_data$q23_f, '1=0;2=1;3=2;4=3;5=4;6=5;7=6;8=7;9=8;10=9;11=10;-7=-7;98=98;99=99')
table(TJ_data$q23_f)

table(TJ_data$q23_g)
TJ_data$q23_g = recode(TJ_data$q23_g, '1=0;2=1;3=2;4=3;5=4;6=5;7=6;8=7;9=8;10=9;11=10;-7=-7;98=98;99=99')
table(TJ_data$q23_g)

table(TJ_data$q23_h)
TJ_data$q23_h = recode(TJ_data$q23_h, '1=0;2=1;3=2;4=3;5=4;6=5;7=6;8=7;9=8;10=9;11=10;-7=-7;98=98;99=99')
table(TJ_data$q23_h)

table(UZ_data$q23_a)
UZ_data$q23_a = recode(UZ_data$q23_a, '1=0;2=1;3=2;4=3;5=4;6=5;7=6;8=7;9=8;10=9;11=10;-7=-7;98=98;99=99')
table(UZ_data$q23_a)

table(UZ_data$q23_b)
UZ_data$q23_b = recode(UZ_data$q23_b, '1=0;2=1;3=2;4=3;5=4;6=5;7=6;8=7;9=8;10=9;11=10;-7=-7;98=98;99=99')
table(UZ_data$q23_b)

table(UZ_data$q23_c)
UZ_data$q23_c = recode(UZ_data$q23_c, '1=0;2=1;3=2;4=3;5=4;6=5;7=6;8=7;9=8;10=9;11=10;-7=-7;98=98;99=99')
table(UZ_data$q23_c)

table(UZ_data$q23_d)
UZ_data$q23_d = recode(UZ_data$q23_d, '1=0;2=1;3=2;4=3;5=4;6=5;7=6;8=7;9=8;10=9;11=10;-7=-7;98=98;99=99')
table(UZ_data$q23_d)

table(UZ_data$q23_e)
UZ_data$q23_e = recode(UZ_data$q23_e, '1=0;2=1;3=2;4=3;5=4;6=5;7=6;8=7;9=8;10=9;11=10;-7=-7;98=98;99=99')
table(UZ_data$q23_e)

table(UZ_data$q23_f)
UZ_data$q23_f = recode(UZ_data$q23_f, '1=0;2=1;3=2;4=3;5=4;6=5;7=6;8=7;9=8;10=9;11=10;-7=-7;98=98;99=99')
table(UZ_data$q23_f)

table(UZ_data$q23_g)
UZ_data$q23_g = recode(UZ_data$q23_g, '1=0;2=1;3=2;4=3;5=4;6=5;7=6;8=7;9=8;10=9;11=10;-7=-7;98=98;99=99')
table(UZ_data$q23_g)

table(UZ_data$q23_h)
UZ_data$q23_h = recode(UZ_data$q23_h, '1=0;2=1;3=2;4=3;5=4;6=5;7=6;8=7;9=8;10=9;11=10;-7=-7;98=98;99=99')
table(UZ_data$q23_h)

#KZ
# Code Missing Value
variables <- c('q1', 'q2', 'q3', 'q3a', 'q4', 'q4_oth', 'q5', 'q5a', 'q6', 'q7', 'q8', 'q9', 'q10', 'q11', 
               'q12_a', 'q12_b', 'q12_c', 'q12_d', 'q12_e', 'q12_f', 'q13_a', 'q13_a_oth', 'q13_b', 
               'q13_b_oth', 'q14_a', 'q14_b', 'q14_c', 'q14_d', 'q14_e', 'q15_a', 'q15_b', 'q15_c', 
               'q15_d', 'q15_e', 'q15_f', 'q15_g', 'q16', 'q18_a', 'q18_b', 'q18_c', 'q20_a', 'q20_b', 
               'q20_c', 'q20_d', 'q20_e', 'q20_f', 'q20_g', 'q20_h', 'q22', 'q23_a', 'q23_b', 'q23_c', 
               'q23_d', 'q23_e', 'q23_f', 'q23_g', 'q23_h', 'q24_a', 'q24_b', 'q24_c', 'q24_d', 'q24_e', 
               'q24_f', 'q25_a', 'q25_b', 'q25_c', 'q25_d', 'q25_e', 'q25_f', 'q26', 'q27_a', 'q27_b', 
               'q27_c', 'q27_d', 'q28_a', 'q28_b', 'q29_a', 'q29_b', 'q30', 'q32', 'q34', 'q35_a', 
               'q35_b', 'q35_c', 'q35_d', 'q36_a', 'q36_b', 'q36_c', 'q37', 'q38_a', 'q38_b', 'q38_c', 
               'q38_d', 'q38_e', 'q38_f', 'q39_a', 'q39_b', 'q39_c', 'q40_b', 'q40_c', 'q40_d', 'q40_e')

# Replace specified values with NA in the defined variables
KZ_data <- KZ_data %>%
  mutate(across(all_of(variables), ~ replace(.x, .x %in% c(93, 94, 97, 98, 99, -7, -9), NA)))

names(KZ_data)

#KG
# Recode specific values in specified columns to NA
variables  <- c(
  'q1', 'q2', 'q3', 'q3a', 'q4', 'q4_oth', 'q5', 'q5a', 'q6', 'q7', 'q8', 'q9', 'q10', 'q11', 
  'q12_a', 'q12_b', 'q12_c', 'q12_d', 'q12_e', 'q12_f', 'q13_a', 'q13_a_oth', 'q13_b', 
  'q13_b_oth', 'q14_a', 'q14_b', 'q14_c', 'q14_d', 'q14_e', 'q15_a', 'q15_b', 'q15_c', 
  'q15_d', 'q15_e', 'q15_f', 'q15_g', 'q16', 'q18_a', 'q18_b', 'q18_c', 'q20_a', 'q20_b', 
  'q20_c', 'q20_d', 'q20_e', 'q20_f', 'q20_g', 'q20_h', 'q22', 'q23_a', 'q23_b', 'q23_c', 
  'q23_d', 'q23_e', 'q23_f', 'q23_g', 'q23_h', 'q24_a', 'q24_b', 'q24_c', 'q24_d', 'q24_e', 
  'q24_f', 'q25_a', 'q25_b', 'q25_c', 'q25_d', 'q25_e', 'q25_f', 'q26', 'q27_a', 'q27_b', 
  'q27_c', 'q27_d', 'q28_a', 'q28_b', 'q29_a', 'q29_b', 'q30', 'q32', 'q34', 'q35_a', 
  'q35_b', 'q35_c', 'q35_d', 'q36_a', 'q36_b', 'q36_c', 'q37', 'q38_a', 'q38_b', 'q38_c', 
  'q38_d', 'q38_e', 'q38_f', 'q39_a', 'q39_b', 'q39_c', 'q40_b', 'q40_c', 'q40_d', 'q40_e'
)

#KG
# Replace specified values with NA in the defined variables
KG_data <- KG_data %>%
  mutate(across(all_of(variables), ~ replace(.x, .x %in% c(93, 94, 97, 98, 99, -7, -9), NA)))

names(KG_data)

#GA
# Recode specific values in specified columns to NA
variables  <- c(
  'q1', 'q2', 'q3', 'age', 'q4', 'q4_oth', 'q5', 'q5a', 'q6', 'q7', 'q8', 'q9', 'q10', 'q11', 
  'q12_a', 'q12_b', 'q12_c', 'q12_d', 'q12_e', 'q12_f', 'q13_a', 'q13_a_oth', 'q13_b', 
  'q13_b_oth', 'q14_a', 'q14_b', 'q14_c', 'q14_d', 'q14_e', 'q15_a', 'q15_b', 'q15_c', 
  'q15_d', 'q15_e', 'q15_f', 'q15_g', 'q16', 'q18_a', 'q18_b', 'q18_c', 'q20_a', 'q20_b', 
  'q20_c', 'q20_d', 'q20_e', 'q20_f', 'q20_g', 'q20_h', 'q22', 'q23_a', 'q23_b', 'q23_c', 
  'q23_d', 'q23_e', 'q23_f', 'q23_g', 'q23_h', 'q24_a', 'q24_b', 'q24_c', 'q24_d', 'q24_e', 
  'q24_f', 'q25_a', 'q25_b', 'q25_c', 'q25_d', 'q25_e', 'q25_f', 'q26', 'q27_a', 'q27_b', 
  'q27_c', 'q27_d', 'q28_a', 'q28_b', 'q29_a', 'q29_b', 'q30', 'q32', 'q34', 'q35_a', 
  'q35_b', 'q35_c', 'q35_d', 'q36_a', 'q36_b', 'q36_c', 'q37', 'q38_a', 'q38_b', 'q38_c', 
  'q38_d', 'q38_e', 'q38_f', 'q39_a', 'q39_b', 'q39_c', 'q40_b', 'q40_c', 'q40_d', 'q40_e'
)

#GA
# Replace specified values with NA in the defined variables
GA_data <- GA_data %>%
  mutate(across(all_of(variables), ~ replace(.x, .x %in% c(93, 94, 97, 98, 99, -7, -9), NA)))

names(GA_data)

#TJ
# Recode specific values in specified columns to NA
variables <- c(
  'q1','q2','q3','q3a','q4_oth','q5','q5a','q6','q7','q8','q9',       
  'q10a','q10b','q11','q12_c','q12_d','q12_e','q12_f','q12a','q12b',     
  'q13_a','q13_a_oth','q13_b','q13_b_oth','q14_a','q14_b','q14_c','q14_d','q14_e',    
  'q15_c','q15_d','q15_e','q15_f','q15_g','q16','q18_a','q18_b','q18_c',    
  'q19','q21','q20_a','q20_e','q22','q23_a','q23_b','q23_c','q23_d',    
  'q23_e','q23_f','q23_g','q23_h','q24_a','q24_b','q24_c','q24_d','q24_e',    
  'q24_f','q25_a','q25_b','q25_c','q25_d','q25_e','q25_f','q26','q27_c',    
  'q27_d','q28_a','q28_b','q29_a','q29_b','q30','q31','q32','q33',      
  'q34','q35_a','q35_b','q35_c','q35_d','q36_a','q36_b','q36_c','q37',      
  'q38_a','q38_b','q38_c','q38_d','q38_e','q38_f','q39a_a','q39a_b','q39a_c',   
  'q39b_a','q39b_b','q39b_c'
)

#TJ
# Replace specified values with NA in the defined variables
TJ_data <- TJ_data %>%
  mutate(across(all_of(variables), ~ replace(.x, .x %in% c(93, 94, 97, 98, 99, -7, -9), NA)))

names(TJ_data)

#UZ
# Recode specific values in specified columns to NA
variables <- c(
  'q1', 'q2', 'q3', 'q3a', 'q4_oth', 'q5', 'q5a', 'q6', 'q7', 'q8', 'q9', 'q11', 
  'q12_b', 'q13_a', 'q13_a_oth', 'q13_b', 
  'q13_b_oth', 'q14_a', 'q14_b', 'q14_d', 'q14_e', 
  'q15_d', 'q15_e', 'q15_f', 'q15_g', 'q16', 'q17_b', 'q17_c', 'q19', 'q20_a', 
  'q20_e', 'q21', 'q22', 'q23_a', 'q23_b', 'q23_c', 
  'q23_d', 'q23_e', 'q23_f', 'q23_g', 'q23_h', 'q24_a', 'q24_b', 'q24_c', 'q24_d', 'q24_e', 
  'q24_f', 'q25_a', 'q25_b', 'q25_c', 'q25_d', 'q25_e', 'q25_f', 'q26', 
  'q27_c', 'q27_d', 'q29_a', 'q29_b', 'q30', 'q32', 'q34', 'q35_a', 
  'q35_b', 'q35_c', 'q35_d', 'q36_a', 'q36_b', 'q36_c', 'q37', 'q38_a', 'q38_b', 'q38_c', 
  'q38_f', 'q39_a', 'q39_b', 'q39_c'
)

#UZ
# Replace specified values with NA in the defined variables
UZ_data <- UZ_data %>%
  mutate(across(all_of(variables), ~ replace(.x, .x %in% c(93, 94, 97, 98, 99, -7, -9), NA)))

names(UZ_data)

#Making All Missing NA instead of NaN
KZ_data[] <- lapply(KZ_data, function(x) {
  # Replace NaN with NA only in numeric columns
  if(is.numeric(x)) {
    x[is.nan(x)] <- NA
  }
  return(x)
})

KG_data[] <- lapply(KG_data, function(x) {
  # Replace NaN with NA only in numeric columns
  if(is.numeric(x)) {
    x[is.nan(x)] <- NA
  }
  return(x)
})

GA_data[] <- lapply(GA_data, function(x) {
  # Replace NaN with NA only in numeric columns
  if(is.numeric(x)) {
    x[is.nan(x)] <- NA
  }
  return(x)
})

TJ_data[] <- lapply(TJ_data, function(x) {
  # Replace NaN with NA only in numeric columns
  if(is.numeric(x)) {
    x[is.nan(x)] <- NA
  }
  return(x)
})

UZ_data[] <- lapply(UZ_data, function(x) {
  # Replace NaN with NA only in numeric columns
  if(is.numeric(x)) {
    x[is.nan(x)] <- NA
  }
  return(x)
})

rm(variables)

save.image("C:/Users/102142/Dropbox/Research/Internet Politics/Central Asia/Central Asia Grant Survey Project/Data Files/github-CAB-Original-Coding/CAB_data.RData")
