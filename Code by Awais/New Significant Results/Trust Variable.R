Domestic_Issues_Trust_CG <- glm(Domestic_Issues ~ trust_central_n_sc, 
                         data = CAB_data_Recoded, 
                         family = binomial)
summary(Domestic_Issues_Trust_CG)


#####
Political_Violence_Trust_CG <- glm(Political_Violence ~ trust_central_n_sc, 
                                data = CAB_data_Recoded, 
                                family = binomial)
summary(Political_Violence_Trust_CG)


#####
Economic_Issues_Trust_CG <- glm(Economic_Issues ~ trust_central_n_sc, 
                                   data = CAB_data_Recoded, 
                                   family = binomial)
summary(Economic_Issues_Trust_CG)

##########################################################################################################
Domestic_Issues_Trust_LG <- glm(Domestic_Issues ~ trust_local_n_sc, 
                                data = CAB_data_Recoded, 
                                family = binomial)
summary(Domestic_Issues_Trust_LG)


#####
Political_Violence_Trust_LG <- glm(Political_Violence ~ trust_local_n_sc, 
                                   data = CAB_data_Recoded, 
                                   family = binomial)
summary(Political_Violence_Trust_LG)


#####
Economic_Issues_Trust_LG <- glm(Economic_Issues ~ trust_local_n_sc, 
                                data = CAB_data_Recoded, 
                                family = binomial)
summary(Economic_Issues_Trust_LG)

##########################################################################################################

Domestic_Issues_Trust_RG <- glm(Domestic_Issues ~ trust_russia_n_sc, 
                                data = CAB_data_Recoded, 
                                family = binomial)
summary(Domestic_Issues_Trust_RG)


#####
Political_Violence_Trust_RG <- glm(Political_Violence ~ trust_russia_n_sc, 
                                   data = CAB_data_Recoded, 
                                   family = binomial)
summary(Political_Violence_Trust_RG)


#####
Economic_Issues_Trust_RG <- glm(Economic_Issues ~ trust_russia_n_sc, 
                                data = CAB_data_Recoded, 
                                family = binomial)
summary(Economic_Issues_Trust_RG)

##########################################################################################################

Domestic_Issues_Trust_USG <- glm(Domestic_Issues ~ trust_US_n_sc, 
                                data = CAB_data_Recoded, 
                                family = binomial)
summary(Domestic_Issues_Trust_USG)


#####
Political_Violence_Trust_USG <- glm(Political_Violence ~ trust_US_n_sc, 
                                   data = CAB_data_Recoded, 
                                   family = binomial)
summary(Political_Violence_Trust_USG)


#####
Economic_Issues_Trust_USG <- glm(Economic_Issues ~ trust_US_n_sc, 
                                data = CAB_data_Recoded, 
                                family = binomial)
summary(Economic_Issues_Trust_USG)



##########################################################################################################



Domestic_Issues_Trust_CHG <- glm(Domestic_Issues ~ trust_china_n_sc, 
                                 data = CAB_data_Recoded, 
                                 family = binomial)
summary(Domestic_Issues_Trust_CHG)


#####
Political_Violence_Trust_CHG <- glm(Political_Violence ~ trust_china_n_sc, 
                                    data = CAB_data_Recoded, 
                                    family = binomial)
summary(Political_Violence_Trust_CHG)


#####
Economic_Issues_Trust_CHG <- glm(Economic_Issues ~ trust_china_n_sc, 
                                 data = CAB_data_Recoded, 
                                 family = binomial)
summary(Economic_Issues_Trust_CHG)

##########################################################################################################

Domestic_Issues_Trust_EU <- glm(Domestic_Issues ~ trust_EU_n_sc, 
                                 data = CAB_data_Recoded, 
                                 family = binomial)
summary(Domestic_Issues_Trust_EU)


#####
Political_Violence_Trust_EU <- glm(Political_Violence ~ trust_EU_n_sc, 
                                    data = CAB_data_Recoded, 
                                    family = binomial)
summary(Political_Violence_Trust_EU)


#####
Economic_Issues_Trust_EU <- glm(Economic_Issues ~ trust_EU_n_sc, 
                                 data = CAB_data_Recoded, 
                                 family = binomial)
summary(Economic_Issues_Trust_EU)

##########################################################################################################


