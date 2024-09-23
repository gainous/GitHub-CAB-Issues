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

#####
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
Economic_Issues_Trust_RG <- glm(Economic_Issues ~ trust_russia_n_sc, 
                                data = CAB_data_Recoded, 
                                family = binomial)
summary(Economic_Issues_Trust_RG)

#####
Domestic_Issues_Trust_RG <- glm(Domestic_Issues ~ trust_russia_n_sc, 
                                data = CAB_data_Recoded, 
                                family = binomial)
summary(Domestic_Issues_Trust_RG)


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


table(CAB_data_Recoded$Economic_Issues)