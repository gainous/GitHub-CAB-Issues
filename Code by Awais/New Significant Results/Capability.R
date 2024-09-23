Domestic_Issues_SysCapable <- glm(Domestic_Issues ~ system_capable_n_sc, 
                                data = CAB_data_Recoded, 
                                family = binomial)
summary(Domestic_Issues_SysCapable)


#####
Political_Violence_SysCapable <- glm(Political_Violence ~ system_capable_n_sc, 
                                   data = CAB_data_Recoded, 
                                   family = binomial)
summary(Political_Violence_SysCapable)


#####
Economic_Issues_SysCapable <- glm(Economic_Issues ~ system_capable_n_sc, 
                                data = CAB_data_Recoded, 
                                family = binomial)
summary(Economic_Issues_SysCapable)

####################################################################

Domestic_Issues_SysProud <- glm(Domestic_Issues ~ system_proud_n_sc, 
                                  data = CAB_data_Recoded, 
                                  family = binomial)
summary(Domestic_Issues_SysProud)


#####
Political_Violence_SysProud <- glm(Political_Violence ~ system_proud_n_sc, 
                                     data = CAB_data_Recoded, 
                                     family = binomial)
summary(Political_Violence_SysProud)


#####
Economic_Issues_SysProud <- glm(Economic_Issues ~ system_proud_n_sc, 
                                  data = CAB_data_Recoded, 
                                  family = binomial)
summary(Economic_Issues_SysProud)


####################################################################


Domestic_Issues_SysDeserves <- glm(Domestic_Issues ~ system_deserves_n_sc, 
                                data = CAB_data_Recoded, 
                                family = binomial)
summary(Domestic_Issues_SysDeserves)


#####
Political_Violence_SysDeserves <- glm(Political_Violence ~ system_deserves_n_sc, 
                                   data = CAB_data_Recoded, 
                                   family = binomial)
summary(Political_Violence_SysDeserves)


#####
Economic_Issues_SysDeserves <- glm(Economic_Issues ~ system_deserves_n_sc, 
                                data = CAB_data_Recoded, 
                                family = binomial)
summary(Economic_Issues_SysDeserves)

####################################################################

Domestic_Issues_SysLive <- glm(Domestic_Issues ~ system_live_n_sc, 
                                   data = CAB_data_Recoded, 
                                   family = binomial)
summary(Domestic_Issues_SysLive)


#####
Political_Violence_SysLive <- glm(Political_Violence ~ system_live_n_sc, 
                                      data = CAB_data_Recoded, 
                                      family = binomial)
summary(Political_Violence_SysLive)


#####
Economic_Issues_SysLive <- glm(Economic_Issues ~ system_live_n_sc, 
                                   data = CAB_data_Recoded, 
                                   family = binomial)
summary(Economic_Issues_SysLive)

####################################################################

