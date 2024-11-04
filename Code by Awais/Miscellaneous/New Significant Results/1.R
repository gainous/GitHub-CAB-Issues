library(dplyr)
library(jtools)
# Domestic Issues ####
Domestic_Issues <- glm(Domestic_Issues ~ tiktok_n_sc + facebook_n_sc + instagram_n_sc + twitter_n_sc + 
                         vkontakte_n_sc + age_n_sc + urbanicity_Village + gender_Male + edu_n_sc + inc_n_sc, 
                       data = CAB_data_Recoded, family = binomial)

colnames(summ(Domestic_Issues)$coeftable)
summ(Domestic_Issues) %>% .$coeftable %>% as.data.frame() %>% filter(p < 0.05)

# Political Violence ####

Political_Violence <- glm(Political_Violence ~ tiktok_n_sc + facebook_n_sc + instagram_n_sc + twitter_n_sc + 
                            vkontakte_n_sc + age_n_sc + urbanicity_Village + gender_Male + edu_n_sc + inc_n_sc, 
                          data = CAB_data_Recoded, family = binomial)

colnames(summ(Political_Violence)$coeftable)
summ(Political_Violence) %>% .$coeftable %>% as.data.frame() %>% filter(p < 0.05)


# Economic_Issues ####
Economic_Issues <- glm(Economic_Issues ~ tiktok_n_sc + facebook_n_sc + instagram_n_sc + twitter_n_sc + 
                         vkontakte_n_sc + age_n_sc + urbanicity_Village + gender_Male + edu_n_sc + inc_n_sc, 
                       data = CAB_data_Recoded, family = binomial)

colnames(summ(Political_Violence)$coeftable)
summ(Political_Violence) %>% .$coeftable %>% as.data.frame() %>% filter(p < 0.05)

# VPN Use ####
Economic_Issues_vpn <- glm(Economic_Issues ~ vpn_use_Yes, 
                       data = CAB_data_Recoded, family = binomial)
summary(Economic_Issues_vpn)

Political_Violence_vpn <- glm(Political_Violence ~ vpn_use_Yes, 
                           data = CAB_data_Recoded, family = binomial)
summary(Political_Violence_vpn)

Domestic_Issues_vpn <- glm(Domestic_Issues ~ vpn_use_Yes, 
                           data = CAB_data_Recoded, family = binomial)
summary(Domestic_Issues_vpn)


###

Economic_Issues_vpn1 <- glm(Economic_Issues ~ facebook_n_sc, 
                            data = CAB_data_Recoded, family = binomial)
summary(Economic_Issues_vpn1)

Economic_Issues_vpn2 <- glm(Economic_Issues ~ vpn_use_Yes + facebook_n_sc, 
                           data = CAB_data_Recoded, family = binomial)
summary(Economic_Issues_vpn2)


###
Political_Violence_vpn1 <- glm(Political_Violence ~ vpn_use_Yes + facebook_n_sc + tiktok_n_sc + instagram_n_sc + twitter_n_sc, 
                            data = CAB_data_Recoded, family = binomial)
summary(Political_Violence_vpn1)

Political_Violence_vpn2 <- glm(Political_Violence ~ vpn_use_Yes + facebook_n_sc, 
                            data = CAB_data_Recoded, family = binomial)
summary(Political_Violence_vpn2)

###
Domestic_Issues_vpn1 <- glm(Domestic_Issues ~ vpn_use_Yes + facebook_n_sc + tiktok_n_sc + instagram_n_sc + twitter_n_sc, 
                               data = CAB_data_Recoded, family = binomial)
summary(Domestic_Issues_vpn1)

Domestic_Issues_vpn2 <- glm(Domestic_Issues ~ vpn_use_Yes + facebook_n_sc, 
                            data = CAB_data_Recoded, family = binomial)
summary(Domestic_Issues_vpn2)
