# Model 1: TikTok only
Domestic_Issues_1 <- glm(Domestic_Issues ~ tiktok_n_sc, 
                         data = CAB_data_Recoded, 
                         family = binomial)
summary(Domestic_Issues_1)

# Model 2: TikTok + Facebook
Domestic_Issues_2 <- glm(Domestic_Issues ~ tiktok_n_sc + facebook_n_sc, 
                         data = CAB_data_Recoded, 
                         family = binomial)
summary(Domestic_Issues_2)

# Model 3: TikTok + Facebook + Instagram
Domestic_Issues_3 <- glm(Domestic_Issues ~ tiktok_n_sc + facebook_n_sc + instagram_n_sc, 
                         data = CAB_data_Recoded, 
                         family = binomial)
summary(Domestic_Issues_3)

# Model 4: TikTok + Facebook + Instagram + Twitter
Domestic_Issues_4 <- glm(Domestic_Issues ~ tiktok_n_sc + facebook_n_sc + instagram_n_sc + twitter_n_sc, 
                         data = CAB_data_Recoded, 
                         family = binomial)
summary(Domestic_Issues_4)

# Model 5: TikTok + Facebook + Instagram + Twitter + VKontakte
Domestic_Issues_5 <- glm(Domestic_Issues ~ tiktok_n_sc + facebook_n_sc + instagram_n_sc + twitter_n_sc + vkontakte_n_sc, 
                         data = CAB_data_Recoded, 
                         family = binomial)
summary(Domestic_Issues_5)

# Model 6: TikTok + Facebook + Instagram + Twitter + VKontakte + Age
Domestic_Issues_6 <- glm(Domestic_Issues ~ tiktok_n_sc + facebook_n_sc + instagram_n_sc + twitter_n_sc + vkontakte_n_sc + age_n_sc, 
                         data = CAB_data_Recoded, 
                         family = binomial)
summary(Domestic_Issues_6)

# Model 7: TikTok + Facebook + Instagram + Twitter + VKontakte + Age + Urbanicity
Domestic_Issues_7 <- glm(Domestic_Issues ~ tiktok_n_sc + facebook_n_sc + instagram_n_sc + twitter_n_sc + vkontakte_n_sc + age_n_sc + urbanicity_Village, 
                         data = CAB_data_Recoded, 
                         family = binomial)
summary(Domestic_Issues_7)

# Model 8: TikTok + Facebook + Instagram + Twitter + VKontakte + Age + Urbanicity + Gender
Domestic_Issues_8 <- glm(Domestic_Issues ~ tiktok_n_sc + facebook_n_sc + instagram_n_sc + twitter_n_sc + vkontakte_n_sc + age_n_sc + urbanicity_Village + gender_Male, 
                         data = CAB_data_Recoded, 
                         family = binomial)
summary(Domestic_Issues_8)

# Model 9: TikTok + Facebook + Instagram + Twitter + VKontakte + Age + Urbanicity + Gender + Education
Domestic_Issues_9 <- glm(Domestic_Issues ~ tiktok_n_sc + facebook_n_sc + instagram_n_sc + twitter_n_sc + vkontakte_n_sc + age_n_sc + urbanicity_Village + gender_Male + edu_n_sc, 
                         data = CAB_data_Recoded, 
                         family = binomial)
summary(Domestic_Issues_9)

# Model 10: TikTok + Facebook + Instagram + Twitter + VKontakte + Age + Urbanicity + Gender + Education + Income
Domestic_Issues_10 <- glm(Domestic_Issues ~ tiktok_n_sc + facebook_n_sc + instagram_n_sc + twitter_n_sc + vkontakte_n_sc + age_n_sc + urbanicity_Village + gender_Male + edu_n_sc + inc_n_sc, 
                          data = CAB_data_Recoded, 
                          family = binomial)
summary(Domestic_Issues_10)
