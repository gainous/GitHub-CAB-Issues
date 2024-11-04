library(psy)
library(psych)

# Resonant Activism Network ####

resonant_activisim_network = cbind(
  CAB_data_dem$participate_rally,
  CAB_data_dem$sm_critical_local_n_sc,
  CAB_data_dem$trust_central_n_sc,
  CAB_data_dem$trust_local_n_sc,
  CAB_data_dem$avoidance_unfriending_n_sc,
  CAB_data_dem$avoidance_leaving_group_n_sc,
  CAB_data_dem$trust_facebook_n_sc,
  CAB_data_dem$trust_western_n_sc)
cronbach(resonant_activisim_network)
result<- alpha(resonant_activisim_network)
print(result)

# Polarized Echo Chamber ####
polarized_echo_chamber = cbind(
  CAB_data_dem$avoidance_unfriending_n_sc,
  CAB_data_dem$avoidance_blocking_n_sc,
  CAB_data_dem$echo_chamber_n_sc,
  CAB_data_dem$sm_disagreement_politics_n_sc,
  CAB_data_dem$sm_disagreement_news_n_sc,
  CAB_data_dem$sm_disagreement_issues_n_sc,
  CAB_data_dem$news_balance_n_sc)
cronbach(polarized_echo_chamber)

result2<- alpha(polarized_echo_chamber)
print(resul2)


# Cautious Moderates ####
cautious_moderates = cbind(
  CAB_data_dem$pol_disagree_n,
  CAB_data_dem$sm_engage_friends_n_sc,
  CAB_data_dem$sm_engage_groups_n_sc,
  CAB_data_dem$trust_central_n_sc,
  CAB_data_dem$system_capable_n_sc,
  CAB_data_dem$news_balance_n_sc)
cronbach(cautious_moderates)
result3<- alpha(cautious_moderates)
print(resul3)


# Misinformation Skeptics ####
misinformation_skeptics = cbind(
  CAB_data_dem$news_balance_n_sc,
  CAB_data_dem$vpn_use_Yes,
  CAB_data_dem$sm_engage_offline_sc,
  CAB_data_dem$trust_state_n_sc,
  CAB_data_dem$trust_western_n_sc)
cronbach(misinformation_skeptics)
result4<- alpha(misinformation_skeptics)
print(resul4)


# Surveillance-Averse Libertarians ####
surv_averse_lib = cbind(
  CAB_data_dem$tracking_central_n_sc,
  CAB_data_dem$tracking_local_n_sc,
  CAB_data_dem$vpn_use_Yes,
  CAB_data_dem$tracking_companies_n_sc,
  CAB_data_dem$sm_engage_friends_n_sc,
  CAB_data_dem$sm_engage_groups_n_sc,
  CAB_data_dem$news_balance_n_sc,
  CAB_data_dem$trust_western_n_sc)
cronbach(surv_averse_lib)

  
#Nationalistic Defenders ####
national_defenders = cbind(
    CAB_data_dem$system_deserves_n_sc,
    CAB_data_dem$sm_engage_supportive_sc,
    CAB_data_dem$sm_critical_local_n_sc,
    CAB_data_dem$sm_critical_central_n_sc)
cronbach(national_defenders)
result5<- alpha(national_defenders)
print(resul5)

# Ideologically Consistent Conservatives ####
idconsisten_conserv = cbind(
  CAB_data_dem$trust_central_n_sc,
  CAB_data_dem$trust_local_n_sc,
  CAB_data_dem$system_capable_n_sc,
  CAB_data_dem$system_proud_n_sc,
  CAB_data_dem$participate_rally,
  CAB_data_dem$trust_state_n_sc,
  CAB_data_dem$pol_news_tv_n_sc)
cronbach(idconsisten_conserv)


# Silent Observers ####
sil_obs = cbind(
  CAB_data_dem$pol_discuss,
  CAB_data_dem$trust_central_n_sc,
  CAB_data_dem$trust_local_n_sc,
  CAB_data_dem$sm_critical_local_n_sc,
  CAB_data_dem$sm_critical_central_n_sc,
  CAB_data_dem$participate_rally,
  CAB_data_dem$sm_disagreement_politics_n_sc)
cronbach(sil_obs)

#Disillusioned Reformists ####
dil_ref = cbind(
  CAB_data_dem$sm_engage_critical_n_sc,
  CAB_data_dem$system_capable_n_sc,
  CAB_data_dem$system_hurdles_participate_n_sc,
  CAB_data_dem$participate_rally_n,
  CAB_data_dem$trust_western_n_sc)
cronbach(dil_ref)


# Social Media Warriors ####
sm_warriors = cbind(
  CAB_data_dem$sm_engage_critical_n_sc,
  CAB_data_dem$sm_engage_friends_n_sc,
  CAB_data_dem$sm_engage_groups_n_sc,
  CAB_data_dem$sm_engage_post_n_sc,
  CAB_data_dem$participate_rally_n,
  CAB_data_dem$participate_protest_n, 
  CAB_data_dem$participate_meeting_n, 
  CAB_data_dem$participate_volunteer_n, 
  CAB_data_dem$participate_member_n, 
  CAB_data_dem$participate_community_n, 
  CAB_data_dem$participate_contact_n, 
  CAB_data_dem$participate_vote_n)
cronbach(sm_warriors)





