#Kazakhstan
#Recoding all Variables##########################################################################
##############################################################################################
#################################################################################################
library(dplyr)
library(car)

# Internet Use Factor
table(KZ_data$q2)
KZ_data$internet_use = recode(KZ_data$q2, "1 = 'Yes'; 2 = 'No'")
KZ_data$internet_use = factor(KZ_data$internet_use)
table(KZ_data$internet_use)  # Frequency table for internet_use

# Age Numeric
KZ_data$age_n = KZ_data$q3
summary(KZ_data$age_n)  # Summary for age_n

# Age Collapsed Factor
KZ_data$age_collapsed = recode(KZ_data$q3a, "1 = '18 - 24'; 2 = '25 - 34'; 3 = '35 - 44'; 4 = '45 - 54'; 5 = '55 - 64'; 6 = '65+'")
KZ_data$age_collapsed = factor(KZ_data$age_collapsed, ordered = TRUE)
table(KZ_data$age_collapsed)  # Frequency table for age_collapsed

# Ethnicity Factor
table(KZ_data$q4)
KZ_data$ethnicity = recode(KZ_data$q4, "1 = 'Kazakh'; 2 = 'Russian'; 3 = 'Uzbek'; 4 = 'Tajik'; 5 = 'Kyrgyz'; 6 = 'Other'")
KZ_data$ethnicity = factor(KZ_data$ethnicity)
KZ_data$ethnicity = addNA(KZ_data$ethnicity)
table(KZ_data$ethnicity)  # Frequency table for ethnicity

# Urbanicity Factor
KZ_data$urbanicity = recode(KZ_data$q5, "1 = 'City'; 2 = 'Village'")
KZ_data$urbanicity = factor(KZ_data$urbanicity)
KZ_data$urbanicity = addNA(KZ_data$urbanicity)
table(KZ_data$urbanicity)  # Frequency table for urbanicity

# Gender Factor
KZ_data$gender = recode(KZ_data$q6, "1 = 'Female'; 2 = 'Male'")
KZ_data$gender = factor(KZ_data$gender)
KZ_data$gender = addNA(KZ_data$gender)
table(KZ_data$gender)  # Frequency table for gender

# Education Factor
KZ_data$edu = recode(KZ_data$q7, "1 = 'No education'; 2 = 'Primary education'; 3 = 'Basic secondary education'; 4 = 'General secondary education'; 5 = 'Complete vocational education'; 6 = 'Incomplete higher education'; 7 = 'Complete higher education'")
KZ_data$edu = factor(KZ_data$edu, ordered = TRUE)
table(KZ_data$edu)  # Frequency table for edu

# Education Numeric
KZ_data$edu_n = KZ_data$q7
summary(KZ_data$edu_n)  # Summary for edu_n

# Income Factor
KZ_data$inc = recode(KZ_data$q8, "1 = 'Less than 70,001'; 2 = '70,001 - 150,000'; 3 = '150,001 – 200,000'; 4 = '200,001 – 250,000'; 5 = '250,001 – 300,000'; 6 = '300,001 – 350,000'; 7 = '350,001 – 400,000'; 8 = '400,001 – 450,000'; 9 = '450,001 – 500,000'; 10 = '500,001 – 550,000'; 11 = '550,001 – 600,000'; 12 = 'More than 600,000'")
KZ_data$inc = factor(KZ_data$inc, levels = c('Less than 70,001', '70,001 - 150,000', '150,001 – 200,000', '200,001 – 250,000', '250,001 – 300,000', '300,001 – 350,000', '350,001 – 400,000', '400,001 – 450,000', '450,001 – 500,000', '500,001 – 550,000', '550,001 – 600,000', 'More than 600,000'), ordered = TRUE)
table(KZ_data$inc)  # Frequency table for Income Factor

# Income Numeric
KZ_data$inc_n = KZ_data$q8
summary(KZ_data$inc_n)  # Summary for Income Numeric

# Political Interest Factor
KZ_data$pol_interest = recode(KZ_data$q9, "1 = 'Very interested'; 2 = 'Somewhat interested'; 3 = 'Somewhat uninterested'; 4 = 'Very uninterested'")
KZ_data$pol_interest = factor(KZ_data$pol_interest, levels = c('Very interested', 'Somewhat interested', 'Somewhat uninterested', 'Very uninterested'), ordered = TRUE)
table(KZ_data$pol_interest)  # Frequency table for Political Interest Factor

# Political Interest Numeric
KZ_data$pol_interest_n = KZ_data$q9
summary(KZ_data$pol_interest_n)  # Summary for Political Interest Numeric

# Discuss Politics Factor
KZ_data$pol_discuss = recode(KZ_data$q10, "1 = 'A few times a day'; 2 = 'Once a day'; 3 = 'Three to five days a week'; 4 = 'Once a week'; 5 = 'Less often than once a week'; 6 = 'Never'")
KZ_data$pol_discuss = factor(KZ_data$pol_discuss, levels = c('A few times a day', 'Once a day', 'Three to five days a week', 'Once a week', 'Less often than once a week', 'Never'), ordered = TRUE)
table(KZ_data$pol_discuss)  # Frequency table for Discuss Politics Factor

#Discuss Politics Numeric
KZ_data$pol_discuss_n = KZ_data$q10
summary(KZ_data$pol_discuss_n)  # Summary for Discuss Politics Numeric

# Disagree when Discussing Politics
KZ_data$pol_disagree = recode(KZ_data$q11, "1 = 'Most of the time'; 2 = 'Some of the time'; 3 = 'Rarely'; 4 = 'Never'")
KZ_data$pol_disagree = factor(KZ_data$pol_disagree, levels = c('Most of the time', 'Some of the time', 'Rarely', 'Never'), ordered = TRUE)
table(KZ_data$pol_disagree)  # Frequency table for Disagree when Discussing Politics

#Disagree Politics Numeric
KZ_data$pol_disagree_n = KZ_data$q11
summary(KZ_data$pol_disagree_n)  # Summary for Disagree Politics Numeric

# Trust Variables Factor
# Trust in Central Government
KZ_data$trust_central = recode(KZ_data$q12_a, "1 = 'A Great Deal'; 2 = 'Quite a Lot of Trust'; 3 = 'Not Very Much Trust'; 4 = 'None at all'")
KZ_data$trust_central = factor(KZ_data$trust_central, levels = c('A Great Deal', 'Quite a Lot of Trust', 'Not Very Much Trust', 'None at all'), ordered = TRUE)
table(KZ_data$trust_central)  # Frequency table for Trust in Central Government

# Trust in Local Government
KZ_data$trust_local = recode(KZ_data$q12_b, "1 = 'A Great Deal'; 2 = 'Quite a Lot of Trust'; 3 = 'Not Very Much Trust'; 4 = 'None at all'")
KZ_data$trust_local = factor(KZ_data$trust_local, levels = c('A Great Deal', 'Quite a Lot of Trust', 'Not Very Much Trust', 'None at all'), ordered = TRUE)
table(KZ_data$trust_local)  # Frequency table for Trust in Local Government

# Trust in Russia
KZ_data$trust_russia = recode(KZ_data$q12_c, "1 = 'A Great Deal'; 2 = 'Quite a Lot of Trust'; 3 = 'Not Very Much Trust'; 4 = 'None at all'")
KZ_data$trust_russia = factor(KZ_data$trust_russia, levels = c('A Great Deal', 'Quite a Lot of Trust', 'Not Very Much Trust', 'None at all'), ordered = TRUE)
table(KZ_data$trust_russia)  # Frequency table for Trust in Russia

# Trust in the US
KZ_data$trust_US = recode(KZ_data$q12_d, "1 = 'A Great Deal'; 2 = 'Quite a Lot of Trust'; 3 = 'Not Very Much Trust'; 4 = 'None at all'")
KZ_data$trust_US = factor(KZ_data$trust_US, levels = c('A Great Deal', 'Quite a Lot of Trust', 'Not Very Much Trust', 'None at all'), ordered = TRUE)
table(KZ_data$trust_US)  # Frequency table for Trust in the US

# Trust in China
KZ_data$trust_china = recode(KZ_data$q12_e, "1 = 'A Great Deal'; 2 = 'Quite a Lot of Trust'; 3 = 'Not Very Much Trust'; 4 = 'None at all'")
KZ_data$trust_china = factor(KZ_data$trust_china, levels = c('A Great Deal', 'Quite a Lot of Trust', 'Not Very Much Trust', 'None at all'), ordered = TRUE)
table(KZ_data$trust_china)  # Frequency table for Trust in China

# Trust in the EU
KZ_data$trust_EU = recode(KZ_data$q12_f, "1 = 'A Great Deal'; 2 = 'Quite a Lot of Trust'; 3 = 'Not Very Much Trust'; 4 = 'None at all'")
KZ_data$trust_EU = factor(KZ_data$trust_EU, levels = c('A Great Deal', 'Quite a Lot of Trust', 'Not Very Much Trust', 'None at all'), ordered = TRUE)
table(KZ_data$trust_EU)  # Frequency table for Trust in the EU

# Trust Central Numeric
KZ_data$trust_central_n = KZ_data$q12_a
summary(KZ_data$trust_central_n)

# Trust Local Numeric
KZ_data$trust_local_n = KZ_data$q12_b
summary(KZ_data$trust_local_n)

# Trust Russia Numeric
KZ_data$trust_russia_n = KZ_data$q12_c
summary(KZ_data$trust_russia_n)

# Trust US Numeric
KZ_data$trust_US_n = KZ_data$q12_d
summary(KZ_data$trust_US_n)

# Trust China Numeric
KZ_data$trust_china_n = KZ_data$q12_e
summary(KZ_data$trust_china_n)

# Trust EU Numeric
KZ_data$trust_EU_n = KZ_data$q12_f
summary(KZ_data$trust_EU_n)

library(car)

# Recoding 'Most Important Issue First Mention' with car::recode
KZ_data$important_issue_first = recode(KZ_data$q13_a, "1 = 'Political instability'; 2 = 'Unemployment'; 3 = 'Inflation Prices'; 4 = 'Wages pensions'; 5 = 'Taxes'; 6 = 'Access to basic needs'; 7 = 'General economic situation'; 8 = 'Deterioration of the quality and access to education'; 9 = 'Construction of houses residential premises'; 10 = 'Terrorism'; 11 = 'War conflict in other countries'; 12 = 'Increase in crime violence concerns about personal safety'; 13 = 'Corruption'; 14 = 'Lack of opportunities'; 15 = 'Discrimination ethnic or religious tensions'; 16 = 'Infrastructure'; 17 = 'Deterioration of the environment'; 18 = 'Unresolved territorial conflicts'; 19 = 'Emigration'")
KZ_data$important_issue_first = factor(KZ_data$important_issue_first)
KZ_data$important_issue_first = addNA(KZ_data$important_issue_first)
table(KZ_data$important_issue_first)  # Frequency table for important_issue_first

# Recoding 'Most Important Issue Second Mention' with car::recode
KZ_data$important_issue_second = recode(KZ_data$q13_b, "1 = 'Political instability'; 2 = 'Unemployment'; 3 = 'Inflation Prices'; 4 = 'Wages pensions'; 5 = 'Taxes'; 6 = 'Access to basic needs'; 7 = 'General economic situation'; 8 = 'Deterioration of the quality and access to education'; 9 = 'Construction of houses residential premises'; 10 = 'Terrorism'; 11 = 'War conflict in other countries'; 12 = 'Increase in crime violence concerns about personal safety'; 13 = 'Corruption'; 14 = 'Lack of opportunities'; 15 = 'Discrimination ethnic or religious tensions'; 16 = 'Infrastructure'; 17 = 'Deterioration of the environment'; 18 = 'Unresolved territorial conflicts'; 19 = 'Emigration'")
KZ_data$important_issue_second = factor(KZ_data$important_issue_second)
KZ_data$important_issue_second = addNA(KZ_data$important_issue_second)
table(KZ_data$important_issue_second)  # Frequency table for important_issue_first

# System Approval Variables Factor
# Perform the recode
KZ_data$system_capable = recode(KZ_data$q14_a, "1 = 'Strongly agree'; 2 = 'Somewhat agree'; 3 = 'Neither disagree nor agree'; 4 = 'Somewhat disagree'; 5 = 'Strongly disagree'")
KZ_data$system_capable = factor(KZ_data$system_capable, levels = c('Strongly agree', 'Somewhat agree', 'Neither disagree nor agree', 'Somewhat disagree', 'Strongly disagree'), ordered = TRUE)
table(KZ_data$system_capable)  # Frequency table for System Capable

# Numeric variable for System Capable
KZ_data$system_capable_n = as.numeric(KZ_data$q14_a)
summary(KZ_data$system_capable_n)  # Summary for System Capable (Numeric)

KZ_data$system_proud = recode(KZ_data$q14_b, "1 = 'Strongly agree'; 2 = 'Somewhat agree'; 3 = 'Neither disagree nor agree'; 4 = 'Somewhat disagree'; 5 = 'Strongly disagree'")
KZ_data$system_proud = factor(KZ_data$system_proud, levels = c('Strongly agree', 'Somewhat agree', 'Neither disagree nor agree', 'Somewhat disagree', 'Strongly disagree'), ordered = TRUE)
table(KZ_data$system_proud)  # Frequency table for System Proud

# Numeric variable for System Proud
KZ_data$system_proud_n = as.numeric(KZ_data$q14_b)
summary(KZ_data$system_proud_n)  # Summary for System Proud (Numeric)

KZ_data$system_deserves = recode(KZ_data$q14_c, "1 = 'Strongly agree'; 2 = 'Somewhat agree'; 3 = 'Neither disagree nor agree'; 4 = 'Somewhat disagree'; 5 = 'Strongly disagree'")
KZ_data$system_deserves = factor(KZ_data$system_deserves, levels = c('Strongly agree', 'Somewhat agree', 'Neither disagree nor agree', 'Somewhat disagree', 'Strongly disagree'), ordered = TRUE)
table(KZ_data$system_deserves)  # Frequency table for System Deserves

# Numeric variable for System Deserves
KZ_data$system_deserves_n = as.numeric(KZ_data$q14_c)
summary(KZ_data$system_deserves_n)  # Summary for System Deserves (Numeric)

KZ_data$system_live = recode(KZ_data$q14_d, "1 = 'Strongly agree'; 2 = 'Somewhat agree'; 3 = 'Neither disagree nor agree'; 4 = 'Somewhat disagree'; 5 = 'Strongly disagree'")
KZ_data$system_live = factor(KZ_data$system_live, levels = c('Strongly agree', 'Somewhat agree', 'Neither disagree nor agree', 'Somewhat disagree', 'Strongly disagree'), ordered = TRUE)
table(KZ_data$system_live)  # Frequency table for System Live

# Numeric variable for System Live
KZ_data$system_live_n = as.numeric(KZ_data$q14_d)
summary(KZ_data$system_live_n)  # Summary for System Live (Numeric)

KZ_data$system_hurdles_participate = recode(KZ_data$q14_e, "1 = 'Strongly agree'; 2 = 'Somewhat agree'; 3 = 'Neither disagree nor agree'; 4 = 'Somewhat disagree'; 5 = 'Strongly disagree'")
KZ_data$system_hurdles_participate = factor(KZ_data$system_hurdles_participate, levels = c('Strongly agree', 'Somewhat agree', 'Neither disagree nor agree', 'Somewhat disagree', 'Strongly disagree'), ordered = TRUE)
table(KZ_data$system_hurdles_participate)  # Frequency table for System Hurdles for Participation

# Numeric variable for System Hurdles for Participation
KZ_data$system_hurdles_participate_n = as.numeric(KZ_data$q14_e)
summary(KZ_data$system_hurdles_participate_n)  # Summary for System Hurdles for Participation (Numeric)

# Participate Variables Factor
# Perform the recode
KZ_data$participate_rally = recode(KZ_data$q15_a, "1 = 'Yes'; 2 = 'No'")
KZ_data$participate_rally = factor(KZ_data$participate_rally, levels = c('Yes', 'No'), ordered = FALSE)
table(KZ_data$participate_rally)  # Frequency table for Participate Rally

# Numeric variable for Participate Rally
KZ_data$participate_rally_n = as.numeric(KZ_data$q15_a)
summary(KZ_data$participate_rally_n)  # Summary for Participate Rally (Numeric)

KZ_data$participate_protest = recode(KZ_data$q15_b, "1 = 'Yes'; 2 = 'No'")
KZ_data$participate_protest = factor(KZ_data$participate_protest, levels = c('Yes', 'No'), ordered = FALSE)
table(KZ_data$participate_protest)  # Frequency table for Participate Protest

# Numeric variable for Participate Protest
KZ_data$participate_protest_n = as.numeric(KZ_data$q15_b)
summary(KZ_data$participate_protest_n)  # Summary for Participate Protest (Numeric)

KZ_data$participate_meeting = recode(KZ_data$q15_c, "1 = 'Yes'; 2 = 'No'")
KZ_data$participate_meeting = factor(KZ_data$participate_meeting, levels = c('Yes', 'No'), ordered = FALSE)
table(KZ_data$participate_meeting)  # Frequency table for Participate Meeting

# Numeric variable for Participate Meeting
KZ_data$participate_meeting_n = as.numeric(KZ_data$q15_c)
summary(KZ_data$participate_meeting_n)  # Summary for Participate Meeting (Numeric)

KZ_data$participate_volunteer = recode(KZ_data$q15_d, "1 = 'Yes'; 2 = 'No'")
KZ_data$participate_volunteer = factor(KZ_data$participate_volunteer, levels = c('Yes', 'No'), ordered = FALSE)
table(KZ_data$participate_volunteer)  # Frequency table for Participate Volunteer

# Numeric variable for Participate Volunteer
KZ_data$participate_volunteer_n = as.numeric(KZ_data$q15_d)
summary(KZ_data$participate_volunteer_n)  # Summary for Participate Volunteer (Numeric)

KZ_data$participate_member = recode(KZ_data$q15_e, "1 = 'Yes'; 2 = 'No'")
KZ_data$participate_member = factor(KZ_data$participate_member, levels = c('Yes', 'No'), ordered = FALSE)
table(KZ_data$participate_member)  # Frequency table for Participate Member

# Numeric variable for Participate Member
KZ_data$participate_member_n = as.numeric(KZ_data$q15_e)
summary(KZ_data$participate_member_n)  # Summary for Participate Member (Numeric)

KZ_data$participate_community = recode(KZ_data$q15_f, "1 = 'Yes'; 2 = 'No'")
KZ_data$participate_community = factor(KZ_data$participate_community, levels = c('Yes', 'No'), ordered = FALSE)
table(KZ_data$participate_community)  # Frequency table for Participate Community

# Numeric variable for Participate Community
KZ_data$participate_community_n = as.numeric(KZ_data$q15_f)
summary(KZ_data$participate_community_n)  # Summary for Participate Community (Numeric)

KZ_data$participate_contact = recode(KZ_data$q15_g, "1 = 'Yes'; 2 = 'No'")
KZ_data$participate_contact = factor(KZ_data$participate_contact, levels = c('Yes', 'No'), ordered = FALSE)
table(KZ_data$participate_contact)  # Frequency table for Participate Contact

# Numeric variable for Participate Contact
KZ_data$participate_contact_n = as.numeric(KZ_data$q15_g)
summary(KZ_data$participate_contact_n)  # Summary for Participate Contact (Numeric)

KZ_data$participate_vote = recode(KZ_data$q16, "1 = 'Yes'; 2 = 'No'")
KZ_data$participate_vote = factor(KZ_data$participate_vote, levels = c('Yes', 'No'), ordered = FALSE)
table(KZ_data$participate_vote)  # Frequency table for Participate Vote

# Numeric variable for Participate Vote
KZ_data$participate_vote_n = as.numeric(KZ_data$q16)
summary(KZ_data$participate_vote_n)  # Summary for Participate Vote (Numeric)

# Digital Tracking Variables Factor
# Perform the recode
KZ_data$tracking_central = recode(KZ_data$q18_a, "1 = 'Not Comfortable at all'; 2 = '2'; 3 = '3'; 4 = '4'; 5 = 'Very Comfortable'")
KZ_data$tracking_central = factor(KZ_data$tracking_central, levels = c('Not Comfortable at all', '2', '3', '4', 'Very Comfortable'), ordered = TRUE)
table(KZ_data$tracking_central)  # Frequency table for Tracking Central

# Numeric variable for Tracking Central
KZ_data$tracking_central_n = as.numeric(KZ_data$q18_a)
summary(KZ_data$tracking_central_n)  # Summary for Tracking Central (Numeric)

KZ_data$tracking_local = recode(KZ_data$q18_b, "1 = 'Not Comfortable at all'; 2 = '2'; 3 = '3'; 4 = '4'; 5 = 'Very Comfortable'")
KZ_data$tracking_local = factor(KZ_data$tracking_local, levels = c('Not Comfortable at all', '2', '3', '4', 'Very Comfortable'), ordered = TRUE)
table(KZ_data$tracking_local)  # Frequency table for Tracking Local

# Numeric variable for Tracking Local
KZ_data$tracking_local_n = as.numeric(KZ_data$q18_b)
summary(KZ_data$tracking_local_n)  # Summary for Tracking Local (Numeric)

KZ_data$tracking_companies = recode(KZ_data$q18_c, "1 = 'Not Comfortable at all'; 2 = '2'; 3 = '3'; 4 = '4'; 5 = 'Very Comfortable'")
KZ_data$tracking_companies = factor(KZ_data$tracking_companies, levels = c('Not Comfortable at all', '2', '3', '4', 'Very Comfortable'), ordered = TRUE)
table(KZ_data$tracking_companies)  # Frequency table for Tracking Companies

# Numeric variable for Tracking Companies
KZ_data$tracking_companies_n = as.numeric(KZ_data$q18_c)
summary(KZ_data$tracking_companies_n)  # Summary for Tracking Companies (Numeric)

# Democracy Variables Factor
# Perform the recode
KZ_data$democracy_elections = recode(KZ_data$q20_a, "1 = 'Strongly agree'; 2 = 'Somewhat agree'; 3 = 'Neither disagree nor agree'; 4 = 'Somewhat disagree'; 5 = 'Strongly disagree'")
KZ_data$democracy_elections = factor(KZ_data$democracy_elections, levels = c('Strongly agree', 'Somewhat agree', 'Neither disagree nor agree', 'Somewhat disagree', 'Strongly disagree'), ordered = TRUE)
table(KZ_data$democracy_elections)  # Frequency table for Democracy Elections

# Numeric variable for Democracy Elections
KZ_data$democracy_elections_n = as.numeric(KZ_data$q20_a)
summary(KZ_data$democracy_elections_n)  # Summary for Democracy Elections (Numeric)

KZ_data$democracy_speech = recode(KZ_data$q20_b, "1 = 'Strongly agree'; 2 = 'Somewhat agree'; 3 = 'Neither disagree nor agree'; 4 = 'Somewhat disagree'; 5 = 'Strongly disagree'")
KZ_data$democracy_speech = factor(KZ_data$democracy_speech, levels = c('Strongly agree', 'Somewhat agree', 'Neither disagree nor agree', 'Somewhat disagree', 'Strongly disagree'), ordered = TRUE)
table(KZ_data$democracy_speech)  # Frequency table for Democracy Speech

# Numeric variable for Democracy Speech
KZ_data$democracy_speech_n = as.numeric(KZ_data$q20_b)
summary(KZ_data$democracy_speech_n)  # Summary for Democracy Speech (Numeric)

KZ_data$democracy_oversight = recode(KZ_data$q20_c, "1 = 'Strongly agree'; 2 = 'Somewhat agree'; 3 = 'Neither disagree nor agree'; 4 = 'Somewhat disagree'; 5 = 'Strongly disagree'")
KZ_data$democracy_oversight = factor(KZ_data$democracy_oversight, levels = c('Strongly agree', 'Somewhat agree', 'Neither disagree nor agree', 'Somewhat disagree', 'Strongly disagree'), ordered = TRUE)
table(KZ_data$democracy_oversight)  # Frequency table for Democracy Oversight

# Numeric variable for Democracy Oversight
KZ_data$democracy_oversight_n = as.numeric(KZ_data$q20_c)
summary(KZ_data$democracy_oversight_n)  # Summary for Democracy Oversight (Numeric)

KZ_data$democracy_organize = recode(KZ_data$q20_d, "1 = 'Strongly agree'; 2 = 'Somewhat agree'; 3 = 'Neither disagree nor agree'; 4 = 'Somewhat disagree'; 5 = 'Strongly disagree'")
KZ_data$democracy_organize = factor(KZ_data$democracy_organize, levels = c('Strongly agree', 'Somewhat agree', 'Neither disagree nor agree', 'Somewhat disagree', 'Strongly disagree'), ordered = TRUE)
table(KZ_data$democracy_organize)  # Frequency table for Democracy Organize

# Numeric variable for Democracy Organize
KZ_data$democracy_organize_n = as.numeric(KZ_data$q20_d)
summary(KZ_data$democracy_organize_n)  # Summary for Democracy Organize (Numeric)

KZ_data$democracy_press = recode(KZ_data$q20_e, "1 = 'Strongly agree'; 2 = 'Somewhat agree'; 3 = 'Neither disagree nor agree'; 4 = 'Somewhat disagree'; 5 = 'Strongly disagree'")
KZ_data$democracy_press = factor(KZ_data$democracy_press, levels = c('Strongly agree', 'Somewhat agree', 'Neither disagree nor agree', 'Somewhat disagree', 'Strongly disagree'), ordered = TRUE)
table(KZ_data$democracy_press)  # Frequency table for Democracy Press

# Numeric variable for Democracy Press
KZ_data$democracy_press_n = as.numeric(KZ_data$q20_e)
summary(KZ_data$democracy_press_n)  # Summary for Democracy Press (Numeric)

KZ_data$democracy_parties = recode(KZ_data$q20_f, "1 = 'Strongly agree'; 2 = 'Somewhat agree'; 3 = 'Neither disagree nor agree'; 4 = 'Somewhat disagree'; 5 = 'Strongly disagree'")
KZ_data$democracy_parties = factor(KZ_data$democracy_parties, levels = c('Strongly agree', 'Somewhat agree', 'Neither disagree nor agree', 'Somewhat disagree', 'Strongly disagree'), ordered = TRUE)
table(KZ_data$democracy_parties)  # Frequency table for Democracy Parties

# Numeric variable for Democracy Parties
KZ_data$democracy_parties_n = as.numeric(KZ_data$q20_f)
summary(KZ_data$democracy_parties_n)  # Summary for Democracy Parties (Numeric)

KZ_data$democracy_protests = recode(KZ_data$q20_g, "1 = 'Strongly agree'; 2 = 'Somewhat agree'; 3 = 'Neither disagree nor agree'; 4 = 'Somewhat disagree'; 5 = 'Strongly disagree'")
KZ_data$democracy_protests = factor(KZ_data$democracy_protests, levels = c('Strongly agree', 'Somewhat agree', 'Neither disagree nor agree', 'Somewhat disagree', 'Strongly disagree'), ordered = TRUE)
table(KZ_data$democracy_protests)  # Frequency table for Democracy Protests

# Numeric variable for Democracy Protests
KZ_data$democracy_protests_n = as.numeric(KZ_data$q20_g)
summary(KZ_data$democracy_protests_n)  # Summary for Democracy Protests (Numeric)

KZ_data$democracy_courts = recode(KZ_data$q20_h, "1 = 'Strongly agree'; 2 = 'Somewhat agree'; 3 = 'Neither disagree nor agree'; 4 = 'Somewhat disagree'; 5 = 'Strongly disagree'")
KZ_data$democracy_courts = factor(KZ_data$democracy_courts, levels = c('Strongly agree', 'Somewhat agree', 'Neither disagree nor agree', 'Somewhat disagree', 'Strongly disagree'), ordered = TRUE)
table(KZ_data$democracy_courts)  # Frequency table for Democracy Courts

# Numeric variable for Democracy Courts
KZ_data$democracy_courts_n = as.numeric(KZ_data$q20_h)
summary(KZ_data$democracy_courts_n)  # Summary for Democracy Courts (Numeric)

#Social Media Use
# Perform the recode for the factor variable
KZ_data$sm_use = recode(KZ_data$q22, "1 = 'Yes'; 2 = 'No'")
KZ_data$sm_use = factor(KZ_data$sm_use, levels = c('Yes', 'No'), ordered = TRUE)
KZ_data$sm_use = addNA(KZ_data$sm_use)
table(KZ_data$sm_use)  # Frequency table for Social Media Use

#Social Media Platform Variables Factor
#Perform the recode
KZ_data$facebook = recode(KZ_data$q23_a, '0="Do not use this social media"; 1="0-1"; 2="1-2"; 3="2-3"; 4="3-4"; 5="4-5"; 6="5-6"; 7="6-7"; 8="7-8"; 9="8-9"; 10="More than 9"')
KZ_data$facebook = factor(KZ_data$facebook, levels = c('Do not use this social media', '0-1', '1-2', '2-3', '3-4', '4-5', '5-6', '6-7', '7-8', '8-9', 'More than 9'), ordered = TRUE)
table(KZ_data$facebook) # Display a frequency table for 'facebook'

# Create a numeric variable for facebook
KZ_data$facebook_n = as.numeric(KZ_data$q23_a) 
summary(KZ_data$facebook_n) # Provide a summary for 'facebook_n' (Numeric)

KZ_data$vkontakte = recode(KZ_data$q23_b, '0="Do not use this social media"; 1="0-1"; 2="1-2"; 3="2-3"; 4="3-4"; 5="4-5"; 6="5-6"; 7="6-7"; 8="7-8"; 9="8-9"; 10="More than 9"')
KZ_data$vkontakte = factor(KZ_data$vkontakte, levels = c('Do not use this social media', '0-1', '1-2', '2-3', '3-4', '4-5', '5-6', '6-7', '7-8', '8-9', 'More than 9'), ordered = TRUE)
table(KZ_data$vkontakte) # Display a frequency table for 'vkontakte'

# Create a numeric variable for vkontakte
KZ_data$vkontakte_n = as.numeric(KZ_data$q23_b) 
summary(KZ_data$vkontakte_n) # Provide a summary for 'vkontakte_n' (Numeric)

KZ_data$instagram = recode(KZ_data$q23_c, '0="Do not use this social media"; 1="0-1"; 2="1-2"; 3="2-3"; 4="3-4"; 5="4-5"; 6="5-6"; 7="6-7"; 8="7-8"; 9="8-9"; 10="More than 9"')
KZ_data$instagram = factor(KZ_data$instagram, levels = c('Do not use this social media', '0-1', '1-2', '2-3', '3-4', '4-5', '5-6', '6-7', '7-8', '8-9', 'More than 9'), ordered = TRUE)
table(KZ_data$instagram) # Display a frequency table for 'instagram'

# Create a numeric variable for instagram
KZ_data$instagram_n = as.numeric(KZ_data$q23_c) 
summary(KZ_data$instagram_n) # Provide a summary for 'instagram_n' (Numeric)


KZ_data$tiktok = recode(KZ_data$q23_d, '0="Do not use this social media"; 1="0-1"; 2="1-2"; 3="2-3"; 4="3-4"; 5="4-5"; 6="5-6"; 7="6-7"; 8="7-8"; 9="8-9"; 10="More than 9"')
KZ_data$tiktok = factor(KZ_data$tiktok, levels = c('Do not use this social media', '0-1', '1-2', '2-3', '3-4', '4-5', '5-6', '6-7', '7-8', '8-9', 'More than 9'), ordered = TRUE)
table(KZ_data$tiktok) # Display a frequency table for 'tiktok'

# Create a numeric variable for tiktok
KZ_data$tiktok_n = as.numeric(KZ_data$q23_d) 
summary(KZ_data$tiktok_n) # Provide a summary for 'tiktok_n' (Numeric)

KZ_data$twitter = recode(KZ_data$q23_e, '0="Do not use this social media"; 1="0-1"; 2="1-2"; 3="2-3"; 4="3-4"; 5="4-5"; 6="5-6"; 7="6-7"; 8="7-8"; 9="8-9"; 10="More than 9"')
KZ_data$twitter = factor(KZ_data$twitter, levels = c('Do not use this social media', '0-1', '1-2', '2-3', '3-4', '4-5', '5-6', '6-7', '7-8', '8-9', 'More than 9'), ordered = TRUE)
table(KZ_data$twitter) # Display a frequency table for 'twitter'

# Create a numeric variable for twitter
KZ_data$twitter_n = as.numeric(KZ_data$q23_e) 
summary(KZ_data$twitter_n) # Provide a summary for 'twitter_n' (Numeric)

KZ_data$youtube = recode(KZ_data$q23_f, '0="Do not use this social media"; 1="0-1"; 2="1-2"; 3="2-3"; 4="3-4"; 5="4-5"; 6="5-6"; 7="6-7"; 8="7-8"; 9="8-9"; 10="More than 9"')
KZ_data$youtube = factor(KZ_data$youtube, levels = c('Do not use this social media', '0-1', '1-2', '2-3', '3-4', '4-5', '5-6', '6-7', '7-8', '8-9', 'More than 9'), ordered = TRUE)
table(KZ_data$youtube) # Display a frequency table for 'youtube'

# Create a numeric variable for youtube
KZ_data$youtube_n = as.numeric(KZ_data$q23_f) 
summary(KZ_data$youtube_n) # Provide a summary for 'youtube_n' (Numeric)

KZ_data$whatsapp = recode(KZ_data$q23_g, '0="Do not use this social media"; 1="0-1"; 2="1-2"; 3="2-3"; 4="3-4"; 5="4-5"; 6="5-6"; 7="6-7"; 8="7-8"; 9="8-9"; 10="More than 9"')
KZ_data$whatsapp = factor(KZ_data$whatsapp, levels = c('Do not use this social media', '0-1', '1-2', '2-3', '3-4', '4-5', '5-6', '6-7', '7-8', '8-9', 'More than 9'), ordered = TRUE)
table(KZ_data$whatsapp) # Display a frequency table for 'whatsapp'

# Create a numeric variable for whatsapp
KZ_data$whatsapp_n = as.numeric(KZ_data$q23_g) 
summary(KZ_data$whatsapp_n) # Provide a summary for 'whatsapp_n' (Numeric)

KZ_data$telegram = recode(KZ_data$q23_h, '0="Do not use this social media"; 1="0-1"; 2="1-2"; 3="2-3"; 4="3-4"; 5="4-5"; 6="5-6"; 7="6-7"; 8="7-8"; 9="8-9"; 10="More than 9"')
KZ_data$telegram = factor(KZ_data$telegram, levels = c('Do not use this social media', '0-1', '1-2', '2-3', '3-4', '4-5', '5-6', '6-7', '7-8', '8-9', 'More than 9'), ordered = TRUE)
table(KZ_data$telegram) # Display a frequency table for 'telegram'

# Create a numeric variable for whatsapp
KZ_data$telegram_n = as.numeric(KZ_data$q23_h) 
summary(KZ_data$telegram_n) # Provide a summary for 'telegram_n' (Numeric)

# Perform the recode for 'pol_news_tv'
KZ_data$pol_news_tv = recode(KZ_data$q24_a, '1="A few times a day"; 2="Once a day"; 3="Three to five days a week"; 4="Once a week"; 5="Less often than once a week"; 6="Never"')
KZ_data$pol_news_tv = factor(KZ_data$pol_news_tv, levels = c('A few times a day', 'Once a day', 'Three to five days a week', 'Once a week', 'Less often than once a week', 'Never'), ordered = TRUE)
table(KZ_data$pol_news_tv) # Display a frequency table for 'pol_news_tv'

# Create a numeric variable for 'pol_news_tv'
KZ_data$pol_news_tv_n = as.numeric(KZ_data$q24_a) 
summary(KZ_data$pol_news_tv_n) # Provide a summary for 'pol_news_tv_n' (Numeric)

# Perform the recode for 'pol_news_facebook'
KZ_data$pol_news_facebook = recode(KZ_data$q24_b, '1="A few times a day"; 2="Once a day"; 3="Three to five days a week"; 4="Once a week"; 5="Less often than once a week"; 6="Never"')
KZ_data$pol_news_facebook = factor(KZ_data$pol_news_facebook, levels = c('A few times a day', 'Once a day', 'Three to five days a week', 'Once a week', 'Less often than once a week', 'Never'), ordered = TRUE)
table(KZ_data$pol_news_facebook) # Display a frequency table for 'pol_news_facebook'

# Create a numeric variable for 'pol_news_facebook'
KZ_data$pol_news_facebook_n = as.numeric(KZ_data$q24_b) 
summary(KZ_data$pol_news_facebook_n) # Provide a summary for 'pol_news_facebook_n' (Numeric)

# Perform the recode for 'pol_news_vkontakte'
KZ_data$pol_news_vkontakte = recode(KZ_data$q24_c, '1="A few times a day"; 2="Once a day"; 3="Three to five days a week"; 4="Once a week"; 5="Less often than once a week"; 6="Never"')
KZ_data$pol_news_vkontakte = factor(KZ_data$pol_news_vkontakte, levels = c('A few times a day', 'Once a day', 'Three to five days a week', 'Once a week', 'Less often than once a week', 'Never'), ordered = TRUE)
table(KZ_data$pol_news_vkontakte) # Display a frequency table for 'pol_news_vkontakte'

# Create a numeric variable for 'pol_news_vkontakte'
KZ_data$pol_news_vkontakte_n = as.numeric(KZ_data$q24_c) 
summary(KZ_data$pol_news_vkontakte_n) # Provide a summary for 'pol_news_vkontakte_n' (Numeric)

# Perform the recode for 'pol_news_tiktok'
KZ_data$pol_news_tiktok = recode(KZ_data$q24_d, '1="A few times a day"; 2="Once a day"; 3="Three to five days a week"; 4="Once a week"; 5="Less often than once a week"; 6="Never"')
KZ_data$pol_news_tiktok = factor(KZ_data$pol_news_tiktok, levels = c('A few times a day', 'Once a day', 'Three to five days a week', 'Once a week', 'Less often than once a week', 'Never'), ordered = TRUE)
table(KZ_data$pol_news_tiktok) # Display a frequency table for 'pol_news_tiktok'

# Create a numeric variable for 'pol_news_tiktok'
KZ_data$pol_news_tiktok_n = as.numeric(KZ_data$q24_d) 
summary(KZ_data$pol_news_tiktok_n) # Provide a summary for 'pol_news_tiktok_n' (Numeric)

# Perform the recode for 'pol_news_twitter'
KZ_data$pol_news_twitter = recode(KZ_data$q24_e, '1="A few times a day"; 2="Once a day"; 3="Three to five days a week"; 4="Once a week"; 5="Less often than once a week"; 6="Never"')
KZ_data$pol_news_twitter = factor(KZ_data$pol_news_twitter, levels = c('A few times a day', 'Once a day', 'Three to five days a week', 'Once a week', 'Less often than once a week', 'Never'), ordered = TRUE)
table(KZ_data$pol_news_twitter) # Display a frequency table for 'pol_news_twitter'

# Create a numeric variable for 'pol_news_twitter'
KZ_data$pol_news_twitter_n = as.numeric(KZ_data$q24_e) 
summary(KZ_data$pol_news_twitter_n) # Provide a summary for 'pol_news_twitter_n' (Numeric)

# Perform the recode for 'pol_news_odnoklassniki'
KZ_data$pol_news_odnoklassniki = recode(KZ_data$q24_f, '1="A few times a day"; 2="Once a day"; 3="Three to five days a week"; 4="Once a week"; 5="Less often than once a week"; 6="Never"')
KZ_data$pol_news_odnoklassniki = factor(KZ_data$pol_news_odnoklassniki, levels = c('A few times a day', 'Once a day', 'Three to five days a week', 'Once a week', 'Less often than once a week', 'Never'), ordered = TRUE)
table(KZ_data$pol_news_odnoklassniki) # Display a frequency table for 'pol_news_odnoklassniki'

# Create a numeric variable for 'pol_news_odnoklassniki'
KZ_data$pol_news_odnoklassniki_n = as.numeric(KZ_data$q24_f) 
summary(KZ_data$pol_news_odnoklassniki_n) # Provide a summary for 'pol_news_odnoklassniki_n' (Numeric)

# Perform the recode for 'trust_state'
KZ_data$trust_state = recode(KZ_data$q25_a, '1="Very trustworthy"; 2="Somewhat trustworthy"; 3="Somewhat untrustworthy"; 4="Very untrustworthy"')
KZ_data$trust_state = factor(KZ_data$trust_state, levels = c('Very trustworthy', 'Somewhat trustworthy', 'Somewhat untrustworthy', 'Very untrustworthy'), ordered = TRUE)
table(KZ_data$trust_state) # Display a frequency table for 'trust_state'

# Create a numeric variable for 'trust_state'
KZ_data$trust_state_n = as.numeric(KZ_data$q25_a) 
summary(KZ_data$trust_state_n) # Provide a summary for 'trust_state_n' (Numeric)

# Perform the recode for 'trust_russian_media'
KZ_data$trust_russian_media = recode(KZ_data$q25_b, '1="Very trustworthy"; 2="Somewhat trustworthy"; 3="Somewhat untrustworthy"; 4="Very untrustworthy"')
KZ_data$trust_russian_media = factor(KZ_data$trust_russian_media, levels = c('Very trustworthy', 'Somewhat trustworthy', 'Somewhat untrustworthy', 'Very untrustworthy'), ordered = TRUE)
table(KZ_data$trust_russian_media) # Display a frequency table for 'trust_russian_media'

# Create a numeric variable for 'trust_russian_media'
KZ_data$trust_russian_media_n = as.numeric(KZ_data$q25_b) 
summary(KZ_data$trust_russian_media_n) # Provide a summary for 'trust_russian_media_n' (Numeric)

# Perform the recode for 'trust_internet'
KZ_data$trust_internet = recode(KZ_data$q25_c, '1="Very trustworthy"; 2="Somewhat trustworthy"; 3="Somewhat untrustworthy"; 4="Very untrustworthy"')
KZ_data$trust_internet = factor(KZ_data$trust_internet, levels = c('Very trustworthy', 'Somewhat trustworthy', 'Somewhat untrustworthy', 'Very untrustworthy'), ordered = TRUE)
table(KZ_data$trust_internet) # Display a frequency table for 'trust_internet'

# Create a numeric variable for 'trust_internet'
KZ_data$trust_internet_n = as.numeric(KZ_data$q25_c) 
summary(KZ_data$trust_internet_n) # Provide a summary for 'trust_internet_n' (Numeric)

# Perform the recode for 'trust_facebook'
KZ_data$trust_facebook = recode(KZ_data$q25_d, '1="Very trustworthy"; 2="Somewhat trustworthy"; 3="Somewhat untrustworthy"; 4="Very untrustworthy"')
KZ_data$trust_facebook = factor(KZ_data$trust_facebook, levels = c('Very trustworthy', 'Somewhat trustworthy', 'Somewhat untrustworthy', 'Very untrustworthy'), ordered = TRUE)
table(KZ_data$trust_facebook) # Display a frequency table for 'trust_facebook'

# Create a numeric variable for 'trust_parties'
KZ_data$trust_facebook_n = as.numeric(KZ_data$q25_d) 
summary(KZ_data$trust_facebook_n) # Provide a summary for 'trust_facebook_n' (Numeric)

# Perform the recode for 'trust_vkontakte'
KZ_data$trust_vkontakte = recode(KZ_data$q25_e, '1="Very trustworthy"; 2="Somewhat trustworthy"; 3="Somewhat untrustworthy"; 4="Very untrustworthy"')
KZ_data$trust_vkontakte = factor(KZ_data$trust_vkontakte, levels = c('Very trustworthy', 'Somewhat trustworthy', 'Somewhat untrustworthy', 'Very untrustworthy'), ordered = TRUE)
table(KZ_data$trust_vkontakte) # Display a frequency table for 'trust_vkontakte'

# Create a numeric variable for 'trust_vkontakte'
KZ_data$trust_vkontakte_n = as.numeric(KZ_data$q25_e) 
summary(KZ_data$trust_vkontakte_n) # Provide a summary for 'trust_vkontakte_n' (Numeric)

# Perform the recode for 'trust_western'
KZ_data$trust_western = recode(KZ_data$q25_f, '1="Very trustworthy"; 2="Somewhat trustworthy"; 3="Somewhat untrustworthy"; 4="Very untrustworthy"')
KZ_data$trust_western = factor(KZ_data$trust_western, levels = c('Very trustworthy', 'Somewhat trustworthy', 'Somewhat untrustworthy', 'Very untrustworthy'), ordered = TRUE)
table(KZ_data$trust_western) # Display a frequency table for 'trust_western'

# Create a numeric variable for 'trust_western'
KZ_data$trust_western_n = as.numeric(KZ_data$q25_f) 
summary(KZ_data$trust_western_n) # Provide a summary for 'trust_western_n' (Numeric)

#News Balance Factor
# Perform the recode for 'news_balance'
table(KG_data$q26)
KZ_data$news_balance = recode(KZ_data$q26, '1="Only from traditional sources like television newspapers radio";2="Mostly from television newspapers radio but some from the internet social media";3="From an equal balance of television newspapers radio and the internet social media";4="Mostly from social media but some from television newspapers radio";5="Only from the internet social media"')
KZ_data$news_balance = factor(KZ_data$news_balance, levels = c('Only from traditional sources like television newspapers radio', 'Mostly from television newspapers radio but some from the internet social media', 'From an equal balance of television newspapers radio and the internet social media', 'Mostly from social media but some from television newspapers radio', 'Only from the internet social media'), ordered = TRUE)
KZ_data$news_balance = addNA(KZ_data$news_balance)
table(KZ_data$news_balance) # Display a frequency table for 'news_balance'

# Tone of Social Media News about Government Factor
# Perform the recode for 'q27_a' (Tone of Social Media News about Government)
KZ_data$sm_critical_local = recode(KZ_data$q27_a, '1="Very often";2="Rather often";3="Occasionally";4="Never"')
KZ_data$sm_critical_local = factor(KZ_data$sm_critical_local, levels = c('Very often', 'Rather often', 'Occasionally', 'Never'), ordered = TRUE)
table(KZ_data$sm_critical_local) # Display a frequency table for 'sm_critical_local'

# Create a numeric variable for 'sm_critical_local'
KZ_data$sm_critical_local_n = as.numeric(KZ_data$q27_a)
summary(KZ_data$sm_critical_local_n) # Provide a summary for 'sm_critical_local_n' (Numeric)

# Perform the recode for 'q27_b' (Tone of Social Media News about Government)
KZ_data$sm_critical_central = recode(KZ_data$q27_b, '1="Very often";2="Rather often";3="Occasionally";4="Never"')
KZ_data$sm_critical_central = factor(KZ_data$sm_critical_central, levels = c('Very often', 'Rather often', 'Occasionally', 'Never'), ordered = TRUE)
table(KZ_data$sm_critical_central) # Display a frequency table for 'sm_critical_central'

# Create a numeric variable for 'sm_critical_central'
KZ_data$sm_critical_central_n = as.numeric(KZ_data$q27_b)
summary(KZ_data$sm_critical_central_n) # Provide a summary for 'sm_critical_central_n' (Numeric)

# Perform the recode for 'q27_c' (Tone of Social Media News about Government)
KZ_data$sm_positive_local = recode(KZ_data$q27_c, '1="Very often";2="Rather often";3="Occasionally";4="Never"')
KZ_data$sm_positive_local = factor(KZ_data$sm_positive_local, levels = c('Very often', 'Rather often', 'Occasionally', 'Never'), ordered = TRUE)
table(KZ_data$sm_positive_local) # Display a frequency table for 'sm_positive_local'

# Create a numeric variable for 'sm_positive_local'
KZ_data$sm_positive_local_n = as.numeric(KZ_data$q27_c)
summary(KZ_data$sm_positive_local_n) # Provide a summary for 'sm_positive_local_n' (Numeric)

# Perform the recode for 'q27_d' (Tone of Social Media News about Government)
KZ_data$sm_positive_central = recode(KZ_data$q27_d, '1="Very often";2="Rather often";3="Occasionally";4="Never"')
KZ_data$sm_positive_central = factor(KZ_data$sm_positive_central, levels = c('Very often', 'Rather often', 'Occasionally', 'Never'), ordered = TRUE)
table(KZ_data$sm_positive_central) # Display a frequency table for 'sm_positive_central'

# Create a numeric variable for 'sm_positive_central'
KZ_data$sm_positive_central_n = as.numeric(KZ_data$q27_d)
summary(KZ_data$sm_positive_central_n) # Provide a summary for 'sm_positive_central_n' (Numeric)

# TV Tone of News about Government Factor
# Perform the recode for 'q28_a' (TV Tone of News about Government)
KZ_data$tv_critical_local = recode(KZ_data$q28_a, '1="Very often";2="Rather often";3="Occasionally";4="Never"')
KZ_data$tv_critical_local = factor(KZ_data$tv_critical_local, levels = c('Very often', 'Rather often', 'Occasionally', 'Never'), ordered = TRUE)
table(KZ_data$tv_critical_local) # Display a frequency table for 'tv_critical_local'

# Create a numeric variable for 'tv_critical_local'
KZ_data$tv_critical_local_n = as.numeric(KZ_data$q28_a)
summary(KZ_data$tv_critical_local_n) # Provide a summary for 'tv_critical_local_n' (Numeric)

# Perform the recode for 'q28_b' (TV Tone of News about Government)
KZ_data$tv_critical_central = recode(KZ_data$q28_b, '1="Very often";2="Rather often";3="Occasionally";4="Never"')
KZ_data$tv_critical_central = factor(KZ_data$tv_critical_central, levels = c('Very often', 'Rather often', 'Occasionally', 'Never'), ordered = TRUE)
table(KZ_data$tv_critical_central) # Display a frequency table for 'tv_critical_central'

# Create a numeric variable for 'tv_critical_central'
KZ_data$tv_critical_central_n = as.numeric(KZ_data$q28_b)
summary(KZ_data$tv_critical_central_n) # Provide a summary for 'tv_critical_central_n' (Numeric)

# Clickable Links Factor
# Perform the recode for 'q29_a' (Clickable Links)
KZ_data$clickable_state = recode(KZ_data$q29_a, '1="Almost everyday";2="About once a week";3="A few times a month";4="About once a month";5="Never"')
KZ_data$clickable_state = factor(KZ_data$clickable_state, levels = c('Almost everyday', 'About once a week', 'A few times a month', 'About once a month', 'Never'), ordered = TRUE)
table(KZ_data$clickable_state) # Display a frequency table for 'clickable_state'

# Create a numeric variable for 'clickable_state'
KZ_data$clickable_state_n = as.numeric(KZ_data$q29_a)
summary(KZ_data$clickable_state_n) # Provide a summary for 'clickable_state_n' (Numeric)

# Perform the recode for 'q29_b' (Clickable Links)
KZ_data$clickable_russian = recode(KZ_data$q29_b, '1="Almost everyday";2="About once a week";3="A few times a month";4="About once a month";5="Never"')
KZ_data$clickable_russian = factor(KZ_data$clickable_russian, levels = c('Almost everyday', 'About once a week', 'A few times a month', 'About once a month', 'Never'), ordered = TRUE)
table(KZ_data$clickable_russian) # Display a frequency table for 'clickable_russian'

# Create a numeric variable for 'clickable_russian'
KZ_data$clickable_russian_n = as.numeric(KZ_data$q29_b)
summary(KZ_data$clickable_russian_n) # Provide a summary for 'clickable_russian_n' (Numeric)

# Artificial Intelligence Attitude Factor
# Perform the recode for 'AI' (Artificial Intelligence Attitude)
KZ_data$AI = recode(KZ_data$q30, '1="More excited than concerned";2="Equally concerned and excited";3="More concerned than excited";4="I do not know what artificial intelligence is"')
KZ_data$AI = factor(KZ_data$AI, levels = c('More excited than concerned', 'Equally concerned and excited', 'More concerned than excited', 'I do not know what artificial intelligence is'), ordered = TRUE)
table(KZ_data$AI) # Display a frequency table for 'AI'

# Artificial Intelligence Attitude Ordinal (coding I don't know as NA)
# Perform the recode
KZ_data$AI_ordinal = recode(KZ_data$q30, '1="More excited than concerned";2="Equally concerned and excited";3="More concerned than excited";4=NA')
KZ_data$AI_ordinal = factor(KZ_data$AI_ordinal, levels = c('More excited than concerned', 'Equally concerned and excited', 'More concerned than excited', 'I do not know what artificial intelligence is'), ordered = TRUE)
table(KZ_data$AI_ordinal) # Display a frequency table for 'AI_ordinal'

# Convert 'AI_ordinal' to an ordered factor/ordinal
KZ_data$AI_ordinal = factor(KZ_data$AI_ordinal, ordered = TRUE)

# Artificial Intelligence Attitude Numeric
# Perform the recode
KZ_data$AI_n = recode(KZ_data$q30, '1=1;2=2;3=3;4=NA')
summary(KZ_data$AI_n) # Provide a summary for 'AI_n' (Numeric)

#Echo Chamber Numeric
KZ_data$echo_chamber_n = as.numeric(KZ_data$q31)
summary(KZ_data$echo_chamber_n) # Provide a summary for 'echo_chamber_n' (Numeric)

# Awareness of Government Posters Factor
# Perform the recode for 'paid_posters'
KZ_data$paid_posters = recode(KZ_data$q32, '1="I never heard of them";2="I have heard of them but I have not seen any posts from them";3="I think I have seen posts from them"')
KZ_data$paid_posters = factor(KZ_data$paid_posters, levels = c('I never heard of them', 'I have heard of them but I have not seen any posts from them', 'I think I have seen posts from them'), ordered = TRUE)
KZ_data$paid_posters = addNA(KZ_data$paid_posters)
table(KZ_data$paid_posters) # Display a frequency table for 'paid_posters'

# Create a numeric variable for 'paid_posters'
KZ_data$paid_posters_n = as.numeric(KZ_data$q32)
summary(KZ_data$paid_posters_n) # Provide a summary for 'paid_posters_n' (Numeric)

# Awareness of Government Posters Ordinal (coding "I don't know" as NA)
# Perform the recode
KZ_data$paid_posters_ordinal = recode(KZ_data$q32, '1="I never heard of them";2="I have heard of them but I have not seen any posts from them";3="I think I have seen posts from them";4=NA')
KZ_data$paid_posters_ordinal = factor(KZ_data$paid_posters_ordinal, levels = c('I never heard of them', 'I have heard of them but I have not seen any posts from them', 'I think I have seen posts from them'), ordered = TRUE)
KZ_data$paid_posters_ordinal = addNA(KZ_data$paid_posters_ordinal)
table(KZ_data$paid_posters_ordinal) # Display a frequency table for 'paid_posters_ordinal'

# Convert 'paid_posters_ordinal' to an ordered factor/ordinal
KZ_data$paid_posters_ordinal = factor(KZ_data$paid_posters_ordinal, ordered = TRUE)

# Awareness of Government Posters Numeric
# Perform the recode
KZ_data$paid_posters_n = recode(KZ_data$q32, '1=1;2=2;3=3;4=NA')
summary(KZ_data$paid_posters_n) # Provide a summary for 'paid_posters_n' (Numeric)

#Estimate of Percentage of Online Posts Paid by Government
KZ_data$paid_posters_percentage_n = as.numeric(KZ_data$q33)
summary(KZ_data$paid_posters_percentage_n) # Provide a summary for paid_posters_percentage_n (Numeric)

# VPN Use Factor
# Perform the recode for 'vpn_use'
KZ_data$vpn_use = recode(KZ_data$q34, '1="Yes";2="No"')
KZ_data$vpn_use = factor(KZ_data$vpn_use, levels = c('Yes', 'No'), ordered = TRUE)
KZ_data$vpn_use = addNA(KZ_data$vpn_use)
table(KZ_data$vpn_use) # Display a frequency table for 'vpn_use'

# Create a numeric variable for 'vpn_use'
KZ_data$vpn_use_n = as.numeric(KZ_data$q34)
summary(KZ_data$vpn_use_n) # Provide a summary for 'vpn_use_n' (Numeric)

#Selective Exposure
# Perform the recode for 'avoidance_blocking'
KZ_data$avoidance_blocking = recode(KZ_data$q35_a, '1="Often"; 2="Sometimes"; 3="Very rarely"; 4="Never"')
KZ_data$avoidance_blocking = factor(KZ_data$avoidance_blocking, levels = c('Often', 'Sometimes', 'Very rarely', 'Never'), ordered = TRUE)
table(KZ_data$avoidance_blocking) # Display a frequency table for 'avoidance_blocking'

# Create a numeric variable for 'avoidance_blocking'
KZ_data$avoidance_blocking_n = as.numeric(KZ_data$q35_a) 
summary(KZ_data$avoidance_blocking_n) # Provide a summary for 'avoidance_blocking_numeric' (Numeric)

# Perform the recode for 'avoidance_unfriending'
KZ_data$avoidance_unfriending = recode(KZ_data$q35_b, '1="Often"; 2="Sometimes"; 3="Very rarely"; 4="Never"')
KZ_data$avoidance_unfriending = factor(KZ_data$avoidance_unfriending, levels = c('Often', 'Sometimes', 'Very rarely', 'Never'), ordered = TRUE)
table(KZ_data$avoidance_unfriending) # Display a frequency table for 'avoidance_unfriending'

# Create a numeric variable for 'avoidance_unfriending'
KZ_data$avoidance_unfriending_n = as.numeric(KZ_data$q35_b) 
summary(KZ_data$avoidance_unfriending_n) # Provide a summary for 'avoidance_unfriending_numeric' (Numeric)

# Perform the recode for 'avoidance_leaving_group'
KZ_data$avoidance_leaving_group = recode(KZ_data$q35_c, '1="Often"; 2="Sometimes"; 3="Very rarely"; 4="Never"')
KZ_data$avoidance_leaving_group = factor(KZ_data$avoidance_leaving_group, levels = c('Often', 'Sometimes', 'Very rarely', 'Never'), ordered = TRUE)
table(KZ_data$avoidance_leaving_group) # Display a frequency table for 'avoidance_leaving_group'

# Create a numeric variable for 'avoidance_leaving_group'
KZ_data$avoidance_leaving_group_n = as.numeric(KZ_data$q35_c) 
summary(KZ_data$avoidance_leaving_group_n) # Provide a summary for 'avoidance_leaving_group_numeric' (Numeric)

# Perform the recode for 'avoidance_unsubscribing'
KZ_data$avoidance_unsubscribing = recode(KZ_data$q35_d, '1="Often"; 2="Sometimes"; 3="Very rarely"; 4="Never"')
KZ_data$avoidance_unsubscribing = factor(KZ_data$avoidance_unsubscribing, levels = c('Often', 'Sometimes', 'Very rarely', 'Never'), ordered = TRUE)
table(KZ_data$avoidance_unsubscribing) # Display a frequency table for 'avoidance_unsubscribing'

# Create a numeric variable for 'avoidance_unsubscribing'
KZ_data$avoidance_unsubscribing_n = as.numeric(KZ_data$q35_d) 
summary(KZ_data$avoidance_unsubscribing_n) # Provide a summary for 'avoidance_unsubscribing_numeric' (Numeric)

#Exposure to SM Disagreement
# Perform the recode for 'sm_disagreement_politics'
KZ_data$sm_disagreement_politics = recode(KZ_data$q36_a, '1="Often"; 2="Sometimes"; 3="Very rarely"; 4="Never"')
KZ_data$sm_disagreement_politics = factor(KZ_data$sm_disagreement_politics, levels = c('Often', 'Sometimes', 'Very rarely', 'Never'), ordered = TRUE)
table(KZ_data$sm_disagreement_politics) # Display a frequency table for 'sm_disagreement_politics'

# Create a numeric variable for 'sm_disagreement_politics'
KZ_data$sm_disagreement_politics_n = as.numeric(KZ_data$q36_a) 
summary(KZ_data$sm_disagreement_politics_n) # Provide a summary for 'sm_disagreement_politics_numeric' (Numeric)

# Perform the recode for 'sm_disagreement_news'
KZ_data$sm_disagreement_news = recode(KZ_data$q36_b, '1="Often"; 2="Sometimes"; 3="Very rarely"; 4="Never"')
KZ_data$sm_disagreement_news = factor(KZ_data$sm_disagreement_news, levels = c('Often', 'Sometimes', 'Very rarely', 'Never'), ordered = TRUE)
table(KZ_data$sm_disagreement_news) # Display a frequency table for 'sm_disagreement_news'

# Create a numeric variable for 'sm_disagreement_news'
KZ_data$sm_disagreement_news_n = as.numeric(KZ_data$q36_b) 
summary(KZ_data$sm_disagreement_news_n) # Provide a summary for 'sm_disagreement_news_numeric' (Numeric)

# Perform the recode for 'sm_disagreement_issues'
KZ_data$sm_disagreement_issues = recode(KZ_data$q36_c, '1="Often"; 2="Sometimes"; 3="Very rarely"; 4="Never"')
KZ_data$sm_disagreement_issues = factor(KZ_data$sm_disagreement_issues, levels = c('Often', 'Sometimes', 'Very rarely', 'Never'), ordered = TRUE)
table(KZ_data$sm_disagreement_issues) # Display a frequency table for 'sm_disagreement_issues'

# Create a numeric variable for 'sm_disagreement_issues'
KZ_data$sm_disagreement_issues_n = as.numeric(KZ_data$q36_c) 
summary(KZ_data$sm_disagreement_issues_n) # Provide a summary for 'sm_disagreement_issues_numeric' (Numeric)

#Network Breadth
# Perform the recode for 'network_breadth'
KZ_data$network_breadth = recode(KZ_data$q37, '1="1-2"; 2="3-5"; 3="6-8"; 4="8-12"; 5="More than 12"')
KZ_data$network_breadth = factor(KZ_data$network_breadth, levels = c('1-2', '3-5', '6-8', '8-12', 'More than 12'), ordered = TRUE)
table(KZ_data$network_breadth) # Display a frequency table for 'network_breadth'

# Create a numeric variable for 'network_breadth'
KZ_data$network_breadth_n = as.numeric(KZ_data$q37) 
summary(KZ_data$network_breadth_n) # Provide a summary for 'network_breadth_numeric' (Numeric)

#Social Media Political Activity
# Perform the recode for 'sm_engage_friends'
KZ_data$sm_engage_friends = recode(KZ_data$q38_a, '1="A few times a day"; 2="Once a day"; 3="Several times a week"; 4="Once a week"; 5="Less often"; 6="Never"')
KZ_data$sm_engage_friends = factor(KZ_data$sm_engage_friends, levels = c('A few times a day', 'Once a day', 'Several times a week', 'Once a week', 'Less often', 'Never'), ordered = TRUE)
table(KZ_data$sm_engage_friends) # Display a frequency table for 'sm_engage_friends'

# Create a numeric variable for 'sm_engage_friends'
KZ_data$sm_engage_friends_n = as.numeric(KZ_data$q38_a) 
summary(KZ_data$sm_engage_friends_n) # Provide a summary for 'sm_engage_friends_n' (Numeric)

# Perform the recode for 'sm_engage_groups'
KZ_data$sm_engage_groups = recode(KZ_data$q38_b, '1="A few times a day"; 2="Once a day"; 3="Several times a week"; 4="Once a week"; 5="Less often"; 6="Never"')
KZ_data$sm_engage_groups = factor(KZ_data$sm_engage_groups, levels = c('A few times a day', 'Once a day', 'Several times a week', 'Once a week', 'Less often', 'Never'), ordered = TRUE)
table(KZ_data$sm_engage_groups) # Display a frequency table for 'sm_engage_groups'

# Create a numeric variable for 'sm_engage_groups'
KZ_data$sm_engage_groups_n = as.numeric(KZ_data$q38_b) 
summary(KZ_data$sm_engage_groups_n) # Provide a summary for 'sm_engage_groups_n' (Numeric)
# Perform the recode for 'sm_engage_post'
KZ_data$sm_engage_post = recode(KZ_data$q38_c, '1="A few times a day"; 2="Once a day"; 3="Several times a week"; 4="Once a week"; 5="Less often"; 6="Never"')
KZ_data$sm_engage_post = factor(KZ_data$sm_engage_post, levels = c('A few times a day', 'Once a day', 'Several times a week', 'Once a week', 'Less often', 'Never'), ordered = TRUE)
table(KZ_data$sm_engage_post) # Display a frequency table for 'sm_engage_post'

# Create a numeric variable for 'sm_engage_post'
KZ_data$sm_engage_post_n = as.numeric(KZ_data$q38_c) 
summary(KZ_data$sm_engage_post_n) # Provide a summary for 'sm_engage_post_n' (Numeric)

# Perform the recode for 'sm_engage_critical'
KZ_data$sm_engage_critical = recode(KZ_data$q38_d, '1="A few times a day"; 2="Once a day"; 3="Several times a week"; 4="Once a week"; 5="Less often"; 6="Never"')
KZ_data$sm_engage_critical = factor(KZ_data$sm_engage_critical, levels = c('A few times a day', 'Once a day', 'Several times a week', 'Once a week', 'Less often', 'Never'), ordered = TRUE)
table(KZ_data$sm_engage_critical) # Display a frequency table for 'sm_engage_critical'

# Create a numeric variable for 'sm_engage_critical'
KZ_data$sm_engage_critical_n = as.numeric(KZ_data$q38_d) 
summary(KZ_data$sm_engage_critical_n) # Provide a summary for 'sm_engage_critical_n' (Numeric)

# Perform the recode for 'sm_engage_supportive'
KZ_data$sm_engage_supportive = recode(KZ_data$q38_e, '1="A few times a day"; 2="Once a day"; 3="Several times a week"; 4="Once a week"; 5="Less often"; 6="Never"')
KZ_data$sm_engage_supportive = factor(KZ_data$sm_engage_supportive, levels = c('A few times a day', 'Once a day', 'Several times a week', 'Once a week', 'Less often', 'Never'), ordered = TRUE)
table(KZ_data$sm_engage_supportive) # Display a frequency table for 'sm_engage_supportive'

# Create a numeric variable for 'sm_engage_supportive'
KZ_data$sm_engage_supportive_n = as.numeric(KZ_data$q38_e) 
summary(KZ_data$sm_engage_supportive_n) # Provide a summary for 'sm_engage_supportive_n' (Numeric)

# Perform the recode for 'sm_engage_offline'
KZ_data$sm_engage_offline = recode(KZ_data$q38_f, '1="A few times a day"; 2="Once a day"; 3="Several times a week"; 4="Once a week"; 5="Less often"; 6="Never"')
KZ_data$sm_engage_offline = factor(KZ_data$sm_engage_offline, levels = c('A few times a day', 'Once a day', 'Several times a week', 'Once a week', 'Less often', 'Never'), ordered = TRUE)
table(KZ_data$sm_engage_offline) # Display a frequency table for 'sm_engage_offline'

# Create a numeric variable for 'sm_engage_offline'
KZ_data$sm_engage_offline_n = as.numeric(KZ_data$q38_f) 
summary(KZ_data$sm_engage_offline_n) # Provide a summary for 'sm_engage_offline_n' (Numeric)

#Attitudes about Global Power and Ukraine
# Perform the recode for 'ukraine_china'
KZ_data$ukraine_china = recode(KZ_data$q39_a, '1="Significantly more favorable"; 2="Somewhat more favorable"; 3="Unchanged"; 4="Somewhat more unfavorable"; 5="Significantly more unfavorable"')
KZ_data$ukraine_china = factor(KZ_data$ukraine_china, levels = c('Significantly more favorable', 'Somewhat more favorable', 'Unchanged', 'Somewhat more unfavorable', 'Significantly more unfavorable'), ordered = TRUE)
table(KZ_data$ukraine_china) # Display a frequency table for 'ukraine_china'

# Create a numeric variable for 'ukraine_china'
KZ_data$ukraine_china_n = as.numeric(KZ_data$q39_a) 
summary(KZ_data$ukraine_china_n) # Provide a summary for 'ukraine_china_n' (Numeric)

# Perform the recode for 'ukraine_russia'
KZ_data$ukraine_russia = recode(KZ_data$q39_b, '1="Significantly more favorable"; 2="Somewhat more favorable"; 3="Unchanged"; 4="Somewhat more unfavorable"; 5="Significantly more unfavorable"')
KZ_data$ukraine_russia = factor(KZ_data$ukraine_russia, levels = c('Significantly more favorable', 'Somewhat more favorable', 'Unchanged', 'Somewhat more unfavorable', 'Significantly more unfavorable'), ordered = TRUE)
table(KZ_data$ukraine_russia) # Display a frequency table for 'ukraine_russia'

# Create a numeric variable for 'ukraine_russia'
KZ_data$ukraine_russia_n = as.numeric(KZ_data$q39_b) 
summary(KZ_data$ukraine_russia_n) # Provide a summary for 'ukraine_russia_n' (Numeric)

# Perform the recode for 'ukraine_US'
KZ_data$ukraine_US = recode(KZ_data$q39_c, '1="Significantly more favorable"; 2="Somewhat more favorable"; 3="Unchanged"; 4="Somewhat more unfavorable"; 5="Significantly more unfavorable"')
KZ_data$ukraine_US = factor(KZ_data$ukraine_US, levels = c('Significantly more favorable', 'Somewhat more favorable', 'Unchanged', 'Somewhat more unfavorable', 'Significantly more unfavorable'), ordered = TRUE)
table(KZ_data$ukraine_US) # Display a frequency table for 'ukraine_US'

# Create a numeric variable for 'ukraine_US'
KZ_data$ukraine_US_n = as.numeric(KZ_data$q39_c) 
summary(KZ_data$ukraine_US_n) # Provide a summary for 'ukraine_US_n' (Numeric)

##Attention to Ukraine and Who's Responsible
# Perform the recode for 'ukraine_attention'
KZ_data$ukraine_attention = recode(KZ_data$q40_b, '1="Strongly agree"; 2="Somewhat agree"; 3="Neither disagree nor agree"; 4="Somewhat disagree"; 5="Strongly disagree"')
KZ_data$ukraine_attention = factor(KZ_data$ukraine_attention, levels = c('Strongly agree', 'Somewhat agree', 'Neither disagree nor agree', 'Somewhat disagree', 'Strongly disagree'), ordered = TRUE)
table(KZ_data$ukraine_attention) # Display a frequency table for 'ukraine_attention'

# Create a numeric variable for 'ukraine_attention'
KZ_data$ukraine_attention_n = as.numeric(KZ_data$q40_b) 
summary(KZ_data$ukraine_attention_n) # Provide a summary for 'ukraine_attention_n' (Numeric)

# Perform the recode for 'ukraine_russia_responsible'
KZ_data$ukraine_russia_responsible = recode(KZ_data$q40_c, '1="Strongly agree"; 2="Somewhat agree"; 3="Neither disagree nor agree"; 4="Somewhat disagree"; 5="Strongly disagree"')
KZ_data$ukraine_russia_responsible = factor(KZ_data$ukraine_russia_responsible, levels = c('Strongly agree', 'Somewhat agree', 'Neither disagree nor agree', 'Somewhat disagree', 'Strongly disagree'), ordered = TRUE)
table(KZ_data$ukraine_russia_responsible) # Display a frequency table for 'ukraine_russia_responsible'

# Create a numeric variable for 'ukraine_russia_responsible'
KZ_data$ukraine_russia_responsible_n = as.numeric(KZ_data$q40_c) 
summary(KZ_data$ukraine_russia_responsible_n) # Provide a summary for 'ukraine_russia_responsible_n' (Numeric)

# Perform the recode for 'ukraine_ukraine_responsible'
KZ_data$ukraine_ukraine_responsible = recode(KZ_data$q40_d, '1="Strongly agree"; 2="Somewhat agree"; 3="Neither disagree nor agree"; 4="Somewhat disagree"; 5="Strongly disagree"')
KZ_data$ukraine_ukraine_responsible = factor(KZ_data$ukraine_ukraine_responsible, levels = c('Strongly agree', 'Somewhat agree', 'Neither disagree nor agree', 'Somewhat disagree', 'Strongly disagree'), ordered = TRUE)
table(KZ_data$ukraine_ukraine_responsible) # Display a frequency table for 'ukraine_ukraine_responsible'

# Create a numeric variable for 'ukraine_ukraine_responsible'
KZ_data$ukraine_ukraine_responsible_n = as.numeric(KZ_data$q40_d) 
summary(KZ_data$ukraine_ukraine_responsible_n) # Provide a summary for 'ukraine_ukraine_responsible_n' (Numeric)

# Perform the recode for 'ukraine_US_responsible'
KZ_data$ukraine_US_responsible = recode(KZ_data$q40_e, '1="Strongly agree"; 2="Somewhat agree"; 3="Neither disagree nor agree"; 4="Somewhat disagree"; 5="Strongly disagree"')
KZ_data$ukraine_US_responsible = factor(KZ_data$ukraine_US_responsible, levels = c('Strongly agree', 'Somewhat agree', 'Neither disagree nor agree', 'Somewhat disagree', 'Strongly disagree'), ordered = TRUE)
table(KZ_data$ukraine_US_responsible) # Display a frequency table for 'ukraine_US_responsible'

# Create a numeric variable for 'ukraine_US_responsible'
KZ_data$ukraine_US_responsible_n = as.numeric(KZ_data$q40_e) 
summary(KZ_data$ukraine_US_responsible_n) # Provide a summary for 'ukraine_US_responsible_n' (Numeric)

#Creating Dummy Variables from all Relevant KZ Variables############################################################################################
library(car)

# internet_use
KZ_data$internet_use_No = recode(KZ_data$internet_use, '"No"=1; NA=NA; else=0')
KZ_data$internet_use_Yes = recode(KZ_data$internet_use, '"Yes"=1; NA=NA; else=0')

# ethnicity
KZ_data$ethnicity_Kazakh = recode(KZ_data$ethnicity, '"Kazakh"=1; NA=NA; else=0')
KZ_data$ethnicity_Russian = recode(KZ_data$ethnicity, '"Russian"=1; NA=NA; else=0')
KZ_data$ethnicity_Uzbek = recode(KZ_data$ethnicity, '"Uzbek"=1; NA=NA; else=0')
KZ_data$ethnicity_Tajik = recode(KZ_data$ethnicity, '"Tajik"=1; NA=NA; else=0')
KZ_data$ethnicity_Kyrgyz = recode(KZ_data$ethnicity, '"Kyrgyz"=1; NA=NA; else=0')
KZ_data$ethnicity_Other = recode(KZ_data$ethnicity, '"Other"=1; NA=NA; else=0')

# urbanicity
KZ_data$urbanicity_City = recode(KZ_data$urbanicity, '"City"=1; NA=NA; else=0')
KZ_data$urbanicity_Village = recode(KZ_data$urbanicity, '"Village"=1; NA=NA; else=0')

# gender
KZ_data$gender_Female = recode(KZ_data$gender, '"Female"=1; NA=NA; else=0')
KZ_data$gender_Male = recode(KZ_data$gender, '"Male"=1; NA=NA; else=0')

# important_issue_first
KZ_data$important_issue_first_Political_instability = recode(KZ_data$important_issue_first, "'Political instability'=1; NA=NA; else=0")
KZ_data$important_issue_first_Unemployment = recode(KZ_data$important_issue_first, "'Unemployment'=1; NA=NA; else=0")
KZ_data$important_issue_first_Inflation_Prices = recode(KZ_data$important_issue_first, "'Inflation Prices'=1; NA=NA; else=0")
KZ_data$important_issue_first_Wages_pensions = recode(KZ_data$important_issue_first, "'Wages pensions'=1; NA=NA; else=0")
KZ_data$important_issue_first_Taxes = recode(KZ_data$important_issue_first, "'Taxes'=1; NA=NA; else=0")
KZ_data$important_issue_first_Access_to_basic_needs = recode(KZ_data$important_issue_first, "'Access to basic needs'=1; NA=NA; else=0")
KZ_data$important_issue_first_General_economic_situation = recode(KZ_data$important_issue_first, "'General economic situation'=1; NA=NA; else=0")
KZ_data$important_issue_first_Deterioration_of_the_quality_and_access_to_education = recode(KZ_data$important_issue_first, "'Deterioration of the quality and access to education'=1; NA=NA; else=0")
KZ_data$important_issue_first_Construction_of_houses_residential_premises = recode(KZ_data$important_issue_first, "'Construction of houses residential premises'=1; NA=NA; else=0")
KZ_data$important_issue_first_Terrorism = recode(KZ_data$important_issue_first, "'Terrorism'=1; NA=NA; else=0")
KZ_data$important_issue_first_War_conflict_in_other_countries = recode(KZ_data$important_issue_first, "'War conflict in other countries'=1; NA=NA; else=0")
KZ_data$important_issue_first_Increase_in_crime_violence_concerns_about_personal_safety = recode(KZ_data$important_issue_first, "'Increase in crime violence concerns about personal safety'=1; NA=NA; else=0")
KZ_data$important_issue_first_Corruption = recode(KZ_data$important_issue_first, "'Corruption'=1; NA=NA; else=0")
KZ_data$important_issue_first_Lack_of_opportunities = recode(KZ_data$important_issue_first, "'Lack of opportunities'=1; NA=NA; else=0")
KZ_data$important_issue_first_Discrimination_ethnic_or_religious_tensions = recode(KZ_data$important_issue_first, "'Discrimination ethnic or religious tensions'=1; NA=NA; else=0")
KZ_data$important_issue_first_Infrastructure = recode(KZ_data$important_issue_first, "'Infrastructure'=1; NA=NA; else=0")
KZ_data$important_issue_first_Deterioration_of_the_environment = recode(KZ_data$important_issue_first, "'Deterioration of the environment'=1; NA=NA; else=0")
KZ_data$important_issue_first_Unresolved_territorial_conflicts = recode(KZ_data$important_issue_first, "'Unresolved territorial conflicts'=1; NA=NA; else=0")
KZ_data$important_issue_first_Emigration = recode(KZ_data$important_issue_first, "'Emigration'=1; NA=NA; else=0")

# important_issue_second
KZ_data$important_issue_second_Political_instability = recode(KZ_data$important_issue_second, "'Political instability'=1; NA=NA; else=0")
KZ_data$important_issue_second_Unemployment = recode(KZ_data$important_issue_second, "'Unemployment'=1; NA=NA; else=0")
KZ_data$important_issue_second_Inflation_Prices = recode(KZ_data$important_issue_second, "'Inflation Prices'=1; NA=NA; else=0")
KZ_data$important_issue_second_Wages_pensions = recode(KZ_data$important_issue_second, "'Wages pensions'=1; NA=NA; else=0")
KZ_data$important_issue_second_Taxes = recode(KZ_data$important_issue_second, "'Taxes'=1; NA=NA; else=0")
KZ_data$important_issue_second_Access_to_basic_needs = recode(KZ_data$important_issue_second, "'Access to basic needs'=1; NA=NA; else=0")
KZ_data$important_issue_second_General_economic_situation = recode(KZ_data$important_issue_second, "'General economic situation'=1; NA=NA; else=0")
KZ_data$important_issue_second_Deterioration_of_the_quality_and_access_to_education = recode(KZ_data$important_issue_second, "'Deterioration of the quality and access to education'=1; NA=NA; else=0")
KZ_data$important_issue_second_Construction_of_houses_residential_premises = recode(KZ_data$important_issue_second, "'Construction of houses residential premises'=1; NA=NA; else=0")
KZ_data$important_issue_second_Terrorism = recode(KZ_data$important_issue_second, "'Terrorism'=1; NA=NA; else=0")
KZ_data$important_issue_second_War_conflict_in_other_countries = recode(KZ_data$important_issue_second, "'War conflict in other countries'=1; NA=NA; else=0")
KZ_data$important_issue_second_Increase_in_crime_violence_concerns_about_personal_safety = recode(KZ_data$important_issue_second, "'Increase in crime violence concerns about personal safety'=1; NA=NA; else=0")
KZ_data$important_issue_second_Corruption = recode(KZ_data$important_issue_second, "'Corruption'=1; NA=NA; else=0")
KZ_data$important_issue_second_Lack_of_opportunities = recode(KZ_data$important_issue_second, "'Lack of opportunities'=1; NA=NA; else=0")
KZ_data$important_issue_second_Discrimination_ethnic_or_religious_tensions = recode(KZ_data$important_issue_second, "'Discrimination ethnic or religious tensions'=1; NA=NA; else=0")
KZ_data$important_issue_second_Infrastructure = recode(KZ_data$important_issue_second, "'Infrastructure'=1; NA=NA; else=0")
KZ_data$important_issue_second_Deterioration_of_the_environment = recode(KZ_data$important_issue_second, "'Deterioration of the environment'=1; NA=NA; else=0")
KZ_data$important_issue_second_Unresolved_territorial_conflicts = recode(KZ_data$important_issue_second, "'Unresolved territorial conflicts'=1; NA=NA; else=0")
KZ_data$important_issue_second_Emigration = recode(KZ_data$important_issue_second, "'Emigration'=1; NA=NA; else=0")

# sm_use
KZ_data$sm_use_No = recode(KZ_data$sm_use, '"No"=1; NA=NA; else=0')
KZ_data$sm_use_Yes = recode(KZ_data$sm_use, '"Yes"=1; NA=NA; else=0')

# vpn_use
KZ_data$vpn_use_No = recode(KZ_data$vpn_use, '"No"=1; NA=NA; else=0')
KZ_data$vpn_use_Yes = recode(KZ_data$vpn_use, '"Yes"=1; NA=NA; else=0')

# paid_posters
# Assuming 'I never heard of them', 'I have heard of them but I have not seen any posts from them', 'I think I have seen posts from them' as categories
KZ_data$paid_posters_never_heard = recode(KZ_data$paid_posters, '"I never heard of them"=1; NA=NA; else=0')
KZ_data$paid_posters_heard_not_seen = recode(KZ_data$paid_posters, '"I have heard of them but I have not seen any posts from them"=1; NA=NA; else=0')
KZ_data$paid_posters_seen_posts = recode(KZ_data$paid_posters, '"I think I have seen posts from them"=1; NA=NA; else=0')

# Frequency tables for internet use
table(KZ_data$internet_use_No)
table(KZ_data$internet_use_Yes)

# Frequency tables for ethnicity
table(KZ_data$ethnicity_Kazakh)
table(KZ_data$ethnicity_Russian)
table(KZ_data$ethnicity_Uzbek)
table(KZ_data$ethnicity_Tajik)
table(KZ_data$ethnicity_Kyrgyz)
table(KZ_data$ethnicity_Other)

# Frequency tables for urbanicity
table(KZ_data$urbanicity_City)
table(KZ_data$urbanicity_Village)

# Frequency tables for gender
table(KZ_data$gender_Female)
table(KZ_data$gender_Male)

# Frequency tables for the first important issue
table(KZ_data$important_issue_first_Political_instability)
table(KZ_data$important_issue_first_Unemployment)
table(KZ_data$important_issue_first_Inflation_Prices)
table(KZ_data$important_issue_first_Wages_pensions)
table(KZ_data$important_issue_first_Taxes)
table(KZ_data$important_issue_first_Access_to_basic_needs)
table(KZ_data$important_issue_first_General_economic_situation)
table(KZ_data$important_issue_first_Deterioration_of_the_quality_and_access_to_education)
table(KZ_data$important_issue_first_Construction_of_houses_residential_premises)
table(KZ_data$important_issue_first_Terrorism)
table(KZ_data$important_issue_first_War_conflict_in_other_countries)
table(KZ_data$important_issue_first_Increase_in_crime_violence_concerns_about_personal_safety)
table(KZ_data$important_issue_first_Corruption)
table(KZ_data$important_issue_first_Lack_of_opportunities)
table(KZ_data$important_issue_first_Discrimination_ethnic_or_religious_tensions)
table(KZ_data$important_issue_first_Infrastructure)
table(KZ_data$important_issue_first_Deterioration_of_the_environment)
table(KZ_data$important_issue_first_Unresolved_territorial_conflicts)
table(KZ_data$important_issue_first_Emigration)

# Frequency tables for the first important issue
table(KZ_data$important_issue_second_Political_instability)
table(KZ_data$important_issue_second_Unemployment)
table(KZ_data$important_issue_second_Inflation_Prices)
table(KZ_data$important_issue_second_Wages_pensions)
table(KZ_data$important_issue_second_Taxes)
table(KZ_data$important_issue_second_Access_to_basic_needs)
table(KZ_data$important_issue_second_General_economic_situation)
table(KZ_data$important_issue_second_Deterioration_of_the_quality_and_access_to_education)
table(KZ_data$important_issue_second_Construction_of_houses_residential_premises)
table(KZ_data$important_issue_second_Terrorism)
table(KZ_data$important_issue_second_War_conflict_in_other_countries)
table(KZ_data$important_issue_second_Increase_in_crime_violence_concerns_about_personal_safety)
table(KZ_data$important_issue_second_Corruption)
table(KZ_data$important_issue_second_Lack_of_opportunities)
table(KZ_data$important_issue_second_Discrimination_ethnic_or_religious_tensions)
table(KZ_data$important_issue_second_Infrastructure)
table(KZ_data$important_issue_second_Deterioration_of_the_environment)
table(KZ_data$important_issue_second_Unresolved_territorial_conflicts)
table(KZ_data$important_issue_second_Emigration)
# Frequency tables for social media use
table(KZ_data$sm_use_No)
table(KZ_data$sm_use_Yes)

# Frequency tables for VPN use
table(KZ_data$vpn_use_No)
table(KZ_data$vpn_use_Yes)

# Frequency tables for paid posters
table(KZ_data$paid_posters_never_heard)
table(KZ_data$paid_posters_heard_not_seen)
table(KZ_data$paid_posters_seen_posts)

#Scaling
library(scales)
KZ_data$age_n_sc = rescale(KZ_data$age_n, to = c(0, 1))
KZ_data$edu_n_sc = rescale(KZ_data$edu_n, to = c(0, 1))
KZ_data$inc_n_sc = rescale(KZ_data$inc_n, to = c(0, 1))
KZ_data$pol_interest_n_sc = rescale(KZ_data$pol_interest_n, to = c(1, 0))
KZ_data$trust_central_n_sc = rescale(KZ_data$trust_central_n, to = c(1, 0))
KZ_data$trust_local_n_sc = rescale(KZ_data$trust_local_n, to = c(1, 0))
KZ_data$trust_russia_n_sc = rescale(KZ_data$trust_russia_n, to = c(1, 0))
KZ_data$trust_US_n_sc = rescale(KZ_data$trust_US_n, to = c(1, 0))
KZ_data$trust_china_n_sc = rescale(KZ_data$trust_china_n, to = c(1, 0))
KZ_data$trust_EU_n_sc = rescale(KZ_data$trust_EU_n, to = c(1, 0))
KZ_data$system_capable_n_sc = rescale(KZ_data$system_capable_n, to = c(1, 0))
KZ_data$system_proud_n_sc = rescale(KZ_data$system_proud_n, to = c(1, 0))
KZ_data$system_deserves_n_sc = rescale(KZ_data$system_deserves_n, to = c(1, 0))
KZ_data$system_live_n_sc = rescale(KZ_data$system_live_n, to = c(1, 0))
KZ_data$system_hurdles_participate_n_sc = rescale(KZ_data$system_hurdles_participate_n, to = c(1, 0))
KZ_data$tracking_central_n_sc = rescale(KZ_data$tracking_central_n, to = c(0, 1))
KZ_data$tracking_local_n_sc = rescale(KZ_data$tracking_local_n, to = c(0, 1))
KZ_data$tracking_companies_n_sc = rescale(KZ_data$tracking_companies_n, to = c(0, 1))
KZ_data$democracy_elections_n_sc = rescale(KZ_data$democracy_elections_n, to = c(1, 0))
KZ_data$democracy_speech_n_sc = rescale(KZ_data$democracy_speech_n, to = c(1, 0))
KZ_data$democracy_oversight_n_sc = rescale(KZ_data$democracy_oversight_n, to = c(1, 0))
KZ_data$democracy_organize_n_sc = rescale(KZ_data$democracy_organize_n, to = c(1, 0))
KZ_data$democracy_press_n_sc = rescale(KZ_data$democracy_press_n, to = c(1, 0))
KZ_data$democracy_parties_n_sc = rescale(KZ_data$democracy_parties_n, to = c(1, 0))
KZ_data$democracy_protests_n_sc = rescale(KZ_data$democracy_protests_n, to = c(1, 0))
KZ_data$democracy_courts_n_sc = rescale(KZ_data$democracy_courts_n, to = c(1, 0))
KZ_data$facebook_n_sc = rescale(KZ_data$facebook_n, to = c(0, 1))
KZ_data$vkontakte_n_sc = rescale(KZ_data$vkontakte_n, to = c(0, 1))
KZ_data$instagram_n_sc = rescale(KZ_data$instagram_n, to = c(0, 1))
KZ_data$tiktok_n_sc = rescale(KZ_data$tiktok_n, to = c(0, 1))
KZ_data$twitter_n_sc = rescale(KZ_data$twitter_n, to = c(0, 1))
KZ_data$youtube_n_sc = rescale(KZ_data$youtube_n, to = c(0, 1))
KZ_data$whatsapp_n_sc = rescale(KZ_data$whatsapp_n, to = c(0, 1))
KZ_data$telegram_n_sc = rescale(KZ_data$telegram_n, to = c(0, 1))
KZ_data$pol_news_tv_n_sc = rescale(KZ_data$pol_news_tv_n, to = c(1, 0))
KZ_data$pol_news_facebook_n_sc = rescale(KZ_data$pol_news_facebook_n, to = c(1, 0))
KZ_data$pol_news_vkontakte_n_sc = rescale(KZ_data$pol_news_vkontakte_n, to = c(1, 0))
KZ_data$pol_news_tiktok_n_sc = rescale(KZ_data$pol_news_tiktok_n, to = c(1, 0))
KZ_data$pol_news_twitter_n_sc = rescale(KZ_data$pol_news_twitter_n, to = c(1, 0))
KZ_data$pol_news_odnoklassniki_n_sc = rescale(KZ_data$pol_news_odnoklassniki_n, to = c(1, 0))
KZ_data$trust_state_n_sc = rescale(KZ_data$trust_state_n, to = c(1, 0))
KZ_data$trust_russian_media_n_sc = rescale(KZ_data$trust_russian_media_n, to = c(1, 0))
KZ_data$trust_internet_n_sc = rescale(KZ_data$trust_internet_n, to = c(1, 0))
KZ_data$trust_facebook_n_sc = rescale(KZ_data$trust_facebook_n, to = c(1, 0))
KZ_data$trust_vkontakte_n_sc = rescale(KZ_data$trust_vkontakte_n, to = c(1, 0))
KZ_data$trust_western_n_sc = rescale(KZ_data$trust_western_n, to = c(1, 0))
KZ_data$news_balance_n_sc = rescale(KZ_data$news_balance_n, to = c(0, 1))
KZ_data$sm_critical_local_n_sc = rescale(KZ_data$sm_critical_local_n, to = c(1, 0))
KZ_data$sm_critical_central_n_sc = rescale(KZ_data$sm_critical_central_n, to = c(1, 0))
KZ_data$sm_positive_local_n_sc = rescale(KZ_data$sm_positive_local_n, to = c(1, 0))
KZ_data$sm_positive_central_n_sc = rescale(KZ_data$sm_positive_central_n, to = c(1, 0))
KZ_data$tv_critical_local_n_sc = rescale(KZ_data$tv_critical_local_n, to = c(1, 0))
KZ_data$tv_critical_central_n_sc = rescale(KZ_data$tv_critical_central_n, to = c(1, 0))
KZ_data$clickable_state_n_sc = rescale(KZ_data$clickable_state_n, to = c(1, 0))
KZ_data$clickable_russian_n_sc = rescale(KZ_data$clickable_russian_n, to = c(1, 0))
KZ_data$AI_n_sc = rescale(KZ_data$AI_n, to = c(1, 0))
KZ_data$echo_chamber_n_sc = rescale(KZ_data$echo_chamber_n, to = c(0, 1))
KZ_data$paid_posters_percentage_n_sc = rescale(KZ_data$paid_posters_percentage_n, to = c(0, 1))
KZ_data$avoidance_blocking_n_sc = rescale(KZ_data$avoidance_blocking_n, to = c(1, 0))
KZ_data$avoidance_unfriending_n_sc = rescale(KZ_data$avoidance_unfriending_n, to = c(1, 0))
KZ_data$avoidance_leaving_group_n_sc = rescale(KZ_data$avoidance_leaving_group_n, to = c(1, 0))
KZ_data$avoidance_unsubscribing_n_sc = rescale(KZ_data$avoidance_unsubscribing_n, to = c(1, 0))
KZ_data$sm_disagreement_politics_n_sc = rescale(KZ_data$sm_disagreement_politics_n, to = c(0, 1))
KZ_data$sm_disagreement_news_n_sc = rescale(KZ_data$sm_disagreement_news_n, to = c(0, 1))
KZ_data$sm_disagreement_issues_n_sc = rescale(KZ_data$sm_disagreement_issues_n, to = c(0, 1))
KZ_data$network_breadth_n_sc = rescale(KZ_data$network_breadth_n, to = c(0, 1))
KZ_data$sm_engage_friends_n_sc = rescale(KZ_data$sm_engage_friends_n, to = c(1, 0))
KZ_data$sm_engage_groups_n_sc = rescale(KZ_data$sm_engage_groups_n, to = c(1, 0))
KZ_data$sm_engage_post_n_sc = rescale(KZ_data$sm_engage_post_n, to = c(1, 0))
KZ_data$sm_engage_critical_n_sc = rescale(KZ_data$sm_engage_critical_n, to = c(1, 0))
KZ_data$sm_engage_supportive_sc = rescale(KZ_data$sm_engage_supportive_n, to = c(1, 0))
KZ_data$sm_engage_offline_sc = rescale(KZ_data$sm_engage_offline_n, to = c(1, 0))
KZ_data$ukraine_china_sc = rescale(KZ_data$ukraine_china_n, to = c(1, 0))
KZ_data$ukraine_russia_sc = rescale(KZ_data$ukraine_russia_n, to = c(1, 0))
KZ_data$ukraine_US_sc = rescale(KZ_data$ukraine_US_n, to = c(1, 0))
KZ_data$ukraine_attention_sc = rescale(KZ_data$ukraine_attention_n, to = c(1, 0))
KZ_data$ukraine_russia_responsible_sc = rescale(KZ_data$ukraine_russia_responsible_n, to = c(1, 0))
KZ_data$ukraine_ukraine_responsible_sc = rescale(KZ_data$ukraine_ukraine_responsible_n, to = c(1, 0))
KZ_data$ukraine_US_responsible_sc = rescale(KZ_data$ukraine_US_responsible_n, to = c(1, 0))

#Kyrgyzstan 
#Recoding all Variables##########################################################################
##############################################################################################
#################################################################################################
library(dplyr)
library(car)

# Internet Use Factor
KG_data$internet_use = recode(KG_data$q2, "1 = 'Yes'; 2 = 'No'")
KG_data$internet_use = factor(KG_data$internet_use)
table(KG_data$internet_use)  # Frequency table for internet_use

# Age Numeric
KG_data$age_n = KG_data$q3
summary(KG_data$age_n)  # Summary for age_n

# Age Collapsed Factor
KG_data$age_collapsed = recode(KG_data$q3a, "1 = '18 - 24'; 2 = '25 - 34'; 3 = '35 - 44'; 4 = '45 - 54'; 5 = '55 - 64'; 6 = '65+'")
KG_data$age_collapsed = factor(KG_data$age_collapsed, ordered = TRUE)
table(KG_data$age_collapsed)  # Frequency table for age_collapsed

# Ethnicity Factor
table(KG_data$q4)
KG_data$ethnicity = recode(KG_data$q4, "1 = 'Kazakh'; 2 = 'Russian'; 3 = 'Uzbek'; 4 = 'Tajik'; 5 = 'Kyrgyz'; 6 = 'Other'")
KG_data$ethnicity = factor(KG_data$ethnicity)
KG_data$ethnicity = addNA(KG_data$ethnicity)
table(KG_data$ethnicity)  # Frequency table for ethnicity

# Urbanicity Factor
KG_data$urbanicity = recode(KG_data$q5, "1 = 'City'; 2 = 'Village'")
KG_data$urbanicity = factor(KG_data$urbanicity)
KG_data$urbanicity = addNA(KG_data$urbanicity)
table(KG_data$urbanicity)  # Frequency table for urbanicity

# Gender Factor
KG_data$gender = recode(KG_data$q6, "1 = 'Female'; 2 = 'Male'")
KG_data$gender = factor(KG_data$gender)
KG_data$gender = addNA(KG_data$gender)
table(KG_data$gender)  # Frequency table for gender

# Education Factor
KG_data$edu = recode(KG_data$q7, "1 = 'No education'; 2 = 'Primary education'; 3 = 'Basic secondary education'; 4 = 'General secondary education'; 5 = 'Complete vocational education'; 6 = 'Incomplete higher education'; 7 = 'Complete higher education'")
KG_data$edu = factor(KG_data$edu, ordered = TRUE)
table(KG_data$edu)  # Frequency table for edu

# Education Numeric
KG_data$edu_n = KG_data$q7
summary(KG_data$edu_n)  # Summary for edu_n

# Income Factor
KG_data$inc = recode(KG_data$q8, "1 = 'Less than 70,001'; 2 = '70,001 - 150,000'; 3 = '150,001 – 200,000'; 4 = '200,001 – 250,000'; 5 = '250,001 – 300,000'; 6 = '300,001 – 350,000'; 7 = '350,001 – 400,000'; 8 = '400,001 – 450,000'; 9 = '450,001 – 500,000'; 10 = '500,001 – 550,000'; 11 = '550,001 – 600,000'; 12 = 'More than 600,000'")
KG_data$inc = factor(KG_data$inc, levels = c('Less than 70,001', '70,001 - 150,000', '150,001 – 200,000', '200,001 – 250,000', '250,001 – 300,000', '300,001 – 350,000', '350,001 – 400,000', '400,001 – 450,000', '450,001 – 500,000', '500,001 – 550,000', '550,001 – 600,000', 'More than 600,000'), ordered = TRUE)
table(KG_data$inc)  # Frequency table for Income Factor

# Income Numeric
KG_data$inc_n = KG_data$q8
summary(KG_data$inc_n)  # Summary for Income Numeric

# Political Interest Factor
KG_data$pol_interest = recode(KG_data$q9, "1 = 'Very interested'; 2 = 'Somewhat interested'; 3 = 'Somewhat uninterested'; 4 = 'Very uninterested'")
KG_data$pol_interest = factor(KG_data$pol_interest, levels = c('Very interested', 'Somewhat interested', 'Somewhat uninterested', 'Very uninterested'), ordered = TRUE)
table(KG_data$pol_interest)  # Frequency table for Political Interest Factor

# Political Interest Numeric
KG_data$pol_interest_n = KG_data$q9
summary(KG_data$pol_interest_n)  # Summary for Political Interest Numeric

# Discuss Politics Factor
KG_data$pol_discuss = recode(KG_data$q10, "1 = 'A few times a day'; 2 = 'Once a day'; 3 = 'Three to five days a week'; 4 = 'Once a week'; 5 = 'Less often than once a week'; 6 = 'Never'")
KG_data$pol_discuss = factor(KG_data$pol_discuss, levels = c('A few times a day', 'Once a day', 'Three to five days a week', 'Once a week', 'Less often than once a week', 'Never'), ordered = TRUE)
table(KG_data$pol_discuss)  # Frequency table for Discuss Politics Factor

#Discuss Politics Numeric
KG_data$pol_discuss_n = KG_data$q10
summary(KG_data$pol_discuss_n)  # Summary for Discuss Politics Numeric

# Disagree when Discussing Politics
KG_data$pol_disagree = recode(KG_data$q11, "1 = 'Most of the time'; 2 = 'Some of the time'; 3 = 'Rarely'; 4 = 'Never'")
KG_data$pol_disagree = factor(KG_data$pol_disagree, levels = c('Most of the time', 'Some of the time', 'Rarely', 'Never'), ordered = TRUE)
table(KG_data$pol_disagree)  # Frequency table for Disagree when Discussing Politics

#Disagree Politics Numeric
KG_data$pol_disagree_n = KG_data$q11
summary(KG_data$pol_disagree_n)  # Summary for Disagree Politics Numeric

# Trust Variables Factor
# Trust in Central Government
KG_data$trust_central = recode(KG_data$q12_a, "1 = 'A Great Deal'; 2 = 'Quite a Lot of Trust'; 3 = 'Not Very Much Trust'; 4 = 'None at all'")
KG_data$trust_central = factor(KG_data$trust_central, levels = c('A Great Deal', 'Quite a Lot of Trust', 'Not Very Much Trust', 'None at all'), ordered = TRUE)
table(KG_data$trust_central)  # Frequency table for Trust in Central Government

# Trust in Local Government
KG_data$trust_local = recode(KG_data$q12_b, "1 = 'A Great Deal'; 2 = 'Quite a Lot of Trust'; 3 = 'Not Very Much Trust'; 4 = 'None at all'")
KG_data$trust_local = factor(KG_data$trust_local, levels = c('A Great Deal', 'Quite a Lot of Trust', 'Not Very Much Trust', 'None at all'), ordered = TRUE)
table(KG_data$trust_local)  # Frequency table for Trust in Local Government

# Trust in Russia
KG_data$trust_russia = recode(KG_data$q12_c, "1 = 'A Great Deal'; 2 = 'Quite a Lot of Trust'; 3 = 'Not Very Much Trust'; 4 = 'None at all'")
KG_data$trust_russia = factor(KG_data$trust_russia, levels = c('A Great Deal', 'Quite a Lot of Trust', 'Not Very Much Trust', 'None at all'), ordered = TRUE)
table(KG_data$trust_russia)  # Frequency table for Trust in Russia

# Trust in the US
KG_data$trust_US = recode(KG_data$q12_d, "1 = 'A Great Deal'; 2 = 'Quite a Lot of Trust'; 3 = 'Not Very Much Trust'; 4 = 'None at all'")
KG_data$trust_US = factor(KG_data$trust_US, levels = c('A Great Deal', 'Quite a Lot of Trust', 'Not Very Much Trust', 'None at all'), ordered = TRUE)
table(KG_data$trust_US)  # Frequency table for Trust in the US

# Trust in China
KG_data$trust_china = recode(KG_data$q12_e, "1 = 'A Great Deal'; 2 = 'Quite a Lot of Trust'; 3 = 'Not Very Much Trust'; 4 = 'None at all'")
KG_data$trust_china = factor(KG_data$trust_china, levels = c('A Great Deal', 'Quite a Lot of Trust', 'Not Very Much Trust', 'None at all'), ordered = TRUE)
table(KG_data$trust_china)  # Frequency table for Trust in China

# Trust in the EU
KG_data$trust_EU = recode(KG_data$q12_f, "1 = 'A Great Deal'; 2 = 'Quite a Lot of Trust'; 3 = 'Not Very Much Trust'; 4 = 'None at all'")
KG_data$trust_EU = factor(KG_data$trust_EU, levels = c('A Great Deal', 'Quite a Lot of Trust', 'Not Very Much Trust', 'None at all'), ordered = TRUE)
table(KG_data$trust_EU)  # Frequency table for Trust in the EU

# Trust Central Numeric
KG_data$trust_central_n = KG_data$q12_a
summary(KG_data$trust_central_n)

# Trust Local Numeric
KG_data$trust_local_n = KG_data$q12_b
summary(KG_data$trust_local_n)

# Trust Russia Numeric
KG_data$trust_russia_n = KG_data$q12_c
summary(KG_data$trust_russia_n)

# Trust US Numeric
KG_data$trust_US_n = KG_data$q12_d
summary(KG_data$trust_US_n)

# Trust China Numeric
KG_data$trust_china_n = KG_data$q12_e
summary(KG_data$trust_china_n)

# Trust EU Numeric
KG_data$trust_EU_n = KG_data$q12_f
summary(KG_data$trust_EU_n)

library(car)

# Recoding 'Most Important Issue First Mention' with car::recode
KG_data$important_issue_first = recode(KG_data$q13_a, "1 = 'Political instability'; 2 = 'Unemployment'; 3 = 'Inflation Prices'; 4 = 'Wages pensions'; 5 = 'Taxes'; 6 = 'Access to basic needs'; 7 = 'General economic situation'; 8 = 'Deterioration of the quality and access to education'; 9 = 'Construction of houses residential premises'; 10 = 'Terrorism'; 11 = 'War conflict in other countries'; 12 = 'Increase in crime violence concerns about personal safety'; 13 = 'Corruption'; 14 = 'Lack of opportunities'; 15 = 'Discrimination ethnic or religious tensions'; 16 = 'Infrastructure'; 17 = 'Deterioration of the environment'; 18 = 'Unresolved territorial conflicts'; 19 = 'Emigration'")
KG_data$important_issue_first = factor(KG_data$important_issue_first)
KG_data$important_issue_first = addNA(KG_data$important_issue_first)
table(KG_data$important_issue_first)  # Frequency table for important_issue_first

# Recoding 'Most Important Issue Second Mention' with car::recode
KG_data$important_issue_second = recode(KG_data$q13_b, "1 = 'Political instability'; 2 = 'Unemployment'; 3 = 'Inflation Prices'; 4 = 'Wages pensions'; 5 = 'Taxes'; 6 = 'Access to basic needs'; 7 = 'General economic situation'; 8 = 'Deterioration of the quality and access to education'; 9 = 'Construction of houses residential premises'; 10 = 'Terrorism'; 11 = 'War conflict in other countries'; 12 = 'Increase in crime violence concerns about personal safety'; 13 = 'Corruption'; 14 = 'Lack of opportunities'; 15 = 'Discrimination ethnic or religious tensions'; 16 = 'Infrastructure'; 17 = 'Deterioration of the environment'; 18 = 'Unresolved territorial conflicts'; 19 = 'Emigration'")
KG_data$important_issue_second = factor(KG_data$important_issue_second)
KG_data$important_issue_second = addNA(KG_data$important_issue_second)
table(KG_data$important_issue_second)  # Frequency table for important_issue_first

# System Approval Variables Factor
# Perform the recode
KG_data$system_capable = recode(KG_data$q14_a, "1 = 'Strongly agree'; 2 = 'Somewhat agree'; 3 = 'Neither disagree nor agree'; 4 = 'Somewhat disagree'; 5 = 'Strongly disagree'")
KG_data$system_capable = factor(KG_data$system_capable, levels = c('Strongly agree', 'Somewhat agree', 'Neither disagree nor agree', 'Somewhat disagree', 'Strongly disagree'), ordered = TRUE)
table(KG_data$system_capable)  # Frequency table for System Capable

# Numeric variable for System Capable
KG_data$system_capable_n = as.numeric(KG_data$q14_a)
summary(KG_data$system_capable_n)  # Summary for System Capable (Numeric)

KG_data$system_proud = recode(KG_data$q14_b, "1 = 'Strongly agree'; 2 = 'Somewhat agree'; 3 = 'Neither disagree nor agree'; 4 = 'Somewhat disagree'; 5 = 'Strongly disagree'")
KG_data$system_proud = factor(KG_data$system_proud, levels = c('Strongly agree', 'Somewhat agree', 'Neither disagree nor agree', 'Somewhat disagree', 'Strongly disagree'), ordered = TRUE)
table(KG_data$system_proud)  # Frequency table for System Proud

# Numeric variable for System Proud
KG_data$system_proud_n = as.numeric(KG_data$q14_b)
summary(KG_data$system_proud_n)  # Summary for System Proud (Numeric)

KG_data$system_deserves = recode(KG_data$q14_c, "1 = 'Strongly agree'; 2 = 'Somewhat agree'; 3 = 'Neither disagree nor agree'; 4 = 'Somewhat disagree'; 5 = 'Strongly disagree'")
KG_data$system_deserves = factor(KG_data$system_deserves, levels = c('Strongly agree', 'Somewhat agree', 'Neither disagree nor agree', 'Somewhat disagree', 'Strongly disagree'), ordered = TRUE)
table(KG_data$system_deserves)  # Frequency table for System Deserves

# Numeric variable for System Deserves
KG_data$system_deserves_n = as.numeric(KG_data$q14_c)
summary(KG_data$system_deserves_n)  # Summary for System Deserves (Numeric)

KG_data$system_live = recode(KG_data$q14_d, "1 = 'Strongly agree'; 2 = 'Somewhat agree'; 3 = 'Neither disagree nor agree'; 4 = 'Somewhat disagree'; 5 = 'Strongly disagree'")
KG_data$system_live = factor(KG_data$system_live, levels = c('Strongly agree', 'Somewhat agree', 'Neither disagree nor agree', 'Somewhat disagree', 'Strongly disagree'), ordered = TRUE)
table(KG_data$system_live)  # Frequency table for System Live

# Numeric variable for System Live
KG_data$system_live_n = as.numeric(KG_data$q14_d)
summary(KG_data$system_live_n)  # Summary for System Live (Numeric)

KG_data$system_hurdles_participate = recode(KG_data$q14_e, "1 = 'Strongly agree'; 2 = 'Somewhat agree'; 3 = 'Neither disagree nor agree'; 4 = 'Somewhat disagree'; 5 = 'Strongly disagree'")
KG_data$system_hurdles_participate = factor(KG_data$system_hurdles_participate, levels = c('Strongly agree', 'Somewhat agree', 'Neither disagree nor agree', 'Somewhat disagree', 'Strongly disagree'), ordered = TRUE)
table(KG_data$system_hurdles_participate)  # Frequency table for System Hurdles for Participation

# Numeric variable for System Hurdles for Participation
KG_data$system_hurdles_participate_n = as.numeric(KG_data$q14_e)
summary(KG_data$system_hurdles_participate_n)  # Summary for System Hurdles for Participation (Numeric)

# Participate Variables Factor
# Perform the recode
KG_data$participate_rally = recode(KG_data$q15_a, "1 = 'Yes'; 2 = 'No'")
KG_data$participate_rally = factor(KG_data$participate_rally, levels = c('Yes', 'No'), ordered = FALSE)
table(KG_data$participate_rally)  # Frequency table for Participate Rally

# Numeric variable for Participate Rally
KG_data$participate_rally_n = as.numeric(KG_data$q15_a)
summary(KG_data$participate_rally_n)  # Summary for Participate Rally (Numeric)

KG_data$participate_protest = recode(KG_data$q15_b, "1 = 'Yes'; 2 = 'No'")
KG_data$participate_protest = factor(KG_data$participate_protest, levels = c('Yes', 'No'), ordered = FALSE)
table(KG_data$participate_protest)  # Frequency table for Participate Protest

# Numeric variable for Participate Protest
KG_data$participate_protest_n = as.numeric(KG_data$q15_b)
summary(KG_data$participate_protest_n)  # Summary for Participate Protest (Numeric)

KG_data$participate_meeting = recode(KG_data$q15_c, "1 = 'Yes'; 2 = 'No'")
KG_data$participate_meeting = factor(KG_data$participate_meeting, levels = c('Yes', 'No'), ordered = FALSE)
table(KG_data$participate_meeting)  # Frequency table for Participate Meeting

# Numeric variable for Participate Meeting
KG_data$participate_meeting_n = as.numeric(KG_data$q15_c)
summary(KG_data$participate_meeting_n)  # Summary for Participate Meeting (Numeric)

KG_data$participate_volunteer = recode(KG_data$q15_d, "1 = 'Yes'; 2 = 'No'")
KG_data$participate_volunteer = factor(KG_data$participate_volunteer, levels = c('Yes', 'No'), ordered = FALSE)
table(KG_data$participate_volunteer)  # Frequency table for Participate Volunteer

# Numeric variable for Participate Volunteer
KG_data$participate_volunteer_n = as.numeric(KG_data$q15_d)
summary(KG_data$participate_volunteer_n)  # Summary for Participate Volunteer (Numeric)

KG_data$participate_member = recode(KG_data$q15_e, "1 = 'Yes'; 2 = 'No'")
KG_data$participate_member = factor(KG_data$participate_member, levels = c('Yes', 'No'), ordered = FALSE)
table(KG_data$participate_member)  # Frequency table for Participate Member

# Numeric variable for Participate Member
KG_data$participate_member_n = as.numeric(KG_data$q15_e)
summary(KG_data$participate_member_n)  # Summary for Participate Member (Numeric)

KG_data$participate_community = recode(KG_data$q15_f, "1 = 'Yes'; 2 = 'No'")
KG_data$participate_community = factor(KG_data$participate_community, levels = c('Yes', 'No'), ordered = FALSE)
table(KG_data$participate_community)  # Frequency table for Participate Community

# Numeric variable for Participate Community
KG_data$participate_community_n = as.numeric(KG_data$q15_f)
summary(KG_data$participate_community_n)  # Summary for Participate Community (Numeric)

KG_data$participate_contact = recode(KG_data$q15_g, "1 = 'Yes'; 2 = 'No'")
KG_data$participate_contact = factor(KG_data$participate_contact, levels = c('Yes', 'No'), ordered = FALSE)
table(KG_data$participate_contact)  # Frequency table for Participate Contact

# Numeric variable for Participate Contact
KG_data$participate_contact_n = as.numeric(KG_data$q15_g)
summary(KG_data$participate_contact_n)  # Summary for Participate Contact (Numeric)

KG_data$participate_vote = recode(KG_data$q16, "1 = 'Yes'; 2 = 'No'")
KG_data$participate_vote = factor(KG_data$participate_vote, levels = c('Yes', 'No'), ordered = FALSE)
table(KG_data$participate_vote)  # Frequency table for Participate Vote

# Numeric variable for Participate Vote
KG_data$participate_vote_n = as.numeric(KG_data$q16)
summary(KG_data$participate_vote_n)  # Summary for Participate Vote (Numeric)

# Digital Tracking Variables Factor
# Perform the recode
KG_data$tracking_central = recode(KG_data$q18_a, "1 = 'Not Comfortable at all'; 2 = '2'; 3 = '3'; 4 = '4'; 5 = 'Very Comfortable'")
KG_data$tracking_central = factor(KG_data$tracking_central, levels = c('Not Comfortable at all', '2', '3', '4', 'Very Comfortable'), ordered = TRUE)
table(KG_data$tracking_central)  # Frequency table for Tracking Central

# Numeric variable for Tracking Central
KG_data$tracking_central_n = as.numeric(KG_data$q18_a)
summary(KG_data$tracking_central_n)  # Summary for Tracking Central (Numeric)

KG_data$tracking_local = recode(KG_data$q18_b, "1 = 'Not Comfortable at all'; 2 = '2'; 3 = '3'; 4 = '4'; 5 = 'Very Comfortable'")
KG_data$tracking_local = factor(KG_data$tracking_local, levels = c('Not Comfortable at all', '2', '3', '4', 'Very Comfortable'), ordered = TRUE)
table(KG_data$tracking_local)  # Frequency table for Tracking Local

# Numeric variable for Tracking Local
KG_data$tracking_local_n = as.numeric(KG_data$q18_b)
summary(KG_data$tracking_local_n)  # Summary for Tracking Local (Numeric)

KG_data$tracking_companies = recode(KG_data$q18_c, "1 = 'Not Comfortable at all'; 2 = '2'; 3 = '3'; 4 = '4'; 5 = 'Very Comfortable'")
KG_data$tracking_companies = factor(KG_data$tracking_companies, levels = c('Not Comfortable at all', '2', '3', '4', 'Very Comfortable'), ordered = TRUE)
table(KG_data$tracking_companies)  # Frequency table for Tracking Companies

# Numeric variable for Tracking Companies
KG_data$tracking_companies_n = as.numeric(KG_data$q18_c)
summary(KG_data$tracking_companies_n)  # Summary for Tracking Companies (Numeric)

# Democracy Variables Factor
# Perform the recode
KG_data$democracy_elections = recode(KG_data$q20_a, "1 = 'Strongly agree'; 2 = 'Somewhat agree'; 3 = 'Neither disagree nor agree'; 4 = 'Somewhat disagree'; 5 = 'Strongly disagree'")
KG_data$democracy_elections = factor(KG_data$democracy_elections, levels = c('Strongly agree', 'Somewhat agree', 'Neither disagree nor agree', 'Somewhat disagree', 'Strongly disagree'), ordered = TRUE)
table(KG_data$democracy_elections)  # Frequency table for Democracy Elections

# Numeric variable for Democracy Elections
KG_data$democracy_elections_n = as.numeric(KG_data$q20_a)
summary(KG_data$democracy_elections_n)  # Summary for Democracy Elections (Numeric)

KG_data$democracy_speech = recode(KG_data$q20_b, "1 = 'Strongly agree'; 2 = 'Somewhat agree'; 3 = 'Neither disagree nor agree'; 4 = 'Somewhat disagree'; 5 = 'Strongly disagree'")
KG_data$democracy_speech = factor(KG_data$democracy_speech, levels = c('Strongly agree', 'Somewhat agree', 'Neither disagree nor agree', 'Somewhat disagree', 'Strongly disagree'), ordered = TRUE)
table(KG_data$democracy_speech)  # Frequency table for Democracy Speech

# Numeric variable for Democracy Speech
KG_data$democracy_speech_n = as.numeric(KG_data$q20_b)
summary(KG_data$democracy_speech_n)  # Summary for Democracy Speech (Numeric)

KG_data$democracy_oversight = recode(KG_data$q20_c, "1 = 'Strongly agree'; 2 = 'Somewhat agree'; 3 = 'Neither disagree nor agree'; 4 = 'Somewhat disagree'; 5 = 'Strongly disagree'")
KG_data$democracy_oversight = factor(KG_data$democracy_oversight, levels = c('Strongly agree', 'Somewhat agree', 'Neither disagree nor agree', 'Somewhat disagree', 'Strongly disagree'), ordered = TRUE)
table(KG_data$democracy_oversight)  # Frequency table for Democracy Oversight

# Numeric variable for Democracy Oversight
KG_data$democracy_oversight_n = as.numeric(KG_data$q20_c)
summary(KG_data$democracy_oversight_n)  # Summary for Democracy Oversight (Numeric)

KG_data$democracy_organize = recode(KG_data$q20_d, "1 = 'Strongly agree'; 2 = 'Somewhat agree'; 3 = 'Neither disagree nor agree'; 4 = 'Somewhat disagree'; 5 = 'Strongly disagree'")
KG_data$democracy_organize = factor(KG_data$democracy_organize, levels = c('Strongly agree', 'Somewhat agree', 'Neither disagree nor agree', 'Somewhat disagree', 'Strongly disagree'), ordered = TRUE)
table(KG_data$democracy_organize)  # Frequency table for Democracy Organize

# Numeric variable for Democracy Organize
KG_data$democracy_organize_n = as.numeric(KG_data$q20_d)
summary(KG_data$democracy_organize_n)  # Summary for Democracy Organize (Numeric)

KG_data$democracy_press = recode(KG_data$q20_e, "1 = 'Strongly agree'; 2 = 'Somewhat agree'; 3 = 'Neither disagree nor agree'; 4 = 'Somewhat disagree'; 5 = 'Strongly disagree'")
KG_data$democracy_press = factor(KG_data$democracy_press, levels = c('Strongly agree', 'Somewhat agree', 'Neither disagree nor agree', 'Somewhat disagree', 'Strongly disagree'), ordered = TRUE)
table(KG_data$democracy_press)  # Frequency table for Democracy Press

# Numeric variable for Democracy Press
KG_data$democracy_press_n = as.numeric(KG_data$q20_e)
summary(KG_data$democracy_press_n)  # Summary for Democracy Press (Numeric)

KG_data$democracy_parties = recode(KG_data$q20_f, "1 = 'Strongly agree'; 2 = 'Somewhat agree'; 3 = 'Neither disagree nor agree'; 4 = 'Somewhat disagree'; 5 = 'Strongly disagree'")
KG_data$democracy_parties = factor(KG_data$democracy_parties, levels = c('Strongly agree', 'Somewhat agree', 'Neither disagree nor agree', 'Somewhat disagree', 'Strongly disagree'), ordered = TRUE)
table(KG_data$democracy_parties)  # Frequency table for Democracy Parties

# Numeric variable for Democracy Parties
KG_data$democracy_parties_n = as.numeric(KG_data$q20_f)
summary(KG_data$democracy_parties_n)  # Summary for Democracy Parties (Numeric)

KG_data$democracy_protests = recode(KG_data$q20_g, "1 = 'Strongly agree'; 2 = 'Somewhat agree'; 3 = 'Neither disagree nor agree'; 4 = 'Somewhat disagree'; 5 = 'Strongly disagree'")
KG_data$democracy_protests = factor(KG_data$democracy_protests, levels = c('Strongly agree', 'Somewhat agree', 'Neither disagree nor agree', 'Somewhat disagree', 'Strongly disagree'), ordered = TRUE)
table(KG_data$democracy_protests)  # Frequency table for Democracy Protests

# Numeric variable for Democracy Protests
KG_data$democracy_protests_n = as.numeric(KG_data$q20_g)
summary(KG_data$democracy_protests_n)  # Summary for Democracy Protests (Numeric)

KG_data$democracy_courts = recode(KG_data$q20_h, "1 = 'Strongly agree'; 2 = 'Somewhat agree'; 3 = 'Neither disagree nor agree'; 4 = 'Somewhat disagree'; 5 = 'Strongly disagree'")
KG_data$democracy_courts = factor(KG_data$democracy_courts, levels = c('Strongly agree', 'Somewhat agree', 'Neither disagree nor agree', 'Somewhat disagree', 'Strongly disagree'), ordered = TRUE)
table(KG_data$democracy_courts)  # Frequency table for Democracy Courts

# Numeric variable for Democracy Courts
KG_data$democracy_courts_n = as.numeric(KG_data$q20_h)
summary(KG_data$democracy_courts_n)  # Summary for Democracy Courts (Numeric)

#Social Media Use
# Perform the recode for the factor variable
KG_data$sm_use = recode(KG_data$q22, "1 = 'Yes'; 2 = 'No'")
KG_data$sm_use = factor(KG_data$sm_use, levels = c('Yes', 'No'), ordered = TRUE)
KG_data$sm_use = addNA(KG_data$sm_use)
table(KG_data$sm_use)  # Frequency table for Social Media Use

#Social Media Platform Variables Factor
#Perform the recode
KG_data$facebook = recode(KG_data$q23_a, '0="Do not use this social media"; 1="0-1"; 2="1-2"; 3="2-3"; 4="3-4"; 5="4-5"; 6="5-6"; 7="6-7"; 8="7-8"; 9="8-9"; 10="More than 9"')
KG_data$facebook = factor(KG_data$facebook, levels = c('Do not use this social media', '0-1', '1-2', '2-3', '3-4', '4-5', '5-6', '6-7', '7-8', '8-9', 'More than 9'), ordered = TRUE)
table(KG_data$facebook) # Display a frequency table for 'facebook'

# Create a numeric variable for facebook
KG_data$facebook_n = as.numeric(KG_data$q23_a) 
summary(KG_data$facebook_n) # Provide a summary for 'facebook_n' (Numeric)

KG_data$vkontakte = recode(KG_data$q23_b, '0="Do not use this social media"; 1="0-1"; 2="1-2"; 3="2-3"; 4="3-4"; 5="4-5"; 6="5-6"; 7="6-7"; 8="7-8"; 9="8-9"; 10="More than 9"')
KG_data$vkontakte = factor(KG_data$vkontakte, levels = c('Do not use this social media', '0-1', '1-2', '2-3', '3-4', '4-5', '5-6', '6-7', '7-8', '8-9', 'More than 9'), ordered = TRUE)
table(KG_data$vkontakte) # Display a frequency table for 'vkontakte'

# Create a numeric variable for vkontakte
KG_data$vkontakte_n = as.numeric(KG_data$q23_b) 
summary(KG_data$vkontakte_n) # Provide a summary for 'vkontakte_n' (Numeric)

KG_data$instagram = recode(KG_data$q23_c, '0="Do not use this social media"; 1="0-1"; 2="1-2"; 3="2-3"; 4="3-4"; 5="4-5"; 6="5-6"; 7="6-7"; 8="7-8"; 9="8-9"; 10="More than 9"')
KG_data$instagram = factor(KG_data$instagram, levels = c('Do not use this social media', '0-1', '1-2', '2-3', '3-4', '4-5', '5-6', '6-7', '7-8', '8-9', 'More than 9'), ordered = TRUE)
table(KG_data$instagram) # Display a frequency table for 'instagram'

# Create a numeric variable for instagram
KG_data$instagram_n = as.numeric(KG_data$q23_c) 
summary(KG_data$instagram_n) # Provide a summary for 'instagram_n' (Numeric)


KG_data$tiktok = recode(KG_data$q23_d, '0="Do not use this social media"; 1="0-1"; 2="1-2"; 3="2-3"; 4="3-4"; 5="4-5"; 6="5-6"; 7="6-7"; 8="7-8"; 9="8-9"; 10="More than 9"')
KG_data$tiktok = factor(KG_data$tiktok, levels = c('Do not use this social media', '0-1', '1-2', '2-3', '3-4', '4-5', '5-6', '6-7', '7-8', '8-9', 'More than 9'), ordered = TRUE)
table(KG_data$tiktok) # Display a frequency table for 'tiktok'

# Create a numeric variable for tiktok
KG_data$tiktok_n = as.numeric(KG_data$q23_d) 
summary(KG_data$tiktok_n) # Provide a summary for 'tiktok_n' (Numeric)

KG_data$twitter = recode(KG_data$q23_e, '0="Do not use this social media"; 1="0-1"; 2="1-2"; 3="2-3"; 4="3-4"; 5="4-5"; 6="5-6"; 7="6-7"; 8="7-8"; 9="8-9"; 10="More than 9"')
KG_data$twitter = factor(KG_data$twitter, levels = c('Do not use this social media', '0-1', '1-2', '2-3', '3-4', '4-5', '5-6', '6-7', '7-8', '8-9', 'More than 9'), ordered = TRUE)
table(KG_data$twitter) # Display a frequency table for 'twitter'

# Create a numeric variable for twitter
KG_data$twitter_n = as.numeric(KG_data$q23_e) 
summary(KG_data$twitter_n) # Provide a summary for 'twitter_n' (Numeric)

KG_data$youtube = recode(KG_data$q23_f, '0="Do not use this social media"; 1="0-1"; 2="1-2"; 3="2-3"; 4="3-4"; 5="4-5"; 6="5-6"; 7="6-7"; 8="7-8"; 9="8-9"; 10="More than 9"')
KG_data$youtube = factor(KG_data$youtube, levels = c('Do not use this social media', '0-1', '1-2', '2-3', '3-4', '4-5', '5-6', '6-7', '7-8', '8-9', 'More than 9'), ordered = TRUE)
table(KG_data$youtube) # Display a frequency table for 'youtube'

# Create a numeric variable for youtube
KG_data$youtube_n = as.numeric(KG_data$q23_f) 
summary(KG_data$youtube_n) # Provide a summary for 'youtube_n' (Numeric)

KG_data$whatsapp = recode(KG_data$q23_g, '0="Do not use this social media"; 1="0-1"; 2="1-2"; 3="2-3"; 4="3-4"; 5="4-5"; 6="5-6"; 7="6-7"; 8="7-8"; 9="8-9"; 10="More than 9"')
KG_data$whatsapp = factor(KG_data$whatsapp, levels = c('Do not use this social media', '0-1', '1-2', '2-3', '3-4', '4-5', '5-6', '6-7', '7-8', '8-9', 'More than 9'), ordered = TRUE)
table(KG_data$whatsapp) # Display a frequency table for 'whatsapp'

# Create a numeric variable for whatsapp
KG_data$whatsapp_n = as.numeric(KG_data$q23_g) 
summary(KG_data$whatsapp_n) # Provide a summary for 'whatsapp_n' (Numeric)

KG_data$telegram = recode(KG_data$q23_h, '0="Do not use this social media"; 1="0-1"; 2="1-2"; 3="2-3"; 4="3-4"; 5="4-5"; 6="5-6"; 7="6-7"; 8="7-8"; 9="8-9"; 10="More than 9"')
KG_data$telegram = factor(KG_data$telegram, levels = c('Do not use this social media', '0-1', '1-2', '2-3', '3-4', '4-5', '5-6', '6-7', '7-8', '8-9', 'More than 9'), ordered = TRUE)
table(KG_data$telegram) # Display a frequency table for 'telegram'

# Create a numeric variable for whatsapp
KG_data$telegram_n = as.numeric(KG_data$q23_h) 
summary(KG_data$telegram_n) # Provide a summary for 'telegram_n' (Numeric)

# Perform the recode for 'pol_news_tv'
KG_data$pol_news_tv = recode(KG_data$q24_a, '1="A few times a day"; 2="Once a day"; 3="Three to five days a week"; 4="Once a week"; 5="Less often than once a week"; 6="Never"')
KG_data$pol_news_tv = factor(KG_data$pol_news_tv, levels = c('A few times a day', 'Once a day', 'Three to five days a week', 'Once a week', 'Less often than once a week', 'Never'), ordered = TRUE)
table(KG_data$pol_news_tv) # Display a frequency table for 'pol_news_tv'

# Create a numeric variable for 'pol_news_tv'
KG_data$pol_news_tv_n = as.numeric(KG_data$q24_a) 
summary(KG_data$pol_news_tv_n) # Provide a summary for 'pol_news_tv_n' (Numeric)

# Perform the recode for 'pol_news_facebook'
KG_data$pol_news_facebook = recode(KG_data$q24_b, '1="A few times a day"; 2="Once a day"; 3="Three to five days a week"; 4="Once a week"; 5="Less often than once a week"; 6="Never"')
KG_data$pol_news_facebook = factor(KG_data$pol_news_facebook, levels = c('A few times a day', 'Once a day', 'Three to five days a week', 'Once a week', 'Less often than once a week', 'Never'), ordered = TRUE)
table(KG_data$pol_news_facebook) # Display a frequency table for 'pol_news_facebook'

# Create a numeric variable for 'pol_news_facebook'
KG_data$pol_news_facebook_n = as.numeric(KG_data$q24_b) 
summary(KG_data$pol_news_facebook_n) # Provide a summary for 'pol_news_facebook_n' (Numeric)

# Perform the recode for 'pol_news_vkontakte'
KG_data$pol_news_vkontakte = recode(KG_data$q24_c, '1="A few times a day"; 2="Once a day"; 3="Three to five days a week"; 4="Once a week"; 5="Less often than once a week"; 6="Never"')
KG_data$pol_news_vkontakte = factor(KG_data$pol_news_vkontakte, levels = c('A few times a day', 'Once a day', 'Three to five days a week', 'Once a week', 'Less often than once a week', 'Never'), ordered = TRUE)
table(KG_data$pol_news_vkontakte) # Display a frequency table for 'pol_news_vkontakte'

# Create a numeric variable for 'pol_news_vkontakte'
KG_data$pol_news_vkontakte_n = as.numeric(KG_data$q24_c) 
summary(KG_data$pol_news_vkontakte_n) # Provide a summary for 'pol_news_vkontakte_n' (Numeric)

# Perform the recode for 'pol_news_tiktok'
KG_data$pol_news_tiktok = recode(KG_data$q24_d, '1="A few times a day"; 2="Once a day"; 3="Three to five days a week"; 4="Once a week"; 5="Less often than once a week"; 6="Never"')
KG_data$pol_news_tiktok = factor(KG_data$pol_news_tiktok, levels = c('A few times a day', 'Once a day', 'Three to five days a week', 'Once a week', 'Less often than once a week', 'Never'), ordered = TRUE)
table(KG_data$pol_news_tiktok) # Display a frequency table for 'pol_news_tiktok'

# Create a numeric variable for 'pol_news_tiktok'
KG_data$pol_news_tiktok_n = as.numeric(KG_data$q24_d) 
summary(KG_data$pol_news_tiktok_n) # Provide a summary for 'pol_news_tiktok_n' (Numeric)

# Perform the recode for 'pol_news_twitter'
KG_data$pol_news_twitter = recode(KG_data$q24_e, '1="A few times a day"; 2="Once a day"; 3="Three to five days a week"; 4="Once a week"; 5="Less often than once a week"; 6="Never"')
KG_data$pol_news_twitter = factor(KG_data$pol_news_twitter, levels = c('A few times a day', 'Once a day', 'Three to five days a week', 'Once a week', 'Less often than once a week', 'Never'), ordered = TRUE)
table(KG_data$pol_news_twitter) # Display a frequency table for 'pol_news_twitter'

# Create a numeric variable for 'pol_news_twitter'
KG_data$pol_news_twitter_n = as.numeric(KG_data$q24_e) 
summary(KG_data$pol_news_twitter_n) # Provide a summary for 'pol_news_twitter_n' (Numeric)

# Perform the recode for 'pol_news_odnoklassniki'
KG_data$pol_news_odnoklassniki = recode(KG_data$q24_f, '1="A few times a day"; 2="Once a day"; 3="Three to five days a week"; 4="Once a week"; 5="Less often than once a week"; 6="Never"')
KG_data$pol_news_odnoklassniki = factor(KG_data$pol_news_odnoklassniki, levels = c('A few times a day', 'Once a day', 'Three to five days a week', 'Once a week', 'Less often than once a week', 'Never'), ordered = TRUE)
table(KG_data$pol_news_odnoklassniki) # Display a frequency table for 'pol_news_odnoklassniki'

# Create a numeric variable for 'pol_news_odnoklassniki'
KG_data$pol_news_odnoklassniki_n = as.numeric(KG_data$q24_f) 
summary(KG_data$pol_news_odnoklassniki_n) # Provide a summary for 'pol_news_odnoklassniki_n' (Numeric)

# Perform the recode for 'trust_state'
KG_data$trust_state = recode(KG_data$q25_a, '1="Very trustworthy"; 2="Somewhat trustworthy"; 3="Somewhat untrustworthy"; 4="Very untrustworthy"')
KG_data$trust_state = factor(KG_data$trust_state, levels = c('Very trustworthy', 'Somewhat trustworthy', 'Somewhat untrustworthy', 'Very untrustworthy'), ordered = TRUE)
table(KG_data$trust_state) # Display a frequency table for 'trust_state'

# Create a numeric variable for 'trust_state'
KG_data$trust_state_n = as.numeric(KG_data$q25_a) 
summary(KG_data$trust_state_n) # Provide a summary for 'trust_state_n' (Numeric)

# Perform the recode for 'trust_russian_media'
KG_data$trust_russian_media = recode(KG_data$q25_b, '1="Very trustworthy"; 2="Somewhat trustworthy"; 3="Somewhat untrustworthy"; 4="Very untrustworthy"')
KG_data$trust_russian_media = factor(KG_data$trust_russian_media, levels = c('Very trustworthy', 'Somewhat trustworthy', 'Somewhat untrustworthy', 'Very untrustworthy'), ordered = TRUE)
table(KG_data$trust_russian_media) # Display a frequency table for 'trust_russian_media'

# Create a numeric variable for 'trust_russian_media'
KG_data$trust_russian_media_n = as.numeric(KG_data$q25_b) 
summary(KG_data$trust_russian_media_n) # Provide a summary for 'trust_russian_media_n' (Numeric)

# Perform the recode for 'trust_internet'
KG_data$trust_internet = recode(KG_data$q25_c, '1="Very trustworthy"; 2="Somewhat trustworthy"; 3="Somewhat untrustworthy"; 4="Very untrustworthy"')
KG_data$trust_internet = factor(KG_data$trust_internet, levels = c('Very trustworthy', 'Somewhat trustworthy', 'Somewhat untrustworthy', 'Very untrustworthy'), ordered = TRUE)
table(KG_data$trust_internet) # Display a frequency table for 'trust_internet'

# Create a numeric variable for 'trust_internet'
KG_data$trust_internet_n = as.numeric(KG_data$q25_c) 
summary(KG_data$trust_internet_n) # Provide a summary for 'trust_internet_n' (Numeric)

# Perform the recode for 'trust_facebook'
KG_data$trust_facebook = recode(KG_data$q25_d, '1="Very trustworthy"; 2="Somewhat trustworthy"; 3="Somewhat untrustworthy"; 4="Very untrustworthy"')
KG_data$trust_facebook = factor(KG_data$trust_facebook, levels = c('Very trustworthy', 'Somewhat trustworthy', 'Somewhat untrustworthy', 'Very untrustworthy'), ordered = TRUE)
table(KG_data$trust_facebook) # Display a frequency table for 'trust_facebook'

# Create a numeric variable for 'trust_parties'
KG_data$trust_facebook_n = as.numeric(KG_data$q25_d) 
summary(KG_data$trust_facebook_n) # Provide a summary for 'trust_facebook_n' (Numeric)

# Perform the recode for 'trust_vkontakte'
KG_data$trust_vkontakte = recode(KG_data$q25_e, '1="Very trustworthy"; 2="Somewhat trustworthy"; 3="Somewhat untrustworthy"; 4="Very untrustworthy"')
KG_data$trust_vkontakte = factor(KG_data$trust_vkontakte, levels = c('Very trustworthy', 'Somewhat trustworthy', 'Somewhat untrustworthy', 'Very untrustworthy'), ordered = TRUE)
table(KG_data$trust_vkontakte) # Display a frequency table for 'trust_vkontakte'

# Create a numeric variable for 'trust_vkontakte'
KG_data$trust_vkontakte_n = as.numeric(KG_data$q25_e) 
summary(KG_data$trust_vkontakte_n) # Provide a summary for 'trust_vkontakte_n' (Numeric)

# Perform the recode for 'trust_western'
KG_data$trust_western = recode(KG_data$q25_f, '1="Very trustworthy"; 2="Somewhat trustworthy"; 3="Somewhat untrustworthy"; 4="Very untrustworthy"')
KG_data$trust_western = factor(KG_data$trust_western, levels = c('Very trustworthy', 'Somewhat trustworthy', 'Somewhat untrustworthy', 'Very untrustworthy'), ordered = TRUE)
table(KG_data$trust_western) # Display a frequency table for 'trust_western'

# Create a numeric variable for 'trust_western'
KG_data$trust_western_n = as.numeric(KG_data$q25_f) 
summary(KG_data$trust_western_n) # Provide a summary for 'trust_western_n' (Numeric)

#News Balance Factor
# Perform the recode for 'news_balance'
KG_data$news_balance = recode(KG_data$q26, '1="Only from traditional sources like television newspapers radio";2="Mostly from television newspapers radio but some from the internet social media";3="From an equal balance of television newspapers radio and the internet social media";4="Mostly from social media but some from television newspapers radio";5="Only from the internet social media"')
KG_data$news_balance = factor(KG_data$news_balance, levels = c('Only from traditional sources like television newspapers radio', 'Mostly from television newspapers radio but some from the internet social media', 'From an equal balance of television newspapers radio and the internet social media', 'Mostly from social media but some from television newspapers radio', 'Only from the internet social media'), ordered = TRUE)
KG_data$news_balance = addNA(KG_data$news_balance)
table(KG_data$news_balance) # Display a frequency table for 'news_balance'

# Create a numeric variable for 'news_balance'
KG_data$news_balance_n = as.numeric(KG_data$q26)
summary(KG_data$news_balance_n) # Provide a summary for 'news_balance_n' (Numeric)

# Tone of Social Media News about Government Factor
# Perform the recode for 'q27_a' (Tone of Social Media News about Government)
KG_data$sm_critical_local = recode(KG_data$q27_a, '1="Very often";2="Rather often";3="Occasionally";4="Never"')
KG_data$sm_critical_local = factor(KG_data$sm_critical_local, levels = c('Very often', 'Rather often', 'Occasionally', 'Never'), ordered = TRUE)
table(KG_data$sm_critical_local) # Display a frequency table for 'sm_critical_local'

# Create a numeric variable for 'sm_critical_local'
KG_data$sm_critical_local_n = as.numeric(KG_data$q27_a)
summary(KG_data$sm_critical_local_n) # Provide a summary for 'sm_critical_local_n' (Numeric)

# Perform the recode for 'q27_b' (Tone of Social Media News about Government)
KG_data$sm_critical_central = recode(KG_data$q27_b, '1="Very often";2="Rather often";3="Occasionally";4="Never"')
KG_data$sm_critical_central = factor(KG_data$sm_critical_central, levels = c('Very often', 'Rather often', 'Occasionally', 'Never'), ordered = TRUE)
table(KG_data$sm_critical_central) # Display a frequency table for 'sm_critical_central'

# Create a numeric variable for 'sm_critical_central'
KG_data$sm_critical_central_n = as.numeric(KG_data$q27_b)
summary(KG_data$sm_critical_central_n) # Provide a summary for 'sm_critical_central_n' (Numeric)

# Perform the recode for 'q27_c' (Tone of Social Media News about Government)
KG_data$sm_positive_local = recode(KG_data$q27_c, '1="Very often";2="Rather often";3="Occasionally";4="Never"')
KG_data$sm_positive_local = factor(KG_data$sm_positive_local, levels = c('Very often', 'Rather often', 'Occasionally', 'Never'), ordered = TRUE)
table(KG_data$sm_positive_local) # Display a frequency table for 'sm_positive_local'

# Create a numeric variable for 'sm_positive_local'
KG_data$sm_positive_local_n = as.numeric(KG_data$q27_c)
summary(KG_data$sm_positive_local_n) # Provide a summary for 'sm_positive_local_n' (Numeric)

# Perform the recode for 'q27_d' (Tone of Social Media News about Government)
KG_data$sm_positive_central = recode(KG_data$q27_d, '1="Very often";2="Rather often";3="Occasionally";4="Never"')
KG_data$sm_positive_central = factor(KG_data$sm_positive_central, levels = c('Very often', 'Rather often', 'Occasionally', 'Never'), ordered = TRUE)
table(KG_data$sm_positive_central) # Display a frequency table for 'sm_positive_central'

# Create a numeric variable for 'sm_positive_central'
KG_data$sm_positive_central_n = as.numeric(KG_data$q27_d)
summary(KG_data$sm_positive_central_n) # Provide a summary for 'sm_positive_central_n' (Numeric)

# TV Tone of News about Government Factor
# Perform the recode for 'q28_a' (TV Tone of News about Government)
KG_data$tv_critical_local = recode(KG_data$q28_a, '1="Very often";2="Rather often";3="Occasionally";4="Never"')
KG_data$tv_critical_local = factor(KG_data$tv_critical_local, levels = c('Very often', 'Rather often', 'Occasionally', 'Never'), ordered = TRUE)
table(KG_data$tv_critical_local) # Display a frequency table for 'tv_critical_local'

# Create a numeric variable for 'tv_critical_local'
KG_data$tv_critical_local_n = as.numeric(KG_data$q28_a)
summary(KG_data$tv_critical_local_n) # Provide a summary for 'tv_critical_local_n' (Numeric)

# Perform the recode for 'q28_b' (TV Tone of News about Government)
KG_data$tv_critical_central = recode(KG_data$q28_b, '1="Very often";2="Rather often";3="Occasionally";4="Never"')
KG_data$tv_critical_central = factor(KG_data$tv_critical_central, levels = c('Very often', 'Rather often', 'Occasionally', 'Never'), ordered = TRUE)
table(KG_data$tv_critical_central) # Display a frequency table for 'tv_critical_central'

# Create a numeric variable for 'tv_critical_central'
KG_data$tv_critical_central_n = as.numeric(KG_data$q28_b)
summary(KG_data$tv_critical_central_n) # Provide a summary for 'tv_critical_central_n' (Numeric)

# Clickable Links Factor
# Perform the recode for 'q29_a' (Clickable Links)
KG_data$clickable_state = recode(KG_data$q29_a, '1="Almost everyday";2="About once a week";3="A few times a month";4="About once a month";5="Never"')
KG_data$clickable_state = factor(KG_data$clickable_state, levels = c('Almost everyday', 'About once a week', 'A few times a month', 'About once a month', 'Never'), ordered = TRUE)
table(KG_data$clickable_state) # Display a frequency table for 'clickable_state'

# Create a numeric variable for 'clickable_state'
KG_data$clickable_state_n = as.numeric(KG_data$q29_a)
summary(KG_data$clickable_state_n) # Provide a summary for 'clickable_state_n' (Numeric)

# Perform the recode for 'q29_b' (Clickable Links)
KG_data$clickable_russian = recode(KG_data$q29_b, '1="Almost everyday";2="About once a week";3="A few times a month";4="About once a month";5="Never"')
KG_data$clickable_russian = factor(KG_data$clickable_russian, levels = c('Almost everyday', 'About once a week', 'A few times a month', 'About once a month', 'Never'), ordered = TRUE)
table(KG_data$clickable_russian) # Display a frequency table for 'clickable_russian'

# Create a numeric variable for 'clickable_russian'
KG_data$clickable_russian_n = as.numeric(KG_data$q29_b)
summary(KG_data$clickable_russian_n) # Provide a summary for 'clickable_russian_n' (Numeric)

# Artificial Intelligence Attitude Factor
# Perform the recode for 'AI' (Artificial Intelligence Attitude)
KG_data$AI = recode(KG_data$q30, '1="More excited than concerned";2="Equally concerned and excited";3="More concerned than excited";4="I do not know what artificial intelligence is"')
KG_data$AI = factor(KG_data$AI, levels = c('More excited than concerned', 'Equally concerned and excited', 'More concerned than excited', 'I do not know what artificial intelligence is'), ordered = TRUE)
table(KG_data$AI) # Display a frequency table for 'AI'

# Artificial Intelligence Attitude Ordinal (coding I don't know as NA)
# Perform the recode
KG_data$AI_ordinal = recode(KG_data$q30, '1="More excited than concerned";2="Equally concerned and excited";3="More concerned than excited";4=NA')
KG_data$AI_ordinal = factor(KG_data$AI_ordinal, levels = c('More excited than concerned', 'Equally concerned and excited', 'More concerned than excited', 'I do not know what artificial intelligence is'), ordered = TRUE)
table(KG_data$AI_ordinal) # Display a frequency table for 'AI_ordinal'

# Convert 'AI_ordinal' to an ordered factor/ordinal
KG_data$AI_ordinal = factor(KG_data$AI_ordinal, ordered = TRUE)

# Artificial Intelligence Attitude Numeric
# Perform the recode
KG_data$AI_n = recode(KG_data$q30, '1=1;2=2;3=3;4=NA')
summary(KG_data$AI_n) # Provide a summary for 'AI_n' (Numeric)

#Echo Chamber Numeric
KG_data$echo_chamber_n = as.numeric(KG_data$q31)
summary(KG_data$echo_chamber_n) # Provide a summary for 'echo_chamber_n' (Numeric)

# Awareness of Government Posters Factor
# Perform the recode for 'paid_posters'
KG_data$paid_posters = recode(KG_data$q32, '1="I never heard of them";2="I have heard of them but I have not seen any posts from them";3="I think I have seen posts from them"')
KG_data$paid_posters = factor(KG_data$paid_posters, levels = c('I never heard of them', 'I have heard of them but I have not seen any posts from them', 'I think I have seen posts from them'), ordered = TRUE)
KG_data$paid_posters = addNA(KG_data$paid_posters)
table(KG_data$paid_posters) # Display a frequency table for 'paid_posters'

# Create a numeric variable for 'paid_posters'
KG_data$paid_posters_n = as.numeric(KG_data$q32)
summary(KG_data$paid_posters_n) # Provide a summary for 'paid_posters_n' (Numeric)

# Awareness of Government Posters Ordinal (coding "I don't know" as NA)
# Perform the recode
KG_data$paid_posters_ordinal = recode(KG_data$q32, '1="I never heard of them";2="I have heard of them but I have not seen any posts from them";3="I think I have seen posts from them";4=NA')
KG_data$paid_posters_ordinal = factor(KG_data$paid_posters_ordinal, levels = c('I never heard of them', 'I have heard of them but I have not seen any posts from them', 'I think I have seen posts from them'), ordered = TRUE)
KG_data$paid_posters_ordinal = addNA(KG_data$paid_posters_ordinal)
table(KG_data$paid_posters_ordinal) # Display a frequency table for 'paid_posters_ordinal'

# Convert 'paid_posters_ordinal' to an ordered factor/ordinal
KG_data$paid_posters_ordinal = factor(KG_data$paid_posters_ordinal, ordered = TRUE)

# Awareness of Government Posters Numeric
# Perform the recode
KG_data$paid_posters_n = recode(KG_data$q32, '1=1;2=2;3=3;4=NA')
summary(KG_data$paid_posters_n) # Provide a summary for 'paid_posters_n' (Numeric)

#Estimate of Percentage of Online Posts Paid by Government
KG_data$paid_posters_percentage_n = as.numeric(KG_data$q33)
summary(KG_data$paid_posters_percentage_n) # Provide a summary for paid_posters_percentage_n (Numeric)

# VPN Use Factor
# Perform the recode for 'vpn_use'
KG_data$vpn_use = recode(KG_data$q34, '1="Yes";2="No"')
KG_data$vpn_use = factor(KG_data$vpn_use, levels = c('Yes', 'No'), ordered = TRUE)
KG_data$vpn_use = addNA(KG_data$vpn_use)
table(KG_data$vpn_use) # Display a frequency table for 'vpn_use'

#Selective Exposure
# Perform the recode for 'avoidance_blocking'
KG_data$avoidance_blocking = recode(KG_data$q35_a, '1="Often"; 2="Sometimes"; 3="Very rarely"; 4="Never"')
KG_data$avoidance_blocking = factor(KG_data$avoidance_blocking, levels = c('Often', 'Sometimes', 'Very rarely', 'Never'), ordered = TRUE)
table(KG_data$avoidance_blocking) # Display a frequency table for 'avoidance_blocking'

# Create a numeric variable for 'avoidance_blocking'
KG_data$avoidance_blocking_n = as.numeric(KG_data$q35_a) 
summary(KG_data$avoidance_blocking_n) # Provide a summary for 'avoidance_blocking_numeric' (Numeric)

# Perform the recode for 'avoidance_unfriending'
KG_data$avoidance_unfriending = recode(KG_data$q35_b, '1="Often"; 2="Sometimes"; 3="Very rarely"; 4="Never"')
KG_data$avoidance_unfriending = factor(KG_data$avoidance_unfriending, levels = c('Often', 'Sometimes', 'Very rarely', 'Never'), ordered = TRUE)
table(KG_data$avoidance_unfriending) # Display a frequency table for 'avoidance_unfriending'

# Create a numeric variable for 'avoidance_unfriending'
KG_data$avoidance_unfriending_n = as.numeric(KG_data$q35_b) 
summary(KG_data$avoidance_unfriending_n) # Provide a summary for 'avoidance_unfriending_numeric' (Numeric)

# Perform the recode for 'avoidance_leaving_group'
KG_data$avoidance_leaving_group = recode(KG_data$q35_c, '1="Often"; 2="Sometimes"; 3="Very rarely"; 4="Never"')
KG_data$avoidance_leaving_group = factor(KG_data$avoidance_leaving_group, levels = c('Often', 'Sometimes', 'Very rarely', 'Never'), ordered = TRUE)
table(KG_data$avoidance_leaving_group) # Display a frequency table for 'avoidance_leaving_group'

# Create a numeric variable for 'avoidance_leaving_group'
KG_data$avoidance_leaving_group_n = as.numeric(KG_data$q35_c) 
summary(KG_data$avoidance_leaving_group_n) # Provide a summary for 'avoidance_leaving_group_numeric' (Numeric)

# Perform the recode for 'avoidance_unsubscribing'
KG_data$avoidance_unsubscribing = recode(KG_data$q35_d, '1="Often"; 2="Sometimes"; 3="Very rarely"; 4="Never"')
KG_data$avoidance_unsubscribing = factor(KG_data$avoidance_unsubscribing, levels = c('Often', 'Sometimes', 'Very rarely', 'Never'), ordered = TRUE)
table(KG_data$avoidance_unsubscribing) # Display a frequency table for 'avoidance_unsubscribing'

# Create a numeric variable for 'avoidance_unsubscribing'
KG_data$avoidance_unsubscribing_n = as.numeric(KG_data$q35_d) 
summary(KG_data$avoidance_unsubscribing_n) # Provide a summary for 'avoidance_unsubscribing_numeric' (Numeric)

#Exposure to SM Disagreement
# Perform the recode for 'sm_disagreement_politics'
KG_data$sm_disagreement_politics = recode(KG_data$q36_a, '1="Often"; 2="Sometimes"; 3="Very rarely"; 4="Never"')
KG_data$sm_disagreement_politics = factor(KG_data$sm_disagreement_politics, levels = c('Often', 'Sometimes', 'Very rarely', 'Never'), ordered = TRUE)
table(KG_data$sm_disagreement_politics) # Display a frequency table for 'sm_disagreement_politics'

# Create a numeric variable for 'sm_disagreement_politics'
KG_data$sm_disagreement_politics_n = as.numeric(KG_data$q36_a) 
summary(KG_data$sm_disagreement_politics_n) # Provide a summary for 'sm_disagreement_politics_numeric' (Numeric)

# Perform the recode for 'sm_disagreement_news'
KG_data$sm_disagreement_news = recode(KG_data$q36_b, '1="Often"; 2="Sometimes"; 3="Very rarely"; 4="Never"')
KG_data$sm_disagreement_news = factor(KG_data$sm_disagreement_news, levels = c('Often', 'Sometimes', 'Very rarely', 'Never'), ordered = TRUE)
table(KG_data$sm_disagreement_news) # Display a frequency table for 'sm_disagreement_news'

# Create a numeric variable for 'sm_disagreement_news'
KG_data$sm_disagreement_news_n = as.numeric(KG_data$q36_b) 
summary(KG_data$sm_disagreement_news_n) # Provide a summary for 'sm_disagreement_news_numeric' (Numeric)

# Perform the recode for 'sm_disagreement_issues'
KG_data$sm_disagreement_issues = recode(KG_data$q36_c, '1="Often"; 2="Sometimes"; 3="Very rarely"; 4="Never"')
KG_data$sm_disagreement_issues = factor(KG_data$sm_disagreement_issues, levels = c('Often', 'Sometimes', 'Very rarely', 'Never'), ordered = TRUE)
table(KG_data$sm_disagreement_issues) # Display a frequency table for 'sm_disagreement_issues'

# Create a numeric variable for 'sm_disagreement_issues'
KG_data$sm_disagreement_issues_n = as.numeric(KG_data$q36_c) 
summary(KG_data$sm_disagreement_issues_n) # Provide a summary for 'sm_disagreement_issues_numeric' (Numeric)

#Network Breadth
# Perform the recode for 'network_breadth'
KG_data$network_breadth = recode(KG_data$q37, '1="1-2"; 2="3-5"; 3="6-8"; 4="8-12"; 5="More than 12"')
KG_data$network_breadth = factor(KG_data$network_breadth, levels = c('1-2', '3-5', '6-8', '8-12', 'More than 12'), ordered = TRUE)
table(KG_data$network_breadth) # Display a frequency table for 'network_breadth'

# Create a numeric variable for 'network_breadth'
KG_data$network_breadth_n = as.numeric(KG_data$q37) 
summary(KG_data$network_breadth_n) # Provide a summary for 'network_breadth_numeric' (Numeric)

#Social Media Political Activity
# Perform the recode for 'sm_engage_friends'
KG_data$sm_engage_friends = recode(KG_data$q38_a, '1="A few times a day"; 2="Once a day"; 3="Several times a week"; 4="Once a week"; 5="Less often"; 6="Never"')
KG_data$sm_engage_friends = factor(KG_data$sm_engage_friends, levels = c('A few times a day', 'Once a day', 'Several times a week', 'Once a week', 'Less often', 'Never'), ordered = TRUE)
table(KG_data$sm_engage_friends) # Display a frequency table for 'sm_engage_friends'

# Create a numeric variable for 'sm_engage_friends'
KG_data$sm_engage_friends_n = as.numeric(KG_data$q38_a) 
summary(KG_data$sm_engage_friends_n) # Provide a summary for 'sm_engage_friends_n' (Numeric)

# Perform the recode for 'sm_engage_groups'
KG_data$sm_engage_groups = recode(KG_data$q38_b, '1="A few times a day"; 2="Once a day"; 3="Several times a week"; 4="Once a week"; 5="Less often"; 6="Never"')
KG_data$sm_engage_groups = factor(KG_data$sm_engage_groups, levels = c('A few times a day', 'Once a day', 'Several times a week', 'Once a week', 'Less often', 'Never'), ordered = TRUE)
table(KG_data$sm_engage_groups) # Display a frequency table for 'sm_engage_groups'

# Create a numeric variable for 'sm_engage_groups'
KG_data$sm_engage_groups_n = as.numeric(KG_data$q38_b) 
summary(KG_data$sm_engage_groups_n) # Provide a summary for 'sm_engage_groups_n' (Numeric)
# Perform the recode for 'sm_engage_post'
KG_data$sm_engage_post = recode(KG_data$q38_c, '1="A few times a day"; 2="Once a day"; 3="Several times a week"; 4="Once a week"; 5="Less often"; 6="Never"')
KG_data$sm_engage_post = factor(KG_data$sm_engage_post, levels = c('A few times a day', 'Once a day', 'Several times a week', 'Once a week', 'Less often', 'Never'), ordered = TRUE)
table(KG_data$sm_engage_post) # Display a frequency table for 'sm_engage_post'

# Create a numeric variable for 'sm_engage_post'
KG_data$sm_engage_post_n = as.numeric(KG_data$q38_c) 
summary(KG_data$sm_engage_post_n) # Provide a summary for 'sm_engage_post_n' (Numeric)

# Perform the recode for 'sm_engage_critical'
KG_data$sm_engage_critical = recode(KG_data$q38_d, '1="A few times a day"; 2="Once a day"; 3="Several times a week"; 4="Once a week"; 5="Less often"; 6="Never"')
KG_data$sm_engage_critical = factor(KG_data$sm_engage_critical, levels = c('A few times a day', 'Once a day', 'Several times a week', 'Once a week', 'Less often', 'Never'), ordered = TRUE)
table(KG_data$sm_engage_critical) # Display a frequency table for 'sm_engage_critical'

# Create a numeric variable for 'sm_engage_critical'
KG_data$sm_engage_critical_n = as.numeric(KG_data$q38_d) 
summary(KG_data$sm_engage_critical_n) # Provide a summary for 'sm_engage_critical_n' (Numeric)

# Perform the recode for 'sm_engage_supportive'
KG_data$sm_engage_supportive = recode(KG_data$q38_e, '1="A few times a day"; 2="Once a day"; 3="Several times a week"; 4="Once a week"; 5="Less often"; 6="Never"')
KG_data$sm_engage_supportive = factor(KG_data$sm_engage_supportive, levels = c('A few times a day', 'Once a day', 'Several times a week', 'Once a week', 'Less often', 'Never'), ordered = TRUE)
table(KG_data$sm_engage_supportive) # Display a frequency table for 'sm_engage_supportive'

# Create a numeric variable for 'sm_engage_supportive'
KG_data$sm_engage_supportive_n = as.numeric(KG_data$q38_e) 
summary(KG_data$sm_engage_supportive_n) # Provide a summary for 'sm_engage_supportive_n' (Numeric)

# Perform the recode for 'sm_engage_offline'
KG_data$sm_engage_offline = recode(KG_data$q38_f, '1="A few times a day"; 2="Once a day"; 3="Several times a week"; 4="Once a week"; 5="Less often"; 6="Never"')
KG_data$sm_engage_offline = factor(KG_data$sm_engage_offline, levels = c('A few times a day', 'Once a day', 'Several times a week', 'Once a week', 'Less often', 'Never'), ordered = TRUE)
table(KG_data$sm_engage_offline) # Display a frequency table for 'sm_engage_offline'

# Create a numeric variable for 'sm_engage_offline'
KG_data$sm_engage_offline_n = as.numeric(KG_data$q38_f) 
summary(KG_data$sm_engage_offline_n) # Provide a summary for 'sm_engage_offline_n' (Numeric)

#Attiudes about Global Power and Ukraine
# Perform the recode for 'ukraine_china'
KG_data$ukraine_china = recode(KG_data$q39_a, '1="Significantly more favorable"; 2="Somewhat more favorable"; 3="Unchanged"; 4="Somewhat more unfavorable"; 5="Significantly more unfavorable"')
KG_data$ukraine_china = factor(KG_data$ukraine_china, levels = c('Significantly more favorable', 'Somewhat more favorable', 'Unchanged', 'Somewhat more unfavorable', 'Significantly more unfavorable'), ordered = TRUE)
table(KG_data$ukraine_china) # Display a frequency table for 'ukraine_china'

# Create a numeric variable for 'ukraine_china'
KG_data$ukraine_china_n = as.numeric(KG_data$q39_a) 
summary(KG_data$ukraine_china_n) # Provide a summary for 'ukraine_china_n' (Numeric)

# Perform the recode for 'ukraine_russia'
KG_data$ukraine_russia = recode(KG_data$q39_b, '1="Significantly more favorable"; 2="Somewhat more favorable"; 3="Unchanged"; 4="Somewhat more unfavorable"; 5="Significantly more unfavorable"')
KG_data$ukraine_russia = factor(KG_data$ukraine_russia, levels = c('Significantly more favorable', 'Somewhat more favorable', 'Unchanged', 'Somewhat more unfavorable', 'Significantly more unfavorable'), ordered = TRUE)
table(KG_data$ukraine_russia) # Display a frequency table for 'ukraine_russia'

# Create a numeric variable for 'ukraine_russia'
KG_data$ukraine_russia_n = as.numeric(KG_data$q39_b) 
summary(KG_data$ukraine_russia_n) # Provide a summary for 'ukraine_russia_n' (Numeric)

# Perform the recode for 'ukraine_US'
KG_data$ukraine_US = recode(KG_data$q39_c, '1="Significantly more favorable"; 2="Somewhat more favorable"; 3="Unchanged"; 4="Somewhat more unfavorable"; 5="Significantly more unfavorable"')
KG_data$ukraine_US = factor(KG_data$ukraine_US, levels = c('Significantly more favorable', 'Somewhat more favorable', 'Unchanged', 'Somewhat more unfavorable', 'Significantly more unfavorable'), ordered = TRUE)
table(KG_data$ukraine_US) # Display a frequency table for 'ukraine_US'

# Create a numeric variable for 'ukraine_US'
KG_data$ukraine_US_n = as.numeric(KG_data$q39_c) 
summary(KG_data$ukraine_US_n) # Provide a summary for 'ukraine_US_n' (Numeric)

##Attention to Ukraine and Who's Responsible
# Perform the recode for 'ukraine_attention'
KG_data$ukraine_attention = recode(KG_data$q40_b, '1="Strongly agree"; 2="Somewhat agree"; 3="Neither disagree nor agree"; 4="Somewhat disagree"; 5="Strongly disagree"')
KG_data$ukraine_attention = factor(KG_data$ukraine_attention, levels = c('Strongly agree', 'Somewhat agree', 'Neither disagree nor agree', 'Somewhat disagree', 'Strongly disagree'), ordered = TRUE)
table(KG_data$ukraine_attention) # Display a frequency table for 'ukraine_attention'

# Create a numeric variable for 'ukraine_attention'
KG_data$ukraine_attention_n = as.numeric(KG_data$q40_b) 
summary(KG_data$ukraine_attention_n) # Provide a summary for 'ukraine_attention_n' (Numeric)

# Perform the recode for 'ukraine_russia_responsible'
KG_data$ukraine_russia_responsible = recode(KG_data$q40_c, '1="Strongly agree"; 2="Somewhat agree"; 3="Neither disagree nor agree"; 4="Somewhat disagree"; 5="Strongly disagree"')
KG_data$ukraine_russia_responsible = factor(KG_data$ukraine_russia_responsible, levels = c('Strongly agree', 'Somewhat agree', 'Neither disagree nor agree', 'Somewhat disagree', 'Strongly disagree'), ordered = TRUE)
table(KG_data$ukraine_russia_responsible) # Display a frequency table for 'ukraine_russia_responsible'

# Create a numeric variable for 'ukraine_russia_responsible'
KG_data$ukraine_russia_responsible_n = as.numeric(KG_data$q40_c) 
summary(KG_data$ukraine_russia_responsible_n) # Provide a summary for 'ukraine_russia_responsible_n' (Numeric)

# Perform the recode for 'ukraine_ukraine_responsible'
KG_data$ukraine_ukraine_responsible = recode(KG_data$q40_d, '1="Strongly agree"; 2="Somewhat agree"; 3="Neither disagree nor agree"; 4="Somewhat disagree"; 5="Strongly disagree"')
KG_data$ukraine_ukraine_responsible = factor(KG_data$ukraine_ukraine_responsible, levels = c('Strongly agree', 'Somewhat agree', 'Neither disagree nor agree', 'Somewhat disagree', 'Strongly disagree'), ordered = TRUE)
table(KG_data$ukraine_ukraine_responsible) # Display a frequency table for 'ukraine_ukraine_responsible'

# Create a numeric variable for 'ukraine_ukraine_responsible'
KG_data$ukraine_ukraine_responsible_n = as.numeric(KG_data$q40_d) 
summary(KG_data$ukraine_ukraine_responsible_n) # Provide a summary for 'ukraine_ukraine_responsible_n' (Numeric)

# Perform the recode for 'ukraine_US_responsible'
KG_data$ukraine_US_responsible = recode(KG_data$q40_e, '1="Strongly agree"; 2="Somewhat agree"; 3="Neither disagree nor agree"; 4="Somewhat disagree"; 5="Strongly disagree"')
KG_data$ukraine_US_responsible = factor(KG_data$ukraine_US_responsible, levels = c('Strongly agree', 'Somewhat agree', 'Neither disagree nor agree', 'Somewhat disagree', 'Strongly disagree'), ordered = TRUE)
table(KG_data$ukraine_US_responsible) # Display a frequency table for 'ukraine_US_responsible'

# Create a numeric variable for 'ukraine_US_responsible'
KG_data$ukraine_US_responsible_n = as.numeric(KG_data$q40_e) 
summary(KG_data$ukraine_US_responsible_n) # Provide a summary for 'ukraine_US_responsible_n' (Numeric)

#Creating Dummy Variables from all Relevant KG Variables############################################################################################
library(car)

# internet_use
KG_data$internet_use_No = recode(KG_data$internet_use, '"No"=1; NA=NA; else=0')
KG_data$internet_use_Yes = recode(KG_data$internet_use, '"Yes"=1; NA=NA; else=0')

# ethnicity
KG_data$ethnicity_Kazakh = recode(KG_data$ethnicity, '"Kazakh"=1; NA=NA; else=0')
KG_data$ethnicity_Russian = recode(KG_data$ethnicity, '"Russian"=1; NA=NA; else=0')
KG_data$ethnicity_Uzbek = recode(KG_data$ethnicity, '"Uzbek"=1; NA=NA; else=0')
KG_data$ethnicity_Tajik = recode(KG_data$ethnicity, '"Tajik"=1; NA=NA; else=0')
KG_data$ethnicity_Kyrgyz = recode(KG_data$ethnicity, '"Kyrgyz"=1; NA=NA; else=0')
KG_data$ethnicity_Other = recode(KG_data$ethnicity, '"Other"=1; NA=NA; else=0')

# urbanicity
KG_data$urbanicity_City = recode(KG_data$urbanicity, '"City"=1; NA=NA; else=0')
KG_data$urbanicity_Village = recode(KG_data$urbanicity, '"Village"=1; NA=NA; else=0')

# gender
KG_data$gender_Female = recode(KG_data$gender, '"Female"=1; NA=NA; else=0')
KG_data$gender_Male = recode(KG_data$gender, '"Male"=1; NA=NA; else=0')

# important_issue_first
KG_data$important_issue_first_Political_instability = recode(KG_data$important_issue_first, "'Political instability'=1; NA=NA; else=0")
KG_data$important_issue_first_Unemployment = recode(KG_data$important_issue_first, "'Unemployment'=1; NA=NA; else=0")
KG_data$important_issue_first_Inflation_Prices = recode(KG_data$important_issue_first, "'Inflation Prices'=1; NA=NA; else=0")
KG_data$important_issue_first_Wages_pensions = recode(KG_data$important_issue_first, "'Wages pensions'=1; NA=NA; else=0")
KG_data$important_issue_first_Taxes = recode(KG_data$important_issue_first, "'Taxes'=1; NA=NA; else=0")
KG_data$important_issue_first_Access_to_basic_needs = recode(KG_data$important_issue_first, "'Access to basic needs'=1; NA=NA; else=0")
KG_data$important_issue_first_General_economic_situation = recode(KG_data$important_issue_first, "'General economic situation'=1; NA=NA; else=0")
KG_data$important_issue_first_Deterioration_of_the_quality_and_access_to_education = recode(KG_data$important_issue_first, "'Deterioration of the quality and access to education'=1; NA=NA; else=0")
KG_data$important_issue_first_Construction_of_houses_residential_premises = recode(KG_data$important_issue_first, "'Construction of houses residential premises'=1; NA=NA; else=0")
KG_data$important_issue_first_Terrorism = recode(KG_data$important_issue_first, "'Terrorism'=1; NA=NA; else=0")
KG_data$important_issue_first_War_conflict_in_other_countries = recode(KG_data$important_issue_first, "'War conflict in other countries'=1; NA=NA; else=0")
KG_data$important_issue_first_Increase_in_crime_violence_concerns_about_personal_safety = recode(KG_data$important_issue_first, "'Increase in crime violence concerns about personal safety'=1; NA=NA; else=0")
KG_data$important_issue_first_Corruption = recode(KG_data$important_issue_first, "'Corruption'=1; NA=NA; else=0")
KG_data$important_issue_first_Lack_of_opportunities = recode(KG_data$important_issue_first, "'Lack of opportunities'=1; NA=NA; else=0")
KG_data$important_issue_first_Discrimination_ethnic_or_religious_tensions = recode(KG_data$important_issue_first, "'Discrimination ethnic or religious tensions'=1; NA=NA; else=0")
KG_data$important_issue_first_Infrastructure = recode(KG_data$important_issue_first, "'Infrastructure'=1; NA=NA; else=0")
KG_data$important_issue_first_Deterioration_of_the_environment = recode(KG_data$important_issue_first, "'Deterioration of the environment'=1; NA=NA; else=0")
KG_data$important_issue_first_Unresolved_territorial_conflicts = recode(KG_data$important_issue_first, "'Unresolved territorial conflicts'=1; NA=NA; else=0")
KG_data$important_issue_first_Emigration = recode(KG_data$important_issue_first, "'Emigration'=1; NA=NA; else=0")

# important_issue_second
KG_data$important_issue_second_Political_instability = recode(KG_data$important_issue_second, "'Political instability'=1; NA=NA; else=0")
KG_data$important_issue_second_Unemployment = recode(KG_data$important_issue_second, "'Unemployment'=1; NA=NA; else=0")
KG_data$important_issue_second_Inflation_Prices = recode(KG_data$important_issue_second, "'Inflation Prices'=1; NA=NA; else=0")
KG_data$important_issue_second_Wages_pensions = recode(KG_data$important_issue_second, "'Wages pensions'=1; NA=NA; else=0")
KG_data$important_issue_second_Taxes = recode(KG_data$important_issue_second, "'Taxes'=1; NA=NA; else=0")
KG_data$important_issue_second_Access_to_basic_needs = recode(KG_data$important_issue_second, "'Access to basic needs'=1; NA=NA; else=0")
KG_data$important_issue_second_General_economic_situation = recode(KG_data$important_issue_second, "'General economic situation'=1; NA=NA; else=0")
KG_data$important_issue_second_Deterioration_of_the_quality_and_access_to_education = recode(KG_data$important_issue_second, "'Deterioration of the quality and access to education'=1; NA=NA; else=0")
KG_data$important_issue_second_Construction_of_houses_residential_premises = recode(KG_data$important_issue_second, "'Construction of houses residential premises'=1; NA=NA; else=0")
KG_data$important_issue_second_Terrorism = recode(KG_data$important_issue_second, "'Terrorism'=1; NA=NA; else=0")
KG_data$important_issue_second_War_conflict_in_other_countries = recode(KG_data$important_issue_second, "'War conflict in other countries'=1; NA=NA; else=0")
KG_data$important_issue_second_Increase_in_crime_violence_concerns_about_personal_safety = recode(KG_data$important_issue_second, "'Increase in crime violence concerns about personal safety'=1; NA=NA; else=0")
KG_data$important_issue_second_Corruption = recode(KG_data$important_issue_second, "'Corruption'=1; NA=NA; else=0")
KG_data$important_issue_second_Lack_of_opportunities = recode(KG_data$important_issue_second, "'Lack of opportunities'=1; NA=NA; else=0")
KG_data$important_issue_second_Discrimination_ethnic_or_religious_tensions = recode(KG_data$important_issue_second, "'Discrimination ethnic or religious tensions'=1; NA=NA; else=0")
KG_data$important_issue_second_Infrastructure = recode(KG_data$important_issue_second, "'Infrastructure'=1; NA=NA; else=0")
KG_data$important_issue_second_Deterioration_of_the_environment = recode(KG_data$important_issue_second, "'Deterioration of the environment'=1; NA=NA; else=0")
KG_data$important_issue_second_Unresolved_territorial_conflicts = recode(KG_data$important_issue_second, "'Unresolved territorial conflicts'=1; NA=NA; else=0")
KG_data$important_issue_second_Emigration = recode(KG_data$important_issue_second, "'Emigration'=1; NA=NA; else=0")

# sm_use
KG_data$sm_use_No = recode(KG_data$sm_use, '"No"=1; NA=NA; else=0')
KG_data$sm_use_Yes = recode(KG_data$sm_use, '"Yes"=1; NA=NA; else=0')

# vpn_use
KG_data$vpn_use_No = recode(KG_data$vpn_use, '"No"=1; NA=NA; else=0')
KG_data$vpn_use_Yes = recode(KG_data$vpn_use, '"Yes"=1; NA=NA; else=0')

# paid_posters
# Assuming 'I never heard of them', 'I have heard of them but I have not seen any posts from them', 'I think I have seen posts from them' as categories
KG_data$paid_posters_never_heard = recode(KG_data$paid_posters, '"I never heard of them"=1; NA=NA; else=0')
KG_data$paid_posters_heard_not_seen = recode(KG_data$paid_posters, '"I have heard of them but I have not seen any posts from them"=1; NA=NA; else=0')
KG_data$paid_posters_seen_posts = recode(KG_data$paid_posters, '"I think I have seen posts from them"=1; NA=NA; else=0')

# Frequency tables for internet use
table(KG_data$internet_use_No)
table(KG_data$internet_use_Yes)

# Frequency tables for ethnicity
table(KG_data$ethnicity_Kazakh)
table(KG_data$ethnicity_Russian)
table(KG_data$ethnicity_Uzbek)
table(KG_data$ethnicity_Tajik)
table(KG_data$ethnicity_Kyrgyz)
table(KG_data$ethnicity_Other)

# Frequency tables for urbanicity
table(KG_data$urbanicity_City)
table(KG_data$urbanicity_Village)

# Frequency tables for gender
table(KG_data$gender_Female)
table(KG_data$gender_Male)

# Frequency tables for the first important issue
table(KG_data$important_issue_first_Political_instability)
table(KG_data$important_issue_first_Unemployment)
table(KG_data$important_issue_first_Inflation_Prices)
table(KG_data$important_issue_first_Wages_pensions)
table(KG_data$important_issue_first_Taxes)
table(KG_data$important_issue_first_Access_to_basic_needs)
table(KG_data$important_issue_first_General_economic_situation)
table(KG_data$important_issue_first_Deterioration_of_the_quality_and_access_to_education)
table(KG_data$important_issue_first_Construction_of_houses_residential_premises)
table(KG_data$important_issue_first_Terrorism)
table(KG_data$important_issue_first_War_conflict_in_other_countries)
table(KG_data$important_issue_first_Increase_in_crime_violence_concerns_about_personal_safety)
table(KG_data$important_issue_first_Corruption)
table(KG_data$important_issue_first_Lack_of_opportunities)
table(KG_data$important_issue_first_Discrimination_ethnic_or_religious_tensions)
table(KG_data$important_issue_first_Infrastructure)
table(KG_data$important_issue_first_Deterioration_of_the_environment)
table(KG_data$important_issue_first_Unresolved_territorial_conflicts)
table(KG_data$important_issue_first_Emigration)

# Frequency tables for the first important issue
table(KG_data$important_issue_second_Political_instability)
table(KG_data$important_issue_second_Unemployment)
table(KG_data$important_issue_second_Inflation_Prices)
table(KG_data$important_issue_second_Wages_pensions)
table(KG_data$important_issue_second_Taxes)
table(KG_data$important_issue_second_Access_to_basic_needs)
table(KG_data$important_issue_second_General_economic_situation)
table(KG_data$important_issue_second_Deterioration_of_the_quality_and_access_to_education)
table(KG_data$important_issue_second_Construction_of_houses_residential_premises)
table(KG_data$important_issue_second_Terrorism)
table(KG_data$important_issue_second_War_conflict_in_other_countries)
table(KG_data$important_issue_second_Increase_in_crime_violence_concerns_about_personal_safety)
table(KG_data$important_issue_second_Corruption)
table(KG_data$important_issue_second_Lack_of_opportunities)
table(KG_data$important_issue_second_Discrimination_ethnic_or_religious_tensions)
table(KG_data$important_issue_second_Infrastructure)
table(KG_data$important_issue_second_Deterioration_of_the_environment)
table(KG_data$important_issue_second_Unresolved_territorial_conflicts)
table(KG_data$important_issue_second_Emigration)
# Frequency tables for social media use
table(KG_data$sm_use_No)
table(KG_data$sm_use_Yes)

# Frequency tables for VPN use
table(KG_data$vpn_use_No)
table(KG_data$vpn_use_Yes)

# Frequency tables for paid posters
table(KG_data$paid_posters_never_heard)
table(KG_data$paid_posters_heard_not_seen)
table(KG_data$paid_posters_seen_posts)

#Scaling
library(scales)
KG_data$age_n_sc = rescale(KG_data$age_n, to = c(0, 1))
KG_data$edu_n_sc = rescale(KG_data$edu_n, to = c(0, 1))
KG_data$inc_n_sc = rescale(KG_data$inc_n, to = c(0, 1))
KG_data$pol_interest_n_sc = rescale(KG_data$pol_interest_n, to = c(1, 0))
KG_data$trust_central_n_sc = rescale(KG_data$trust_central_n, to = c(1, 0))
KG_data$trust_local_n_sc = rescale(KG_data$trust_local_n, to = c(1, 0))
KG_data$trust_russia_n_sc = rescale(KG_data$trust_russia_n, to = c(1, 0))
KG_data$trust_US_n_sc = rescale(KG_data$trust_US_n, to = c(1, 0))
KG_data$trust_china_n_sc = rescale(KG_data$trust_china_n, to = c(1, 0))
KG_data$trust_EU_n_sc = rescale(KG_data$trust_EU_n, to = c(1, 0))
KG_data$system_capable_n_sc = rescale(KG_data$system_capable_n, to = c(1, 0))
KG_data$system_proud_n_sc = rescale(KG_data$system_proud_n, to = c(1, 0))
KG_data$system_deserves_n_sc = rescale(KG_data$system_deserves_n, to = c(1, 0))
KG_data$system_live_n_sc = rescale(KG_data$system_live_n, to = c(1, 0))
KG_data$system_hurdles_participate_n_sc = rescale(KG_data$system_hurdles_participate_n, to = c(1, 0))
KG_data$tracking_central_n_sc = rescale(KG_data$tracking_central_n, to = c(0, 1))
KG_data$tracking_local_n_sc = rescale(KG_data$tracking_local_n, to = c(0, 1))
KG_data$tracking_companies_n_sc = rescale(KG_data$tracking_companies_n, to = c(0, 1))
KG_data$democracy_elections_n_sc = rescale(KG_data$democracy_elections_n, to = c(1, 0))
KG_data$democracy_speech_n_sc = rescale(KG_data$democracy_speech_n, to = c(1, 0))
KG_data$democracy_oversight_n_sc = rescale(KG_data$democracy_oversight_n, to = c(1, 0))
KG_data$democracy_organize_n_sc = rescale(KG_data$democracy_organize_n, to = c(1, 0))
KG_data$democracy_press_n_sc = rescale(KG_data$democracy_press_n, to = c(1, 0))
KG_data$democracy_parties_n_sc = rescale(KG_data$democracy_parties_n, to = c(1, 0))
KG_data$democracy_protests_n_sc = rescale(KG_data$democracy_protests_n, to = c(1, 0))
KG_data$democracy_courts_n_sc = rescale(KG_data$democracy_courts_n, to = c(1, 0))
KG_data$facebook_n_sc = rescale(KG_data$facebook_n, to = c(0, 1))
KG_data$vkontakte_n_sc = rescale(KG_data$vkontakte_n, to = c(0, 1))
KG_data$instagram_n_sc = rescale(KG_data$instagram_n, to = c(0, 1))
KG_data$tiktok_n_sc = rescale(KG_data$tiktok_n, to = c(0, 1))
KG_data$twitter_n_sc = rescale(KG_data$twitter_n, to = c(0, 1))
KG_data$youtube_n_sc = rescale(KG_data$youtube_n, to = c(0, 1))
KG_data$whatsapp_n_sc = rescale(KG_data$whatsapp_n, to = c(0, 1))
KG_data$telegram_n_sc = rescale(KG_data$telegram_n, to = c(0, 1))
KG_data$pol_news_tv_n_sc = rescale(KG_data$pol_news_tv_n, to = c(1, 0))
KG_data$pol_news_facebook_n_sc = rescale(KG_data$pol_news_facebook_n, to = c(1, 0))
KG_data$pol_news_vkontakte_n_sc = rescale(KG_data$pol_news_vkontakte_n, to = c(1, 0))
KG_data$pol_news_tiktok_n_sc = rescale(KG_data$pol_news_tiktok_n, to = c(1, 0))
KG_data$pol_news_twitter_n_sc = rescale(KG_data$pol_news_twitter_n, to = c(1, 0))
KG_data$pol_news_odnoklassniki_n_sc = rescale(KG_data$pol_news_odnoklassniki_n, to = c(1, 0))
KG_data$trust_state_n_sc = rescale(KG_data$trust_state_n, to = c(1, 0))
KG_data$trust_russian_media_n_sc = rescale(KG_data$trust_russian_media_n, to = c(1, 0))
KG_data$trust_internet_n_sc = rescale(KG_data$trust_internet_n, to = c(1, 0))
KG_data$trust_facebook_n_sc = rescale(KG_data$trust_facebook_n, to = c(1, 0))
KG_data$trust_vkontakte_n_sc = rescale(KG_data$trust_vkontakte_n, to = c(1, 0))
KG_data$trust_western_n_sc = rescale(KG_data$trust_western_n, to = c(1, 0))
KG_data$news_balance_n_sc = rescale(KG_data$news_balance_n, to = c(0, 1))
KG_data$sm_critical_local_n_sc = rescale(KG_data$sm_critical_local_n, to = c(1, 0))
KG_data$sm_critical_central_n_sc = rescale(KG_data$sm_critical_central_n, to = c(1, 0))
KG_data$sm_positive_local_n_sc = rescale(KG_data$sm_positive_local_n, to = c(1, 0))
KG_data$sm_positive_central_n_sc = rescale(KG_data$sm_positive_central_n, to = c(1, 0))
KG_data$tv_critical_local_n_sc = rescale(KG_data$tv_critical_local_n, to = c(1, 0))
KG_data$tv_critical_central_n_sc = rescale(KG_data$tv_critical_central_n, to = c(1, 0))
KG_data$clickable_state_n_sc = rescale(KG_data$clickable_state_n, to = c(1, 0))
KG_data$clickable_russian_n_sc = rescale(KG_data$clickable_russian_n, to = c(1, 0))
KG_data$AI_n_sc = rescale(KG_data$AI_n, to = c(1, 0))
KG_data$echo_chamber_n_sc = rescale(KG_data$echo_chamber_n, to = c(0, 1))
KG_data$paid_posters_percentage_n_sc = rescale(KG_data$paid_posters_percentage_n, to = c(0, 1))
KG_data$avoidance_blocking_n_sc = rescale(KG_data$avoidance_blocking_n, to = c(1, 0))
KG_data$avoidance_unfriending_n_sc = rescale(KG_data$avoidance_unfriending_n, to = c(1, 0))
KG_data$avoidance_leaving_group_n_sc = rescale(KG_data$avoidance_leaving_group_n, to = c(1, 0))
KG_data$avoidance_unsubscribing_n_sc = rescale(KG_data$avoidance_unsubscribing_n, to = c(1, 0))
KG_data$sm_disagreement_politics_n_sc = rescale(KG_data$sm_disagreement_politics_n, to = c(0, 1))
KG_data$sm_disagreement_news_n_sc = rescale(KG_data$sm_disagreement_news_n, to = c(0, 1))
KG_data$sm_disagreement_issues_n_sc = rescale(KG_data$sm_disagreement_issues_n, to = c(0, 1))
KG_data$network_breadth_n_sc = rescale(KG_data$network_breadth_n, to = c(0, 1))
KG_data$sm_engage_friends_n_sc = rescale(KG_data$sm_engage_friends_n, to = c(1, 0))
KG_data$sm_engage_groups_n_sc = rescale(KG_data$sm_engage_groups_n, to = c(1, 0))
KG_data$sm_engage_post_n_sc = rescale(KG_data$sm_engage_post_n, to = c(1, 0))
KG_data$sm_engage_critical_n_sc = rescale(KG_data$sm_engage_critical_n, to = c(1, 0))
KG_data$sm_engage_supportive_sc = rescale(KG_data$sm_engage_supportive_n, to = c(1, 0))
KG_data$sm_engage_offline_sc = rescale(KG_data$sm_engage_offline_n, to = c(1, 0))
KG_data$ukraine_china_sc = rescale(KG_data$ukraine_china_n, to = c(1, 0))
KG_data$ukraine_russia_sc = rescale(KG_data$ukraine_russia_n, to = c(1, 0))
KG_data$ukraine_US_sc = rescale(KG_data$ukraine_US_n, to = c(1, 0))
KG_data$ukraine_attention_sc = rescale(KG_data$ukraine_attention_n, to = c(1, 0))
KG_data$ukraine_russia_responsible_sc = rescale(KG_data$ukraine_russia_responsible_n, to = c(1, 0))
KG_data$ukraine_ukraine_responsible_sc = rescale(KG_data$ukraine_ukraine_responsible_n, to = c(1, 0))
KG_data$ukraine_US_responsible_sc = rescale(KG_data$ukraine_US_responsible_n, to = c(1, 0))

#Georgia
#Recoding all Variables##########################################################################
##############################################################################################
#################################################################################################
library(dplyr)
library(car)

# Internet Use Factor
GA_data$internet_use = recode(GA_data$q2, "1 = 'Yes'; 2 = 'No'")
GA_data$internet_use = factor(GA_data$internet_use)
table(GA_data$internet_use)  # Frequency table for internet_use

# Age Numeric
GA_data$age_n = GA_data$age
summary(GA_data$age_n)  # Summary for age_n

# Age Collapsed Factor
GA_data$age_collapsed = recode(GA_data$q3, "1 = '18 - 24'; 2 = '25 - 34'; 3 = '35 - 44'; 4 = '45 - 54'; 5 = '55 - 64'; 6 = '65+'")
GA_data$age_collapsed = factor(GA_data$age_collapsed, ordered = TRUE)
table(GA_data$age_collapsed)  # Frequency table for age_collapsed

# Ethnicity Factor
table(GA_data$q4)
GA_data$ethnicity = recode(GA_data$q4, "1 = 'Georgian'; 2 = 'Armenian'; 3 = 'Azerbaijani'; 4 = 'Other'")
GA_data$ethnicity = factor(GA_data$ethnicity)
GA_data$ethnicity = addNA(GA_data$ethnicity)
table(GA_data$ethnicity)  # Frequency table for ethnicity

# Urbanicity Factor
GA_data$urbanicity = recode(GA_data$q5, "1 = 'City'; 2 = 'Village'")
GA_data$urbanicity = factor(GA_data$urbanicity)
GA_data$urbanicity = addNA(GA_data$urbanicity)
table(GA_data$urbanicity)  # Frequency table for urbanicity

# Gender Factor
GA_data$gender = recode(GA_data$q6, "1 = 'Female'; 2 = 'Male'")
GA_data$gender = factor(GA_data$gender)
GA_data$gender = addNA(GA_data$gender)
table(GA_data$gender)  # Frequency table for gender

# Education Factor
GA_data$edu = recode(GA_data$q7, "1 = 'No education'; 2 = 'Primary education'; 3 = 'Basic secondary education'; 4 = 'General secondary education'; 5 = 'Complete vocational education'; 6 = 'Incomplete higher education'; 7 = 'Complete higher education'")
GA_data$edu = factor(GA_data$edu, ordered = TRUE)
table(GA_data$edu)  # Frequency table for edu

# Education Numeric
GA_data$edu_n = GA_data$q7
summary(GA_data$edu_n)  # Summary for edu_n

# Income Factor
GA_data$inc = recode(GA_data$q8, "1 = 'Less than 70,001'; 2 = '70,001 - 150,000'; 3 = '150,001 – 200,000'; 4 = '200,001 – 250,000'; 5 = '250,001 – 300,000'; 6 = '300,001 – 350,000'; 7 = '350,001 – 400,000'; 8 = '400,001 – 450,000'; 9 = '450,001 – 500,000'; 10 = '500,001 – 550,000'; 11 = '550,001 – 600,000'; 12 = 'More than 600,000'")
GA_data$inc = factor(GA_data$inc, levels = c('Less than 70,001', '70,001 - 150,000', '150,001 – 200,000', '200,001 – 250,000', '250,001 – 300,000', '300,001 – 350,000', '350,001 – 400,000', '400,001 – 450,000', '450,001 – 500,000', '500,001 – 550,000', '550,001 – 600,000', 'More than 600,000'), ordered = TRUE)
table(GA_data$inc)  # Frequency table for Income Factor

# Income Numeric
GA_data$inc_n = GA_data$q8
summary(GA_data$inc_n)  # Summary for Income Numeric

# Political Interest Factor
GA_data$pol_interest = recode(GA_data$q9, "1 = 'Very interested'; 2 = 'Somewhat interested'; 3 = 'Somewhat uninterested'; 4 = 'Very uninterested'")
GA_data$pol_interest = factor(GA_data$pol_interest, levels = c('Very interested', 'Somewhat interested', 'Somewhat uninterested', 'Very uninterested'), ordered = TRUE)
table(GA_data$pol_interest)  # Frequency table for Political Interest Factor

# Political Interest Numeric
GA_data$pol_interest_n = GA_data$q9
summary(GA_data$pol_interest_n)  # Summary for Political Interest Numeric

# Discuss Politics Factor
GA_data$pol_discuss = recode(GA_data$q10, "1 = 'A few times a day'; 2 = 'Once a day'; 3 = 'Three to five days a week'; 4 = 'Once a week'; 5 = 'Less often than once a week'; 6 = 'Never'")
GA_data$pol_discuss = factor(GA_data$pol_discuss, levels = c('A few times a day', 'Once a day', 'Three to five days a week', 'Once a week', 'Less often than once a week', 'Never'), ordered = TRUE)
table(GA_data$pol_discuss)  # Frequency table for Discuss Politics Factor

#Discuss Politics Numeric
GA_data$pol_discuss_n = GA_data$q10
summary(GA_data$pol_discuss_n)  # Summary for Discuss Politics Numeric

# Disagree when Discussing Politics
GA_data$pol_disagree = recode(GA_data$q11, "1 = 'Most of the time'; 2 = 'Some of the time'; 3 = 'Rarely'; 4 = 'Never'")
GA_data$pol_disagree = factor(GA_data$pol_disagree, levels = c('Most of the time', 'Some of the time', 'Rarely', 'Never'), ordered = TRUE)
table(GA_data$pol_disagree)  # Frequency table for Disagree when Discussing Politics

#Disagree Politics Numeric
GA_data$pol_disagree_n = GA_data$q11
summary(GA_data$pol_disagree_n)  # Summary for Disagree Politics Numeric

# Trust Variables Factor
# Trust in Central Government
GA_data$trust_central = recode(GA_data$q12_a, "1 = 'A Great Deal'; 2 = 'Quite a Lot of Trust'; 3 = 'Not Very Much Trust'; 4 = 'None at all'")
GA_data$trust_central = factor(GA_data$trust_central, levels = c('A Great Deal', 'Quite a Lot of Trust', 'Not Very Much Trust', 'None at all'), ordered = TRUE)
table(GA_data$trust_central)  # Frequency table for Trust in Central Government

# Trust in Local Government
GA_data$trust_local = recode(GA_data$q12_b, "1 = 'A Great Deal'; 2 = 'Quite a Lot of Trust'; 3 = 'Not Very Much Trust'; 4 = 'None at all'")
GA_data$trust_local = factor(GA_data$trust_local, levels = c('A Great Deal', 'Quite a Lot of Trust', 'Not Very Much Trust', 'None at all'), ordered = TRUE)
table(GA_data$trust_local)  # Frequency table for Trust in Local Government

# Trust in Russia
GA_data$trust_russia = recode(GA_data$q12_c, "1 = 'A Great Deal'; 2 = 'Quite a Lot of Trust'; 3 = 'Not Very Much Trust'; 4 = 'None at all'")
GA_data$trust_russia = factor(GA_data$trust_russia, levels = c('A Great Deal', 'Quite a Lot of Trust', 'Not Very Much Trust', 'None at all'), ordered = TRUE)
table(GA_data$trust_russia)  # Frequency table for Trust in Russia

# Trust in the US
GA_data$trust_US = recode(GA_data$q12_d, "1 = 'A Great Deal'; 2 = 'Quite a Lot of Trust'; 3 = 'Not Very Much Trust'; 4 = 'None at all'")
GA_data$trust_US = factor(GA_data$trust_US, levels = c('A Great Deal', 'Quite a Lot of Trust', 'Not Very Much Trust', 'None at all'), ordered = TRUE)
table(GA_data$trust_US)  # Frequency table for Trust in the US

# Trust in China
GA_data$trust_china = recode(GA_data$q12_e, "1 = 'A Great Deal'; 2 = 'Quite a Lot of Trust'; 3 = 'Not Very Much Trust'; 4 = 'None at all'")
GA_data$trust_china = factor(GA_data$trust_china, levels = c('A Great Deal', 'Quite a Lot of Trust', 'Not Very Much Trust', 'None at all'), ordered = TRUE)
table(GA_data$trust_china)  # Frequency table for Trust in China

# Trust in the EU
GA_data$trust_EU = recode(GA_data$q12_f, "1 = 'A Great Deal'; 2 = 'Quite a Lot of Trust'; 3 = 'Not Very Much Trust'; 4 = 'None at all'")
GA_data$trust_EU = factor(GA_data$trust_EU, levels = c('A Great Deal', 'Quite a Lot of Trust', 'Not Very Much Trust', 'None at all'), ordered = TRUE)
table(GA_data$trust_EU)  # Frequency table for Trust in the EU

# Trust Central Numeric
GA_data$trust_central_n = GA_data$q12_a
summary(GA_data$trust_central_n)

# Trust Local Numeric
GA_data$trust_local_n = GA_data$q12_b
summary(GA_data$trust_local_n)

# Trust Russia Numeric
GA_data$trust_russia_n = GA_data$q12_c
summary(GA_data$trust_russia_n)

# Trust US Numeric
GA_data$trust_US_n = GA_data$q12_d
summary(GA_data$trust_US_n)

# Trust China Numeric
GA_data$trust_china_n = GA_data$q12_e
summary(GA_data$trust_china_n)

# Trust EU Numeric
GA_data$trust_EU_n = GA_data$q12_f
summary(GA_data$trust_EU_n)

library(car)

# Recoding 'Most Important Issue First Mention' with car::recode
GA_data$important_issue_first = recode(GA_data$q13_a, "1 = 'Political instability'; 2 = 'Unemployment'; 3 = 'Inflation Prices'; 4 = 'Wages pensions'; 5 = 'Taxes'; 6 = 'Access to basic needs'; 7 = 'General economic situation'; 8 = 'Deterioration of the quality and access to education'; 9 = 'Construction of houses residential premises'; 10 = 'Terrorism'; 11 = 'War conflict in other countries'; 12 = 'Increase in crime violence concerns about personal safety'; 13 = 'Corruption'; 14 = 'Lack of opportunities'; 15 = 'Discrimination ethnic or religious tensions'; 16 = 'Infrastructure'; 17 = 'Deterioration of the environment'; 18 = 'Unresolved territorial conflicts'; 19 = 'Emigration'")
GA_data$important_issue_first = factor(GA_data$important_issue_first)
GA_data$important_issue_first = addNA(GA_data$important_issue_first)
table(GA_data$important_issue_first)  # Frequency table for important_issue_first

# Recoding 'Most Important Issue Second Mention' with car::recode
GA_data$important_issue_second = recode(GA_data$q13_b, "1 = 'Political instability'; 2 = 'Unemployment'; 3 = 'Inflation Prices'; 4 = 'Wages pensions'; 5 = 'Taxes'; 6 = 'Access to basic needs'; 7 = 'General economic situation'; 8 = 'Deterioration of the quality and access to education'; 9 = 'Construction of houses residential premises'; 10 = 'Terrorism'; 11 = 'War conflict in other countries'; 12 = 'Increase in crime violence concerns about personal safety'; 13 = 'Corruption'; 14 = 'Lack of opportunities'; 15 = 'Discrimination ethnic or religious tensions'; 16 = 'Infrastructure'; 17 = 'Deterioration of the environment'; 18 = 'Unresolved territorial conflicts'; 19 = 'Emigration'")
GA_data$important_issue_second = factor(GA_data$important_issue_second)
GA_data$important_issue_second = addNA(GA_data$important_issue_second)
table(GA_data$important_issue_second)  # Frequency table for important_issue_first

# System Approval Variables Factor
# Perform the recode
GA_data$system_capable = recode(GA_data$q14_a, "1 = 'Strongly agree'; 2 = 'Somewhat agree'; 3 = 'Neither disagree nor agree'; 4 = 'Somewhat disagree'; 5 = 'Strongly disagree'")
GA_data$system_capable = factor(GA_data$system_capable, levels = c('Strongly agree', 'Somewhat agree', 'Neither disagree nor agree', 'Somewhat disagree', 'Strongly disagree'), ordered = TRUE)
table(GA_data$system_capable)  # Frequency table for System Capable

# Numeric variable for System Capable
GA_data$system_capable_n = as.numeric(GA_data$q14_a)
summary(GA_data$system_capable_n)  # Summary for System Capable (Numeric)

GA_data$system_proud = recode(GA_data$q14_b, "1 = 'Strongly agree'; 2 = 'Somewhat agree'; 3 = 'Neither disagree nor agree'; 4 = 'Somewhat disagree'; 5 = 'Strongly disagree'")
GA_data$system_proud = factor(GA_data$system_proud, levels = c('Strongly agree', 'Somewhat agree', 'Neither disagree nor agree', 'Somewhat disagree', 'Strongly disagree'), ordered = TRUE)
table(GA_data$system_proud)  # Frequency table for System Proud

# Numeric variable for System Proud
GA_data$system_proud_n = as.numeric(GA_data$q14_b)
summary(GA_data$system_proud_n)  # Summary for System Proud (Numeric)

GA_data$system_deserves = recode(GA_data$q14_c, "1 = 'Strongly agree'; 2 = 'Somewhat agree'; 3 = 'Neither disagree nor agree'; 4 = 'Somewhat disagree'; 5 = 'Strongly disagree'")
GA_data$system_deserves = factor(GA_data$system_deserves, levels = c('Strongly agree', 'Somewhat agree', 'Neither disagree nor agree', 'Somewhat disagree', 'Strongly disagree'), ordered = TRUE)
table(GA_data$system_deserves)  # Frequency table for System Deserves

# Numeric variable for System Deserves
GA_data$system_deserves_n = as.numeric(GA_data$q14_c)
summary(GA_data$system_deserves_n)  # Summary for System Deserves (Numeric)

GA_data$system_live = recode(GA_data$q14_d, "1 = 'Strongly agree'; 2 = 'Somewhat agree'; 3 = 'Neither disagree nor agree'; 4 = 'Somewhat disagree'; 5 = 'Strongly disagree'")
GA_data$system_live = factor(GA_data$system_live, levels = c('Strongly agree', 'Somewhat agree', 'Neither disagree nor agree', 'Somewhat disagree', 'Strongly disagree'), ordered = TRUE)
table(GA_data$system_live)  # Frequency table for System Live

# Numeric variable for System Live
GA_data$system_live_n = as.numeric(GA_data$q14_d)
summary(GA_data$system_live_n)  # Summary for System Live (Numeric)

GA_data$system_hurdles_participate = recode(GA_data$q14_e, "1 = 'Strongly agree'; 2 = 'Somewhat agree'; 3 = 'Neither disagree nor agree'; 4 = 'Somewhat disagree'; 5 = 'Strongly disagree'")
GA_data$system_hurdles_participate = factor(GA_data$system_hurdles_participate, levels = c('Strongly agree', 'Somewhat agree', 'Neither disagree nor agree', 'Somewhat disagree', 'Strongly disagree'), ordered = TRUE)
table(GA_data$system_hurdles_participate)  # Frequency table for System Hurdles for Participation

# Numeric variable for System Hurdles for Participation
GA_data$system_hurdles_participate_n = as.numeric(GA_data$q14_e)
summary(GA_data$system_hurdles_participate_n)  # Summary for System Hurdles for Participation (Numeric)

# Participate Variables Factor
# Perform the recode
GA_data$participate_rally = recode(GA_data$q15_a, "1 = 'Yes'; 2 = 'No'")
GA_data$participate_rally = factor(GA_data$participate_rally, levels = c('Yes', 'No'), ordered = FALSE)
table(GA_data$participate_rally)  # Frequency table for Participate Rally

# Numeric variable for Participate Rally
GA_data$participate_rally_n = as.numeric(GA_data$q15_a)
summary(GA_data$participate_rally_n)  # Summary for Participate Rally (Numeric)

GA_data$participate_protest = recode(GA_data$q15_b, "1 = 'Yes'; 2 = 'No'")
GA_data$participate_protest = factor(GA_data$participate_protest, levels = c('Yes', 'No'), ordered = FALSE)
table(GA_data$participate_protest)  # Frequency table for Participate Protest

# Numeric variable for Participate Protest
GA_data$participate_protest_n = as.numeric(GA_data$q15_b)
summary(GA_data$participate_protest_n)  # Summary for Participate Protest (Numeric)

GA_data$participate_meeting = recode(GA_data$q15_c, "1 = 'Yes'; 2 = 'No'")
GA_data$participate_meeting = factor(GA_data$participate_meeting, levels = c('Yes', 'No'), ordered = FALSE)
table(GA_data$participate_meeting)  # Frequency table for Participate Meeting

# Numeric variable for Participate Meeting
GA_data$participate_meeting_n = as.numeric(GA_data$q15_c)
summary(GA_data$participate_meeting_n)  # Summary for Participate Meeting (Numeric)

GA_data$participate_volunteer = recode(GA_data$q15_d, "1 = 'Yes'; 2 = 'No'")
GA_data$participate_volunteer = factor(GA_data$participate_volunteer, levels = c('Yes', 'No'), ordered = FALSE)
table(GA_data$participate_volunteer)  # Frequency table for Participate Volunteer

# Numeric variable for Participate Volunteer
GA_data$participate_volunteer_n = as.numeric(GA_data$q15_d)
summary(GA_data$participate_volunteer_n)  # Summary for Participate Volunteer (Numeric)

GA_data$participate_member = recode(GA_data$q15_e, "1 = 'Yes'; 2 = 'No'")
GA_data$participate_member = factor(GA_data$participate_member, levels = c('Yes', 'No'), ordered = FALSE)
table(GA_data$participate_member)  # Frequency table for Participate Member

# Numeric variable for Participate Member
GA_data$participate_member_n = as.numeric(GA_data$q15_e)
summary(GA_data$participate_member_n)  # Summary for Participate Member (Numeric)

GA_data$participate_community = recode(GA_data$q15_f, "1 = 'Yes'; 2 = 'No'")
GA_data$participate_community = factor(GA_data$participate_community, levels = c('Yes', 'No'), ordered = FALSE)
table(GA_data$participate_community)  # Frequency table for Participate Community

# Numeric variable for Participate Community
GA_data$participate_community_n = as.numeric(GA_data$q15_f)
summary(GA_data$participate_community_n)  # Summary for Participate Community (Numeric)

GA_data$participate_contact = recode(GA_data$q15_g, "1 = 'Yes'; 2 = 'No'")
GA_data$participate_contact = factor(GA_data$participate_contact, levels = c('Yes', 'No'), ordered = FALSE)
table(GA_data$participate_contact)  # Frequency table for Participate Contact

# Numeric variable for Participate Contact
GA_data$participate_contact_n = as.numeric(GA_data$q15_g)
summary(GA_data$participate_contact_n)  # Summary for Participate Contact (Numeric)

GA_data$participate_vote = recode(GA_data$q16, "1 = 'Yes'; 2 = 'No'")
GA_data$participate_vote = factor(GA_data$participate_vote, levels = c('Yes', 'No'), ordered = FALSE)
table(GA_data$participate_vote)  # Frequency table for Participate Vote

# Numeric variable for Participate Vote
GA_data$participate_vote_n = as.numeric(GA_data$q16)
summary(GA_data$participate_vote_n)  # Summary for Participate Vote (Numeric)

# Digital Tracking Variables Factor
# Perform the recode
GA_data$tracking_central = recode(GA_data$q18_a, "1 = 'Not Comfortable at all'; 2 = '2'; 3 = '3'; 4 = '4'; 5 = 'Very Comfortable'")
GA_data$tracking_central = factor(GA_data$tracking_central, levels = c('Not Comfortable at all', '2', '3', '4', 'Very Comfortable'), ordered = TRUE)
table(GA_data$tracking_central)  # Frequency table for Tracking Central

# Numeric variable for Tracking Central
GA_data$tracking_central_n = as.numeric(GA_data$q18_a)
summary(GA_data$tracking_central_n)  # Summary for Tracking Central (Numeric)

GA_data$tracking_local = recode(GA_data$q18_b, "1 = 'Not Comfortable at all'; 2 = '2'; 3 = '3'; 4 = '4'; 5 = 'Very Comfortable'")
GA_data$tracking_local = factor(GA_data$tracking_local, levels = c('Not Comfortable at all', '2', '3', '4', 'Very Comfortable'), ordered = TRUE)
table(GA_data$tracking_local)  # Frequency table for Tracking Local

# Numeric variable for Tracking Local
GA_data$tracking_local_n = as.numeric(GA_data$q18_b)
summary(GA_data$tracking_local_n)  # Summary for Tracking Local (Numeric)

GA_data$tracking_companies = recode(GA_data$q18_c, "1 = 'Not Comfortable at all'; 2 = '2'; 3 = '3'; 4 = '4'; 5 = 'Very Comfortable'")
GA_data$tracking_companies = factor(GA_data$tracking_companies, levels = c('Not Comfortable at all', '2', '3', '4', 'Very Comfortable'), ordered = TRUE)
table(GA_data$tracking_companies)  # Frequency table for Tracking Companies

# Numeric variable for Tracking Companies
GA_data$tracking_companies_n = as.numeric(GA_data$q18_c)
summary(GA_data$tracking_companies_n)  # Summary for Tracking Companies (Numeric)

# Democracy Variables Factor
# Perform the recode
GA_data$democracy_elections = recode(GA_data$q20_a, "1 = 'Strongly agree'; 2 = 'Somewhat agree'; 3 = 'Neither disagree nor agree'; 4 = 'Somewhat disagree'; 5 = 'Strongly disagree'")
GA_data$democracy_elections = factor(GA_data$democracy_elections, levels = c('Strongly agree', 'Somewhat agree', 'Neither disagree nor agree', 'Somewhat disagree', 'Strongly disagree'), ordered = TRUE)
table(GA_data$democracy_elections)  # Frequency table for Democracy Elections

# Numeric variable for Democracy Elections
GA_data$democracy_elections_n = as.numeric(GA_data$q20_a)
summary(GA_data$democracy_elections_n)  # Summary for Democracy Elections (Numeric)

GA_data$democracy_speech = recode(GA_data$q20_b, "1 = 'Strongly agree'; 2 = 'Somewhat agree'; 3 = 'Neither disagree nor agree'; 4 = 'Somewhat disagree'; 5 = 'Strongly disagree'")
GA_data$democracy_speech = factor(GA_data$democracy_speech, levels = c('Strongly agree', 'Somewhat agree', 'Neither disagree nor agree', 'Somewhat disagree', 'Strongly disagree'), ordered = TRUE)
table(GA_data$democracy_speech)  # Frequency table for Democracy Speech

# Numeric variable for Democracy Speech
GA_data$democracy_speech_n = as.numeric(GA_data$q20_b)
summary(GA_data$democracy_speech_n)  # Summary for Democracy Speech (Numeric)

GA_data$democracy_oversight = recode(GA_data$q20_c, "1 = 'Strongly agree'; 2 = 'Somewhat agree'; 3 = 'Neither disagree nor agree'; 4 = 'Somewhat disagree'; 5 = 'Strongly disagree'")
GA_data$democracy_oversight = factor(GA_data$democracy_oversight, levels = c('Strongly agree', 'Somewhat agree', 'Neither disagree nor agree', 'Somewhat disagree', 'Strongly disagree'), ordered = TRUE)
table(GA_data$democracy_oversight)  # Frequency table for Democracy Oversight

# Numeric variable for Democracy Oversight
GA_data$democracy_oversight_n = as.numeric(GA_data$q20_c)
summary(GA_data$democracy_oversight_n)  # Summary for Democracy Oversight (Numeric)

GA_data$democracy_organize = recode(GA_data$q20_d, "1 = 'Strongly agree'; 2 = 'Somewhat agree'; 3 = 'Neither disagree nor agree'; 4 = 'Somewhat disagree'; 5 = 'Strongly disagree'")
GA_data$democracy_organize = factor(GA_data$democracy_organize, levels = c('Strongly agree', 'Somewhat agree', 'Neither disagree nor agree', 'Somewhat disagree', 'Strongly disagree'), ordered = TRUE)
table(GA_data$democracy_organize)  # Frequency table for Democracy Organize

# Numeric variable for Democracy Organize
GA_data$democracy_organize_n = as.numeric(GA_data$q20_d)
summary(GA_data$democracy_organize_n)  # Summary for Democracy Organize (Numeric)

GA_data$democracy_press = recode(GA_data$q20_e, "1 = 'Strongly agree'; 2 = 'Somewhat agree'; 3 = 'Neither disagree nor agree'; 4 = 'Somewhat disagree'; 5 = 'Strongly disagree'")
GA_data$democracy_press = factor(GA_data$democracy_press, levels = c('Strongly agree', 'Somewhat agree', 'Neither disagree nor agree', 'Somewhat disagree', 'Strongly disagree'), ordered = TRUE)
table(GA_data$democracy_press)  # Frequency table for Democracy Press

# Numeric variable for Democracy Press
GA_data$democracy_press_n = as.numeric(GA_data$q20_e)
summary(GA_data$democracy_press_n)  # Summary for Democracy Press (Numeric)

GA_data$democracy_parties = recode(GA_data$q20_f, "1 = 'Strongly agree'; 2 = 'Somewhat agree'; 3 = 'Neither disagree nor agree'; 4 = 'Somewhat disagree'; 5 = 'Strongly disagree'")
GA_data$democracy_parties = factor(GA_data$democracy_parties, levels = c('Strongly agree', 'Somewhat agree', 'Neither disagree nor agree', 'Somewhat disagree', 'Strongly disagree'), ordered = TRUE)
table(GA_data$democracy_parties)  # Frequency table for Democracy Parties

# Numeric variable for Democracy Parties
GA_data$democracy_parties_n = as.numeric(GA_data$q20_f)
summary(GA_data$democracy_parties_n)  # Summary for Democracy Parties (Numeric)

GA_data$democracy_protests = recode(GA_data$q20_g, "1 = 'Strongly agree'; 2 = 'Somewhat agree'; 3 = 'Neither disagree nor agree'; 4 = 'Somewhat disagree'; 5 = 'Strongly disagree'")
GA_data$democracy_protests = factor(GA_data$democracy_protests, levels = c('Strongly agree', 'Somewhat agree', 'Neither disagree nor agree', 'Somewhat disagree', 'Strongly disagree'), ordered = TRUE)
table(GA_data$democracy_protests)  # Frequency table for Democracy Protests

# Numeric variable for Democracy Protests
GA_data$democracy_protests_n = as.numeric(GA_data$q20_g)
summary(GA_data$democracy_protests_n)  # Summary for Democracy Protests (Numeric)

GA_data$democracy_courts = recode(GA_data$q20_h, "1 = 'Strongly agree'; 2 = 'Somewhat agree'; 3 = 'Neither disagree nor agree'; 4 = 'Somewhat disagree'; 5 = 'Strongly disagree'")
GA_data$democracy_courts = factor(GA_data$democracy_courts, levels = c('Strongly agree', 'Somewhat agree', 'Neither disagree nor agree', 'Somewhat disagree', 'Strongly disagree'), ordered = TRUE)
table(GA_data$democracy_courts)  # Frequency table for Democracy Courts

# Numeric variable for Democracy Courts
GA_data$democracy_courts_n = as.numeric(GA_data$q20_h)
summary(GA_data$democracy_courts_n)  # Summary for Democracy Courts (Numeric)

#Social Media Use
# Perform the recode for the factor variable
GA_data$sm_use = recode(GA_data$q22, "1 = 'Yes'; 2 = 'No'")
GA_data$sm_use = factor(GA_data$sm_use, levels = c('Yes', 'No'), ordered = TRUE)
GA_data$sm_use = addNA(GA_data$sm_use)
table(GA_data$sm_use)  # Frequency table for Social Media Use

#Social Media Platform Variables Factor
#Perform the recode
GA_data$facebook = recode(GA_data$q23_a, '0="Do not use this social media"; 1="0-1"; 2="1-2"; 3="2-3"; 4="3-4"; 5="4-5"; 6="5-6"; 7="6-7"; 8="7-8"; 9="8-9"; 10="More than 9"')
GA_data$facebook = factor(GA_data$facebook, levels = c('Do not use this social media', '0-1', '1-2', '2-3', '3-4', '4-5', '5-6', '6-7', '7-8', '8-9', 'More than 9'), ordered = TRUE)
table(GA_data$facebook) # Display a frequency table for 'facebook'

# Create a numeric variable for facebook
GA_data$facebook_n = as.numeric(GA_data$q23_a) 
summary(GA_data$facebook_n) # Provide a summary for 'facebook_n' (Numeric)

GA_data$vkontakte = recode(GA_data$q23_b, '0="Do not use this social media"; 1="0-1"; 2="1-2"; 3="2-3"; 4="3-4"; 5="4-5"; 6="5-6"; 7="6-7"; 8="7-8"; 9="8-9"; 10="More than 9"')
GA_data$vkontakte = factor(GA_data$vkontakte, levels = c('Do not use this social media', '0-1', '1-2', '2-3', '3-4', '4-5', '5-6', '6-7', '7-8', '8-9', 'More than 9'), ordered = TRUE)
table(GA_data$vkontakte) # Display a frequency table for 'vkontakte'

# Create a numeric variable for vkontakte
GA_data$vkontakte_n = as.numeric(GA_data$q23_b) 
summary(GA_data$vkontakte_n) # Provide a summary for 'vkontakte_n' (Numeric)

GA_data$instagram = recode(GA_data$q23_c, '0="Do not use this social media"; 1="0-1"; 2="1-2"; 3="2-3"; 4="3-4"; 5="4-5"; 6="5-6"; 7="6-7"; 8="7-8"; 9="8-9"; 10="More than 9"')
GA_data$instagram = factor(GA_data$instagram, levels = c('Do not use this social media', '0-1', '1-2', '2-3', '3-4', '4-5', '5-6', '6-7', '7-8', '8-9', 'More than 9'), ordered = TRUE)
table(GA_data$instagram) # Display a frequency table for 'instagram'

# Create a numeric variable for instagram
GA_data$instagram_n = as.numeric(GA_data$q23_c) 
summary(GA_data$instagram_n) # Provide a summary for 'instagram_n' (Numeric)


GA_data$tiktok = recode(GA_data$q23_d, '0="Do not use this social media"; 1="0-1"; 2="1-2"; 3="2-3"; 4="3-4"; 5="4-5"; 6="5-6"; 7="6-7"; 8="7-8"; 9="8-9"; 10="More than 9"')
GA_data$tiktok = factor(GA_data$tiktok, levels = c('Do not use this social media', '0-1', '1-2', '2-3', '3-4', '4-5', '5-6', '6-7', '7-8', '8-9', 'More than 9'), ordered = TRUE)
table(GA_data$tiktok) # Display a frequency table for 'tiktok'

# Create a numeric variable for tiktok
GA_data$tiktok_n = as.numeric(GA_data$q23_d) 
summary(GA_data$tiktok_n) # Provide a summary for 'tiktok_n' (Numeric)

GA_data$twitter = recode(GA_data$q23_e, '0="Do not use this social media"; 1="0-1"; 2="1-2"; 3="2-3"; 4="3-4"; 5="4-5"; 6="5-6"; 7="6-7"; 8="7-8"; 9="8-9"; 10="More than 9"')
GA_data$twitter = factor(GA_data$twitter, levels = c('Do not use this social media', '0-1', '1-2', '2-3', '3-4', '4-5', '5-6', '6-7', '7-8', '8-9', 'More than 9'), ordered = TRUE)
table(GA_data$twitter) # Display a frequency table for 'twitter'

# Create a numeric variable for twitter
GA_data$twitter_n = as.numeric(GA_data$q23_e) 
summary(GA_data$twitter_n) # Provide a summary for 'twitter_n' (Numeric)

GA_data$youtube = recode(GA_data$q23_f, '0="Do not use this social media"; 1="0-1"; 2="1-2"; 3="2-3"; 4="3-4"; 5="4-5"; 6="5-6"; 7="6-7"; 8="7-8"; 9="8-9"; 10="More than 9"')
GA_data$youtube = factor(GA_data$youtube, levels = c('Do not use this social media', '0-1', '1-2', '2-3', '3-4', '4-5', '5-6', '6-7', '7-8', '8-9', 'More than 9'), ordered = TRUE)
table(GA_data$youtube) # Display a frequency table for 'youtube'

# Create a numeric variable for youtube
GA_data$youtube_n = as.numeric(GA_data$q23_f) 
summary(GA_data$youtube_n) # Provide a summary for 'youtube_n' (Numeric)

GA_data$whatsapp = recode(GA_data$q23_g, '0="Do not use this social media"; 1="0-1"; 2="1-2"; 3="2-3"; 4="3-4"; 5="4-5"; 6="5-6"; 7="6-7"; 8="7-8"; 9="8-9"; 10="More than 9"')
GA_data$whatsapp = factor(GA_data$whatsapp, levels = c('Do not use this social media', '0-1', '1-2', '2-3', '3-4', '4-5', '5-6', '6-7', '7-8', '8-9', 'More than 9'), ordered = TRUE)
table(GA_data$whatsapp) # Display a frequency table for 'whatsapp'

# Create a numeric variable for whatsapp
GA_data$whatsapp_n = as.numeric(GA_data$q23_g) 
summary(GA_data$whatsapp_n) # Provide a summary for 'whatsapp_n' (Numeric)

GA_data$telegram = recode(GA_data$q23_h, '0="Do not use this social media"; 1="0-1"; 2="1-2"; 3="2-3"; 4="3-4"; 5="4-5"; 6="5-6"; 7="6-7"; 8="7-8"; 9="8-9"; 10="More than 9"')
GA_data$telegram = factor(GA_data$telegram, levels = c('Do not use this social media', '0-1', '1-2', '2-3', '3-4', '4-5', '5-6', '6-7', '7-8', '8-9', 'More than 9'), ordered = TRUE)
table(GA_data$telegram) # Display a frequency table for 'telegram'

# Create a numeric variable for whatsapp
GA_data$telegram_n = as.numeric(GA_data$q23_h) 
summary(GA_data$telegram_n) # Provide a summary for 'telegram_n' (Numeric)

# Perform the recode for 'pol_news_tv'
GA_data$pol_news_tv = recode(GA_data$q24_a, '1="A few times a day"; 2="Once a day"; 3="Three to five days a week"; 4="Once a week"; 5="Less often than once a week"; 6="Never"')
GA_data$pol_news_tv = factor(GA_data$pol_news_tv, levels = c('A few times a day', 'Once a day', 'Three to five days a week', 'Once a week', 'Less often than once a week', 'Never'), ordered = TRUE)
table(GA_data$pol_news_tv) # Display a frequency table for 'pol_news_tv'

# Create a numeric variable for 'pol_news_tv'
GA_data$pol_news_tv_n = as.numeric(GA_data$q24_a) 
summary(GA_data$pol_news_tv_n) # Provide a summary for 'pol_news_tv_n' (Numeric)

# Perform the recode for 'pol_news_facebook'
GA_data$pol_news_facebook = recode(GA_data$q24_b, '1="A few times a day"; 2="Once a day"; 3="Three to five days a week"; 4="Once a week"; 5="Less often than once a week"; 6="Never"')
GA_data$pol_news_facebook = factor(GA_data$pol_news_facebook, levels = c('A few times a day', 'Once a day', 'Three to five days a week', 'Once a week', 'Less often than once a week', 'Never'), ordered = TRUE)
table(GA_data$pol_news_facebook) # Display a frequency table for 'pol_news_facebook'

# Create a numeric variable for 'pol_news_facebook'
GA_data$pol_news_facebook_n = as.numeric(GA_data$q24_b) 
summary(GA_data$pol_news_facebook_n) # Provide a summary for 'pol_news_facebook_n' (Numeric)

# Perform the recode for 'pol_news_vkontakte'
GA_data$pol_news_vkontakte = recode(GA_data$q24_c, '1="A few times a day"; 2="Once a day"; 3="Three to five days a week"; 4="Once a week"; 5="Less often than once a week"; 6="Never"')
GA_data$pol_news_vkontakte = factor(GA_data$pol_news_vkontakte, levels = c('A few times a day', 'Once a day', 'Three to five days a week', 'Once a week', 'Less often than once a week', 'Never'), ordered = TRUE)
table(GA_data$pol_news_vkontakte) # Display a frequency table for 'pol_news_vkontakte'

# Create a numeric variable for 'pol_news_vkontakte'
GA_data$pol_news_vkontakte_n = as.numeric(GA_data$q24_c) 
summary(GA_data$pol_news_vkontakte_n) # Provide a summary for 'pol_news_vkontakte_n' (Numeric)

# Perform the recode for 'pol_news_tiktok'
GA_data$pol_news_tiktok = recode(GA_data$q24_d, '1="A few times a day"; 2="Once a day"; 3="Three to five days a week"; 4="Once a week"; 5="Less often than once a week"; 6="Never"')
GA_data$pol_news_tiktok = factor(GA_data$pol_news_tiktok, levels = c('A few times a day', 'Once a day', 'Three to five days a week', 'Once a week', 'Less often than once a week', 'Never'), ordered = TRUE)
table(GA_data$pol_news_tiktok) # Display a frequency table for 'pol_news_tiktok'

# Create a numeric variable for 'pol_news_tiktok'
GA_data$pol_news_tiktok_n = as.numeric(GA_data$q24_d) 
summary(GA_data$pol_news_tiktok_n) # Provide a summary for 'pol_news_tiktok_n' (Numeric)

# Perform the recode for 'pol_news_twitter'
GA_data$pol_news_twitter = recode(GA_data$q24_e, '1="A few times a day"; 2="Once a day"; 3="Three to five days a week"; 4="Once a week"; 5="Less often than once a week"; 6="Never"')
GA_data$pol_news_twitter = factor(GA_data$pol_news_twitter, levels = c('A few times a day', 'Once a day', 'Three to five days a week', 'Once a week', 'Less often than once a week', 'Never'), ordered = TRUE)
table(GA_data$pol_news_twitter) # Display a frequency table for 'pol_news_twitter'

# Create a numeric variable for 'pol_news_twitter'
GA_data$pol_news_twitter_n = as.numeric(GA_data$q24_e) 
summary(GA_data$pol_news_twitter_n) # Provide a summary for 'pol_news_twitter_n' (Numeric)

# Perform the recode for 'pol_news_odnoklassniki'
GA_data$pol_news_odnoklassniki = recode(GA_data$q24_f, '1="A few times a day"; 2="Once a day"; 3="Three to five days a week"; 4="Once a week"; 5="Less often than once a week"; 6="Never"')
GA_data$pol_news_odnoklassniki = factor(GA_data$pol_news_odnoklassniki, levels = c('A few times a day', 'Once a day', 'Three to five days a week', 'Once a week', 'Less often than once a week', 'Never'), ordered = TRUE)
table(GA_data$pol_news_odnoklassniki) # Display a frequency table for 'pol_news_odnoklassniki'

# Create a numeric variable for 'pol_news_odnoklassniki'
GA_data$pol_news_odnoklassniki_n = as.numeric(GA_data$q24_f) 
summary(GA_data$pol_news_odnoklassniki_n) # Provide a summary for 'pol_news_odnoklassniki_n' (Numeric)

# Perform the recode for 'trust_state'
GA_data$trust_state = recode(GA_data$q25_a, '1="Very trustworthy"; 2="Somewhat trustworthy"; 3="Somewhat untrustworthy"; 4="Very untrustworthy"')
GA_data$trust_state = factor(GA_data$trust_state, levels = c('Very trustworthy', 'Somewhat trustworthy', 'Somewhat untrustworthy', 'Very untrustworthy'), ordered = TRUE)
table(GA_data$trust_state) # Display a frequency table for 'trust_state'

# Create a numeric variable for 'trust_state'
GA_data$trust_state_n = as.numeric(GA_data$q25_a) 
summary(GA_data$trust_state_n) # Provide a summary for 'trust_state_n' (Numeric)

# Perform the recode for 'trust_russian_media'
GA_data$trust_russian_media = recode(GA_data$q25_b, '1="Very trustworthy"; 2="Somewhat trustworthy"; 3="Somewhat untrustworthy"; 4="Very untrustworthy"')
GA_data$trust_russian_media = factor(GA_data$trust_russian_media, levels = c('Very trustworthy', 'Somewhat trustworthy', 'Somewhat untrustworthy', 'Very untrustworthy'), ordered = TRUE)
table(GA_data$trust_russian_media) # Display a frequency table for 'trust_russian_media'

# Create a numeric variable for 'trust_russian_media'
GA_data$trust_russian_media_n = as.numeric(GA_data$q25_b) 
summary(GA_data$trust_russian_media_n) # Provide a summary for 'trust_russian_media_n' (Numeric)

# Perform the recode for 'trust_internet'
GA_data$trust_internet = recode(GA_data$q25_c, '1="Very trustworthy"; 2="Somewhat trustworthy"; 3="Somewhat untrustworthy"; 4="Very untrustworthy"')
GA_data$trust_internet = factor(GA_data$trust_internet, levels = c('Very trustworthy', 'Somewhat trustworthy', 'Somewhat untrustworthy', 'Very untrustworthy'), ordered = TRUE)
table(GA_data$trust_internet) # Display a frequency table for 'trust_internet'

# Create a numeric variable for 'trust_internet'
GA_data$trust_internet_n = as.numeric(GA_data$q25_c) 
summary(GA_data$trust_internet_n) # Provide a summary for 'trust_internet_n' (Numeric)

# Perform the recode for 'trust_facebook'
GA_data$trust_facebook = recode(GA_data$q25_d, '1="Very trustworthy"; 2="Somewhat trustworthy"; 3="Somewhat untrustworthy"; 4="Very untrustworthy"')
GA_data$trust_facebook = factor(GA_data$trust_facebook, levels = c('Very trustworthy', 'Somewhat trustworthy', 'Somewhat untrustworthy', 'Very untrustworthy'), ordered = TRUE)
table(GA_data$trust_facebook) # Display a frequency table for 'trust_facebook'

# Create a numeric variable for 'trust_parties'
GA_data$trust_facebook_n = as.numeric(GA_data$q25_d) 
summary(GA_data$trust_facebook_n) # Provide a summary for 'trust_facebook_n' (Numeric)

# Perform the recode for 'trust_vkontakte'
GA_data$trust_vkontakte = recode(GA_data$q25_e, '1="Very trustworthy"; 2="Somewhat trustworthy"; 3="Somewhat untrustworthy"; 4="Very untrustworthy"')
GA_data$trust_vkontakte = factor(GA_data$trust_vkontakte, levels = c('Very trustworthy', 'Somewhat trustworthy', 'Somewhat untrustworthy', 'Very untrustworthy'), ordered = TRUE)
table(GA_data$trust_vkontakte) # Display a frequency table for 'trust_vkontakte'

# Create a numeric variable for 'trust_vkontakte'
GA_data$trust_vkontakte_n = as.numeric(GA_data$q25_e) 
summary(GA_data$trust_vkontakte_n) # Provide a summary for 'trust_vkontakte_n' (Numeric)

# Perform the recode for 'trust_western'
GA_data$trust_western = recode(GA_data$q25_f, '1="Very trustworthy"; 2="Somewhat trustworthy"; 3="Somewhat untrustworthy"; 4="Very untrustworthy"')
GA_data$trust_western = factor(GA_data$trust_western, levels = c('Very trustworthy', 'Somewhat trustworthy', 'Somewhat untrustworthy', 'Very untrustworthy'), ordered = TRUE)
table(GA_data$trust_western) # Display a frequency table for 'trust_western'

# Create a numeric variable for 'trust_western'
GA_data$trust_western_n = as.numeric(GA_data$q25_f) 
summary(GA_data$trust_western_n) # Provide a summary for 'trust_western_n' (Numeric)

#News Balance Factor
# Perform the recode for 'news_balance'
GA_data$news_balance = recode(GA_data$q26, '1="Only from traditional sources like television newspapers radio";2="Mostly from television newspapers radio but some from the internet social media";3="From an equal balance of television newspapers radio and the internet social media";4="Mostly from social media but some from television newspapers radio";5="Only from the internet social media"')
GA_data$news_balance = factor(GA_data$news_balance, levels = c('Only from traditional sources like television newspapers radio', 'Mostly from television newspapers radio but some from the internet social media', 'From an equal balance of television newspapers radio and the internet social media', 'Mostly from social media but some from television newspapers radio', 'Only from the internet social media'), ordered = TRUE)
GA_data$news_balance = addNA(GA_data$news_balance)
table(GA_data$news_balance) # Display a frequency table for 'news_balance'

# Create a numeric variable for 'news_balance'
GA_data$news_balance_n = as.numeric(GA_data$q26)
summary(GA_data$news_balance_n) # Provide a summary for 'news_balance_n' (Numeric)

# Tone of Social Media News about Government Factor
# Perform the recode for 'q27_a' (Tone of Social Media News about Government)
GA_data$sm_critical_local = recode(GA_data$q27_a, '1="Very often";2="Rather often";3="Occasionally";4="Never"')
GA_data$sm_critical_local = factor(GA_data$sm_critical_local, levels = c('Very often', 'Rather often', 'Occasionally', 'Never'), ordered = TRUE)
table(GA_data$sm_critical_local) # Display a frequency table for 'sm_critical_local'

# Create a numeric variable for 'sm_critical_local'
GA_data$sm_critical_local_n = as.numeric(GA_data$q27_a)
summary(GA_data$sm_critical_local_n) # Provide a summary for 'sm_critical_local_n' (Numeric)

# Perform the recode for 'q27_b' (Tone of Social Media News about Government)
GA_data$sm_critical_central = recode(GA_data$q27_b, '1="Very often";2="Rather often";3="Occasionally";4="Never"')
GA_data$sm_critical_central = factor(GA_data$sm_critical_central, levels = c('Very often', 'Rather often', 'Occasionally', 'Never'), ordered = TRUE)
table(GA_data$sm_critical_central) # Display a frequency table for 'sm_critical_central'

# Create a numeric variable for 'sm_critical_central'
GA_data$sm_critical_central_n = as.numeric(GA_data$q27_b)
summary(GA_data$sm_critical_central_n) # Provide a summary for 'sm_critical_central_n' (Numeric)

# Perform the recode for 'q27_c' (Tone of Social Media News about Government)
GA_data$sm_positive_local = recode(GA_data$q27_c, '1="Very often";2="Rather often";3="Occasionally";4="Never"')
GA_data$sm_positive_local = factor(GA_data$sm_positive_local, levels = c('Very often', 'Rather often', 'Occasionally', 'Never'), ordered = TRUE)
table(GA_data$sm_positive_local) # Display a frequency table for 'sm_positive_local'

# Create a numeric variable for 'sm_positive_local'
GA_data$sm_positive_local_n = as.numeric(GA_data$q27_c)
summary(GA_data$sm_positive_local_n) # Provide a summary for 'sm_positive_local_n' (Numeric)

# Perform the recode for 'q27_d' (Tone of Social Media News about Government)
GA_data$sm_positive_central = recode(GA_data$q27_d, '1="Very often";2="Rather often";3="Occasionally";4="Never"')
GA_data$sm_positive_central = factor(GA_data$sm_positive_central, levels = c('Very often', 'Rather often', 'Occasionally', 'Never'), ordered = TRUE)
table(GA_data$sm_positive_central) # Display a frequency table for 'sm_positive_central'

# Create a numeric variable for 'sm_positive_central'
GA_data$sm_positive_central_n = as.numeric(GA_data$q27_d)
summary(GA_data$sm_positive_central_n) # Provide a summary for 'sm_positive_central_n' (Numeric)

# TV Tone of News about Government Factor
# Perform the recode for 'q28_a' (TV Tone of News about Government)
GA_data$tv_critical_local = recode(GA_data$q28_a, '1="Very often";2="Rather often";3="Occasionally";4="Never"')
GA_data$tv_critical_local = factor(GA_data$tv_critical_local, levels = c('Very often', 'Rather often', 'Occasionally', 'Never'), ordered = TRUE)
table(GA_data$tv_critical_local) # Display a frequency table for 'tv_critical_local'

# Create a numeric variable for 'tv_critical_local'
GA_data$tv_critical_local_n = as.numeric(GA_data$q28_a)
summary(GA_data$tv_critical_local_n) # Provide a summary for 'tv_critical_local_n' (Numeric)

# Perform the recode for 'q28_b' (TV Tone of News about Government)
GA_data$tv_critical_central = recode(GA_data$q28_b, '1="Very often";2="Rather often";3="Occasionally";4="Never"')
GA_data$tv_critical_central = factor(GA_data$tv_critical_central, levels = c('Very often', 'Rather often', 'Occasionally', 'Never'), ordered = TRUE)
table(GA_data$tv_critical_central) # Display a frequency table for 'tv_critical_central'

# Create a numeric variable for 'tv_critical_central'
GA_data$tv_critical_central_n = as.numeric(GA_data$q28_b)
summary(GA_data$tv_critical_central_n) # Provide a summary for 'tv_critical_central_n' (Numeric)

# Clickable Links Factor
# Perform the recode for 'q29_a' (Clickable Links)
GA_data$clickable_state = recode(GA_data$q29_a, '1="Almost everyday";2="About once a week";3="A few times a month";4="About once a month";5="Never"')
GA_data$clickable_state = factor(GA_data$clickable_state, levels = c('Almost everyday', 'About once a week', 'A few times a month', 'About once a month', 'Never'), ordered = TRUE)
table(GA_data$clickable_state) # Display a frequency table for 'clickable_state'

# Create a numeric variable for 'clickable_state'
GA_data$clickable_state_n = as.numeric(GA_data$q29_a)
summary(GA_data$clickable_state_n) # Provide a summary for 'clickable_state_n' (Numeric)

# Perform the recode for 'q29_b' (Clickable Links)
GA_data$clickable_russian = recode(GA_data$q29_b, '1="Almost everyday";2="About once a week";3="A few times a month";4="About once a month";5="Never"')
GA_data$clickable_russian = factor(GA_data$clickable_russian, levels = c('Almost everyday', 'About once a week', 'A few times a month', 'About once a month', 'Never'), ordered = TRUE)
table(GA_data$clickable_russian) # Display a frequency table for 'clickable_russian'

# Create a numeric variable for 'clickable_russian'
GA_data$clickable_russian_n = as.numeric(GA_data$q29_b)
summary(GA_data$clickable_russian_n) # Provide a summary for 'clickable_russian_n' (Numeric)

# Artificial Intelligence Attitude Factor
# Perform the recode for 'AI' (Artificial Intelligence Attitude)
GA_data$AI = recode(GA_data$q30, '1="More excited than concerned";2="Equally concerned and excited";3="More concerned than excited";4="I do not know what artificial intelligence is"')
GA_data$AI = factor(GA_data$AI, levels = c('More excited than concerned', 'Equally concerned and excited', 'More concerned than excited', 'I do not know what artificial intelligence is'), ordered = TRUE)
table(GA_data$AI) # Display a frequency table for 'AI'

# Artificial Intelligence Attitude Ordinal (coding I don't know as NA)
# Perform the recode
GA_data$AI_ordinal = recode(GA_data$q30, '1="More excited than concerned";2="Equally concerned and excited";3="More concerned than excited";4=NA')
GA_data$AI_ordinal = factor(GA_data$AI_ordinal, levels = c('More excited than concerned', 'Equally concerned and excited', 'More concerned than excited', 'I do not know what artificial intelligence is'), ordered = TRUE)
table(GA_data$AI_ordinal) # Display a frequency table for 'AI_ordinal'

# Convert 'AI_ordinal' to an ordered factor/ordinal
GA_data$AI_ordinal = factor(GA_data$AI_ordinal, ordered = TRUE)

# Artificial Intelligence Attitude Numeric
# Perform the recode
GA_data$AI_n = recode(GA_data$q30, '1=1;2=2;3=3;4=NA')
summary(GA_data$AI_n) # Provide a summary for 'AI_n' (Numeric)

#Echo Chamber Numeric
GA_data$echo_chamber_n = as.numeric(GA_data$q31)
summary(GA_data$echo_chamber_n) # Provide a summary for 'echo_chamber_n' (Numeric)

# Awareness of Government Posters Factor
# Perform the recode for 'paid_posters'
GA_data$paid_posters = recode(GA_data$q32, '1="I never heard of them";2="I have heard of them but I have not seen any posts from them";3="I think I have seen posts from them"')
GA_data$paid_posters = factor(GA_data$paid_posters, levels = c('I never heard of them', 'I have heard of them but I have not seen any posts from them', 'I think I have seen posts from them'), ordered = TRUE)
GA_data$paid_posters = addNA(GA_data$paid_posters)
table(GA_data$paid_posters) # Display a frequency table for 'paid_posters'

# Create a numeric variable for 'paid_posters'
GA_data$paid_posters_n = as.numeric(GA_data$q32)
summary(GA_data$paid_posters_n) # Provide a summary for 'paid_posters_n' (Numeric)

# Awareness of Government Posters Ordinal (coding "I don't know" as NA)
# Perform the recode
GA_data$paid_posters_ordinal = recode(GA_data$q32, '1="I never heard of them";2="I have heard of them but I have not seen any posts from them";3="I think I have seen posts from them";4=NA')
GA_data$paid_posters_ordinal = factor(GA_data$paid_posters_ordinal, levels = c('I never heard of them', 'I have heard of them but I have not seen any posts from them', 'I think I have seen posts from them'), ordered = TRUE)
GA_data$paid_posters_ordinal = addNA(GA_data$paid_posters_ordinal)
table(GA_data$paid_posters_ordinal) # Display a frequency table for 'paid_posters_ordinal'

# Convert 'paid_posters_ordinal' to an ordered factor/ordinal
GA_data$paid_posters_ordinal = factor(GA_data$paid_posters_ordinal, ordered = TRUE)

# Awareness of Government Posters Numeric
# Perform the recode
GA_data$paid_posters_n = recode(GA_data$q32, '1=1;2=2;3=3;4=NA')
summary(GA_data$paid_posters_n) # Provide a summary for 'paid_posters_n' (Numeric)

#Estimate of Percentage of Online Posts Paid by Government
GA_data$paid_posters_percentage_n = as.numeric(GA_data$q33)
summary(GA_data$paid_posters_percentage_n) # Provide a summary for paid_posters_percentage_n (Numeric)

# VPN Use Factor
# Perform the recode for 'vpn_use'
GA_data$vpn_use = recode(GA_data$q34, '1="Yes";2="No"')
GA_data$vpn_use = factor(GA_data$vpn_use, levels = c('Yes', 'No'), ordered = TRUE)
GA_data$vpn_use = addNA(GA_data$vpn_use)
table(GA_data$vpn_use) # Display a frequency table for 'vpn_use'

# Create a numeric variable for 'vpn_use'
GA_data$vpn_use_n = as.numeric(GA_data$q34)
summary(GA_data$vpn_use_n) # Provide a summary for 'vpn_use_n' (Numeric)

#Selective Exposure
# Perform the recode for 'avoidance_blocking'
GA_data$avoidance_blocking = recode(GA_data$q35_a, '1="Often"; 2="Sometimes"; 3="Very rarely"; 4="Never"')
GA_data$avoidance_blocking = factor(GA_data$avoidance_blocking, levels = c('Often', 'Sometimes', 'Very rarely', 'Never'), ordered = TRUE)
table(GA_data$avoidance_blocking) # Display a frequency table for 'avoidance_blocking'

# Create a numeric variable for 'avoidance_blocking'
GA_data$avoidance_blocking_n = as.numeric(GA_data$q35_a) 
summary(GA_data$avoidance_blocking_n) # Provide a summary for 'avoidance_blocking_numeric' (Numeric)

# Perform the recode for 'avoidance_unfriending'
GA_data$avoidance_unfriending = recode(GA_data$q35_b, '1="Often"; 2="Sometimes"; 3="Very rarely"; 4="Never"')
GA_data$avoidance_unfriending = factor(GA_data$avoidance_unfriending, levels = c('Often', 'Sometimes', 'Very rarely', 'Never'), ordered = TRUE)
table(GA_data$avoidance_unfriending) # Display a frequency table for 'avoidance_unfriending'

# Create a numeric variable for 'avoidance_unfriending'
GA_data$avoidance_unfriending_n = as.numeric(GA_data$q35_b) 
summary(GA_data$avoidance_unfriending_n) # Provide a summary for 'avoidance_unfriending_numeric' (Numeric)

# Perform the recode for 'avoidance_leaving_group'
GA_data$avoidance_leaving_group = recode(GA_data$q35_c, '1="Often"; 2="Sometimes"; 3="Very rarely"; 4="Never"')
GA_data$avoidance_leaving_group = factor(GA_data$avoidance_leaving_group, levels = c('Often', 'Sometimes', 'Very rarely', 'Never'), ordered = TRUE)
table(GA_data$avoidance_leaving_group) # Display a frequency table for 'avoidance_leaving_group'

# Create a numeric variable for 'avoidance_leaving_group'
GA_data$avoidance_leaving_group_n = as.numeric(GA_data$q35_c) 
summary(GA_data$avoidance_leaving_group_n) # Provide a summary for 'avoidance_leaving_group_numeric' (Numeric)

# Perform the recode for 'avoidance_unsubscribing'
GA_data$avoidance_unsubscribing = recode(GA_data$q35_d, '1="Often"; 2="Sometimes"; 3="Very rarely"; 4="Never"')
GA_data$avoidance_unsubscribing = factor(GA_data$avoidance_unsubscribing, levels = c('Often', 'Sometimes', 'Very rarely', 'Never'), ordered = TRUE)
table(GA_data$avoidance_unsubscribing) # Display a frequency table for 'avoidance_unsubscribing'

# Create a numeric variable for 'avoidance_unsubscribing'
GA_data$avoidance_unsubscribing_n = as.numeric(GA_data$q35_d) 
summary(GA_data$avoidance_unsubscribing_n) # Provide a summary for 'avoidance_unsubscribing_numeric' (Numeric)

#Exposure to SM Disagreement
# Perform the recode for 'sm_disagreement_politics'
GA_data$sm_disagreement_politics = recode(GA_data$q36_a, '1="Often"; 2="Sometimes"; 3="Very rarely"; 4="Never"')
GA_data$sm_disagreement_politics = factor(GA_data$sm_disagreement_politics, levels = c('Often', 'Sometimes', 'Very rarely', 'Never'), ordered = TRUE)
table(GA_data$sm_disagreement_politics) # Display a frequency table for 'sm_disagreement_politics'

# Create a numeric variable for 'sm_disagreement_politics'
GA_data$sm_disagreement_politics_n = as.numeric(GA_data$q36_a) 
summary(GA_data$sm_disagreement_politics_n) # Provide a summary for 'sm_disagreement_politics_numeric' (Numeric)

# Perform the recode for 'sm_disagreement_news'
GA_data$sm_disagreement_news = recode(GA_data$q36_b, '1="Often"; 2="Sometimes"; 3="Very rarely"; 4="Never"')
GA_data$sm_disagreement_news = factor(GA_data$sm_disagreement_news, levels = c('Often', 'Sometimes', 'Very rarely', 'Never'), ordered = TRUE)
table(GA_data$sm_disagreement_news) # Display a frequency table for 'sm_disagreement_news'

# Create a numeric variable for 'sm_disagreement_news'
GA_data$sm_disagreement_news_n = as.numeric(GA_data$q36_b) 
summary(GA_data$sm_disagreement_news_n) # Provide a summary for 'sm_disagreement_news_numeric' (Numeric)

# Perform the recode for 'sm_disagreement_issues'
GA_data$sm_disagreement_issues = recode(GA_data$q36_c, '1="Often"; 2="Sometimes"; 3="Very rarely"; 4="Never"')
GA_data$sm_disagreement_issues = factor(GA_data$sm_disagreement_issues, levels = c('Often', 'Sometimes', 'Very rarely', 'Never'), ordered = TRUE)
table(GA_data$sm_disagreement_issues) # Display a frequency table for 'sm_disagreement_issues'

# Create a numeric variable for 'sm_disagreement_issues'
GA_data$sm_disagreement_issues_n = as.numeric(GA_data$q36_c) 
summary(GA_data$sm_disagreement_issues_n) # Provide a summary for 'sm_disagreement_issues_numeric' (Numeric)

#Network Breadth
# Perform the recode for 'network_breadth'
GA_data$network_breadth = recode(GA_data$q37, '1="1-2"; 2="3-5"; 3="6-8"; 4="8-12"; 5="More than 12"')
GA_data$network_breadth = factor(GA_data$network_breadth, levels = c('1-2', '3-5', '6-8', '8-12', 'More than 12'), ordered = TRUE)
table(GA_data$network_breadth) # Display a frequency table for 'network_breadth'

# Create a numeric variable for 'network_breadth'
GA_data$network_breadth_n = as.numeric(GA_data$q37) 
summary(GA_data$network_breadth_n) # Provide a summary for 'network_breadth_numeric' (Numeric)

#Social Media Political Activity
# Perform the recode for 'sm_engage_friends'
GA_data$sm_engage_friends = recode(GA_data$q38_a, '1="A few times a day"; 2="Once a day"; 3="Several times a week"; 4="Once a week"; 5="Less often"; 6="Never"')
GA_data$sm_engage_friends = factor(GA_data$sm_engage_friends, levels = c('A few times a day', 'Once a day', 'Several times a week', 'Once a week', 'Less often', 'Never'), ordered = TRUE)
table(GA_data$sm_engage_friends) # Display a frequency table for 'sm_engage_friends'

# Create a numeric variable for 'sm_engage_friends'
GA_data$sm_engage_friends_n = as.numeric(GA_data$q38_a) 
summary(GA_data$sm_engage_friends_n) # Provide a summary for 'sm_engage_friends_n' (Numeric)

# Perform the recode for 'sm_engage_groups'
GA_data$sm_engage_groups = recode(GA_data$q38_b, '1="A few times a day"; 2="Once a day"; 3="Several times a week"; 4="Once a week"; 5="Less often"; 6="Never"')
GA_data$sm_engage_groups = factor(GA_data$sm_engage_groups, levels = c('A few times a day', 'Once a day', 'Several times a week', 'Once a week', 'Less often', 'Never'), ordered = TRUE)
table(GA_data$sm_engage_groups) # Display a frequency table for 'sm_engage_groups'

# Create a numeric variable for 'sm_engage_groups'
GA_data$sm_engage_groups_n = as.numeric(GA_data$q38_b) 
summary(GA_data$sm_engage_groups_n) # Provide a summary for 'sm_engage_groups_n' (Numeric)
# Perform the recode for 'sm_engage_post'
GA_data$sm_engage_post = recode(GA_data$q38_c, '1="A few times a day"; 2="Once a day"; 3="Several times a week"; 4="Once a week"; 5="Less often"; 6="Never"')
GA_data$sm_engage_post = factor(GA_data$sm_engage_post, levels = c('A few times a day', 'Once a day', 'Several times a week', 'Once a week', 'Less often', 'Never'), ordered = TRUE)
table(GA_data$sm_engage_post) # Display a frequency table for 'sm_engage_post'

# Create a numeric variable for 'sm_engage_post'
GA_data$sm_engage_post_n = as.numeric(GA_data$q38_c) 
summary(GA_data$sm_engage_post_n) # Provide a summary for 'sm_engage_post_n' (Numeric)

# Perform the recode for 'sm_engage_critical'
GA_data$sm_engage_critical = recode(GA_data$q38_d, '1="A few times a day"; 2="Once a day"; 3="Several times a week"; 4="Once a week"; 5="Less often"; 6="Never"')
GA_data$sm_engage_critical = factor(GA_data$sm_engage_critical, levels = c('A few times a day', 'Once a day', 'Several times a week', 'Once a week', 'Less often', 'Never'), ordered = TRUE)
table(GA_data$sm_engage_critical) # Display a frequency table for 'sm_engage_critical'

# Create a numeric variable for 'sm_engage_critical'
GA_data$sm_engage_critical_n = as.numeric(GA_data$q38_d) 
summary(GA_data$sm_engage_critical_n) # Provide a summary for 'sm_engage_critical_n' (Numeric)

# Perform the recode for 'sm_engage_supportive'
GA_data$sm_engage_supportive = recode(GA_data$q38_e, '1="A few times a day"; 2="Once a day"; 3="Several times a week"; 4="Once a week"; 5="Less often"; 6="Never"')
GA_data$sm_engage_supportive = factor(GA_data$sm_engage_supportive, levels = c('A few times a day', 'Once a day', 'Several times a week', 'Once a week', 'Less often', 'Never'), ordered = TRUE)
table(GA_data$sm_engage_supportive) # Display a frequency table for 'sm_engage_supportive'

# Create a numeric variable for 'sm_engage_supportive'
GA_data$sm_engage_supportive_n = as.numeric(GA_data$q38_e) 
summary(GA_data$sm_engage_supportive_n) # Provide a summary for 'sm_engage_supportive_n' (Numeric)

# Perform the recode for 'sm_engage_offline'
GA_data$sm_engage_offline = recode(GA_data$q38_f, '1="A few times a day"; 2="Once a day"; 3="Several times a week"; 4="Once a week"; 5="Less often"; 6="Never"')
GA_data$sm_engage_offline = factor(GA_data$sm_engage_offline, levels = c('A few times a day', 'Once a day', 'Several times a week', 'Once a week', 'Less often', 'Never'), ordered = TRUE)
table(GA_data$sm_engage_offline) # Display a frequency table for 'sm_engage_offline'

# Create a numeric variable for 'sm_engage_offline'
GA_data$sm_engage_offline_n = as.numeric(GA_data$q38_f) 
summary(GA_data$sm_engage_offline_n) # Provide a summary for 'sm_engage_offline_n' (Numeric)

#Attiudes about Global Power and Ukraine
# Perform the recode for 'ukraine_china'
GA_data$ukraine_china = recode(GA_data$q39_a, '1="Significantly more favorable"; 2="Somewhat more favorable"; 3="Unchanged"; 4="Somewhat more unfavorable"; 5="Significantly more unfavorable"')
GA_data$ukraine_china = factor(GA_data$ukraine_china, levels = c('Significantly more favorable', 'Somewhat more favorable', 'Unchanged', 'Somewhat more unfavorable', 'Significantly more unfavorable'), ordered = TRUE)
table(GA_data$ukraine_china) # Display a frequency table for 'ukraine_china'

# Create a numeric variable for 'ukraine_china'
GA_data$ukraine_china_n = as.numeric(GA_data$q39_a) 
summary(GA_data$ukraine_china_n) # Provide a summary for 'ukraine_china_n' (Numeric)

# Perform the recode for 'ukraine_russia'
GA_data$ukraine_russia = recode(GA_data$q39_b, '1="Significantly more favorable"; 2="Somewhat more favorable"; 3="Unchanged"; 4="Somewhat more unfavorable"; 5="Significantly more unfavorable"')
GA_data$ukraine_russia = factor(GA_data$ukraine_russia, levels = c('Significantly more favorable', 'Somewhat more favorable', 'Unchanged', 'Somewhat more unfavorable', 'Significantly more unfavorable'), ordered = TRUE)
table(GA_data$ukraine_russia) # Display a frequency table for 'ukraine_russia'

# Create a numeric variable for 'ukraine_russia'
GA_data$ukraine_russia_n = as.numeric(GA_data$q39_b) 
summary(GA_data$ukraine_russia_n) # Provide a summary for 'ukraine_russia_n' (Numeric)

# Perform the recode for 'ukraine_US'
GA_data$ukraine_US = recode(GA_data$q39_c, '1="Significantly more favorable"; 2="Somewhat more favorable"; 3="Unchanged"; 4="Somewhat more unfavorable"; 5="Significantly more unfavorable"')
GA_data$ukraine_US = factor(GA_data$ukraine_US, levels = c('Significantly more favorable', 'Somewhat more favorable', 'Unchanged', 'Somewhat more unfavorable', 'Significantly more unfavorable'), ordered = TRUE)
table(GA_data$ukraine_US) # Display a frequency table for 'ukraine_US'

# Create a numeric variable for 'ukraine_US'
GA_data$ukraine_US_n = as.numeric(GA_data$q39_c) 
summary(GA_data$ukraine_US_n) # Provide a summary for 'ukraine_US_n' (Numeric)

##Attention to Ukraine and Who's Responsible
# Perform the recode for 'ukraine_attention'
GA_data$ukraine_attention = recode(GA_data$q40_b, '1="Strongly agree"; 2="Somewhat agree"; 3="Neither disagree nor agree"; 4="Somewhat disagree"; 5="Strongly disagree"')
GA_data$ukraine_attention = factor(GA_data$ukraine_attention, levels = c('Strongly agree', 'Somewhat agree', 'Neither disagree nor agree', 'Somewhat disagree', 'Strongly disagree'), ordered = TRUE)
table(GA_data$ukraine_attention) # Display a frequency table for 'ukraine_attention'

# Create a numeric variable for 'ukraine_attention'
GA_data$ukraine_attention_n = as.numeric(GA_data$q40_b) 
summary(GA_data$ukraine_attention_n) # Provide a summary for 'ukraine_attention_n' (Numeric)

# Perform the recode for 'ukraine_russia_responsible'
GA_data$ukraine_russia_responsible = recode(GA_data$q40_c, '1="Strongly agree"; 2="Somewhat agree"; 3="Neither disagree nor agree"; 4="Somewhat disagree"; 5="Strongly disagree"')
GA_data$ukraine_russia_responsible = factor(GA_data$ukraine_russia_responsible, levels = c('Strongly agree', 'Somewhat agree', 'Neither disagree nor agree', 'Somewhat disagree', 'Strongly disagree'), ordered = TRUE)
table(GA_data$ukraine_russia_responsible) # Display a frequency table for 'ukraine_russia_responsible'

# Create a numeric variable for 'ukraine_russia_responsible'
GA_data$ukraine_russia_responsible_n = as.numeric(GA_data$q40_c) 
summary(GA_data$ukraine_russia_responsible_n) # Provide a summary for 'ukraine_russia_responsible_n' (Numeric)

# Perform the recode for 'ukraine_ukraine_responsible'
GA_data$ukraine_ukraine_responsible = recode(GA_data$q40_d, '1="Strongly agree"; 2="Somewhat agree"; 3="Neither disagree nor agree"; 4="Somewhat disagree"; 5="Strongly disagree"')
GA_data$ukraine_ukraine_responsible = factor(GA_data$ukraine_ukraine_responsible, levels = c('Strongly agree', 'Somewhat agree', 'Neither disagree nor agree', 'Somewhat disagree', 'Strongly disagree'), ordered = TRUE)
table(GA_data$ukraine_ukraine_responsible) # Display a frequency table for 'ukraine_ukraine_responsible'

# Create a numeric variable for 'ukraine_ukraine_responsible'
GA_data$ukraine_ukraine_responsible_n = as.numeric(GA_data$q40_d) 
summary(GA_data$ukraine_ukraine_responsible_n) # Provide a summary for 'ukraine_ukraine_responsible_n' (Numeric)

# Perform the recode for 'ukraine_US_responsible'
GA_data$ukraine_US_responsible = recode(GA_data$q40_e, '1="Strongly agree"; 2="Somewhat agree"; 3="Neither disagree nor agree"; 4="Somewhat disagree"; 5="Strongly disagree"')
GA_data$ukraine_US_responsible = factor(GA_data$ukraine_US_responsible, levels = c('Strongly agree', 'Somewhat agree', 'Neither disagree nor agree', 'Somewhat disagree', 'Strongly disagree'), ordered = TRUE)
table(GA_data$ukraine_US_responsible) # Display a frequency table for 'ukraine_US_responsible'

# Create a numeric variable for 'ukraine_US_responsible'
GA_data$ukraine_US_responsible_n = as.numeric(GA_data$q40_e) 
summary(GA_data$ukraine_US_responsible_n) # Provide a summary for 'ukraine_US_responsible_n' (Numeric)

#Creating Dummy Variables from all Relevant GA Variables############################################################################################
library(car)

# internet_use
GA_data$internet_use_No = recode(GA_data$internet_use, '"No"=1; NA=NA; else=0')
GA_data$internet_use_Yes = recode(GA_data$internet_use, '"Yes"=1; NA=NA; else=0')

# ethnicity
GA_data$ethnicity_Kazakh = recode(GA_data$ethnicity, '"Kazakh"=1; NA=NA; else=0')
GA_data$ethnicity_Russian = recode(GA_data$ethnicity, '"Russian"=1; NA=NA; else=0')
GA_data$ethnicity_Uzbek = recode(GA_data$ethnicity, '"Uzbek"=1; NA=NA; else=0')
GA_data$ethnicity_Tajik = recode(GA_data$ethnicity, '"Tajik"=1; NA=NA; else=0')
GA_data$ethnicity_Kyrgyz = recode(GA_data$ethnicity, '"Kyrgyz"=1; NA=NA; else=0')
GA_data$ethnicity_Other = recode(GA_data$ethnicity, '"Other"=1; NA=NA; else=0')

# urbanicity
GA_data$urbanicity_City = recode(GA_data$urbanicity, '"City"=1; NA=NA; else=0')
GA_data$urbanicity_Village = recode(GA_data$urbanicity, '"Village"=1; NA=NA; else=0')

# gender
GA_data$gender_Female = recode(GA_data$gender, '"Female"=1; NA=NA; else=0')
GA_data$gender_Male = recode(GA_data$gender, '"Male"=1; NA=NA; else=0')

# important_issue_first
GA_data$important_issue_first_Political_instability = recode(GA_data$important_issue_first, "'Political instability'=1; NA=NA; else=0")
GA_data$important_issue_first_Unemployment = recode(GA_data$important_issue_first, "'Unemployment'=1; NA=NA; else=0")
GA_data$important_issue_first_Inflation_Prices = recode(GA_data$important_issue_first, "'Inflation Prices'=1; NA=NA; else=0")
GA_data$important_issue_first_Wages_pensions = recode(GA_data$important_issue_first, "'Wages pensions'=1; NA=NA; else=0")
GA_data$important_issue_first_Taxes = recode(GA_data$important_issue_first, "'Taxes'=1; NA=NA; else=0")
GA_data$important_issue_first_Access_to_basic_needs = recode(GA_data$important_issue_first, "'Access to basic needs'=1; NA=NA; else=0")
GA_data$important_issue_first_General_economic_situation = recode(GA_data$important_issue_first, "'General economic situation'=1; NA=NA; else=0")
GA_data$important_issue_first_Deterioration_of_the_quality_and_access_to_education = recode(GA_data$important_issue_first, "'Deterioration of the quality and access to education'=1; NA=NA; else=0")
GA_data$important_issue_first_Construction_of_houses_residential_premises = recode(GA_data$important_issue_first, "'Construction of houses residential premises'=1; NA=NA; else=0")
GA_data$important_issue_first_Terrorism = recode(GA_data$important_issue_first, "'Terrorism'=1; NA=NA; else=0")
GA_data$important_issue_first_War_conflict_in_other_countries = recode(GA_data$important_issue_first, "'War conflict in other countries'=1; NA=NA; else=0")
GA_data$important_issue_first_Increase_in_crime_violence_concerns_about_personal_safety = recode(GA_data$important_issue_first, "'Increase in crime violence concerns about personal safety'=1; NA=NA; else=0")
GA_data$important_issue_first_Corruption = recode(GA_data$important_issue_first, "'Corruption'=1; NA=NA; else=0")
GA_data$important_issue_first_Lack_of_opportunities = recode(GA_data$important_issue_first, "'Lack of opportunities'=1; NA=NA; else=0")
GA_data$important_issue_first_Discrimination_ethnic_or_religious_tensions = recode(GA_data$important_issue_first, "'Discrimination ethnic or religious tensions'=1; NA=NA; else=0")
GA_data$important_issue_first_Infrastructure = recode(GA_data$important_issue_first, "'Infrastructure'=1; NA=NA; else=0")
GA_data$important_issue_first_Deterioration_of_the_environment = recode(GA_data$important_issue_first, "'Deterioration of the environment'=1; NA=NA; else=0")
GA_data$important_issue_first_Unresolved_territorial_conflicts = recode(GA_data$important_issue_first, "'Unresolved territorial conflicts'=1; NA=NA; else=0")
GA_data$important_issue_first_Emigration = recode(GA_data$important_issue_first, "'Emigration'=1; NA=NA; else=0")

# important_issue_second
GA_data$important_issue_second_Political_instability = recode(GA_data$important_issue_second, "'Political instability'=1; NA=NA; else=0")
GA_data$important_issue_second_Unemployment = recode(GA_data$important_issue_second, "'Unemployment'=1; NA=NA; else=0")
GA_data$important_issue_second_Inflation_Prices = recode(GA_data$important_issue_second, "'Inflation Prices'=1; NA=NA; else=0")
GA_data$important_issue_second_Wages_pensions = recode(GA_data$important_issue_second, "'Wages pensions'=1; NA=NA; else=0")
GA_data$important_issue_second_Taxes = recode(GA_data$important_issue_second, "'Taxes'=1; NA=NA; else=0")
GA_data$important_issue_second_Access_to_basic_needs = recode(GA_data$important_issue_second, "'Access to basic needs'=1; NA=NA; else=0")
GA_data$important_issue_second_General_economic_situation = recode(GA_data$important_issue_second, "'General economic situation'=1; NA=NA; else=0")
GA_data$important_issue_second_Deterioration_of_the_quality_and_access_to_education = recode(GA_data$important_issue_second, "'Deterioration of the quality and access to education'=1; NA=NA; else=0")
GA_data$important_issue_second_Construction_of_houses_residential_premises = recode(GA_data$important_issue_second, "'Construction of houses residential premises'=1; NA=NA; else=0")
GA_data$important_issue_second_Terrorism = recode(GA_data$important_issue_second, "'Terrorism'=1; NA=NA; else=0")
GA_data$important_issue_second_War_conflict_in_other_countries = recode(GA_data$important_issue_second, "'War conflict in other countries'=1; NA=NA; else=0")
GA_data$important_issue_second_Increase_in_crime_violence_concerns_about_personal_safety = recode(GA_data$important_issue_second, "'Increase in crime violence concerns about personal safety'=1; NA=NA; else=0")
GA_data$important_issue_second_Corruption = recode(GA_data$important_issue_second, "'Corruption'=1; NA=NA; else=0")
GA_data$important_issue_second_Lack_of_opportunities = recode(GA_data$important_issue_second, "'Lack of opportunities'=1; NA=NA; else=0")
GA_data$important_issue_second_Discrimination_ethnic_or_religious_tensions = recode(GA_data$important_issue_second, "'Discrimination ethnic or religious tensions'=1; NA=NA; else=0")
GA_data$important_issue_second_Infrastructure = recode(GA_data$important_issue_second, "'Infrastructure'=1; NA=NA; else=0")
GA_data$important_issue_second_Deterioration_of_the_environment = recode(GA_data$important_issue_second, "'Deterioration of the environment'=1; NA=NA; else=0")
GA_data$important_issue_second_Unresolved_territorial_conflicts = recode(GA_data$important_issue_second, "'Unresolved territorial conflicts'=1; NA=NA; else=0")
GA_data$important_issue_second_Emigration = recode(GA_data$important_issue_second, "'Emigration'=1; NA=NA; else=0")

# sm_use
GA_data$sm_use_No = recode(GA_data$sm_use, '"No"=1; NA=NA; else=0')
GA_data$sm_use_Yes = recode(GA_data$sm_use, '"Yes"=1; NA=NA; else=0')

# vpn_use
GA_data$vpn_use_No = recode(GA_data$vpn_use, '"No"=1; NA=NA; else=0')
GA_data$vpn_use_Yes = recode(GA_data$vpn_use, '"Yes"=1; NA=NA; else=0')

# paid_posters
# Assuming 'I never heard of them', 'I have heard of them but I have not seen any posts from them', 'I think I have seen posts from them' as categories
GA_data$paid_posters_never_heard = recode(GA_data$paid_posters, '"I never heard of them"=1; NA=NA; else=0')
GA_data$paid_posters_heard_not_seen = recode(GA_data$paid_posters, '"I have heard of them but I have not seen any posts from them"=1; NA=NA; else=0')
GA_data$paid_posters_seen_posts = recode(GA_data$paid_posters, '"I think I have seen posts from them"=1; NA=NA; else=0')

# Frequency tables for internet use
table(GA_data$internet_use_No)
table(GA_data$internet_use_Yes)

# Frequency tables for ethnicity
table(GA_data$ethnicity_Kazakh)
table(GA_data$ethnicity_Russian)
table(GA_data$ethnicity_Uzbek)
table(GA_data$ethnicity_Tajik)
table(GA_data$ethnicity_Kyrgyz)
table(GA_data$ethnicity_Other)

# Frequency tables for urbanicity
table(GA_data$urbanicity_City)
table(GA_data$urbanicity_Village)

# Frequency tables for gender
table(GA_data$gender_Female)
table(GA_data$gender_Male)

# Frequency tables for the first important issue
table(GA_data$important_issue_first_Political_instability)
table(GA_data$important_issue_first_Unemployment)
table(GA_data$important_issue_first_Inflation_Prices)
table(GA_data$important_issue_first_Wages_pensions)
table(GA_data$important_issue_first_Taxes)
table(GA_data$important_issue_first_Access_to_basic_needs)
table(GA_data$important_issue_first_General_economic_situation)
table(GA_data$important_issue_first_Deterioration_of_the_quality_and_access_to_education)
table(GA_data$important_issue_first_Construction_of_houses_residential_premises)
table(GA_data$important_issue_first_Terrorism)
table(GA_data$important_issue_first_War_conflict_in_other_countries)
table(GA_data$important_issue_first_Increase_in_crime_violence_concerns_about_personal_safety)
table(GA_data$important_issue_first_Corruption)
table(GA_data$important_issue_first_Lack_of_opportunities)
table(GA_data$important_issue_first_Discrimination_ethnic_or_religious_tensions)
table(GA_data$important_issue_first_Infrastructure)
table(GA_data$important_issue_first_Deterioration_of_the_environment)
table(GA_data$important_issue_first_Unresolved_territorial_conflicts)
table(GA_data$important_issue_first_Emigration)

# Frequency tables for the first important issue
table(GA_data$important_issue_second_Political_instability)
table(GA_data$important_issue_second_Unemployment)
table(GA_data$important_issue_second_Inflation_Prices)
table(GA_data$important_issue_second_Wages_pensions)
table(GA_data$important_issue_second_Taxes)
table(GA_data$important_issue_second_Access_to_basic_needs)
table(GA_data$important_issue_second_General_economic_situation)
table(GA_data$important_issue_second_Deterioration_of_the_quality_and_access_to_education)
table(GA_data$important_issue_second_Construction_of_houses_residential_premises)
table(GA_data$important_issue_second_Terrorism)
table(GA_data$important_issue_second_War_conflict_in_other_countries)
table(GA_data$important_issue_second_Increase_in_crime_violence_concerns_about_personal_safety)
table(GA_data$important_issue_second_Corruption)
table(GA_data$important_issue_second_Lack_of_opportunities)
table(GA_data$important_issue_second_Discrimination_ethnic_or_religious_tensions)
table(GA_data$important_issue_second_Infrastructure)
table(GA_data$important_issue_second_Deterioration_of_the_environment)
table(GA_data$important_issue_second_Unresolved_territorial_conflicts)
table(GA_data$important_issue_second_Emigration)
# Frequency tables for social media use
table(GA_data$sm_use_No)
table(GA_data$sm_use_Yes)

# Frequency tables for VPN use
table(GA_data$vpn_use_No)
table(GA_data$vpn_use_Yes)

# Frequency tables for paid posters
table(GA_data$paid_posters_never_heard)
table(GA_data$paid_posters_heard_not_seen)
table(GA_data$paid_posters_seen_posts)

#Scaling
library(scales)
GA_data$age_n_sc = rescale(GA_data$age_n, to = c(0, 1))
GA_data$edu_n_sc = rescale(GA_data$edu_n, to = c(0, 1))
GA_data$inc_n_sc = rescale(GA_data$inc_n, to = c(0, 1))
GA_data$pol_interest_n_sc = rescale(GA_data$pol_interest_n, to = c(1, 0))
GA_data$trust_central_n_sc = rescale(GA_data$trust_central_n, to = c(1, 0))
GA_data$trust_local_n_sc = rescale(GA_data$trust_local_n, to = c(1, 0))
GA_data$trust_russia_n_sc = rescale(GA_data$trust_russia_n, to = c(1, 0))
GA_data$trust_US_n_sc = rescale(GA_data$trust_US_n, to = c(1, 0))
GA_data$trust_china_n_sc = rescale(GA_data$trust_china_n, to = c(1, 0))
GA_data$trust_EU_n_sc = rescale(GA_data$trust_EU_n, to = c(1, 0))
GA_data$system_capable_n_sc = rescale(GA_data$system_capable_n, to = c(1, 0))
GA_data$system_proud_n_sc = rescale(GA_data$system_proud_n, to = c(1, 0))
GA_data$system_deserves_n_sc = rescale(GA_data$system_deserves_n, to = c(1, 0))
GA_data$system_live_n_sc = rescale(GA_data$system_live_n, to = c(1, 0))
GA_data$system_hurdles_participate_n_sc = rescale(GA_data$system_hurdles_participate_n, to = c(1, 0))
GA_data$tracking_central_n_sc = rescale(GA_data$tracking_central_n, to = c(0, 1))
GA_data$tracking_local_n_sc = rescale(GA_data$tracking_local_n, to = c(0, 1))
GA_data$tracking_companies_n_sc = rescale(GA_data$tracking_companies_n, to = c(0, 1))
GA_data$democracy_elections_n_sc = rescale(GA_data$democracy_elections_n, to = c(1, 0))
GA_data$democracy_speech_n_sc = rescale(GA_data$democracy_speech_n, to = c(1, 0))
GA_data$democracy_oversight_n_sc = rescale(GA_data$democracy_oversight_n, to = c(1, 0))
GA_data$democracy_organize_n_sc = rescale(GA_data$democracy_organize_n, to = c(1, 0))
GA_data$democracy_press_n_sc = rescale(GA_data$democracy_press_n, to = c(1, 0))
GA_data$democracy_parties_n_sc = rescale(GA_data$democracy_parties_n, to = c(1, 0))
GA_data$democracy_protests_n_sc = rescale(GA_data$democracy_protests_n, to = c(1, 0))
GA_data$democracy_courts_n_sc = rescale(GA_data$democracy_courts_n, to = c(1, 0))
GA_data$facebook_n_sc = rescale(GA_data$facebook_n, to = c(0, 1))
GA_data$vkontakte_n_sc = rescale(GA_data$vkontakte_n, to = c(0, 1))
GA_data$instagram_n_sc = rescale(GA_data$instagram_n, to = c(0, 1))
GA_data$tiktok_n_sc = rescale(GA_data$tiktok_n, to = c(0, 1))
GA_data$twitter_n_sc = rescale(GA_data$twitter_n, to = c(0, 1))
GA_data$youtube_n_sc = rescale(GA_data$youtube_n, to = c(0, 1))
GA_data$whatsapp_n_sc = rescale(GA_data$whatsapp_n, to = c(0, 1))
GA_data$telegram_n_sc = rescale(GA_data$telegram_n, to = c(0, 1))
GA_data$pol_news_tv_n_sc = rescale(GA_data$pol_news_tv_n, to = c(1, 0))
GA_data$pol_news_facebook_n_sc = rescale(GA_data$pol_news_facebook_n, to = c(1, 0))
GA_data$pol_news_vkontakte_n_sc = rescale(GA_data$pol_news_vkontakte_n, to = c(1, 0))
GA_data$pol_news_tiktok_n_sc = rescale(GA_data$pol_news_tiktok_n, to = c(1, 0))
GA_data$pol_news_twitter_n_sc = rescale(GA_data$pol_news_twitter_n, to = c(1, 0))
GA_data$pol_news_odnoklassniki_n_sc = rescale(GA_data$pol_news_odnoklassniki_n, to = c(1, 0))
GA_data$trust_state_n_sc = rescale(GA_data$trust_state_n, to = c(1, 0))
GA_data$trust_russian_media_n_sc = rescale(GA_data$trust_russian_media_n, to = c(1, 0))
GA_data$trust_internet_n_sc = rescale(GA_data$trust_internet_n, to = c(1, 0))
GA_data$trust_facebook_n_sc = rescale(GA_data$trust_facebook_n, to = c(1, 0))
GA_data$trust_vkontakte_n_sc = rescale(GA_data$trust_vkontakte_n, to = c(1, 0))
GA_data$trust_western_n_sc = rescale(GA_data$trust_western_n, to = c(1, 0))
GA_data$news_balance_n_sc = rescale(GA_data$news_balance_n, to = c(0, 1))
GA_data$sm_critical_local_n_sc = rescale(GA_data$sm_critical_local_n, to = c(1, 0))
GA_data$sm_critical_central_n_sc = rescale(GA_data$sm_critical_central_n, to = c(1, 0))
GA_data$sm_positive_local_n_sc = rescale(GA_data$sm_positive_local_n, to = c(1, 0))
GA_data$sm_positive_central_n_sc = rescale(GA_data$sm_positive_central_n, to = c(1, 0))
GA_data$tv_critical_local_n_sc = rescale(GA_data$tv_critical_local_n, to = c(1, 0))
GA_data$tv_critical_central_n_sc = rescale(GA_data$tv_critical_central_n, to = c(1, 0))
GA_data$clickable_state_n_sc = rescale(GA_data$clickable_state_n, to = c(1, 0))
GA_data$clickable_russian_n_sc = rescale(GA_data$clickable_russian_n, to = c(1, 0))
GA_data$AI_n_sc = rescale(GA_data$AI_n, to = c(1, 0))
GA_data$echo_chamber_n_sc = rescale(GA_data$echo_chamber_n, to = c(0, 1))
GA_data$paid_posters_percentage_n_sc = rescale(GA_data$paid_posters_percentage_n, to = c(0, 1))
GA_data$avoidance_blocking_n_sc = rescale(GA_data$avoidance_blocking_n, to = c(1, 0))
GA_data$avoidance_unfriending_n_sc = rescale(GA_data$avoidance_unfriending_n, to = c(1, 0))
GA_data$avoidance_leaving_group_n_sc = rescale(GA_data$avoidance_leaving_group_n, to = c(1, 0))
GA_data$avoidance_unsubscribing_n_sc = rescale(GA_data$avoidance_unsubscribing_n, to = c(1, 0))
GA_data$sm_disagreement_politics_n_sc = rescale(GA_data$sm_disagreement_politics_n, to = c(0, 1))
GA_data$sm_disagreement_news_n_sc = rescale(GA_data$sm_disagreement_news_n, to = c(0, 1))
GA_data$sm_disagreement_issues_n_sc = rescale(GA_data$sm_disagreement_issues_n, to = c(0, 1))
GA_data$network_breadth_n_sc = rescale(GA_data$network_breadth_n, to = c(0, 1))
GA_data$sm_engage_friends_n_sc = rescale(GA_data$sm_engage_friends_n, to = c(1, 0))
GA_data$sm_engage_groups_n_sc = rescale(GA_data$sm_engage_groups_n, to = c(1, 0))
GA_data$sm_engage_post_n_sc = rescale(GA_data$sm_engage_post_n, to = c(1, 0))
GA_data$sm_engage_critical_n_sc = rescale(GA_data$sm_engage_critical_n, to = c(1, 0))
GA_data$sm_engage_supportive_sc = rescale(GA_data$sm_engage_supportive_n, to = c(1, 0))
GA_data$sm_engage_offline_sc = rescale(GA_data$sm_engage_offline_n, to = c(1, 0))
GA_data$ukraine_china_sc = rescale(GA_data$ukraine_china_n, to = c(1, 0))
GA_data$ukraine_russia_sc = rescale(GA_data$ukraine_russia_n, to = c(1, 0))
GA_data$ukraine_US_sc = rescale(GA_data$ukraine_US_n, to = c(1, 0))
GA_data$ukraine_attention_sc = rescale(GA_data$ukraine_attention_n, to = c(1, 0))
GA_data$ukraine_russia_responsible_sc = rescale(GA_data$ukraine_russia_responsible_n, to = c(1, 0))
GA_data$ukraine_ukraine_responsible_sc = rescale(GA_data$ukraine_ukraine_responsible_n, to = c(1, 0))
GA_data$ukraine_US_responsible_sc = rescale(GA_data$ukraine_US_responsible_n, to = c(1, 0))

#Tajikistan
#Recoding all Variables##########################################################################
##############################################################################################
#################################################################################################
library(dplyr)
library(car)

# Internet Use Factor
TJ_data$internet_use = recode(TJ_data$q2, "1 = 'Yes'; 2 = 'No'")
TJ_data$internet_use = factor(TJ_data$internet_use)
TJ_data$internet_use = addNA(TJ_data$internet_use)
table(TJ_data$internet_use)  # Frequency table for internet_use

# Age Numeric
summary(TJ_data$q3)
TJ_data$age_n = TJ_data$q3
summary(TJ_data$age_n)  # Summary for age_n

# Age Collapsed Factor
table(TJ_data$q3a)
TJ_data$age_collapsed = recode(TJ_data$q3a, "1 = '18 - 24'; 2 = '25 - 34'; 3 = '35 - 44'; 4 = '45 - 54'; 5 = '55 - 64'; 6 = '65+'")
TJ_data$age_collapsed = factor(TJ_data$age_collapsed, ordered = TRUE)
table(TJ_data$age_collapsed)  # Frequency table for age_collapsed

# Ethnicity Factor
table(TJ_data$q4)
TJ_data$ethnicity = recode(TJ_data$q4, "2 = 'Russian'; 3 = 'Uzbek'; 4 = 'Tajik'; 5 = 'Kyrgyz'; 6 = 'Other'")
TJ_data$ethnicity = factor(TJ_data$ethnicity)
table(TJ_data$ethnicity)  # Frequency table for ethnicity

# Urbanicity Factor
TJ_data$urbanicity = recode(TJ_data$q5, "1 = 'City'; 2 = 'Village'")
TJ_data$urbanicity = factor(TJ_data$urbanicity)
TJ_data$urbanicity = addNA(TJ_data$urbanicity)
table(TJ_data$urbanicity)  # Frequency table for urbanicity

# Gender Factor
TJ_data$gender = recode(TJ_data$q6, "1 = 'Female'; 2 = 'Male'")
TJ_data$gender = factor(TJ_data$gender)
table(TJ_data$gender)  # Frequency table for gender

# Education Factor
TJ_data$edu = recode(TJ_data$q7, "1 = 'No education'; 2 = 'Primary education'; 3 = 'Basic secondary education'; 4 = 'General secondary education'; 5 = 'Complete vocational education'; 6 = 'Incomplete higher education'; 7 = 'Complete higher education'")
TJ_data$edu = factor(TJ_data$edu, ordered = TRUE)
table(TJ_data$edu)  # Frequency table for edu

# Education Numeric
TJ_data$edu_n = TJ_data$q7
summary(TJ_data$edu_n)  # Summary for edu_n

# Income Factor
TJ_data$inc = recode(TJ_data$q8, "1 = 'Less than 70,001'; 2 = '70,001 - 150,000'; 3 = '150,001 – 200,000'; 4 = '200,001 – 250,000'; 5 = '250,001 – 300,000'; 6 = '300,001 – 350,000'; 7 = '350,001 – 400,000'; 8 = '400,001 – 450,000'; 9 = '450,001 – 500,000'; 10 = '500,001 – 550,000'; 11 = '550,001 – 600,000'; 12 = 'More than 600,000'")
TJ_data$inc = factor(TJ_data$inc, levels = c('Less than 70,001', '70,001 - 150,000', '150,001 – 200,000', '200,001 – 250,000', '250,001 – 300,000', '300,001 – 350,000', '350,001 – 400,000', '400,001 – 450,000', '450,001 – 500,000', '500,001 – 550,000', '550,001 – 600,000', 'More than 600,000'), ordered = TRUE)
table(TJ_data$inc)  # Frequency table for Income Factor

# Income Numeric
TJ_data$inc_n = TJ_data$q8
summary(TJ_data$inc_n)  # Summary for Income Numeric

# Political Interest Factor
TJ_data$pol_interest = recode(TJ_data$q9, "1 = 'Very interested'; 2 = 'Somewhat interested'; 3 = 'Somewhat uninterested'; 4 = 'Very uninterested'")
TJ_data$pol_interest = factor(TJ_data$pol_interest, levels = c('Very interested','Somewhat interested','Somewhat uninterested','Very uninterested'), ordered = TRUE)
table(TJ_data$pol_interest)  # Frequency table for Political Interest Factor

# Political Interest Numeric
TJ_data$pol_interest_n = TJ_data$q9
summary(TJ_data$pol_interest_n)  # Summary for Political Interest Numeric

# Discuss Politics Factor Yes/No
TJ_data$pol_discuss_dummy = recode(TJ_data$q10a, "1 = 'Yes'; 2 = 'No'")
TJ_data$pol_discuss_dummy = factor(TJ_data$pol_discuss_dummy, levels = c('Yes','No'), ordered = FALSE)
table(TJ_data$pol_discuss_dummy)  # Frequency table for Discuss Politics Factor

# Discuss Politics Factor Ordinal
TJ_data$pol_discuss = recode(TJ_data$q10b, "1 = 'A few times a day'; 2 = 'Once a day'; 3 = 'Three to five days a week'; 4 = 'Once a week'; 5 = 'Less often than once a week'; 6 = 'Never'")
TJ_data$pol_discuss = factor(TJ_data$pol_discuss, levels = c('A few times a day', 'Once a day', 'Three to five days a week', 'Once a week', 'Less often than once a week', 'Never'), ordered = TRUE)
table(KZ_data$pol_discuss)  # Frequency table for Discuss Politics Factor

#Discuss Politics Numeric
TJ_data$pol_discuss_n = TJ_data$q10
summary(TJ_data$pol_discuss_n)  # Summary for Discuss Politics Numeric

# Disagree when Discussing Politics
TJ_data$pol_disagree = recode(TJ_data$q11, "1 = 'Most of the time'; 2 = 'Some of the time'; 3 = 'Rarely'; 4 = 'Never'")
TJ_data$pol_disagree = factor(TJ_data$pol_disagree, levels = c('Most of the time', 'Some of the time', 'Rarely', 'Never'), ordered = TRUE)
table(TJ_data$pol_disagree)  # Frequency table for Disagree when Discussing Politics

#Disagree Politics Numeric
TJ_data$pol_disagree_n = TJ_data$q11
summary(TJ_data$pol_disagree_n)  # Summary for Disagree Politics Numeric

# Trust in Russia
TJ_data$trust_russia = recode(TJ_data$q12_c, "1 = 'A Great Deal'; 2 = 'Quite a Lot of Trust'; 3 = 'Not Very Much Trust'; 4 = 'None at all'")
TJ_data$trust_russia = factor(TJ_data$trust_russia, levels = c('A Great Deal', 'Quite a Lot of Trust', 'Not Very Much Trust', 'None at all'), ordered = TRUE)
table(TJ_data$trust_russia)  # Frequency table for Trust in Russia

# Trust in the US
TJ_data$trust_US = recode(TJ_data$q12_d, "1 = 'A Great Deal'; 2 = 'Quite a Lot of Trust'; 3 = 'Not Very Much Trust'; 4 = 'None at all'")
TJ_data$trust_US = factor(TJ_data$trust_US, levels = c('A Great Deal', 'Quite a Lot of Trust', 'Not Very Much Trust', 'None at all'), ordered = TRUE)
table(TJ_data$trust_US)  # Frequency table for Trust in the US

# Trust in China
TJ_data$trust_china = recode(TJ_data$q12_e, "1 = 'A Great Deal'; 2 = 'Quite a Lot of Trust'; 3 = 'Not Very Much Trust'; 4 = 'None at all'")
TJ_data$trust_china = factor(TJ_data$trust_china, levels = c('A Great Deal', 'Quite a Lot of Trust', 'Not Very Much Trust', 'None at all'), ordered = TRUE)
table(TJ_data$trust_china)  # Frequency table for Trust in China

# Trust in the EU
TJ_data$trust_EU = recode(TJ_data$q12_f, "1 = 'A Great Deal'; 2 = 'Quite a Lot of Trust'; 3 = 'Not Very Much Trust'; 4 = 'None at all'")
TJ_data$trust_EU  = factor(TJ_data$trust_EU , levels = c('A Great Deal', 'Quite a Lot of Trust', 'Not Very Much Trust', 'None at all'), ordered = TRUE)
table(TJ_data$trust_EU)  # Frequency table for trust_EU 

# Trust Russia Numeric
TJ_data$trust_russia_n = TJ_data$q12_c
summary(TJ_data$trust_russia_n)

# Trust US Numeric
TJ_data$trust_US_n = TJ_data$q12_d
summary(TJ_data$trust_US_n)

# Trust China Numeric
TJ_data$trust_china_n = TJ_data$q12_e
summary(TJ_data$trust_china_n)

# Trust EU Numeric
TJ_data$trust_EU_n = TJ_data$q12_f
summary(TJ_data$trust_EU_n)

# Most Important Issue First Mention
TJ_data$important_issue_first = recode(TJ_data$q13_a, "1 = 'Political instability'; 2 = 'Unemployment'; 3 = 'Inflation Prices'; 4 = 'Wages pensions'; 5 = 'Taxes'; 6 = 'Access to basic needs'; 7 = 'General economic situation'; 8 = 'Deterioration of the quality and access to education'; 9 = 'Construction of houses residential premises'; 10 = 'Terrorism'; 11 = 'War conflict in other countries'; 12 = 'Increase in crime violence concerns about personal safety'; 13 = 'Corruption'; 14 = 'Lack of opportunities'; 15 = 'Discrimination ethnic or religious tensions'; 16 = 'Infrastructure'; 17 = 'Deterioration of the environment'; 18 = 'Unresolved territorial conflicts'; 19 = 'Emigration'")
TJ_data$important_issue_first = factor(TJ_data$important_issue_first)
TJ_data$important_issue_first = addNA(TJ_data$important_issue_first)
table(TJ_data$important_issue_first)

# Most Important Issue Second Mention
TJ_data$important_issue_second = recode(TJ_data$q13_b, "1 = 'Political instability'; 2 = 'Unemployment'; 3 = 'Inflation Prices'; 4 = 'Wages pensions'; 5 = 'Taxes'; 6 = 'Access to basic needs'; 7 = 'General economic situation'; 8 = 'Deterioration of the quality and access to education'; 9 = 'Construction of houses residential premises'; 10 = 'Terrorism'; 11 = 'War conflict in other countries'; 12 = 'Increase in crime violence concerns about personal safety'; 13 = 'Corruption'; 14 = 'Lack of opportunities'; 15 = 'Discrimination ethnic or religious tensions'; 16 = 'Infrastructure'; 17 = 'Deterioration of the environment'; 18 = 'Unresolved territorial conflicts'; 19 = 'Emigration'")
TJ_data$important_issue_second = factor(TJ_data$important_issue_second)
TJ_data$important_issue_second = addNA(TJ_data$important_issue_second)
table(TJ_data$important_issue_second)

# System Approval Variables Factor
# Perform the recode
TJ_data$system_capable = recode(TJ_data$q14_a, "1 = 'Strongly agree'; 2 = 'Somewhat agree'; 3 = 'Neither disagree nor agree'; 4 = 'Somewhat disagree'; 5 = 'Strongly disagree'")
TJ_data$system_capable = factor(TJ_data$system_capable, levels = c('Strongly agree', 'Somewhat agree', 'Neither disagree nor agree', 'Somewhat disagree', 'Strongly disagree'), ordered = TRUE)
table(TJ_data$system_capable)  # Frequency table for System Capable

# Numeric variable for System Capable
TJ_data$system_capable_n = as.numeric(TJ_data$q14_a)
summary(TJ_data$system_capable_n)  # Summary for System Capable (Numeric)

TJ_data$system_proud = recode(TJ_data$q14_b, "1 = 'Strongly agree'; 2 = 'Somewhat agree'; 3 = 'Neither disagree nor agree'; 4 = 'Somewhat disagree'; 5 = 'Strongly disagree'")
TJ_data$system_proud = factor(TJ_data$system_proud, levels = c('Strongly agree', 'Somewhat agree', 'Neither disagree nor agree', 'Somewhat disagree', 'Strongly disagree'), ordered = TRUE)
table(TJ_data$system_proud)  # Frequency table for System Proud

# Numeric variable for System Proud
TJ_data$system_proud_n = as.numeric(TJ_data$q14_b)
summary(TJ_data$system_proud_n)  # Summary for System Proud (Numeric)

TJ_data$system_deserves = recode(TJ_data$q14_c, "1 = 'Strongly agree'; 2 = 'Somewhat agree'; 3 = 'Neither disagree nor agree'; 4 = 'Somewhat disagree'; 5 = 'Strongly disagree'")
TJ_data$system_deserves = factor(TJ_data$system_deserves, levels = c('Strongly agree', 'Somewhat agree', 'Neither disagree nor agree', 'Somewhat disagree', 'Strongly disagree'), ordered = TRUE)
table(TJ_data$system_deserves)  # Frequency table for System Deserves

# Numeric variable for System Deserves
TJ_data$system_deserves_n = as.numeric(TJ_data$q14_c)
summary(TJ_data$system_deserves_n)  # Summary for System Deserves (Numeric)

TJ_data$system_live = recode(TJ_data$q14_d, "1 = 'Strongly agree'; 2 = 'Somewhat agree'; 3 = 'Neither disagree nor agree'; 4 = 'Somewhat disagree'; 5 = 'Strongly disagree'")
TJ_data$system_live = factor(TJ_data$system_live, levels = c('Strongly agree', 'Somewhat agree', 'Neither disagree nor agree', 'Somewhat disagree', 'Strongly disagree'), ordered = TRUE)
table(TJ_data$system_live)  # Frequency table for System Live

# Numeric variable for System Live
TJ_data$system_live_n = as.numeric(TJ_data$q14_d)
summary(TJ_data$system_live_n)  # Summary for System Live (Numeric)

TJ_data$system_hurdles_participate = recode(TJ_data$q14_e, "1 = 'Strongly agree'; 2 = 'Somewhat agree'; 3 = 'Neither disagree nor agree'; 4 = 'Somewhat disagree'; 5 = 'Strongly disagree'")
TJ_data$system_hurdles_participate = factor(TJ_data$system_hurdles_participate, levels = c('Strongly agree', 'Somewhat agree', 'Neither disagree nor agree', 'Somewhat disagree', 'Strongly disagree'), ordered = TRUE)
table(TJ_data$system_hurdles_participate)  # Frequency table for System Hurdles for Participation

# Numeric variable for System Hurdles for Participation
TJ_data$system_hurdles_participate_n = as.numeric(TJ_data$q14_e)
summary(TJ_data$system_hurdles_participate_n)  # Summary for System Hurdles for Participation (Numeric)

# Participate Variables Factor
# Perform the recode
TJ_data$participate_meeting = recode(TJ_data$q15_c, "1 = 'Yes'; 2 = 'No'")
TJ_data$participate_meeting = factor(TJ_data$participate_meeting, levels = c('Yes', 'No'), ordered = FALSE)
table(TJ_data$participate_meeting)  # Frequency table for Participate Meeting

# Numeric variable for Participate Meeting
TJ_data$participate_meeting_n = as.numeric(TJ_data$q15_c)
summary(TJ_data$participate_meeting_n)  # Summary for Participate Meeting (Numeric)

TJ_data$participate_volunteer = recode(TJ_data$q15_d, "1 = 'Yes'; 2 = 'No'")
TJ_data$participate_volunteer = factor(TJ_data$participate_volunteer, levels = c('Yes', 'No'), ordered = FALSE)
table(TJ_data$participate_volunteer)  # Frequency table for Participate Volunteer

# Numeric variable for Participate Volunteer
TJ_data$participate_volunteer_n = as.numeric(TJ_data$q15_d)
summary(TJ_data$participate_volunteer_n)  # Summary for Participate Volunteer (Numeric)

TJ_data$participate_member = recode(TJ_data$q15_e, "1 = 'Yes'; 2 = 'No'")
TJ_data$participate_member = factor(TJ_data$participate_member, levels = c('Yes', 'No'), ordered = FALSE)
table(TJ_data$participate_member)  # Frequency table for Participate Member

# Numeric variable for Participate Member
TJ_data$participate_member_n = as.numeric(TJ_data$q15_e)
summary(TJ_data$participate_member_n)  # Summary for Participate Member (Numeric)

TJ_data$participate_community = recode(TJ_data$q15_f, "1 = 'Yes'; 2 = 'No'")
TJ_data$participate_community = factor(TJ_data$participate_community, levels = c('Yes', 'No'), ordered = FALSE)
table(TJ_data$participate_community)  # Frequency table for Participate Community

# Numeric variable for Participate Community
TJ_data$participate_community_n = as.numeric(TJ_data$q15_f)
summary(TJ_data$participate_community_n)  # Summary for Participate Community (Numeric)

TJ_data$participate_contact = recode(TJ_data$q15_g, "1 = 'Yes'; 2 = 'No'")
TJ_data$participate_contact = factor(TJ_data$participate_contact, levels = c('Yes', 'No'), ordered = FALSE)
table(TJ_data$participate_contact)  # Frequency table for Participate Contact

# Numeric variable for Participate Contact
TJ_data$participate_contact_n = as.numeric(TJ_data$q15_g)
summary(TJ_data$participate_contact_n)  # Summary for Participate Contact (Numeric)

TJ_data$participate_vote = recode(TJ_data$q16, "1 = 'Yes'; 2 = 'No'")
TJ_data$participate_vote = factor(TJ_data$participate_vote, levels = c('Yes', 'No'), ordered = FALSE)
table(TJ_data$participate_vote)  # Frequency table for Participate Vote

# Numeric variable for Participate Vote
TJ_data$participate_vote_n = as.numeric(TJ_data$q16)
summary(TJ_data$participate_vote_n)  # Summary for Participate Vote (Numeric)

# Digital Tracking Variables Factor
# Perform the recode
TJ_data$tracking_central = recode(TJ_data$q18_a, "1 = 'Not Comfortable at all'; 2 = '2'; 3 = '3'; 4 = '4'; 5 = 'Very Comfortable'")
TJ_data$tracking_central = factor(TJ_data$tracking_central, levels = c('Not Comfortable at all', '2', '3', '4', 'Very Comfortable'), ordered = TRUE)
table(TJ_data$tracking_central)  # Frequency table for Tracking Central

# Numeric variable for Tracking Central
TJ_data$tracking_central_n = as.numeric(TJ_data$q18_a)
summary(TJ_data$tracking_central_n)  # Summary for Tracking Central (Numeric)

TJ_data$tracking_local = recode(TJ_data$q18_b, "1 = 'Not Comfortable at all'; 2 = '2'; 3 = '3'; 4 = '4'; 5 = 'Very Comfortable'")
TJ_data$tracking_local = factor(TJ_data$tracking_local, levels = c('Not Comfortable at all', '2', '3', '4', 'Very Comfortable'), ordered = TRUE)
table(TJ_data$tracking_local)  # Frequency table for Tracking Local

# Numeric variable for Tracking Local
TJ_data$tracking_local_n = as.numeric(TJ_data$q18_b)
summary(TJ_data$tracking_local_n)  # Summary for Tracking Local (Numeric)

TJ_data$tracking_companies = recode(TJ_data$q18_c, "1 = 'Not Comfortable at all'; 2 = '2'; 3 = '3'; 4 = '4'; 5 = 'Very Comfortable'")
TJ_data$tracking_companies = factor(TJ_data$tracking_companies, levels = c('Not Comfortable at all', '2', '3', '4', 'Very Comfortable'), ordered = TRUE)
table(TJ_data$tracking_companies)  # Frequency table for Tracking Companies

# Numeric variable for Tracking Companies
TJ_data$tracking_companies_n = as.numeric(TJ_data$q18_c)
summary(TJ_data$tracking_companies_n)  # Summary for Tracking Companies (Numeric)

# Democracy Variables Factor
# Perform the recode
TJ_data$democracy_elections = recode(TJ_data$q20_a, "1 = 'Strongly agree'; 2 = 'Somewhat agree'; 3 = 'Neither disagree nor agree'; 4 = 'Somewhat disagree'; 5 = 'Strongly disagree'")
TJ_data$democracy_elections = factor(TJ_data$democracy_elections, levels = c('Strongly agree', 'Somewhat agree', 'Neither disagree nor agree', 'Somewhat disagree', 'Strongly disagree'), ordered = TRUE)
table(TJ_data$democracy_elections)  # Frequency table for Democracy Elections

# Numeric variable for Democracy Elections
TJ_data$democracy_elections_n = as.numeric(TJ_data$q20_a)
summary(TJ_data$democracy_elections_n)  # Summary for Democracy Elections (Numeric)

TJ_data$democracy_press = recode(TJ_data$q20_e, "1 = 'Strongly agree'; 2 = 'Somewhat agree'; 3 = 'Neither disagree nor agree'; 4 = 'Somewhat disagree'; 5 = 'Strongly disagree'")
TJ_data$democracy_press = factor(TJ_data$democracy_press, levels = c('Strongly agree', 'Somewhat agree', 'Neither disagree nor agree', 'Somewhat disagree', 'Strongly disagree'), ordered = TRUE)
table(TJ_data$democracy_press)  # Frequency table for Democracy Press

# Numeric variable for Democracy Press
TJ_data$democracy_press_n = as.numeric(TJ_data$q20_e)
summary(TJ_data$democracy_press_n)  # Summary for Democracy Press (Numeric)

TJ_data$satisfied_participation = recode(TJ_data$q21, "1 = 'Completely Satisfied'; 2 = 'Rather Satisfied'; 3 = 'Rather Dissatisfied'; 4 = 'Completely Dissatisfied'")
TJ_data$satisfied_participation = factor(TJ_data$satisfied_participation, levels = c('Completely Satisfied','Rather Satisfied','Rather Dissatisfied','Completely Dissatisfied'), ordered = TRUE)
table(TJ_data$satisfied_participation)  # Frequency table for satisfied_participation

# Numeric variable for satisfied_participation
TJ_data$satisfied_participation_n = as.numeric(TJ_data$q21)
summary(TJ_data$satisfied_participation_n)  # Summary for satisfied_participation (Numeric)

#Social Media Use
# Perform the recode for the factor variable
TJ_data$sm_use = recode(TJ_data$q22, "1 = 'Yes'; 2 = 'No'")
TJ_data$sm_use = factor(TJ_data$sm_use, levels = c('Yes', 'No'), ordered = TRUE)
TJ_data$sm_use = addNA(TJ_data$sm_use)
table(TJ_data$sm_use)  # Frequency table for Social Media Use

#Social Media Platform Variables Factor
#Perform the recode
TJ_data$facebook = recode(TJ_data$q23_a, '0="Do not use this social media"; 1="0-1"; 2="1-2"; 3="2-3"; 4="3-4"; 5="4-5"; 6="5-6"; 7="6-7"; 8="7-8"; 9="8-9"; 10="More than 9"')
TJ_data$facebook = factor(TJ_data$facebook, levels = c('Do not use this social media', '0-1', '1-2', '2-3', '3-4', '4-5', '5-6', '6-7', '7-8', '8-9', 'More than 9'), ordered = TRUE)
table(TJ_data$facebook) # Display a frequency table for 'facebook'

# Create a numeric variable for facebook
TJ_data$facebook_n = as.numeric(TJ_data$q23_a) 
summary(TJ_data$facebook_n) # Provide a summary for 'facebook_n' (Numeric)

TJ_data$vkontakte = recode(TJ_data$q23_b, '0="Do not use this social media"; 1="0-1"; 2="1-2"; 3="2-3"; 4="3-4"; 5="4-5"; 6="5-6"; 7="6-7"; 8="7-8"; 9="8-9"; 10="More than 9"')
TJ_data$vkontakte = factor(TJ_data$vkontakte, levels = c('Do not use this social media', '0-1', '1-2', '2-3', '3-4', '4-5', '5-6', '6-7', '7-8', '8-9', 'More than 9'), ordered = TRUE)
table(TJ_data$vkontakte) # Display a frequency table for 'vkontakte'

# Create a numeric variable for vkontakte
TJ_data$vkontakte_n = as.numeric(TJ_data$q23_b) 
summary(TJ_data$vkontakte_n) # Provide a summary for 'vkontakte_n' (Numeric)

TJ_data$instagram = recode(TJ_data$q23_c, '0="Do not use this social media"; 1="0-1"; 2="1-2"; 3="2-3"; 4="3-4"; 5="4-5"; 6="5-6"; 7="6-7"; 8="7-8"; 9="8-9"; 10="More than 9"')
TJ_data$instagram = factor(TJ_data$instagram, levels = c('Do not use this social media', '0-1', '1-2', '2-3', '3-4', '4-5', '5-6', '6-7', '7-8', '8-9', 'More than 9'), ordered = TRUE)
table(TJ_data$instagram) # Display a frequency table for 'instagram'

# Create a numeric variable for instagram
TJ_data$instagram_n = as.numeric(TJ_data$q23_c) 
summary(TJ_data$instagram_n) # Provide a summary for 'instagram_n' (Numeric)


TJ_data$tiktok = recode(TJ_data$q23_d, '0="Do not use this social media"; 1="0-1"; 2="1-2"; 3="2-3"; 4="3-4"; 5="4-5"; 6="5-6"; 7="6-7"; 8="7-8"; 9="8-9"; 10="More than 9"')
TJ_data$tiktok = factor(TJ_data$tiktok, levels = c('Do not use this social media', '0-1', '1-2', '2-3', '3-4', '4-5', '5-6', '6-7', '7-8', '8-9', 'More than 9'), ordered = TRUE)
table(TJ_data$tiktok) # Display a frequency table for 'tiktok'

# Create a numeric variable for tiktok
TJ_data$tiktok_n = as.numeric(TJ_data$q23_d) 
summary(TJ_data$tiktok_n) # Provide a summary for 'tiktok_n' (Numeric)

TJ_data$twitter = recode(TJ_data$q23_e, '0="Do not use this social media"; 1="0-1"; 2="1-2"; 3="2-3"; 4="3-4"; 5="4-5"; 6="5-6"; 7="6-7"; 8="7-8"; 9="8-9"; 10="More than 9"')
TJ_data$twitter = factor(TJ_data$twitter, levels = c('Do not use this social media', '0-1', '1-2', '2-3', '3-4', '4-5', '5-6', '6-7', '7-8', '8-9', 'More than 9'), ordered = TRUE)
table(TJ_data$twitter) # Display a frequency table for 'twitter'

# Create a numeric variable for twitter
TJ_data$twitter_n = as.numeric(TJ_data$q23_e) 
summary(TJ_data$twitter_n) # Provide a summary for 'twitter_n' (Numeric)

TJ_data$youtube = recode(TJ_data$q23_f, '0="Do not use this social media"; 1="0-1"; 2="1-2"; 3="2-3"; 4="3-4"; 5="4-5"; 6="5-6"; 7="6-7"; 8="7-8"; 9="8-9"; 10="More than 9"')
TJ_data$youtube = factor(TJ_data$youtube, levels = c('Do not use this social media', '0-1', '1-2', '2-3', '3-4', '4-5', '5-6', '6-7', '7-8', '8-9', 'More than 9'), ordered = TRUE)
table(TJ_data$youtube) # Display a frequency table for 'youtube'

# Create a numeric variable for youtube
TJ_data$youtube_n = as.numeric(TJ_data$q23_f) 
summary(TJ_data$youtube_n) # Provide a summary for 'youtube_n' (Numeric)

TJ_data$whatsapp = recode(TJ_data$q23_g, '0="Do not use this social media"; 1="0-1"; 2="1-2"; 3="2-3"; 4="3-4"; 5="4-5"; 6="5-6"; 7="6-7"; 8="7-8"; 9="8-9"; 10="More than 9"')
TJ_data$whatsapp = factor(TJ_data$whatsapp, levels = c('Do not use this social media', '0-1', '1-2', '2-3', '3-4', '4-5', '5-6', '6-7', '7-8', '8-9', 'More than 9'), ordered = TRUE)
table(TJ_data$whatsapp) # Display a frequency table for 'whatsapp'

# Create a numeric variable for whatsapp
TJ_data$whatsapp_n = as.numeric(TJ_data$q23_g) 
summary(TJ_data$whatsapp_n) # Provide a summary for 'whatsapp_n' (Numeric)

TJ_data$telegram = recode(TJ_data$q23_h, '0="Do not use this social media"; 1="0-1"; 2="1-2"; 3="2-3"; 4="3-4"; 5="4-5"; 6="5-6"; 7="6-7"; 8="7-8"; 9="8-9"; 10="More than 9"')
TJ_data$telegram = factor(TJ_data$telegram, levels = c('Do not use this social media', '0-1', '1-2', '2-3', '3-4', '4-5', '5-6', '6-7', '7-8', '8-9', 'More than 9'), ordered = TRUE)
table(TJ_data$telegram) # Display a frequency table for 'telegram'

# Create a numeric variable for whatsapp
TJ_data$telegram_n = as.numeric(TJ_data$q23_h) 
summary(TJ_data$telegram_n) # Provide a summary for 'telegram_n' (Numeric)

# Perform the recode for 'pol_news_tv'
TJ_data$pol_news_tv = recode(TJ_data$q24_a, '1="A few times a day"; 2="Once a day"; 3="Three to five days a week"; 4="Once a week"; 5="Less often than once a week"; 6="Never"')
TJ_data$pol_news_tv = factor(TJ_data$pol_news_tv, levels = c('A few times a day', 'Once a day', 'Three to five days a week', 'Once a week', 'Less often than once a week', 'Never'), ordered = TRUE)
table(TJ_data$pol_news_tv) # Display a frequency table for 'pol_news_tv'

# Create a numeric variable for 'pol_news_tv'
TJ_data$pol_news_tv_n = as.numeric(TJ_data$q24_a) 
summary(TJ_data$pol_news_tv_n) # Provide a summary for 'pol_news_tv_n' (Numeric)

# Perform the recode for 'pol_news_facebook'
TJ_data$pol_news_facebook = recode(TJ_data$q24_b, '1="A few times a day"; 2="Once a day"; 3="Three to five days a week"; 4="Once a week"; 5="Less often than once a week"; 6="Never"')
TJ_data$pol_news_facebook = factor(TJ_data$pol_news_facebook, levels = c('A few times a day', 'Once a day', 'Three to five days a week', 'Once a week', 'Less often than once a week', 'Never'), ordered = TRUE)
table(TJ_data$pol_news_facebook) # Display a frequency table for 'pol_news_facebook'

# Create a numeric variable for 'pol_news_facebook'
TJ_data$pol_news_facebook_n = as.numeric(TJ_data$q24_b) 
summary(TJ_data$pol_news_facebook_n) # Provide a summary for 'pol_news_facebook_n' (Numeric)

# Perform the recode for 'pol_news_vkontakte'
TJ_data$pol_news_vkontakte = recode(TJ_data$q24_c, '1="A few times a day"; 2="Once a day"; 3="Three to five days a week"; 4="Once a week"; 5="Less often than once a week"; 6="Never"')
TJ_data$pol_news_vkontakte = factor(TJ_data$pol_news_vkontakte, levels = c('A few times a day', 'Once a day', 'Three to five days a week', 'Once a week', 'Less often than once a week', 'Never'), ordered = TRUE)
table(TJ_data$pol_news_vkontakte) # Display a frequency table for 'pol_news_vkontakte'

# Create a numeric variable for 'pol_news_vkontakte'
TJ_data$pol_news_vkontakte_n = as.numeric(TJ_data$q24_c) 
summary(TJ_data$pol_news_vkontakte_n) # Provide a summary for 'pol_news_vkontakte_n' (Numeric)

# Perform the recode for 'pol_news_tiktok'
TJ_data$pol_news_tiktok = recode(TJ_data$q24_d, '1="A few times a day"; 2="Once a day"; 3="Three to five days a week"; 4="Once a week"; 5="Less often than once a week"; 6="Never"')
TJ_data$pol_news_tiktok = factor(TJ_data$pol_news_tiktok, levels = c('A few times a day', 'Once a day', 'Three to five days a week', 'Once a week', 'Less often than once a week', 'Never'), ordered = TRUE)
table(TJ_data$pol_news_tiktok) # Display a frequency table for 'pol_news_tiktok'

# Create a numeric variable for 'pol_news_tiktok'
TJ_data$pol_news_tiktok_n = as.numeric(TJ_data$q24_d) 
summary(TJ_data$pol_news_tiktok_n) # Provide a summary for 'pol_news_tiktok_n' (Numeric)

# Perform the recode for 'pol_news_twitter'
TJ_data$pol_news_twitter = recode(TJ_data$q24_e, '1="A few times a day"; 2="Once a day"; 3="Three to five days a week"; 4="Once a week"; 5="Less often than once a week"; 6="Never"')
TJ_data$pol_news_twitter = factor(TJ_data$pol_news_twitter, levels = c('A few times a day', 'Once a day', 'Three to five days a week', 'Once a week', 'Less often than once a week', 'Never'), ordered = TRUE)
table(TJ_data$pol_news_twitter) # Display a frequency table for 'pol_news_twitter'

# Create a numeric variable for 'pol_news_twitter'
TJ_data$pol_news_twitter_n = as.numeric(TJ_data$q24_e) 
summary(TJ_data$pol_news_twitter_n) # Provide a summary for 'pol_news_twitter_n' (Numeric)

# Perform the recode for 'pol_news_odnoklassniki'
TJ_data$pol_news_odnoklassniki = recode(TJ_data$q24_f, '1="A few times a day"; 2="Once a day"; 3="Three to five days a week"; 4="Once a week"; 5="Less often than once a week"; 6="Never"')
TJ_data$pol_news_odnoklassniki = factor(TJ_data$pol_news_odnoklassniki, levels = c('A few times a day', 'Once a day', 'Three to five days a week', 'Once a week', 'Less often than once a week', 'Never'), ordered = TRUE)
table(TJ_data$pol_news_odnoklassniki) # Display a frequency table for 'pol_news_odnoklassniki'

# Create a numeric variable for 'pol_news_odnoklassniki'
TJ_data$pol_news_odnoklassniki_n = as.numeric(TJ_data$q24_f) 
summary(TJ_data$pol_news_odnoklassniki_n) # Provide a summary for 'pol_news_odnoklassniki_n' (Numeric)

# Perform the recode for 'trust_state'
TJ_data$trust_state = recode(TJ_data$q25_a, '1="Very trustworthy"; 2="Somewhat trustworthy"; 3="Somewhat untrustworthy"; 4="Very untrustworthy"')
TJ_data$trust_state = factor(TJ_data$trust_state, levels = c('Very trustworthy', 'Somewhat trustworthy', 'Somewhat untrustworthy', 'Very untrustworthy'), ordered = TRUE)
table(TJ_data$trust_state) # Display a frequency table for 'trust_state'

# Create a numeric variable for 'trust_state'
TJ_data$trust_state_n = as.numeric(TJ_data$q25_a) 
summary(TJ_data$trust_state_n) # Provide a summary for 'trust_state_n' (Numeric)

# Perform the recode for 'trust_russian_media'
TJ_data$trust_russian_media = recode(TJ_data$q25_b, '1="Very trustworthy"; 2="Somewhat trustworthy"; 3="Somewhat untrustworthy"; 4="Very untrustworthy"')
TJ_data$trust_russian_media = factor(TJ_data$trust_russian_media, levels = c('Very trustworthy', 'Somewhat trustworthy', 'Somewhat untrustworthy', 'Very untrustworthy'), ordered = TRUE)
table(TJ_data$trust_russian_media) # Display a frequency table for 'trust_russian_media'

# Create a numeric variable for 'trust_russian_media'
TJ_data$trust_russian_media_n = as.numeric(TJ_data$q25_b) 
summary(TJ_data$trust_russian_media_n) # Provide a summary for 'trust_russian_media_n' (Numeric)

# Perform the recode for 'trust_internet'
TJ_data$trust_internet = recode(TJ_data$q25_c, '1="Very trustworthy"; 2="Somewhat trustworthy"; 3="Somewhat untrustworthy"; 4="Very untrustworthy"')
TJ_data$trust_internet = factor(TJ_data$trust_internet, levels = c('Very trustworthy', 'Somewhat trustworthy', 'Somewhat untrustworthy', 'Very untrustworthy'), ordered = TRUE)
table(TJ_data$trust_internet) # Display a frequency table for 'trust_internet'

# Create a numeric variable for 'trust_internet'
TJ_data$trust_internet_n = as.numeric(TJ_data$q25_c) 
summary(TJ_data$trust_internet_n) # Provide a summary for 'trust_internet_n' (Numeric)

# Perform the recode for 'trust_facebook'
TJ_data$trust_facebook = recode(TJ_data$q25_d, '1="Very trustworthy"; 2="Somewhat trustworthy"; 3="Somewhat untrustworthy"; 4="Very untrustworthy"')
TJ_data$trust_facebook = factor(TJ_data$trust_facebook, levels = c('Very trustworthy', 'Somewhat trustworthy', 'Somewhat untrustworthy', 'Very untrustworthy'), ordered = TRUE)
table(TJ_data$trust_facebook) # Display a frequency table for 'trust_facebook'

# Create a numeric variable for 'trust_parties'
TJ_data$trust_facebook_n = as.numeric(TJ_data$q25_d) 
summary(TJ_data$trust_facebook_n) # Provide a summary for 'trust_facebook_n' (Numeric)

# Perform the recode for 'trust_vkontakte'
TJ_data$trust_vkontakte = recode(TJ_data$q25_e, '1="Very trustworthy"; 2="Somewhat trustworthy"; 3="Somewhat untrustworthy"; 4="Very untrustworthy"')
TJ_data$trust_vkontakte = factor(TJ_data$trust_vkontakte, levels = c('Very trustworthy', 'Somewhat trustworthy', 'Somewhat untrustworthy', 'Very untrustworthy'), ordered = TRUE)
table(TJ_data$trust_vkontakte) # Display a frequency table for 'trust_vkontakte'

# Create a numeric variable for 'trust_vkontakte'
TJ_data$trust_vkontakte_n = as.numeric(TJ_data$q25_e) 
summary(TJ_data$trust_vkontakte_n) # Provide a summary for 'trust_vkontakte_n' (Numeric)

# Perform the recode for 'trust_western'
TJ_data$trust_western = recode(TJ_data$q25_f, '1="Very trustworthy"; 2="Somewhat trustworthy"; 3="Somewhat untrustworthy"; 4="Very untrustworthy"')
TJ_data$trust_western = factor(TJ_data$trust_western, levels = c('Very trustworthy', 'Somewhat trustworthy', 'Somewhat untrustworthy', 'Very untrustworthy'), ordered = TRUE)
table(TJ_data$trust_western) # Display a frequency table for 'trust_western'

# Create a numeric variable for 'trust_western'
TJ_data$trust_western_n = as.numeric(TJ_data$q25_f) 
summary(TJ_data$trust_western_n) # Provide a summary for 'trust_western_n' (Numeric)

#News Balance Factor
# Perform the recode for 'news_balance'
TJ_data$news_balance = recode(TJ_data$q26, '1="Only from traditional sources like television newspapers radio";2="Mostly from television newspapers radio but some from the internet social media";3="From an equal balance of television newspapers radio and the internet social media";4="Mostly from social media but some from television newspapers radio";5="Only from the internet social media"')
TJ_data$news_balance = factor(TJ_data$news_balance, levels = c('Only from traditional sources like television newspapers radio', 'Mostly from television newspapers radio but some from the internet social media', 'From an equal balance of television newspapers radio and the internet social media', 'Mostly from social media but some from television newspapers radio', 'Only from the internet social media'), ordered = TRUE)
TJ_data$news_balance = addNA(TJ_data$news_balance)
table(TJ_data$news_balance) # Display a frequency table for 'news_balance'

# Create a numeric variable for 'news_balance'
TJ_data$news_balance_n = as.numeric(TJ_data$q26)
summary(TJ_data$news_balance_n) # Provide a summary for 'news_balance_n' (Numeric)

# Tone of Social Media News about Government Factor
# Perform the recode for 'q27_c' (Tone of Social Media News about Government)
TJ_data$sm_positive_local = recode(TJ_data$q27_c, '1="Very often";2="Rather often";3="Occasionally";4="Never"')
TJ_data$sm_positive_local = factor(TJ_data$sm_positive_local, levels = c('Very often', 'Rather often', 'Occasionally', 'Never'), ordered = TRUE)
table(TJ_data$sm_positive_local) # Display a frequency table for 'sm_positive_local'

# Create a numeric variable for 'sm_positive_local'
TJ_data$sm_positive_local_n = as.numeric(TJ_data$q27_c)
summary(TJ_data$sm_positive_local_n) # Provide a summary for 'sm_positive_local_n' (Numeric)

# Perform the recode for 'q27_d' (Tone of Social Media News about Government)
TJ_data$sm_positive_central = recode(TJ_data$q27_d, '1="Very often";2="Rather often";3="Occasionally";4="Never"')
TJ_data$sm_positive_central = factor(TJ_data$sm_positive_central, levels = c('Very often', 'Rather often', 'Occasionally', 'Never'), ordered = TRUE)
table(TJ_data$sm_positive_central) # Display a frequency table for 'sm_positive_central'

# Create a numeric variable for 'sm_positive_central'
TJ_data$sm_positive_central_n = as.numeric(TJ_data$q27_d)
summary(TJ_data$sm_positive_central_n) # Provide a summary for 'sm_positive_central_n' (Numeric)

# TV Tone of News about Government Factor
# Perform the recode for 'q28_a' (TV Tone of News about Government)
TJ_data$tv_critical_local = recode(TJ_data$q28_a, '1="Very often";2="Rather often";3="Occasionally";4="Never"')
TJ_data$tv_critical_local = factor(TJ_data$tv_critical_local, levels = c('Very often', 'Rather often', 'Occasionally', 'Never'), ordered = TRUE)
table(TJ_data$tv_critical_local) # Display a frequency table for 'tv_critical_local'

# Create a numeric variable for 'tv_critical_local'
TJ_data$tv_critical_local_n = as.numeric(TJ_data$q28_a)
summary(TJ_data$tv_critical_local_n) # Provide a summary for 'tv_critical_local_n' (Numeric)

# Perform the recode for 'q28_b' (TV Tone of News about Government)
TJ_data$tv_critical_central = recode(TJ_data$q28_b, '1="Very often";2="Rather often";3="Occasionally";4="Never"')
TJ_data$tv_critical_central = factor(TJ_data$tv_critical_central, levels = c('Very often', 'Rather often', 'Occasionally', 'Never'), ordered = TRUE)
table(TJ_data$tv_critical_central) # Display a frequency table for 'tv_critical_central'

# Create a numeric variable for 'tv_critical_central'
TJ_data$tv_critical_central_n = as.numeric(TJ_data$q28_b)
summary(TJ_data$tv_critical_central_n) # Provide a summary for 'tv_critical_central_n' (Numeric)

# Clickable Links Factor
# Perform the recode for 'q29_a' (Clickable Links)
TJ_data$clickable_state = recode(TJ_data$q29_a, '1="Almost everyday";2="About once a week";3="A few times a month";4="About once a month";5="Never"')
TJ_data$clickable_state = factor(TJ_data$clickable_state, levels = c('Almost everyday', 'About once a week', 'A few times a month', 'About once a month', 'Never'), ordered = TRUE)
table(TJ_data$clickable_state) # Display a frequency table for 'clickable_state'

# Create a numeric variable for 'clickable_state'
TJ_data$clickable_state_n = as.numeric(TJ_data$q29_a)
summary(TJ_data$clickable_state_n) # Provide a summary for 'clickable_state_n' (Numeric)

# Perform the recode for 'q29_b' (Clickable Links)
TJ_data$clickable_russian = recode(TJ_data$q29_b, '1="Almost everyday";2="About once a week";3="A few times a month";4="About once a month";5="Never"')
TJ_data$clickable_russian = factor(TJ_data$clickable_russian, levels = c('Almost everyday', 'About once a week', 'A few times a month', 'About once a month', 'Never'), ordered = TRUE)
table(TJ_data$clickable_russian) # Display a frequency table for 'clickable_russian'

# Create a numeric variable for 'clickable_russian'
TJ_data$clickable_russian_n = as.numeric(TJ_data$q29_b)
summary(TJ_data$clickable_russian_n) # Provide a summary for 'clickable_russian_n' (Numeric)

# Artificial Intelligence Attitude Factor
# Perform the recode for 'AI' (Artificial Intelligence Attitude)
TJ_data$AI = recode(TJ_data$q30, '1="More excited than concerned";2="Equally concerned and excited";3="More concerned than excited";4="I do not know what artificial intelligence is"')
TJ_data$AI = factor(TJ_data$AI, levels = c('More excited than concerned', 'Equally concerned and excited', 'More concerned than excited', 'I do not know what artificial intelligence is'), ordered = TRUE)
table(TJ_data$AI) # Display a frequency table for 'AI'

# Artificial Intelligence Attitude Ordinal (coding I don't know as NA)
# Perform the recode
TJ_data$AI_ordinal = recode(TJ_data$q30, '1="More excited than concerned";2="Equally concerned and excited";3="More concerned than excited";4=NA')
TJ_data$AI_ordinal = factor(TJ_data$AI_ordinal, levels = c('More excited than concerned', 'Equally concerned and excited', 'More concerned than excited', 'I do not know what artificial intelligence is'), ordered = TRUE)
table(TJ_data$AI_ordinal) # Display a frequency table for 'AI_ordinal'

# Convert 'AI_ordinal' to an ordered factor/ordinal
TJ_data$AI_ordinal = factor(TJ_data$AI_ordinal, ordered = TRUE)

# Artificial Intelligence Attitude Numeric
# Perform the recode
TJ_data$AI_n = recode(TJ_data$q30, '1=1;2=2;3=3;4=NA')
summary(TJ_data$AI_n) # Provide a summary for 'AI_n' (Numeric)

#Echo Chamber Numeric
TJ_data$echo_chamber_n = as.numeric(TJ_data$q31)
summary(TJ_data$echo_chamber_n) # Provide a summary for 'echo_chamber_n' (Numeric)

# Awareness of Government Posters Factor
# Perform the recode for 'paid_posters'
TJ_data$paid_posters = recode(TJ_data$q32, '1="I never heard of them";2="I have heard of them but I have not seen any posts from them";3="I think I have seen posts from them"')
TJ_data$paid_posters = factor(TJ_data$paid_posters, levels = c('I never heard of them', 'I have heard of them but I have not seen any posts from them', 'I think I have seen posts from them'), ordered = TRUE)
TJ_data$paid_posters = addNA(TJ_data$paid_posters)
table(TJ_data$paid_posters) # Display a frequency table for 'paid_posters'

# Create a numeric variable for 'paid_posters'
TJ_data$paid_posters_n = as.numeric(TJ_data$q32)
summary(TJ_data$paid_posters_n) # Provide a summary for 'paid_posters_n' (Numeric)

# Awareness of Government Posters Ordinal (coding "I don't know" as NA)
# Perform the recode
TJ_data$paid_posters_ordinal = recode(TJ_data$q32, '1="I never heard of them";2="I have heard of them but I have not seen any posts from them";3="I think I have seen posts from them";4=NA')
TJ_data$paid_posters_ordinal = factor(TJ_data$paid_posters_ordinal, levels = c('I never heard of them', 'I have heard of them but I have not seen any posts from them', 'I think I have seen posts from them'), ordered = TRUE)
table(TJ_data$paid_posters_ordinal) # Display a frequency table for 'paid_posters_ordinal'

# Convert 'paid_posters_ordinal' to an ordered factor/ordinal
TJ_data$paid_posters_ordinal = factor(TJ_data$paid_posters_ordinal, ordered = TRUE)

# Awareness of Government Posters Numeric
# Perform the recode
TJ_data$paid_posters_n = recode(TJ_data$q32, '1=1;2=2;3=3;4=NA')
summary(TJ_data$paid_posters_n) # Provide a summary for 'paid_posters_n' (Numeric)

#Estimate of Percentage of Online Posts Paid by Government
TJ_data$paid_posters_percentage_n = as.numeric(TJ_data$q33)
summary(TJ_data$paid_posters_percentage_n) # Provide a summary for paid_posters_percentage_n (Numeric)

# VPN Use Factor
# Perform the recode for 'vpn_use'
TJ_data$vpn_use = recode(TJ_data$q34, '1="Yes";2="No"')
TJ_data$vpn_use = factor(TJ_data$vpn_use, levels = c('Yes', 'No'), ordered = TRUE)
TJ_data$vpn_use = addNA(TJ_data$vpn_use)
table(TJ_data$vpn_use) # Display a frequency table for 'vpn_use'

# Create a numeric variable for 'vpn_use'
TJ_data$vpn_use_n = as.numeric(TJ_data$q34)
summary(TJ_data$vpn_use_n) # Provide a summary for 'vpn_use_n' (Numeric)

#Selective Exposure
# Perform the recode for 'avoidance_blocking'
TJ_data$avoidance_blocking = recode(TJ_data$q35_a, '1="Often"; 2="Sometimes"; 3="Very rarely"; 4="Never"')
TJ_data$avoidance_blocking = factor(TJ_data$avoidance_blocking, levels = c('Often', 'Sometimes', 'Very rarely', 'Never'), ordered = TRUE)
table(TJ_data$avoidance_blocking) # Display a frequency table for 'avoidance_blocking'

# Create a numeric variable for 'avoidance_blocking'
TJ_data$avoidance_blocking_n = as.numeric(TJ_data$q35_a) 
summary(TJ_data$avoidance_blocking_n) # Provide a summary for 'avoidance_blocking_numeric' (Numeric)

# Perform the recode for 'avoidance_unfriending'
TJ_data$avoidance_unfriending = recode(TJ_data$q35_b, '1="Often"; 2="Sometimes"; 3="Very rarely"; 4="Never"')
TJ_data$avoidance_unfriending = factor(TJ_data$avoidance_unfriending, levels = c('Often', 'Sometimes', 'Very rarely', 'Never'), ordered = TRUE)
table(TJ_data$avoidance_unfriending) # Display a frequency table for 'avoidance_unfriending'

# Create a numeric variable for 'avoidance_unfriending'
TJ_data$avoidance_unfriending_n = as.numeric(TJ_data$q35_b) 
summary(TJ_data$avoidance_unfriending_n) # Provide a summary for 'avoidance_unfriending_numeric' (Numeric)

# Perform the recode for 'avoidance_leaving_group'
TJ_data$avoidance_leaving_group = recode(TJ_data$q35_c, '1="Often"; 2="Sometimes"; 3="Very rarely"; 4="Never"')
TJ_data$avoidance_leaving_group = factor(TJ_data$avoidance_leaving_group, levels = c('Often', 'Sometimes', 'Very rarely', 'Never'), ordered = TRUE)
table(TJ_data$avoidance_leaving_group) # Display a frequency table for 'avoidance_leaving_group'

# Create a numeric variable for 'avoidance_leaving_group'
TJ_data$avoidance_leaving_group_n = as.numeric(TJ_data$q35_c) 
summary(TJ_data$avoidance_leaving_group_n) # Provide a summary for 'avoidance_leaving_group_numeric' (Numeric)

# Perform the recode for 'avoidance_unsubscribing'
TJ_data$avoidance_unsubscribing = recode(TJ_data$q35_d, '1="Often"; 2="Sometimes"; 3="Very rarely"; 4="Never"')
TJ_data$avoidance_unsubscribing = factor(TJ_data$avoidance_unsubscribing, levels = c('Often', 'Sometimes', 'Very rarely', 'Never'), ordered = TRUE)
table(TJ_data$avoidance_unsubscribing) # Display a frequency table for 'avoidance_unsubscribing'

# Create a numeric variable for 'avoidance_unsubscribing'
TJ_data$avoidance_unsubscribing_n = as.numeric(TJ_data$q35_d) 
summary(TJ_data$avoidance_unsubscribing_n) # Provide a summary for 'avoidance_unsubscribing_numeric' (Numeric)

#Exposure to SM Disagreement
# Perform the recode for 'sm_disagreement_politics'
TJ_data$sm_disagreement_politics = recode(TJ_data$q36_a, '1="Often"; 2="Sometimes"; 3="Very rarely"; 4="Never"')
TJ_data$sm_disagreement_politics = factor(TJ_data$sm_disagreement_politics, levels = c('Often', 'Sometimes', 'Very rarely', 'Never'), ordered = TRUE)
table(TJ_data$sm_disagreement_politics) # Display a frequency table for 'sm_disagreement_politics'

# Create a numeric variable for 'sm_disagreement_politics'
TJ_data$sm_disagreement_politics_n = as.numeric(TJ_data$q36_a) 
summary(TJ_data$sm_disagreement_politics_n) # Provide a summary for 'sm_disagreement_politics_numeric' (Numeric)

# Perform the recode for 'sm_disagreement_news'
TJ_data$sm_disagreement_news = recode(TJ_data$q36_b, '1="Often"; 2="Sometimes"; 3="Very rarely"; 4="Never"')
TJ_data$sm_disagreement_news = factor(TJ_data$sm_disagreement_news, levels = c('Often', 'Sometimes', 'Very rarely', 'Never'), ordered = TRUE)
table(TJ_data$sm_disagreement_news) # Display a frequency table for 'sm_disagreement_news'

# Create a numeric variable for 'sm_disagreement_news'
TJ_data$sm_disagreement_news_n = as.numeric(TJ_data$q36_b) 
summary(TJ_data$sm_disagreement_news_n) # Provide a summary for 'sm_disagreement_news_numeric' (Numeric)

# Perform the recode for 'sm_disagreement_issues'
TJ_data$sm_disagreement_issues = recode(TJ_data$q36_c, '1="Often"; 2="Sometimes"; 3="Very rarely"; 4="Never"')
TJ_data$sm_disagreement_issues = factor(TJ_data$sm_disagreement_issues, levels = c('Often', 'Sometimes', 'Very rarely', 'Never'), ordered = TRUE)
table(TJ_data$sm_disagreement_issues) # Display a frequency table for 'sm_disagreement_issues'

# Create a numeric variable for 'sm_disagreement_issues'
TJ_data$sm_disagreement_issues_n = as.numeric(TJ_data$q36_c) 
summary(TJ_data$sm_disagreement_issues_n) # Provide a summary for 'sm_disagreement_issues_numeric' (Numeric)

#Network Breadth
# Perform the recode for 'network_breadth'
TJ_data$network_breadth = recode(TJ_data$q37, '1="1-2"; 2="3-5"; 3="6-8"; 4="8-12"; 5="More than 12"')
TJ_data$network_breadth = factor(TJ_data$network_breadth, levels = c('1-2', '3-5', '6-8', '8-12', 'More than 12'), ordered = TRUE)
table(TJ_data$network_breadth) # Display a frequency table for 'network_breadth'

# Create a numeric variable for 'network_breadth'
TJ_data$network_breadth_n = as.numeric(TJ_data$q37) 
summary(TJ_data$network_breadth_n) # Provide a summary for 'network_breadth_numeric' (Numeric)

#Social Media Political Activity
# Perform the recode for 'sm_engage_friends'
TJ_data$sm_engage_friends = recode(TJ_data$q38_a, '1="A few times a day"; 2="Once a day"; 3="Several times a week"; 4="Once a week"; 5="Less often"; 6="Never"')
TJ_data$sm_engage_friends = factor(TJ_data$sm_engage_friends, levels = c('A few times a day', 'Once a day', 'Several times a week', 'Once a week', 'Less often', 'Never'), ordered = TRUE)
table(TJ_data$sm_engage_friends) # Display a frequency table for 'sm_engage_friends'

# Create a numeric variable for 'sm_engage_friends'
TJ_data$sm_engage_friends_n = as.numeric(TJ_data$q38_a) 
summary(TJ_data$sm_engage_friends_n) # Provide a summary for 'sm_engage_friends_n' (Numeric)

# Perform the recode for 'sm_engage_groups'
TJ_data$sm_engage_groups = recode(TJ_data$q38_b, '1="A few times a day"; 2="Once a day"; 3="Several times a week"; 4="Once a week"; 5="Less often"; 6="Never"')
TJ_data$sm_engage_groups = factor(TJ_data$sm_engage_groups, levels = c('A few times a day', 'Once a day', 'Several times a week', 'Once a week', 'Less often', 'Never'), ordered = TRUE)
table(TJ_data$sm_engage_groups) # Display a frequency table for 'sm_engage_groups'

# Create a numeric variable for 'sm_engage_groups'
TJ_data$sm_engage_groups_n = as.numeric(TJ_data$q38_b) 
summary(TJ_data$sm_engage_groups_n) # Provide a summary for 'sm_engage_groups_n' (Numeric)

# Perform the recode for 'sm_engage_post'
TJ_data$sm_engage_post = recode(TJ_data$q38_c, '1="A few times a day"; 2="Once a day"; 3="Several times a week"; 4="Once a week"; 5="Less often"; 6="Never"')
TJ_data$sm_engage_post = factor(TJ_data$sm_engage_post, levels = c('A few times a day', 'Once a day', 'Several times a week', 'Once a week', 'Less often', 'Never'), ordered = TRUE)
table(TJ_data$sm_engage_post) # Display a frequency table for 'sm_engage_post'

# Create a numeric variable for 'sm_engage_post'
TJ_data$sm_engage_post_n = as.numeric(TJ_data$q38_c) 
summary(TJ_data$sm_engage_post_n) # Provide a summary for 'sm_engage_post_n' (Numeric)

# Perform the recode for 'sm_engage_offline'
TJ_data$sm_engage_offline = recode(TJ_data$q38_f, '1="A few times a day"; 2="Once a day"; 3="Several times a week"; 4="Once a week"; 5="Less often"; 6="Never"')
TJ_data$sm_engage_offline = factor(TJ_data$sm_engage_offline, levels = c('A few times a day', 'Once a day', 'Several times a week', 'Once a week', 'Less often', 'Never'), ordered = TRUE)
table(TJ_data$sm_engage_offline) # Display a frequency table for 'sm_engage_offline'

# Create a numeric variable for 'sm_engage_offline'
TJ_data$sm_engage_offline_n = as.numeric(TJ_data$q38_f) 
summary(TJ_data$sm_engage_offline_n) # Provide a summary for 'sm_engage_offline_n' (Numeric)

#Attitudes about Global Power
# Perform the recode for 'opinion_china'
TJ_data$opinion_china = recode(TJ_data$q39a_a, '1="Very favorable"; 2="Somewhat favorable"; 3="Neither"; 4="Somewhat unfavorable"; 5="Very unfavorable"')
TJ_data$opinion_china = factor(TJ_data$opinion_china, levels = c('Very favorable','Somewhat favorable','Neither','Somewhat unfavorable','Very unfavorable'), ordered = TRUE)
table(TJ_data$opinion_china) # Display a frequency table for 'opinion_china'

# Create a numeric variable for 'opinion_china'
TJ_data$opinion_china_n = as.numeric(TJ_data$q39a_a) 
summary(TJ_data$opinion_china_n) # Provide a summary for 'opinion_china_n' (Numeric)

# Perform the recode for 'opinion_russia'
TJ_data$opinion_russia = recode(TJ_data$q39a_b, '1="Very favorable"; 2="Somewhat favorable"; 3="Neither"; 4="Somewhat unfavorable"; 5="Very unfavorable"')
TJ_data$opinion_russia = factor(TJ_data$opinion_russia, levels = c('Very favorable','Somewhat favorable','Neither','Somewhat unfavorable','Very unfavorable'), ordered = TRUE)
table(TJ_data$opinion_russia) # Display a frequency table for 'opinion_russia'

# Create a numeric variable for 'opinion_russia'
TJ_data$opinion_russia_n = as.numeric(TJ_data$q39a_b) 
summary(TJ_data$opinion_russia_n) # Provide a summary for 'opinion_russia_n' (Numeric)

# Perform the recode for 'opinion_US'
TJ_data$opinion_US = recode(TJ_data$q39a_c, '1="Very favorable"; 2="Somewhat favorable"; 3="Neither"; 4="Somewhat unfavorable"; 5="Very unfavorable"')
TJ_data$opinion_US = factor(TJ_data$opinion_US, levels = c('Very favorable','Somewhat favorable','Neither','Somewhat unfavorable','Very unfavorable'), ordered = TRUE)
table(TJ_data$opinion_US) # Display a frequency table for 'opinion_US'

# Create a numeric variable for 'opinion_US'
TJ_data$opinion_US_n = as.numeric(TJ_data$q39a_c) 
summary(TJ_data$opinion_US_n) # Provide a summary for 'opinion_US_n' (Numeric)#Attiudes about Global Power Change
# Perform the recode for 'change_china'
TJ_data$change_china = recode(TJ_data$q39b_a, '1="Very favorable"; 2="Somewhat favorable"; 3="Neither"; 4="Somewhat unfavorable"; 5="Very unfavorable"')
TJ_data$change_china = factor(TJ_data$change_china, levels = c('Very favorable','Somewhat favorable','Neither','Somewhat unfavorable','Very unfavorable'), ordered = TRUE)
table(TJ_data$change_china) # Display a frequency table for 'change_china'

#Attitudes about Global Power Change Over Time
# Create a numeric variable for 'change_china'
TJ_data$change_china_n = as.numeric(TJ_data$q39b_a) 
summary(TJ_data$change_china_n) # Provide a summary for 'change_china_n' (Numeric)

# Perform the recode for 'change_russia'
TJ_data$change_russia = recode(TJ_data$q39b_b, '1="Very favorable"; 2="Somewhat favorable"; 3="Neither"; 4="Somewhat unfavorable"; 5="Very unfavorable"')
TJ_data$change_russia = factor(TJ_data$change_russia, levels = c('Very favorable','Somewhat favorable','Neither','Somewhat unfavorable','Very unfavorable'), ordered = TRUE)
table(TJ_data$change_russia) # Display a frequency table for 'change_russia'

# Create a numeric variable for 'change_russia'
TJ_data$change_russia_n = as.numeric(TJ_data$q39b_b) 
summary(TJ_data$change_russia_n) # Provide a summary for 'change_russia_n' (Numeric)

# Perform the recode for 'change_US'
TJ_data$change_US = recode(TJ_data$q39b_c, '1="Very favorable"; 2="Somewhat favorable"; 3="Neither"; 4="Somewhat unfavorable"; 5="Very unfavorable"')
TJ_data$change_US = factor(TJ_data$change_US, levels = c('Very favorable','Somewhat favorable','Neither','Somewhat unfavorable','Very unfavorable'), ordered = TRUE)
table(TJ_data$change_US) # Display a frequency table for 'change_US'

# Create a numeric variable for 'change_US'
TJ_data$change_US_n = as.numeric(TJ_data$q39b_c) 
summary(TJ_data$change_US_n) # Provide a summary for 'change_US_n' (Numeric)

#Creating Dummy Variables from all Relevant TJ Variables############################################################################################
library(car)

# internet_use
TJ_data$internet_use_No = recode(TJ_data$internet_use, '"No"=1; NA=NA; else=0')
TJ_data$internet_use_Yes = recode(TJ_data$internet_use, '"Yes"=1; NA=NA; else=0')

# ethnicity
TJ_data$ethnicity_Kazakh = recode(TJ_data$ethnicity, '"Kazakh"=1; NA=NA; else=0')
TJ_data$ethnicity_Russian = recode(TJ_data$ethnicity, '"Russian"=1; NA=NA; else=0')
TJ_data$ethnicity_Uzbek = recode(TJ_data$ethnicity, '"Uzbek"=1; NA=NA; else=0')
TJ_data$ethnicity_Tajik = recode(TJ_data$ethnicity, '"Tajik"=1; NA=NA; else=0')
TJ_data$ethnicity_Kyrgyz = recode(TJ_data$ethnicity, '"Kyrgyz"=1; NA=NA; else=0')
TJ_data$ethnicity_Other = recode(TJ_data$ethnicity, '"Other"=1; NA=NA; else=0')

# urbanicity
TJ_data$urbanicity_City = recode(TJ_data$urbanicity, '"City"=1; NA=NA; else=0')
TJ_data$urbanicity_Village = recode(TJ_data$urbanicity, '"Village"=1; NA=NA; else=0')

# gender
TJ_data$gender_Female = recode(TJ_data$gender, '"Female"=1; NA=NA; else=0')
TJ_data$gender_Male = recode(TJ_data$gender, '"Male"=1; NA=NA; else=0')

# important_issue_first
TJ_data$important_issue_first_Political_instability = recode(TJ_data$important_issue_first, "'Political instability'=1; NA=NA; else=0")
TJ_data$important_issue_first_Unemployment = recode(TJ_data$important_issue_first, "'Unemployment'=1; NA=NA; else=0")
TJ_data$important_issue_first_Inflation_Prices = recode(TJ_data$important_issue_first, "'Inflation Prices'=1; NA=NA; else=0")
TJ_data$important_issue_first_Wages_pensions = recode(TJ_data$important_issue_first, "'Wages pensions'=1; NA=NA; else=0")
TJ_data$important_issue_first_Taxes = recode(TJ_data$important_issue_first, "'Taxes'=1; NA=NA; else=0")
TJ_data$important_issue_first_Access_to_basic_needs = recode(TJ_data$important_issue_first, "'Access to basic needs'=1; NA=NA; else=0")
TJ_data$important_issue_first_General_economic_situation = recode(TJ_data$important_issue_first, "'General economic situation'=1; NA=NA; else=0")
TJ_data$important_issue_first_Deterioration_of_the_quality_and_access_to_education = recode(TJ_data$important_issue_first, "'Deterioration of the quality and access to education'=1; NA=NA; else=0")
TJ_data$important_issue_first_Construction_of_houses_residential_premises = recode(TJ_data$important_issue_first, "'Construction of houses residential premises'=1; NA=NA; else=0")
TJ_data$important_issue_first_Terrorism = recode(TJ_data$important_issue_first, "'Terrorism'=1; NA=NA; else=0")
TJ_data$important_issue_first_War_conflict_in_other_countries = recode(TJ_data$important_issue_first, "'War conflict in other countries'=1; NA=NA; else=0")
TJ_data$important_issue_first_Increase_in_crime_violence_concerns_about_personal_safety = recode(TJ_data$important_issue_first, "'Increase in crime violence concerns about personal safety'=1; NA=NA; else=0")
TJ_data$important_issue_first_Corruption = recode(TJ_data$important_issue_first, "'Corruption'=1; NA=NA; else=0")
TJ_data$important_issue_first_Lack_of_opportunities = recode(TJ_data$important_issue_first, "'Lack of opportunities'=1; NA=NA; else=0")
TJ_data$important_issue_first_Discrimination_ethnic_or_religious_tensions = recode(TJ_data$important_issue_first, "'Discrimination ethnic or religious tensions'=1; NA=NA; else=0")
TJ_data$important_issue_first_Infrastructure = recode(TJ_data$important_issue_first, "'Infrastructure'=1; NA=NA; else=0")
TJ_data$important_issue_first_Deterioration_of_the_environment = recode(TJ_data$important_issue_first, "'Deterioration of the environment'=1; NA=NA; else=0")
TJ_data$important_issue_first_Unresolved_territorial_conflicts = recode(TJ_data$important_issue_first, "'Unresolved territorial conflicts'=1; NA=NA; else=0")
TJ_data$important_issue_first_Emigration = recode(TJ_data$important_issue_first, "'Emigration'=1; NA=NA; else=0")

# important_issue_second
TJ_data$important_issue_second_Political_instability = recode(TJ_data$important_issue_second, "'Political instability'=1; NA=NA; else=0")
TJ_data$important_issue_second_Unemployment = recode(TJ_data$important_issue_second, "'Unemployment'=1; NA=NA; else=0")
TJ_data$important_issue_second_Inflation_Prices = recode(TJ_data$important_issue_second, "'Inflation Prices'=1; NA=NA; else=0")
TJ_data$important_issue_second_Wages_pensions = recode(TJ_data$important_issue_second, "'Wages pensions'=1; NA=NA; else=0")
TJ_data$important_issue_second_Taxes = recode(TJ_data$important_issue_second, "'Taxes'=1; NA=NA; else=0")
TJ_data$important_issue_second_Access_to_basic_needs = recode(TJ_data$important_issue_second, "'Access to basic needs'=1; NA=NA; else=0")
TJ_data$important_issue_second_General_economic_situation = recode(TJ_data$important_issue_second, "'General economic situation'=1; NA=NA; else=0")
TJ_data$important_issue_second_Deterioration_of_the_quality_and_access_to_education = recode(TJ_data$important_issue_second, "'Deterioration of the quality and access to education'=1; NA=NA; else=0")
TJ_data$important_issue_second_Construction_of_houses_residential_premises = recode(TJ_data$important_issue_second, "'Construction of houses residential premises'=1; NA=NA; else=0")
TJ_data$important_issue_second_Terrorism = recode(TJ_data$important_issue_second, "'Terrorism'=1; NA=NA; else=0")
TJ_data$important_issue_second_War_conflict_in_other_countries = recode(TJ_data$important_issue_second, "'War conflict in other countries'=1; NA=NA; else=0")
TJ_data$important_issue_second_Increase_in_crime_violence_concerns_about_personal_safety = recode(TJ_data$important_issue_second, "'Increase in crime violence concerns about personal safety'=1; NA=NA; else=0")
TJ_data$important_issue_second_Corruption = recode(TJ_data$important_issue_second, "'Corruption'=1; NA=NA; else=0")
TJ_data$important_issue_second_Lack_of_opportunities = recode(TJ_data$important_issue_second, "'Lack of opportunities'=1; NA=NA; else=0")
TJ_data$important_issue_second_Discrimination_ethnic_or_religious_tensions = recode(TJ_data$important_issue_second, "'Discrimination ethnic or religious tensions'=1; NA=NA; else=0")
TJ_data$important_issue_second_Infrastructure = recode(TJ_data$important_issue_second, "'Infrastructure'=1; NA=NA; else=0")
TJ_data$important_issue_second_Deterioration_of_the_environment = recode(TJ_data$important_issue_second, "'Deterioration of the environment'=1; NA=NA; else=0")
TJ_data$important_issue_second_Unresolved_territorial_conflicts = recode(TJ_data$important_issue_second, "'Unresolved territorial conflicts'=1; NA=NA; else=0")
TJ_data$important_issue_second_Emigration = recode(TJ_data$important_issue_second, "'Emigration'=1; NA=NA; else=0")

# sm_use
TJ_data$sm_use_No = recode(TJ_data$sm_use, '"No"=1; NA=NA; else=0')
TJ_data$sm_use_Yes = recode(TJ_data$sm_use, '"Yes"=1; NA=NA; else=0')

# vpn_use
TJ_data$vpn_use_No = recode(TJ_data$vpn_use, '"No"=1; NA=NA; else=0')
TJ_data$vpn_use_Yes = recode(TJ_data$vpn_use, '"Yes"=1; NA=NA; else=0')

# paid_posters
# Assuming 'I never heard of them', 'I have heard of them but I have not seen any posts from them', 'I think I have seen posts from them' as categories
TJ_data$paid_posters_never_heard = recode(TJ_data$paid_posters, '"I never heard of them"=1; NA=NA; else=0')
TJ_data$paid_posters_heard_not_seen = recode(TJ_data$paid_posters, '"I have heard of them but I have not seen any posts from them"=1; NA=NA; else=0')
TJ_data$paid_posters_seen_posts = recode(TJ_data$paid_posters, '"I think I have seen posts from them"=1; NA=NA; else=0')

# Frequency tables for internet use
table(TJ_data$internet_use_No)
table(TJ_data$internet_use_Yes)

# Frequency tables for ethnicity
table(TJ_data$ethnicity_Kazakh)
table(TJ_data$ethnicity_Russian)
table(TJ_data$ethnicity_Uzbek)
table(TJ_data$ethnicity_Tajik)
table(TJ_data$ethnicity_Kyrgyz)
table(TJ_data$ethnicity_Other)

# Frequency tables for urbanicity
table(TJ_data$urbanicity_City)
table(TJ_data$urbanicity_Village)

# Frequency tables for gender
table(TJ_data$gender_Female)
table(TJ_data$gender_Male)

# Frequency tables for the first important issue
table(TJ_data$important_issue_first_Political_instability)
table(TJ_data$important_issue_first_Unemployment)
table(TJ_data$important_issue_first_Inflation_Prices)
table(TJ_data$important_issue_first_Wages_pensions)
table(TJ_data$important_issue_first_Taxes)
table(TJ_data$important_issue_first_Access_to_basic_needs)
table(TJ_data$important_issue_first_General_economic_situation)
table(TJ_data$important_issue_first_Deterioration_of_the_quality_and_access_to_education)
table(TJ_data$important_issue_first_Construction_of_houses_residential_premises)
table(TJ_data$important_issue_first_Terrorism)
table(TJ_data$important_issue_first_War_conflict_in_other_countries)
table(TJ_data$important_issue_first_Increase_in_crime_violence_concerns_about_personal_safety)
table(TJ_data$important_issue_first_Corruption)
table(TJ_data$important_issue_first_Lack_of_opportunities)
table(TJ_data$important_issue_first_Discrimination_ethnic_or_religious_tensions)
table(TJ_data$important_issue_first_Infrastructure)
table(TJ_data$important_issue_first_Deterioration_of_the_environment)
table(TJ_data$important_issue_first_Unresolved_territorial_conflicts)
table(TJ_data$important_issue_first_Emigration)

# Frequency tables for the first important issue
table(TJ_data$important_issue_second_Political_instability)
table(TJ_data$important_issue_second_Unemployment)
table(TJ_data$important_issue_second_Inflation_Prices)
table(TJ_data$important_issue_second_Wages_pensions)
table(TJ_data$important_issue_second_Taxes)
table(TJ_data$important_issue_second_Access_to_basic_needs)
table(TJ_data$important_issue_second_General_economic_situation)
table(TJ_data$important_issue_second_Deterioration_of_the_quality_and_access_to_education)
table(TJ_data$important_issue_second_Construction_of_houses_residential_premises)
table(TJ_data$important_issue_second_Terrorism)
table(TJ_data$important_issue_second_War_conflict_in_other_countries)
table(TJ_data$important_issue_second_Increase_in_crime_violence_concerns_about_personal_safety)
table(TJ_data$important_issue_second_Corruption)
table(TJ_data$important_issue_second_Lack_of_opportunities)
table(TJ_data$important_issue_second_Discrimination_ethnic_or_religious_tensions)
table(TJ_data$important_issue_second_Infrastructure)
table(TJ_data$important_issue_second_Deterioration_of_the_environment)
table(TJ_data$important_issue_second_Unresolved_territorial_conflicts)
table(TJ_data$important_issue_second_Emigration)
# Frequency tables for social media use
table(TJ_data$sm_use_No)
table(TJ_data$sm_use_Yes)

# Frequency tables for VPN use
table(TJ_data$vpn_use_No)
table(TJ_data$vpn_use_Yes)

# Frequency tables for paid posters
table(TJ_data$paid_posters_never_heard)
table(TJ_data$paid_posters_heard_not_seen)
table(TJ_data$paid_posters_seen_posts)

#Scaling
library(scales)
TJ_data$age_n_sc = rescale(TJ_data$age_n, to = c(0, 1))
TJ_data$edu_n_sc = rescale(TJ_data$edu_n, to = c(0, 1))
TJ_data$inc_n_sc = rescale(TJ_data$inc_n, to = c(0, 1))
TJ_data$pol_interest_n_sc = rescale(TJ_data$pol_interest_n, to = c(1, 0))
TJ_data$trust_russia_n_sc = rescale(TJ_data$trust_russia_n, to = c(1, 0))
TJ_data$trust_US_n_sc = rescale(TJ_data$trust_US_n, to = c(1, 0))
TJ_data$trust_china_n_sc = rescale(TJ_data$trust_china_n, to = c(1, 0))
TJ_data$trust_EU_n_sc = rescale(TJ_data$trust_EU_n, to = c(1, 0))
TJ_data$system_capable_n_sc = rescale(TJ_data$system_capable_n, to = c(1, 0))
TJ_data$system_proud_n_sc = rescale(TJ_data$system_proud_n, to = c(1, 0))
TJ_data$system_deserves_n_sc = rescale(TJ_data$system_deserves_n, to = c(1, 0))
TJ_data$system_live_n_sc = rescale(TJ_data$system_live_n, to = c(1, 0))
TJ_data$system_hurdles_participate_n_sc = rescale(TJ_data$system_hurdles_participate_n, to = c(1, 0))
TJ_data$tracking_central_n_sc = rescale(TJ_data$tracking_central_n, to = c(0, 1))
TJ_data$tracking_local_n_sc = rescale(TJ_data$tracking_local_n, to = c(0, 1))
TJ_data$tracking_companies_n_sc = rescale(TJ_data$tracking_companies_n, to = c(0, 1))
TJ_data$democracy_elections_n_sc = rescale(TJ_data$democracy_elections_n, to = c(1, 0))
TJ_data$democracy_press_n_sc = rescale(TJ_data$democracy_press_n, to = c(1, 0))
TJ_data$facebook_n_sc = rescale(TJ_data$facebook_n, to = c(0, 1))
TJ_data$vkontakte_n_sc = rescale(TJ_data$vkontakte_n, to = c(0, 1))
TJ_data$instagram_n_sc = rescale(TJ_data$instagram_n, to = c(0, 1))
TJ_data$tiktok_n_sc = rescale(TJ_data$tiktok_n, to = c(0, 1))
TJ_data$twitter_n_sc = rescale(TJ_data$twitter_n, to = c(0, 1))
TJ_data$youtube_n_sc = rescale(TJ_data$youtube_n, to = c(0, 1))
TJ_data$whatsapp_n_sc = rescale(TJ_data$whatsapp_n, to = c(0, 1))
TJ_data$telegram_n_sc = rescale(TJ_data$telegram_n, to = c(0, 1))
TJ_data$pol_news_tv_n_sc = rescale(TJ_data$pol_news_tv_n, to = c(1, 0))
TJ_data$pol_news_facebook_n_sc = rescale(TJ_data$pol_news_facebook_n, to = c(1, 0))
TJ_data$pol_news_vkontakte_n_sc = rescale(TJ_data$pol_news_vkontakte_n, to = c(1, 0))
TJ_data$pol_news_tiktok_n_sc = rescale(TJ_data$pol_news_tiktok_n, to = c(1, 0))
TJ_data$pol_news_twitter_n_sc = rescale(TJ_data$pol_news_twitter_n, to = c(1, 0))
TJ_data$pol_news_odnoklassniki_n_sc = rescale(TJ_data$pol_news_odnoklassniki_n, to = c(1, 0))
TJ_data$trust_state_n_sc = rescale(TJ_data$trust_state_n, to = c(1, 0))
TJ_data$trust_russian_media_n_sc = rescale(TJ_data$trust_russian_media_n, to = c(1, 0))
TJ_data$trust_internet_n_sc = rescale(TJ_data$trust_internet_n, to = c(1, 0))
TJ_data$trust_facebook_n_sc = rescale(TJ_data$trust_facebook_n, to = c(1, 0))
TJ_data$trust_vkontakte_n_sc = rescale(TJ_data$trust_vkontakte_n, to = c(1, 0))
TJ_data$trust_western_n_sc = rescale(TJ_data$trust_western_n, to = c(1, 0))
TJ_data$news_balance_n_sc = rescale(TJ_data$news_balance_n, to = c(0, 1))
TJ_data$sm_positive_local_n_sc = rescale(TJ_data$sm_positive_local_n, to = c(1, 0))
TJ_data$sm_positive_central_n_sc = rescale(TJ_data$sm_positive_central_n, to = c(1, 0))
TJ_data$tv_critical_local_n_sc = rescale(TJ_data$tv_critical_local_n, to = c(1, 0))
TJ_data$tv_critical_central_n_sc = rescale(TJ_data$tv_critical_central_n, to = c(1, 0))
TJ_data$clickable_state_n_sc = rescale(TJ_data$clickable_state_n, to = c(1, 0))
TJ_data$clickable_russian_n_sc = rescale(TJ_data$clickable_russian_n, to = c(1, 0))
TJ_data$AI_n_sc = rescale(TJ_data$AI_n, to = c(1, 0))
TJ_data$echo_chamber_n_sc = rescale(TJ_data$echo_chamber_n, to = c(0, 1))
TJ_data$paid_posters_percentage_n_sc = rescale(TJ_data$paid_posters_percentage_n, to = c(0, 1))
TJ_data$avoidance_blocking_n_sc = rescale(TJ_data$avoidance_blocking_n, to = c(1, 0))
TJ_data$avoidance_unfriending_n_sc = rescale(TJ_data$avoidance_unfriending_n, to = c(1, 0))
TJ_data$avoidance_leaving_group_n_sc = rescale(TJ_data$avoidance_leaving_group_n, to = c(1, 0))
TJ_data$avoidance_unsubscribing_n_sc = rescale(TJ_data$avoidance_unsubscribing_n, to = c(1, 0))
TJ_data$sm_disagreement_politics_n_sc = rescale(TJ_data$sm_disagreement_politics_n, to = c(0, 1))
TJ_data$sm_disagreement_news_n_sc = rescale(TJ_data$sm_disagreement_news_n, to = c(0, 1))
TJ_data$sm_disagreement_issues_n_sc = rescale(TJ_data$sm_disagreement_issues_n, to = c(0, 1))
TJ_data$network_breadth_n_sc = rescale(TJ_data$network_breadth_n, to = c(0, 1))
TJ_data$sm_engage_friends_n_sc = rescale(TJ_data$sm_engage_friends_n, to = c(1, 0))
TJ_data$sm_engage_groups_n_sc = rescale(TJ_data$sm_engage_groups_n, to = c(1, 0))
TJ_data$sm_engage_post_n_sc = rescale(TJ_data$sm_engage_post_n, to = c(1, 0))
TJ_data$sm_engage_offline_sc = rescale(TJ_data$sm_engage_offline_n, to = c(1, 0))
TJ_data$opinion_china_sc = rescale(TJ_data$opinion_china_n, to = c(1, 0))
TJ_data$opinion_russia_sc = rescale(TJ_data$opinion_russia_n, to = c(1, 0))
TJ_data$opinion_US_sc = rescale(TJ_data$opinion_US_n, to = c(1, 0))
TJ_data$change_china_sc = rescale(TJ_data$change_china_n, to = c(1, 0))
TJ_data$change_russia_sc = rescale(TJ_data$change_russia_n, to = c(1, 0))
TJ_data$change_US_sc = rescale(TJ_data$change_US_n, to = c(1, 0))

#Uzbekistan
#Recoding all Variables##########################################################################
##############################################################################################
#################################################################################################
library(dplyr)
library(car)

# Internet Use Factor
UZ_data$internet_use = recode(UZ_data$q2, "1 = 'Yes'; 2 = 'No'")
UZ_data$internet_use = factor(UZ_data$internet_use)
UZ_data$internet_use = addNA(UZ_data$internet_use)
table(UZ_data$internet_use)  # Frequency table for internet_use

# Age Numeric
summary(UZ_data$q3)
UZ_data$age_n = UZ_data$q3
summary(UZ_data$age_n)  # Summary for age_n

# Age Collapsed Factor
table(UZ_data$q3a)
UZ_data$age_collapsed = recode(UZ_data$q3a, "1 = '18 - 24'; 2 = '25 - 34'; 3 = '35 - 44'; 4 = '45 - 54'; 5 = '55 - 64'; 6 = '65+'")
UZ_data$age_collapsed = factor(UZ_data$age_collapsed, ordered = TRUE)
table(UZ_data$age_collapsed)  # Frequency table for age_collapsed

# Ethnicity Factor
table(UZ_data$q4)
UZ_data$ethnicity = recode(UZ_data$q4, "1 = 'Kazakh'; 2 = 'Russian'; 3 = 'Uzbek'; 4 = 'Tajik'; 5 = 'Kyrgyz'; 6 = 'Other'")
UZ_data$ethnicity = factor(UZ_data$ethnicity)
table(UZ_data$ethnicity)  # Frequency table for ethnicity

# Urbanicity Factor
UZ_data$urbanicity = recode(UZ_data$q5, "1 = 'City'; 2 = 'Village'")
UZ_data$urbanicity = factor(UZ_data$urbanicity)
UZ_data$urbanicity = addNA(UZ_data$urbanicity)
table(UZ_data$urbanicity)  # Frequency table for urbanicity

# Gender Factor
UZ_data$gender = recode(UZ_data$q6, "1 = 'Female'; 2 = 'Male'")
UZ_data$gender = factor(UZ_data$gender)
table(UZ_data$gender)  # Frequency table for gender

# Education Factor
UZ_data$edu = recode(UZ_data$q7, "1 = 'No education'; 2 = 'Primary education'; 3 = 'Basic secondary education'; 4 = 'General secondary education'; 5 = 'Complete vocational education'; 6 = 'Incomplete higher education'; 7 = 'Complete higher education'")
UZ_data$edu = factor(UZ_data$edu, ordered = TRUE)
table(UZ_data$edu)  # Frequency table for edu

# Education Numeric
UZ_data$edu_n = UZ_data$q7
summary(UZ_data$edu_n)  # Summary for edu_n

# Income Factor
UZ_data$inc = recode(UZ_data$q8, "1 = 'Less than 70,001'; 2 = '70,001 - 150,000'; 3 = '150,001 – 200,000'; 4 = '200,001 – 250,000'; 5 = '250,001 – 300,000'; 6 = '300,001 – 350,000'; 7 = '350,001 – 400,000'; 8 = '400,001 – 450,000'; 9 = '450,001 – 500,000'; 10 = '500,001 – 550,000'; 11 = '550,001 – 600,000'; 12 = 'More than 600,000'")
UZ_data$inc = factor(UZ_data$inc, levels = c('Less than 70,001', '70,001 - 150,000', '150,001 – 200,000', '200,001 – 250,000', '250,001 – 300,000', '300,001 – 350,000', '350,001 – 400,000', '400,001 – 450,000', '450,001 – 500,000', '500,001 – 550,000', '550,001 – 600,000', 'More than 600,000'), ordered = TRUE)
table(UZ_data$inc)  # Frequency table for Income Factor

# Income Numeric
UZ_data$inc_n = UZ_data$q8
summary(UZ_data$inc_n)  # Summary for Income Numeric

# Political Interest Factor (news for UZ)
UZ_data$pol_interest = recode(UZ_data$q9, "1 = 'Very interested'; 2 = 'Somewhat interested'; 3 = 'Somewhat uninterested'; 4 = 'Very uninterested'")
UZ_data$pol_interest = factor(UZ_data$pol_interest, levels = c('Very interested','Somewhat interested','Somewhat uninterested','Very uninterested'), ordered = TRUE)
table(UZ_data$pol_interest)  # Frequency table for Political Interest Factor

# Political Interest Numeric
UZ_data$pol_interest_n = UZ_data$q9
summary(UZ_data$pol_interest_n)  # Summary for Political Interest Numeric

# Discuss Politics Factor Ordinal
UZ_data$pol_discuss = recode(UZ_data$q10, "1 = 'A few times a day'; 2 = 'Once a day'; 3 = 'Three to five days a week'; 4 = 'Once a week'; 5 = 'Less often than once a week'; 6 = 'Never'")
UZ_data$pol_discuss = factor(UZ_data$pol_discuss, levels = c('A few times a day', 'Once a day', 'Three to five days a week', 'Once a week', 'Less often than once a week', 'Never'), ordered = TRUE)
table(KZ_data$pol_discuss)  # Frequency table for Discuss Politics Factor

#Discuss Politics Numeric
UZ_data$pol_discuss_n = UZ_data$q10
summary(UZ_data$pol_discuss_n)  # Summary for Discuss Politics Numeric

# Disagree when Discussing Politics
UZ_data$pol_disagree = recode(UZ_data$q11, "1 = 'Most of the time'; 2 = 'Some of the time'; 3 = 'Rarely'; 4 = 'Never'")
UZ_data$pol_disagree = factor(UZ_data$pol_disagree, levels = c('Most of the time', 'Some of the time', 'Rarely', 'Never'), ordered = TRUE)
table(UZ_data$pol_disagree)  # Frequency table for Disagree when Discussing Politics

#Disagree Politics Numeric
UZ_data$pol_disagree_n = UZ_data$q11
summary(UZ_data$pol_disagree_n)  # Summary for Disagree Politics Numeric

# Trust in Mahalla
UZ_data$trust_mahalla = recode(UZ_data$q12_b, "1 = 'A Great Deal'; 2 = 'Quite a Lot of Trust'; 3 = 'Not Very Much Trust'; 4 = 'None at all'")
UZ_data$trust_mahalla = factor(UZ_data$trust_mahalla, levels = c('A Great Deal', 'Quite a Lot of Trust', 'Not Very Much Trust', 'None at all'), ordered = TRUE)
table(UZ_data$trust_mahalla)  # Frequency table for Trust in Mahalla

# Trust Mahalla
UZ_data$trust_mahalla_n = UZ_data$q12_b
summary(UZ_data$trust_mahalla_n)

# Most Important Issue First Mention
UZ_data$important_issue_first = recode(UZ_data$q13_a, "1 = 'Political instability'; 2 = 'Unemployment'; 3 = 'Inflation Prices'; 4 = 'Wages pensions'; 5 = 'Taxes'; 6 = 'Access to basic needs'; 7 = 'General economic situation'; 8 = 'Deterioration of the quality and access to education'; 9 = 'Construction of houses residential premises'; 10 = 'Terrorism'; 11 = 'War conflict in other countries'; 12 = 'Increase in crime violence concerns about personal safety'; 13 = 'Corruption'; 14 = 'Lack of opportunities'; 15 = 'Discrimination ethnic or religious tensions'; 16 = 'Infrastructure'; 17 = 'Deterioration of the environment'; 18 = 'Unresolved territorial conflicts'; 19 = 'Emigration'")
UZ_data$important_issue_first = factor(UZ_data$important_issue_first)
UZ_data$important_issue_first = addNA(UZ_data$important_issue_first)
table(UZ_data$important_issue_first)

# Most Important Issue Second Mention
UZ_data$important_issue_second = recode(UZ_data$q13_b, "1 = 'Political instability'; 2 = 'Unemployment'; 3 = 'Inflation Prices'; 4 = 'Wages pensions'; 5 = 'Taxes'; 6 = 'Access to basic needs'; 7 = 'General economic situation'; 8 = 'Deterioration of the quality and access to education'; 9 = 'Construction of houses residential premises'; 10 = 'Terrorism'; 11 = 'War conflict in other countries'; 12 = 'Increase in crime violence concerns about personal safety'; 13 = 'Corruption'; 14 = 'Lack of opportunities'; 15 = 'Discrimination ethnic or religious tensions'; 16 = 'Infrastructure'; 17 = 'Deterioration of the environment'; 18 = 'Unresolved territorial conflicts'; 19 = 'Emigration'")
UZ_data$important_issue_second = factor(UZ_data$important_issue_second)
UZ_data$important_issue_second = addNA(UZ_data$important_issue_second)
table(UZ_data$important_issue_second)

# System Approval Variables Factor
# Perform the recode
UZ_data$system_capable = recode(UZ_data$q14_a, "1 = 'Strongly agree'; 2 = 'Somewhat agree'; 3 = 'Neither disagree nor agree'; 4 = 'Somewhat disagree'; 5 = 'Strongly disagree'")
UZ_data$system_capable = factor(UZ_data$system_capable, levels = c('Strongly agree', 'Somewhat agree', 'Neither disagree nor agree', 'Somewhat disagree', 'Strongly disagree'), ordered = TRUE)
table(UZ_data$system_capable)  # Frequency table for System Capable

# Numeric variable for System Capable
UZ_data$system_capable_n = as.numeric(UZ_data$q14_a)
summary(UZ_data$system_capable_n)  # Summary for System Capable (Numeric)

UZ_data$system_proud = recode(UZ_data$q14_b, "1 = 'Strongly agree'; 2 = 'Somewhat agree'; 3 = 'Neither disagree nor agree'; 4 = 'Somewhat disagree'; 5 = 'Strongly disagree'")
UZ_data$system_proud = factor(UZ_data$system_proud, levels = c('Strongly agree', 'Somewhat agree', 'Neither disagree nor agree', 'Somewhat disagree', 'Strongly disagree'), ordered = TRUE)
table(UZ_data$system_proud)  # Frequency table for System Proud

# Numeric variable for System Proud
UZ_data$system_proud_n = as.numeric(UZ_data$q14_b)
summary(UZ_data$system_proud_n)  # Summary for System Proud (Numeric)

UZ_data$system_live = recode(UZ_data$q14_d, "1 = 'Strongly agree'; 2 = 'Somewhat agree'; 3 = 'Neither disagree nor agree'; 4 = 'Somewhat disagree'; 5 = 'Strongly disagree'")
UZ_data$system_live = factor(UZ_data$system_live, levels = c('Strongly agree', 'Somewhat agree', 'Neither disagree nor agree', 'Somewhat disagree', 'Strongly disagree'), ordered = TRUE)
table(UZ_data$system_live)  # Frequency table for System Live

# Numeric variable for System Live
UZ_data$system_live_n = as.numeric(UZ_data$q14_d)
summary(UZ_data$system_live_n)  # Summary for System Live (Numeric)

UZ_data$system_hurdles_participate = recode(UZ_data$q14_e, "1 = 'Strongly agree'; 2 = 'Somewhat agree'; 3 = 'Neither disagree nor agree'; 4 = 'Somewhat disagree'; 5 = 'Strongly disagree'")
UZ_data$system_hurdles_participate = factor(UZ_data$system_hurdles_participate, levels = c('Strongly agree', 'Somewhat agree', 'Neither disagree nor agree', 'Somewhat disagree', 'Strongly disagree'), ordered = TRUE)
table(UZ_data$system_hurdles_participate)  # Frequency table for System Hurdles for Participation

# Numeric variable for System Hurdles for Participation
UZ_data$system_hurdles_participate_n = as.numeric(UZ_data$q14_e)
summary(UZ_data$system_hurdles_participate_n)  # Summary for System Hurdles for Participation (Numeric)

# Participate Variables Factor
# Perform the recode
UZ_data$participate_volunteer = recode(UZ_data$q15_d, "1 = 'Yes'; 2 = 'No'")
UZ_data$participate_volunteer = factor(UZ_data$participate_volunteer, levels = c('Yes', 'No'), ordered = FALSE)
table(UZ_data$participate_volunteer)  # Frequency table for Participate Volunteer

# Numeric variable for Participate Volunteer
UZ_data$participate_volunteer_n = as.numeric(UZ_data$q15_d)
summary(UZ_data$participate_volunteer_n)  # Summary for Participate Volunteer (Numeric)

UZ_data$participate_member = recode(UZ_data$q15_e, "1 = 'Yes'; 2 = 'No'")
UZ_data$participate_member = factor(UZ_data$participate_member, levels = c('Yes', 'No'), ordered = FALSE)
table(UZ_data$participate_member)  # Frequency table for Participate Member

# Numeric variable for Participate Member
UZ_data$participate_member_n = as.numeric(UZ_data$q15_e)
summary(UZ_data$participate_member_n)  # Summary for Participate Member (Numeric)

UZ_data$participate_community = recode(UZ_data$q15_f, "1 = 'Yes'; 2 = 'No'")
UZ_data$participate_community = factor(UZ_data$participate_community, levels = c('Yes', 'No'), ordered = FALSE)
table(UZ_data$participate_community)  # Frequency table for Participate Community

# Numeric variable for Participate Community
UZ_data$participate_community_n = as.numeric(UZ_data$q15_f)
summary(UZ_data$participate_community_n)  # Summary for Participate Community (Numeric)

UZ_data$participate_contact = recode(UZ_data$q15_g, "1 = 'Yes'; 2 = 'No'")
UZ_data$participate_contact = factor(UZ_data$participate_contact, levels = c('Yes', 'No'), ordered = FALSE)
table(UZ_data$participate_contact)  # Frequency table for Participate Contact

# Numeric variable for Participate Contact
UZ_data$participate_contact_n = as.numeric(UZ_data$q15_g)
summary(UZ_data$participate_contact_n)  # Summary for Participate Contact (Numeric)

UZ_data$participate_vote = recode(UZ_data$q16, "1 = 'Yes'; 2 = 'No'")
UZ_data$participate_vote = factor(UZ_data$participate_vote, levels = c('Yes', 'No'), ordered = FALSE)
table(UZ_data$participate_vote)  # Frequency table for Participate Vote

# Numeric variable for Participate Vote
UZ_data$participate_vote_n = as.numeric(UZ_data$q16)
summary(UZ_data$participate_vote_n)  # Summary for Participate Vote (Numeric)

# Digital Tracking Variables Factor
# Perform the recode
UZ_data$tracking_local = recode(UZ_data$q17_b, "1 = 'Not Comfortable at all'; 2 = '2'; 3 = '3'; 4 = '4'; 5 = 'Very Comfortable'")
UZ_data$tracking_local = factor(UZ_data$tracking_local, levels = c('Not Comfortable at all', '2', '3', '4', 'Very Comfortable'), ordered = TRUE)
table(UZ_data$tracking_local)  # Frequency table for Tracking Local

# Numeric variable for Tracking Local
UZ_data$tracking_local_n = as.numeric(UZ_data$q17_b)
summary(UZ_data$tracking_local_n)  # Summary for Tracking Local (Numeric)

UZ_data$tracking_companies = recode(UZ_data$q17_c, "1 = 'Not Comfortable at all'; 2 = '2'; 3 = '3'; 4 = '4'; 5 = 'Very Comfortable'")
UZ_data$tracking_companies = factor(UZ_data$tracking_companies, levels = c('Not Comfortable at all', '2', '3', '4', 'Very Comfortable'), ordered = TRUE)
table(UZ_data$tracking_companies)  # Frequency table for Tracking Companies

# Numeric variable for Tracking Companies
UZ_data$tracking_companies_n = as.numeric(UZ_data$q17_c)
summary(UZ_data$tracking_companies_n)  # Summary for Tracking Companies (Numeric)

# Democracy Variables Factor
# Perform the recode
UZ_data$democracy_elections = recode(UZ_data$q20_a, "1 = 'Strongly agree'; 2 = 'Somewhat agree'; 3 = 'Neither disagree nor agree'; 4 = 'Somewhat disagree'; 5 = 'Strongly disagree'")
UZ_data$democracy_elections = factor(UZ_data$democracy_elections, levels = c('Strongly agree', 'Somewhat agree', 'Neither disagree nor agree', 'Somewhat disagree', 'Strongly disagree'), ordered = TRUE)
table(UZ_data$democracy_elections)  # Frequency table for Democracy Elections

# Numeric variable for Democracy Elections
UZ_data$democracy_elections_n = as.numeric(UZ_data$q20_a)
summary(UZ_data$democracy_elections_n)  # Summary for Democracy Elections (Numeric)

UZ_data$democracy_press = recode(UZ_data$q20_e, "1 = 'Strongly agree'; 2 = 'Somewhat agree'; 3 = 'Neither disagree nor agree'; 4 = 'Somewhat disagree'; 5 = 'Strongly disagree'")
UZ_data$democracy_press = factor(UZ_data$democracy_press, levels = c('Strongly agree', 'Somewhat agree', 'Neither disagree nor agree', 'Somewhat disagree', 'Strongly disagree'), ordered = TRUE)
table(UZ_data$democracy_press)  # Frequency table for Democracy Press

# Numeric variable for Democracy Press
UZ_data$democracy_press_n = as.numeric(UZ_data$q20_e)
summary(UZ_data$democracy_press_n)  # Summary for Democracy Press (Numeric)

UZ_data$satisfied_participation = recode(UZ_data$q21, "1 = 'Completely Satisfied'; 2 = 'Rather Satisfied'; 3 = 'Rather Dissatisfied'; 4 = 'Completely Dissatisfied'")
UZ_data$satisfied_participation = factor(UZ_data$satisfied_participation, levels = c('Completely Satisfied','Rather Satisfied','Rather Dissatisfied','Completely Dissatisfied'), ordered = TRUE)
table(UZ_data$satisfied_participation)  # Frequency table for satisfied_participation

# Numeric variable for satisfied_participation
UZ_data$satisfied_participation_n = as.numeric(UZ_data$q21)
summary(UZ_data$satisfied_participation_n)  # Summary for satisfied_participation (Numeric)

#Social Media Use
# Perform the recode for the factor variable
UZ_data$sm_use = recode(UZ_data$q22, "1 = 'Yes'; 2 = 'No'")
UZ_data$sm_use = factor(UZ_data$sm_use, levels = c('Yes', 'No'), ordered = TRUE)
UZ_data$sm_use = addNA(UZ_data$sm_use)
table(UZ_data$sm_use)  # Frequency table for Social Media Use

#Social Media Platform Variables Factor
#Perform the recode
UZ_data$facebook = recode(UZ_data$q23_a, '0="Do not use this social media"; 1="0-1"; 2="1-2"; 3="2-3"; 4="3-4"; 5="4-5"; 6="5-6"; 7="6-7"; 8="7-8"; 9="8-9"; 10="More than 9"')
UZ_data$facebook = factor(UZ_data$facebook, levels = c('Do not use this social media', '0-1', '1-2', '2-3', '3-4', '4-5', '5-6', '6-7', '7-8', '8-9', 'More than 9'), ordered = TRUE)
table(UZ_data$facebook) # Display a frequency table for 'facebook'

# Create a numeric variable for facebook
UZ_data$facebook_n = as.numeric(UZ_data$q23_a) 
summary(UZ_data$facebook_n) # Provide a summary for 'facebook_n' (Numeric)

UZ_data$vkontakte = recode(UZ_data$q23_b, '0="Do not use this social media"; 1="0-1"; 2="1-2"; 3="2-3"; 4="3-4"; 5="4-5"; 6="5-6"; 7="6-7"; 8="7-8"; 9="8-9"; 10="More than 9"')
UZ_data$vkontakte = factor(UZ_data$vkontakte, levels = c('Do not use this social media', '0-1', '1-2', '2-3', '3-4', '4-5', '5-6', '6-7', '7-8', '8-9', 'More than 9'), ordered = TRUE)
table(UZ_data$vkontakte) # Display a frequency table for 'vkontakte'

# Create a numeric variable for vkontakte
UZ_data$vkontakte_n = as.numeric(UZ_data$q23_b) 
summary(UZ_data$vkontakte_n) # Provide a summary for 'vkontakte_n' (Numeric)

UZ_data$instagram = recode(UZ_data$q23_c, '0="Do not use this social media"; 1="0-1"; 2="1-2"; 3="2-3"; 4="3-4"; 5="4-5"; 6="5-6"; 7="6-7"; 8="7-8"; 9="8-9"; 10="More than 9"')
UZ_data$instagram = factor(UZ_data$instagram, levels = c('Do not use this social media', '0-1', '1-2', '2-3', '3-4', '4-5', '5-6', '6-7', '7-8', '8-9', 'More than 9'), ordered = TRUE)
table(UZ_data$instagram) # Display a frequency table for 'instagram'

# Create a numeric variable for instagram
UZ_data$instagram_n = as.numeric(UZ_data$q23_c) 
summary(UZ_data$instagram_n) # Provide a summary for 'instagram_n' (Numeric)


UZ_data$tiktok = recode(UZ_data$q23_d, '0="Do not use this social media"; 1="0-1"; 2="1-2"; 3="2-3"; 4="3-4"; 5="4-5"; 6="5-6"; 7="6-7"; 8="7-8"; 9="8-9"; 10="More than 9"')
UZ_data$tiktok = factor(UZ_data$tiktok, levels = c('Do not use this social media', '0-1', '1-2', '2-3', '3-4', '4-5', '5-6', '6-7', '7-8', '8-9', 'More than 9'), ordered = TRUE)
table(UZ_data$tiktok) # Display a frequency table for 'tiktok'

# Create a numeric variable for tiktok
UZ_data$tiktok_n = as.numeric(UZ_data$q23_d) 
summary(UZ_data$tiktok_n) # Provide a summary for 'tiktok_n' (Numeric)

UZ_data$twitter = recode(UZ_data$q23_e, '0="Do not use this social media"; 1="0-1"; 2="1-2"; 3="2-3"; 4="3-4"; 5="4-5"; 6="5-6"; 7="6-7"; 8="7-8"; 9="8-9"; 10="More than 9"')
UZ_data$twitter = factor(UZ_data$twitter, levels = c('Do not use this social media', '0-1', '1-2', '2-3', '3-4', '4-5', '5-6', '6-7', '7-8', '8-9', 'More than 9'), ordered = TRUE)
table(UZ_data$twitter) # Display a frequency table for 'twitter'

# Create a numeric variable for twitter
UZ_data$twitter_n = as.numeric(UZ_data$q23_e) 
summary(UZ_data$twitter_n) # Provide a summary for 'twitter_n' (Numeric)

UZ_data$youtube = recode(UZ_data$q23_f, '0="Do not use this social media"; 1="0-1"; 2="1-2"; 3="2-3"; 4="3-4"; 5="4-5"; 6="5-6"; 7="6-7"; 8="7-8"; 9="8-9"; 10="More than 9"')
UZ_data$youtube = factor(UZ_data$youtube, levels = c('Do not use this social media', '0-1', '1-2', '2-3', '3-4', '4-5', '5-6', '6-7', '7-8', '8-9', 'More than 9'), ordered = TRUE)
table(UZ_data$youtube) # Display a frequency table for 'youtube'

# Create a numeric variable for youtube
UZ_data$youtube_n = as.numeric(UZ_data$q23_f) 
summary(UZ_data$youtube_n) # Provide a summary for 'youtube_n' (Numeric)

UZ_data$whatsapp = recode(UZ_data$q23_g, '0="Do not use this social media"; 1="0-1"; 2="1-2"; 3="2-3"; 4="3-4"; 5="4-5"; 6="5-6"; 7="6-7"; 8="7-8"; 9="8-9"; 10="More than 9"')
UZ_data$whatsapp = factor(UZ_data$whatsapp, levels = c('Do not use this social media', '0-1', '1-2', '2-3', '3-4', '4-5', '5-6', '6-7', '7-8', '8-9', 'More than 9'), ordered = TRUE)
table(UZ_data$whatsapp) # Display a frequency table for 'whatsapp'

# Create a numeric variable for whatsapp
UZ_data$whatsapp_n = as.numeric(UZ_data$q23_g) 
summary(UZ_data$whatsapp_n) # Provide a summary for 'whatsapp_n' (Numeric)

UZ_data$telegram = recode(UZ_data$q23_h, '0="Do not use this social media"; 1="0-1"; 2="1-2"; 3="2-3"; 4="3-4"; 5="4-5"; 6="5-6"; 7="6-7"; 8="7-8"; 9="8-9"; 10="More than 9"')
UZ_data$telegram = factor(UZ_data$telegram, levels = c('Do not use this social media', '0-1', '1-2', '2-3', '3-4', '4-5', '5-6', '6-7', '7-8', '8-9', 'More than 9'), ordered = TRUE)
table(UZ_data$telegram) # Display a frequency table for 'telegram'

# Create a numeric variable for whatsapp
UZ_data$telegram_n = as.numeric(UZ_data$q23_h) 
summary(UZ_data$telegram_n) # Provide a summary for 'telegram_n' (Numeric)

# Perform the recode for 'pol_news_tv'
UZ_data$pol_news_tv = recode(UZ_data$q24_a, '1="A few times a day"; 2="Once a day"; 3="Three to five days a week"; 4="Once a week"; 5="Less often than once a week"; 6="Never"')
UZ_data$pol_news_tv = factor(UZ_data$pol_news_tv, levels = c('A few times a day', 'Once a day', 'Three to five days a week', 'Once a week', 'Less often than once a week', 'Never'), ordered = TRUE)
table(UZ_data$pol_news_tv) # Display a frequency table for 'pol_news_tv'

# Create a numeric variable for 'pol_news_tv'
UZ_data$pol_news_tv_n = as.numeric(UZ_data$q24_a) 
summary(UZ_data$pol_news_tv_n) # Provide a summary for 'pol_news_tv_n' (Numeric)

# Perform the recode for 'pol_news_facebook'
UZ_data$pol_news_facebook = recode(UZ_data$q24_b, '1="A few times a day"; 2="Once a day"; 3="Three to five days a week"; 4="Once a week"; 5="Less often than once a week"; 6="Never"')
UZ_data$pol_news_facebook = factor(UZ_data$pol_news_facebook, levels = c('A few times a day', 'Once a day', 'Three to five days a week', 'Once a week', 'Less often than once a week', 'Never'), ordered = TRUE)
table(UZ_data$pol_news_facebook) # Display a frequency table for 'pol_news_facebook'

# Create a numeric variable for 'pol_news_facebook'
UZ_data$pol_news_facebook_n = as.numeric(UZ_data$q24_b) 
summary(UZ_data$pol_news_facebook_n) # Provide a summary for 'pol_news_facebook_n' (Numeric)

# Perform the recode for 'pol_news_vkontakte'
UZ_data$pol_news_vkontakte = recode(UZ_data$q24_c, '1="A few times a day"; 2="Once a day"; 3="Three to five days a week"; 4="Once a week"; 5="Less often than once a week"; 6="Never"')
UZ_data$pol_news_vkontakte = factor(UZ_data$pol_news_vkontakte, levels = c('A few times a day', 'Once a day', 'Three to five days a week', 'Once a week', 'Less often than once a week', 'Never'), ordered = TRUE)
table(UZ_data$pol_news_vkontakte) # Display a frequency table for 'pol_news_vkontakte'

# Create a numeric variable for 'pol_news_vkontakte'
UZ_data$pol_news_vkontakte_n = as.numeric(UZ_data$q24_c) 
summary(UZ_data$pol_news_vkontakte_n) # Provide a summary for 'pol_news_vkontakte_n' (Numeric)

# Perform the recode for 'pol_news_tiktok'
UZ_data$pol_news_tiktok = recode(UZ_data$q24_d, '1="A few times a day"; 2="Once a day"; 3="Three to five days a week"; 4="Once a week"; 5="Less often than once a week"; 6="Never"')
UZ_data$pol_news_tiktok = factor(UZ_data$pol_news_tiktok, levels = c('A few times a day', 'Once a day', 'Three to five days a week', 'Once a week', 'Less often than once a week', 'Never'), ordered = TRUE)
table(UZ_data$pol_news_tiktok) # Display a frequency table for 'pol_news_tiktok'

# Create a numeric variable for 'pol_news_tiktok'
UZ_data$pol_news_tiktok_n = as.numeric(UZ_data$q24_d) 
summary(UZ_data$pol_news_tiktok_n) # Provide a summary for 'pol_news_tiktok_n' (Numeric)

# Perform the recode for 'pol_news_twitter'
UZ_data$pol_news_twitter = recode(UZ_data$q24_e, '1="A few times a day"; 2="Once a day"; 3="Three to five days a week"; 4="Once a week"; 5="Less often than once a week"; 6="Never"')
UZ_data$pol_news_twitter = factor(UZ_data$pol_news_twitter, levels = c('A few times a day', 'Once a day', 'Three to five days a week', 'Once a week', 'Less often than once a week', 'Never'), ordered = TRUE)
table(UZ_data$pol_news_twitter) # Display a frequency table for 'pol_news_twitter'

# Create a numeric variable for 'pol_news_twitter'
UZ_data$pol_news_twitter_n = as.numeric(UZ_data$q24_e) 
summary(UZ_data$pol_news_twitter_n) # Provide a summary for 'pol_news_twitter_n' (Numeric)

# Perform the recode for 'pol_news_odnoklassniki'
UZ_data$pol_news_odnoklassniki = recode(UZ_data$q24_f, '1="A few times a day"; 2="Once a day"; 3="Three to five days a week"; 4="Once a week"; 5="Less often than once a week"; 6="Never"')
UZ_data$pol_news_odnoklassniki = factor(UZ_data$pol_news_odnoklassniki, levels = c('A few times a day', 'Once a day', 'Three to five days a week', 'Once a week', 'Less often than once a week', 'Never'), ordered = TRUE)
table(UZ_data$pol_news_odnoklassniki) # Display a frequency table for 'pol_news_odnoklassniki'

# Create a numeric variable for 'pol_news_odnoklassniki'
UZ_data$pol_news_odnoklassniki_n = as.numeric(UZ_data$q24_f) 
summary(UZ_data$pol_news_odnoklassniki_n) # Provide a summary for 'pol_news_odnoklassniki_n' (Numeric)

# Perform the recode for 'trust_state'
UZ_data$trust_state = recode(UZ_data$q25_a, '1="Very trustworthy"; 2="Somewhat trustworthy"; 3="Somewhat untrustworthy"; 4="Very untrustworthy"')
UZ_data$trust_state = factor(UZ_data$trust_state, levels = c('Very trustworthy', 'Somewhat trustworthy', 'Somewhat untrustworthy', 'Very untrustworthy'), ordered = TRUE)
table(UZ_data$trust_state) # Display a frequency table for 'trust_state'

# Create a numeric variable for 'trust_state'
UZ_data$trust_state_n = as.numeric(UZ_data$q25_a) 
summary(UZ_data$trust_state_n) # Provide a summary for 'trust_state_n' (Numeric)

# Perform the recode for 'trust_russian_media'
UZ_data$trust_russian_media = recode(UZ_data$q25_b, '1="Very trustworthy"; 2="Somewhat trustworthy"; 3="Somewhat untrustworthy"; 4="Very untrustworthy"')
UZ_data$trust_russian_media = factor(UZ_data$trust_russian_media, levels = c('Very trustworthy', 'Somewhat trustworthy', 'Somewhat untrustworthy', 'Very untrustworthy'), ordered = TRUE)
table(UZ_data$trust_russian_media) # Display a frequency table for 'trust_russian_media'

# Create a numeric variable for 'trust_russian_media'
UZ_data$trust_russian_media_n = as.numeric(UZ_data$q25_b) 
summary(UZ_data$trust_russian_media_n) # Provide a summary for 'trust_russian_media_n' (Numeric)

# Perform the recode for 'trust_internet'
UZ_data$trust_internet = recode(UZ_data$q25_c, '1="Very trustworthy"; 2="Somewhat trustworthy"; 3="Somewhat untrustworthy"; 4="Very untrustworthy"')
UZ_data$trust_internet = factor(UZ_data$trust_internet, levels = c('Very trustworthy', 'Somewhat trustworthy', 'Somewhat untrustworthy', 'Very untrustworthy'), ordered = TRUE)
table(UZ_data$trust_internet) # Display a frequency table for 'trust_internet'

# Create a numeric variable for 'trust_internet'
UZ_data$trust_internet_n = as.numeric(UZ_data$q25_c) 
summary(UZ_data$trust_internet_n) # Provide a summary for 'trust_internet_n' (Numeric)

# Perform the recode for 'trust_facebook'
UZ_data$trust_facebook = recode(UZ_data$q25_d, '1="Very trustworthy"; 2="Somewhat trustworthy"; 3="Somewhat untrustworthy"; 4="Very untrustworthy"')
UZ_data$trust_facebook = factor(UZ_data$trust_facebook, levels = c('Very trustworthy', 'Somewhat trustworthy', 'Somewhat untrustworthy', 'Very untrustworthy'), ordered = TRUE)
table(UZ_data$trust_facebook) # Display a frequency table for 'trust_facebook'

# Create a numeric variable for 'trust_parties'
UZ_data$trust_facebook_n = as.numeric(UZ_data$q25_d) 
summary(UZ_data$trust_facebook_n) # Provide a summary for 'trust_facebook_n' (Numeric)

# Perform the recode for 'trust_vkontakte'
UZ_data$trust_vkontakte = recode(UZ_data$q25_e, '1="Very trustworthy"; 2="Somewhat trustworthy"; 3="Somewhat untrustworthy"; 4="Very untrustworthy"')
UZ_data$trust_vkontakte = factor(UZ_data$trust_vkontakte, levels = c('Very trustworthy', 'Somewhat trustworthy', 'Somewhat untrustworthy', 'Very untrustworthy'), ordered = TRUE)
table(UZ_data$trust_vkontakte) # Display a frequency table for 'trust_vkontakte'

# Create a numeric variable for 'trust_vkontakte'
UZ_data$trust_vkontakte_n = as.numeric(UZ_data$q25_e) 
summary(UZ_data$trust_vkontakte_n) # Provide a summary for 'trust_vkontakte_n' (Numeric)

# Perform the recode for 'trust_western'
UZ_data$trust_western = recode(UZ_data$q25_f, '1="Very trustworthy"; 2="Somewhat trustworthy"; 3="Somewhat untrustworthy"; 4="Very untrustworthy"')
UZ_data$trust_western = factor(UZ_data$trust_western, levels = c('Very trustworthy', 'Somewhat trustworthy', 'Somewhat untrustworthy', 'Very untrustworthy'), ordered = TRUE)
table(UZ_data$trust_western) # Display a frequency table for 'trust_western'

# Create a numeric variable for 'trust_western'
UZ_data$trust_western_n = as.numeric(UZ_data$q25_f) 
summary(UZ_data$trust_western_n) # Provide a summary for 'trust_western_n' (Numeric)

#News Balance Factor
# Perform the recode for 'news_balance'
UZ_data$news_balance = recode(UZ_data$q26, '1="Only from traditional sources like television newspapers radio";2="Mostly from television newspapers radio but some from the internet social media";3="From an equal balance of television newspapers radio and the internet social media";4="Mostly from social media but some from television newspapers radio";5="Only from the internet social media"')
UZ_data$news_balance = factor(UZ_data$news_balance, levels = c('Only from traditional sources like television newspapers radio', 'Mostly from television newspapers radio but some from the internet social media', 'From an equal balance of television newspapers radio and the internet social media', 'Mostly from social media but some from television newspapers radio', 'Only from the internet social media'), ordered = TRUE)
UZ_data$news_balance = addNA(UZ_data$news_balance)
table(UZ_data$news_balance) # Display a frequency table for 'news_balance'

# Create a numeric variable for 'news_balance'
UZ_data$news_balance_n = as.numeric(UZ_data$q26)
summary(UZ_data$news_balance_n) # Provide a summary for 'news_balance_n' (Numeric)

# Tone of Social Media News about Government Factor
# Perform the recode for 'q27_c' (Tone of Social Media News about Government)
UZ_data$sm_positive_local = recode(UZ_data$q27_c, '1="Very often";2="Rather often";3="Occasionally";4="Never"')
UZ_data$sm_positive_local = factor(UZ_data$sm_positive_local, levels = c('Very often', 'Rather often', 'Occasionally', 'Never'), ordered = TRUE)
table(UZ_data$sm_positive_local) # Display a frequency table for 'sm_positive_local'

# Create a numeric variable for 'sm_positive_local'
UZ_data$sm_positive_local_n = as.numeric(UZ_data$q27_c)
summary(UZ_data$sm_positive_local_n) # Provide a summary for 'sm_positive_local_n' (Numeric)

# Perform the recode for 'q27_d' (Tone of Social Media News about Government)
UZ_data$sm_positive_central = recode(UZ_data$q27_d, '1="Very often";2="Rather often";3="Occasionally";4="Never"')
UZ_data$sm_positive_central = factor(UZ_data$sm_positive_central, levels = c('Very often', 'Rather often', 'Occasionally', 'Never'), ordered = TRUE)
table(UZ_data$sm_positive_central) # Display a frequency table for 'sm_positive_central'

# Create a numeric variable for 'sm_positive_central'
UZ_data$sm_positive_central_n = as.numeric(UZ_data$q27_d)
summary(UZ_data$sm_positive_central_n) # Provide a summary for 'sm_positive_central_n' (Numeric)

# Clickable Links Factor
# Perform the recode for 'q29_a' (Clickable Links)
UZ_data$clickable_state = recode(UZ_data$q29_a, '1="Almost everyday";2="About once a week";3="A few times a month";4="About once a month";5="Never"')
UZ_data$clickable_state = factor(UZ_data$clickable_state, levels = c('Almost everyday', 'About once a week', 'A few times a month', 'About once a month', 'Never'), ordered = TRUE)
table(UZ_data$clickable_state) # Display a frequency table for 'clickable_state'

# Create a numeric variable for 'clickable_state'
UZ_data$clickable_state_n = as.numeric(UZ_data$q29_a)
summary(UZ_data$clickable_state_n) # Provide a summary for 'clickable_state_n' (Numeric)

# Perform the recode for 'q29_b' (Clickable Links)
UZ_data$clickable_russian = recode(UZ_data$q29_b, '1="Almost everyday";2="About once a week";3="A few times a month";4="About once a month";5="Never"')
UZ_data$clickable_russian = factor(UZ_data$clickable_russian, levels = c('Almost everyday', 'About once a week', 'A few times a month', 'About once a month', 'Never'), ordered = TRUE)
table(UZ_data$clickable_russian) # Display a frequency table for 'clickable_russian'

# Create a numeric variable for 'clickable_russian'
UZ_data$clickable_russian_n = as.numeric(UZ_data$q29_b)
summary(UZ_data$clickable_russian_n) # Provide a summary for 'clickable_russian_n' (Numeric)

# Artificial Intelligence Attitude Factor
# Perform the recode for 'AI' (Artificial Intelligence Attitude)
UZ_data$AI = recode(UZ_data$q30, '1="More excited than concerned";2="Equally concerned and excited";3="More concerned than excited";4="I do not know what artificial intelligence is"')
UZ_data$AI = factor(UZ_data$AI, levels = c('More excited than concerned', 'Equally concerned and excited', 'More concerned than excited', 'I do not know what artificial intelligence is'), ordered = TRUE)
table(UZ_data$AI) # Display a frequency table for 'AI'

# Artificial Intelligence Attitude Ordinal (coding I don't know as NA)
# Perform the recode
UZ_data$AI_ordinal = recode(UZ_data$q30, '1="More excited than concerned";2="Equally concerned and excited";3="More concerned than excited";4=NA')
UZ_data$AI_ordinal = factor(UZ_data$AI_ordinal, levels = c('More excited than concerned', 'Equally concerned and excited', 'More concerned than excited', 'I do not know what artificial intelligence is'), ordered = TRUE)
table(UZ_data$AI_ordinal) # Display a frequency table for 'AI_ordinal'

# Convert 'AI_ordinal' to an ordered factor/ordinal
UZ_data$AI_ordinal = factor(UZ_data$AI_ordinal, ordered = TRUE)

# Artificial Intelligence Attitude Numeric
# Perform the recode
UZ_data$AI_n = recode(UZ_data$q30, '1=1;2=2;3=3;4=NA')
summary(UZ_data$AI_n) # Provide a summary for 'AI_n' (Numeric)

#Echo Chamber Numeric
UZ_data$echo_chamber_n = as.numeric(UZ_data$q31)
summary(UZ_data$echo_chamber_n) # Provide a summary for 'echo_chamber_n' (Numeric)

# Awareness of Government Posters Factor
# Perform the recode for 'paid_posters'
UZ_data$paid_posters = recode(UZ_data$q32, '1="I never heard of them";2="I have heard of them but I have not seen any posts from them";3="I think I have seen posts from them"')
UZ_data$paid_posters = factor(UZ_data$paid_posters, levels = c('I never heard of them', 'I have heard of them but I have not seen any posts from them', 'I think I have seen posts from them'), ordered = TRUE)
UZ_data$paid_posters = addNA(UZ_data$paid_posters)
table(UZ_data$paid_posters) # Display a frequency table for 'paid_posters'

# Create a numeric variable for 'paid_posters'
UZ_data$paid_posters_n = as.numeric(UZ_data$q32)
summary(UZ_data$paid_posters_n) # Provide a summary for 'paid_posters_n' (Numeric)

# Awareness of Government Posters Ordinal (coding "I don't know" as NA)
# Perform the recode
UZ_data$paid_posters_ordinal = recode(UZ_data$q32, '1="I never heard of them";2="I have heard of them but I have not seen any posts from them";3="I think I have seen posts from them";4=NA')
UZ_data$paid_posters_ordinal = factor(UZ_data$paid_posters_ordinal, levels = c('I never heard of them', 'I have heard of them but I have not seen any posts from them', 'I think I have seen posts from them'), ordered = TRUE)
table(UZ_data$paid_posters_ordinal) # Display a frequency table for 'paid_posters_ordinal'

# Convert 'paid_posters_ordinal' to an ordered factor/ordinal
UZ_data$paid_posters_ordinal = factor(UZ_data$paid_posters_ordinal, ordered = TRUE)

# Awareness of Government Posters Numeric
# Perform the recode
UZ_data$paid_posters_n = recode(UZ_data$q32, '1=1;2=2;3=3;4=NA')
summary(UZ_data$paid_posters_n) # Provide a summary for 'paid_posters_n' (Numeric)

# VPN Use Factor
# Perform the recode for 'vpn_use'
UZ_data$vpn_use = recode(UZ_data$q34, '1="Yes";2="No"')
UZ_data$vpn_use = factor(UZ_data$vpn_use, levels = c('Yes', 'No'), ordered = TRUE)
UZ_data$vpn_use = addNA(UZ_data$vpn_use)
table(UZ_data$vpn_use) # Display a frequency table for 'vpn_use'

# Create a numeric variable for 'vpn_use'
UZ_data$vpn_use_n = as.numeric(UZ_data$q34)
summary(UZ_data$vpn_use_n) # Provide a summary for 'vpn_use_n' (Numeric)

#Selective Exposure
# Perform the recode for 'avoidance_blocking'
UZ_data$avoidance_blocking = recode(UZ_data$q35_a, '1="Often"; 2="Sometimes"; 3="Very rarely"; 4="Never"')
UZ_data$avoidance_blocking = factor(UZ_data$avoidance_blocking, levels = c('Often', 'Sometimes', 'Very rarely', 'Never'), ordered = TRUE)
table(UZ_data$avoidance_blocking) # Display a frequency table for 'avoidance_blocking'

# Create a numeric variable for 'avoidance_blocking'
UZ_data$avoidance_blocking_n = as.numeric(UZ_data$q35_a) 
summary(UZ_data$avoidance_blocking_n) # Provide a summary for 'avoidance_blocking_numeric' (Numeric)

# Perform the recode for 'avoidance_unfriending'
UZ_data$avoidance_unfriending = recode(UZ_data$q35_b, '1="Often"; 2="Sometimes"; 3="Very rarely"; 4="Never"')
UZ_data$avoidance_unfriending = factor(UZ_data$avoidance_unfriending, levels = c('Often', 'Sometimes', 'Very rarely', 'Never'), ordered = TRUE)
table(UZ_data$avoidance_unfriending) # Display a frequency table for 'avoidance_unfriending'

# Create a numeric variable for 'avoidance_unfriending'
UZ_data$avoidance_unfriending_n = as.numeric(UZ_data$q35_b) 
summary(UZ_data$avoidance_unfriending_n) # Provide a summary for 'avoidance_unfriending_numeric' (Numeric)

# Perform the recode for 'avoidance_leaving_group'
UZ_data$avoidance_leaving_group = recode(UZ_data$q35_c, '1="Often"; 2="Sometimes"; 3="Very rarely"; 4="Never"')
UZ_data$avoidance_leaving_group = factor(UZ_data$avoidance_leaving_group, levels = c('Often', 'Sometimes', 'Very rarely', 'Never'), ordered = TRUE)
table(UZ_data$avoidance_leaving_group) # Display a frequency table for 'avoidance_leaving_group'

# Create a numeric variable for 'avoidance_leaving_group'
UZ_data$avoidance_leaving_group_n = as.numeric(UZ_data$q35_c) 
summary(UZ_data$avoidance_leaving_group_n) # Provide a summary for 'avoidance_leaving_group_numeric' (Numeric)

# Perform the recode for 'avoidance_unsubscribing'
UZ_data$avoidance_unsubscribing = recode(UZ_data$q35_d, '1="Often"; 2="Sometimes"; 3="Very rarely"; 4="Never"')
UZ_data$avoidance_unsubscribing = factor(UZ_data$avoidance_unsubscribing, levels = c('Often', 'Sometimes', 'Very rarely', 'Never'), ordered = TRUE)
table(UZ_data$avoidance_unsubscribing) # Display a frequency table for 'avoidance_unsubscribing'

# Create a numeric variable for 'avoidance_unsubscribing'
UZ_data$avoidance_unsubscribing_n = as.numeric(UZ_data$q35_d) 
summary(UZ_data$avoidance_unsubscribing_n) # Provide a summary for 'avoidance_unsubscribing_numeric' (Numeric)

#Exposure to SM Disagreement
# Perform the recode for 'sm_disagreement_politics'
UZ_data$sm_disagreement_politics = recode(UZ_data$q36_a, '1="Often"; 2="Sometimes"; 3="Very rarely"; 4="Never"')
UZ_data$sm_disagreement_politics = factor(UZ_data$sm_disagreement_politics, levels = c('Often', 'Sometimes', 'Very rarely', 'Never'), ordered = TRUE)
table(UZ_data$sm_disagreement_politics) # Display a frequency table for 'sm_disagreement_politics'

# Create a numeric variable for 'sm_disagreement_politics'
UZ_data$sm_disagreement_politics_n = as.numeric(UZ_data$q36_a) 
summary(UZ_data$sm_disagreement_politics_n) # Provide a summary for 'sm_disagreement_politics_numeric' (Numeric)

# Perform the recode for 'sm_disagreement_news'
UZ_data$sm_disagreement_news = recode(UZ_data$q36_b, '1="Often"; 2="Sometimes"; 3="Very rarely"; 4="Never"')
UZ_data$sm_disagreement_news = factor(UZ_data$sm_disagreement_news, levels = c('Often', 'Sometimes', 'Very rarely', 'Never'), ordered = TRUE)
table(UZ_data$sm_disagreement_news) # Display a frequency table for 'sm_disagreement_news'

# Create a numeric variable for 'sm_disagreement_news'
UZ_data$sm_disagreement_news_n = as.numeric(UZ_data$q36_b) 
summary(UZ_data$sm_disagreement_news_n) # Provide a summary for 'sm_disagreement_news_numeric' (Numeric)

# Perform the recode for 'sm_disagreement_issues'
UZ_data$sm_disagreement_issues = recode(UZ_data$q36_c, '1="Often"; 2="Sometimes"; 3="Very rarely"; 4="Never"')
UZ_data$sm_disagreement_issues = factor(UZ_data$sm_disagreement_issues, levels = c('Often', 'Sometimes', 'Very rarely', 'Never'), ordered = TRUE)
table(UZ_data$sm_disagreement_issues) # Display a frequency table for 'sm_disagreement_issues'

# Create a numeric variable for 'sm_disagreement_issues'
UZ_data$sm_disagreement_issues_n = as.numeric(UZ_data$q36_c) 
summary(UZ_data$sm_disagreement_issues_n) # Provide a summary for 'sm_disagreement_issues_numeric' (Numeric)

#Network Breadth
# Perform the recode for 'network_breadth'
UZ_data$network_breadth = recode(UZ_data$q37, '1="1-2"; 2="3-5"; 3="6-8"; 4="8-12"; 5="More than 12"')
UZ_data$network_breadth = factor(UZ_data$network_breadth, levels = c('1-2', '3-5', '6-8', '8-12', 'More than 12'), ordered = TRUE)
table(UZ_data$network_breadth) # Display a frequency table for 'network_breadth'

# Create a numeric variable for 'network_breadth'
UZ_data$network_breadth_n = as.numeric(UZ_data$q37) 
summary(UZ_data$network_breadth_n) # Provide a summary for 'network_breadth_numeric' (Numeric)

#Social Media Political Activity
# Perform the recode for 'sm_engage_friends'
UZ_data$sm_engage_friends = recode(UZ_data$q38_a, '1="A few times a day"; 2="Once a day"; 3="Several times a week"; 4="Once a week"; 5="Less often"; 6="Never"')
UZ_data$sm_engage_friends = factor(UZ_data$sm_engage_friends, levels = c('A few times a day', 'Once a day', 'Several times a week', 'Once a week', 'Less often', 'Never'), ordered = TRUE)
table(UZ_data$sm_engage_friends) # Display a frequency table for 'sm_engage_friends'

# Create a numeric variable for 'sm_engage_friends'
UZ_data$sm_engage_friends_n = as.numeric(UZ_data$q38_a) 
summary(UZ_data$sm_engage_friends_n) # Provide a summary for 'sm_engage_friends_n' (Numeric)

# Perform the recode for 'sm_engage_groups'
UZ_data$sm_engage_groups = recode(UZ_data$q38_b, '1="A few times a day"; 2="Once a day"; 3="Several times a week"; 4="Once a week"; 5="Less often"; 6="Never"')
UZ_data$sm_engage_groups = factor(UZ_data$sm_engage_groups, levels = c('A few times a day', 'Once a day', 'Several times a week', 'Once a week', 'Less often', 'Never'), ordered = TRUE)
table(UZ_data$sm_engage_groups) # Display a frequency table for 'sm_engage_groups'

# Create a numeric variable for 'sm_engage_groups'
UZ_data$sm_engage_groups_n = as.numeric(UZ_data$q38_b) 
summary(UZ_data$sm_engage_groups_n) # Provide a summary for 'sm_engage_groups_n' (Numeric)

# Perform the recode for 'sm_engage_post'
UZ_data$sm_engage_post = recode(UZ_data$q38_c, '1="A few times a day"; 2="Once a day"; 3="Several times a week"; 4="Once a week"; 5="Less often"; 6="Never"')
UZ_data$sm_engage_post = factor(UZ_data$sm_engage_post, levels = c('A few times a day', 'Once a day', 'Several times a week', 'Once a week', 'Less often', 'Never'), ordered = TRUE)
table(UZ_data$sm_engage_post) # Display a frequency table for 'sm_engage_post'

# Create a numeric variable for 'sm_engage_post'
UZ_data$sm_engage_post_n = as.numeric(UZ_data$q38_c) 
summary(UZ_data$sm_engage_post_n) # Provide a summary for 'sm_engage_post_n' (Numeric)

# Perform the recode for 'sm_engage_offline'
UZ_data$sm_engage_offline = recode(UZ_data$q38_f, '1="A few times a day"; 2="Once a day"; 3="Several times a week"; 4="Once a week"; 5="Less often"; 6="Never"')
UZ_data$sm_engage_offline = factor(UZ_data$sm_engage_offline, levels = c('A few times a day', 'Once a day', 'Several times a week', 'Once a week', 'Less often', 'Never'), ordered = TRUE)
table(UZ_data$sm_engage_offline) # Display a frequency table for 'sm_engage_offline'

# Create a numeric variable for 'sm_engage_offline'
UZ_data$sm_engage_offline_n = as.numeric(UZ_data$q38_f) 
summary(UZ_data$sm_engage_offline_n) # Provide a summary for 'sm_engage_offline_n' (Numeric)

#Attiudes about Global Power Change
# Perform the recode for 'change_china'
UZ_data$change_china = recode(UZ_data$q39_a, '1="Very favorable"; 2="Somewhat favorable"; 3="Neither"; 4="Somewhat unfavorable"; 5="Very unfavorable"')
UZ_data$change_china = factor(UZ_data$change_china, levels = c('Very favorable','Somewhat favorable','Neither','Somewhat unfavorable','Very unfavorable'), ordered = TRUE)
table(UZ_data$change_china) # Display a frequency table for 'change_china'

# Create a numeric variable for 'change_china'
UZ_data$change_china_n = as.numeric(UZ_data$q39_a) 
summary(UZ_data$change_china_n) # Provide a summary for 'change_china_n' (Numeric)

# Perform the recode for 'change_russia'
UZ_data$change_russia = recode(UZ_data$q39_b, '1="Very favorable"; 2="Somewhat favorable"; 3="Neither"; 4="Somewhat unfavorable"; 5="Very unfavorable"')
UZ_data$change_russia = factor(UZ_data$change_russia, levels = c('Very favorable','Somewhat favorable','Neither','Somewhat unfavorable','Very unfavorable'), ordered = TRUE)
table(UZ_data$change_russia) # Display a frequency table for 'change_russia'

# Create a numeric variable for 'change_russia'
UZ_data$change_russia_n = as.numeric(UZ_data$q39_b) 
summary(UZ_data$change_russia_n) # Provide a summary for 'change_russia_n' (Numeric)

# Perform the recode for 'change_US'
UZ_data$change_US = recode(UZ_data$q39_c, '1="Very favorable"; 2="Somewhat favorable"; 3="Neither"; 4="Somewhat unfavorable"; 5="Very unfavorable"')
UZ_data$change_US = factor(UZ_data$change_US, levels = c('Very favorable','Somewhat favorable','Neither','Somewhat unfavorable','Very unfavorable'), ordered = TRUE)
table(UZ_data$change_US) # Display a frequency table for 'change_US'

# Create a numeric variable for 'change_US'
UZ_data$change_US_n = as.numeric(UZ_data$q39_c) 
summary(UZ_data$change_US_n) # Provide a summary for 'change_US_n' (Numeric)

#Creating Dummy Variables from all Relevant UZ Variables############################################################################################
library(car)

# internet_use
UZ_data$internet_use_No = recode(UZ_data$internet_use, '"No"=1; NA=NA; else=0')
UZ_data$internet_use_Yes = recode(UZ_data$internet_use, '"Yes"=1; NA=NA; else=0')

# ethnicity
UZ_data$ethnicity_Kazakh = recode(UZ_data$ethnicity, '"Kazakh"=1; NA=NA; else=0')
UZ_data$ethnicity_Russian = recode(UZ_data$ethnicity, '"Russian"=1; NA=NA; else=0')
UZ_data$ethnicity_Uzbek = recode(UZ_data$ethnicity, '"Uzbek"=1; NA=NA; else=0')
UZ_data$ethnicity_Tajik = recode(UZ_data$ethnicity, '"Tajik"=1; NA=NA; else=0')
UZ_data$ethnicity_Kyrgyz = recode(UZ_data$ethnicity, '"Kyrgyz"=1; NA=NA; else=0')
UZ_data$ethnicity_Other = recode(UZ_data$ethnicity, '"Other"=1; NA=NA; else=0')

# urbanicity
UZ_data$urbanicity_City = recode(UZ_data$urbanicity, '"City"=1; NA=NA; else=0')
UZ_data$urbanicity_Village = recode(UZ_data$urbanicity, '"Village"=1; NA=NA; else=0')

# gender
UZ_data$gender_Female = recode(UZ_data$gender, '"Female"=1; NA=NA; else=0')
UZ_data$gender_Male = recode(UZ_data$gender, '"Male"=1; NA=NA; else=0')

# important_issue_first
UZ_data$important_issue_first_Political_instability = recode(UZ_data$important_issue_first, "'Political instability'=1; NA=NA; else=0")
UZ_data$important_issue_first_Unemployment = recode(UZ_data$important_issue_first, "'Unemployment'=1; NA=NA; else=0")
UZ_data$important_issue_first_Inflation_Prices = recode(UZ_data$important_issue_first, "'Inflation Prices'=1; NA=NA; else=0")
UZ_data$important_issue_first_Wages_pensions = recode(UZ_data$important_issue_first, "'Wages pensions'=1; NA=NA; else=0")
UZ_data$important_issue_first_Taxes = recode(UZ_data$important_issue_first, "'Taxes'=1; NA=NA; else=0")
UZ_data$important_issue_first_Access_to_basic_needs = recode(UZ_data$important_issue_first, "'Access to basic needs'=1; NA=NA; else=0")
UZ_data$important_issue_first_General_economic_situation = recode(UZ_data$important_issue_first, "'General economic situation'=1; NA=NA; else=0")
UZ_data$important_issue_first_Deterioration_of_the_quality_and_access_to_education = recode(UZ_data$important_issue_first, "'Deterioration of the quality and access to education'=1; NA=NA; else=0")
UZ_data$important_issue_first_Construction_of_houses_residential_premises = recode(UZ_data$important_issue_first, "'Construction of houses residential premises'=1; NA=NA; else=0")
UZ_data$important_issue_first_Terrorism = recode(UZ_data$important_issue_first, "'Terrorism'=1; NA=NA; else=0")
UZ_data$important_issue_first_War_conflict_in_other_countries = recode(UZ_data$important_issue_first, "'War conflict in other countries'=1; NA=NA; else=0")
UZ_data$important_issue_first_Increase_in_crime_violence_concerns_about_personal_safety = recode(UZ_data$important_issue_first, "'Increase in crime violence concerns about personal safety'=1; NA=NA; else=0")
UZ_data$important_issue_first_Corruption = recode(UZ_data$important_issue_first, "'Corruption'=1; NA=NA; else=0")
UZ_data$important_issue_first_Lack_of_opportunities = recode(UZ_data$important_issue_first, "'Lack of opportunities'=1; NA=NA; else=0")
UZ_data$important_issue_first_Discrimination_ethnic_or_religious_tensions = recode(UZ_data$important_issue_first, "'Discrimination ethnic or religious tensions'=1; NA=NA; else=0")
UZ_data$important_issue_first_Infrastructure = recode(UZ_data$important_issue_first, "'Infrastructure'=1; NA=NA; else=0")
UZ_data$important_issue_first_Deterioration_of_the_environment = recode(UZ_data$important_issue_first, "'Deterioration of the environment'=1; NA=NA; else=0")
UZ_data$important_issue_first_Unresolved_territorial_conflicts = recode(UZ_data$important_issue_first, "'Unresolved territorial conflicts'=1; NA=NA; else=0")
UZ_data$important_issue_first_Emigration = recode(UZ_data$important_issue_first, "'Emigration'=1; NA=NA; else=0")

# important_issue_second
UZ_data$important_issue_second_Political_instability = recode(UZ_data$important_issue_second, "'Political instability'=1; NA=NA; else=0")
UZ_data$important_issue_second_Unemployment = recode(UZ_data$important_issue_second, "'Unemployment'=1; NA=NA; else=0")
UZ_data$important_issue_second_Inflation_Prices = recode(UZ_data$important_issue_second, "'Inflation Prices'=1; NA=NA; else=0")
UZ_data$important_issue_second_Wages_pensions = recode(UZ_data$important_issue_second, "'Wages pensions'=1; NA=NA; else=0")
UZ_data$important_issue_second_Taxes = recode(UZ_data$important_issue_second, "'Taxes'=1; NA=NA; else=0")
UZ_data$important_issue_second_Access_to_basic_needs = recode(UZ_data$important_issue_second, "'Access to basic needs'=1; NA=NA; else=0")
UZ_data$important_issue_second_General_economic_situation = recode(UZ_data$important_issue_second, "'General economic situation'=1; NA=NA; else=0")
UZ_data$important_issue_second_Deterioration_of_the_quality_and_access_to_education = recode(UZ_data$important_issue_second, "'Deterioration of the quality and access to education'=1; NA=NA; else=0")
UZ_data$important_issue_second_Construction_of_houses_residential_premises = recode(UZ_data$important_issue_second, "'Construction of houses residential premises'=1; NA=NA; else=0")
UZ_data$important_issue_second_Terrorism = recode(UZ_data$important_issue_second, "'Terrorism'=1; NA=NA; else=0")
UZ_data$important_issue_second_War_conflict_in_other_countries = recode(UZ_data$important_issue_second, "'War conflict in other countries'=1; NA=NA; else=0")
UZ_data$important_issue_second_Increase_in_crime_violence_concerns_about_personal_safety = recode(UZ_data$important_issue_second, "'Increase in crime violence concerns about personal safety'=1; NA=NA; else=0")
UZ_data$important_issue_second_Corruption = recode(UZ_data$important_issue_second, "'Corruption'=1; NA=NA; else=0")
UZ_data$important_issue_second_Lack_of_opportunities = recode(UZ_data$important_issue_second, "'Lack of opportunities'=1; NA=NA; else=0")
UZ_data$important_issue_second_Discrimination_ethnic_or_religious_tensions = recode(UZ_data$important_issue_second, "'Discrimination ethnic or religious tensions'=1; NA=NA; else=0")
UZ_data$important_issue_second_Infrastructure = recode(UZ_data$important_issue_second, "'Infrastructure'=1; NA=NA; else=0")
UZ_data$important_issue_second_Deterioration_of_the_environment = recode(UZ_data$important_issue_second, "'Deterioration of the environment'=1; NA=NA; else=0")
UZ_data$important_issue_second_Unresolved_territorial_conflicts = recode(UZ_data$important_issue_second, "'Unresolved territorial conflicts'=1; NA=NA; else=0")
UZ_data$important_issue_second_Emigration = recode(UZ_data$important_issue_second, "'Emigration'=1; NA=NA; else=0")

# sm_use
UZ_data$sm_use_No = recode(UZ_data$sm_use, '"No"=1; NA=NA; else=0')
UZ_data$sm_use_Yes = recode(UZ_data$sm_use, '"Yes"=1; NA=NA; else=0')

# vpn_use
UZ_data$vpn_use_No = recode(UZ_data$vpn_use, '"No"=1; NA=NA; else=0')
UZ_data$vpn_use_Yes = recode(UZ_data$vpn_use, '"Yes"=1; NA=NA; else=0')

# paid_posters
# Assuming 'I never heard of them', 'I have heard of them but I have not seen any posts from them', 'I think I have seen posts from them' as categories
UZ_data$paid_posters_never_heard = recode(UZ_data$paid_posters, '"I never heard of them"=1; NA=NA; else=0')
UZ_data$paid_posters_heard_not_seen = recode(UZ_data$paid_posters, '"I have heard of them but I have not seen any posts from them"=1; NA=NA; else=0')
UZ_data$paid_posters_seen_posts = recode(UZ_data$paid_posters, '"I think I have seen posts from them"=1; NA=NA; else=0')

# Frequency tables for internet use
table(UZ_data$internet_use_No)
table(UZ_data$internet_use_Yes)

# Frequency tables for ethnicity
table(UZ_data$ethnicity_Kazakh)
table(UZ_data$ethnicity_Russian)
table(UZ_data$ethnicity_Uzbek)
table(UZ_data$ethnicity_Tajik)
table(UZ_data$ethnicity_Kyrgyz)
table(UZ_data$ethnicity_Other)

# Frequency tables for urbanicity
table(UZ_data$urbanicity_City)
table(UZ_data$urbanicity_Village)

# Frequency tables for gender
table(UZ_data$gender_Female)
table(UZ_data$gender_Male)

# Frequency tables for the first important issue
table(UZ_data$important_issue_first_Political_instability)
table(UZ_data$important_issue_first_Unemployment)
table(UZ_data$important_issue_first_Inflation_Prices)
table(UZ_data$important_issue_first_Wages_pensions)
table(UZ_data$important_issue_first_Taxes)
table(UZ_data$important_issue_first_Access_to_basic_needs)
table(UZ_data$important_issue_first_General_economic_situation)
table(UZ_data$important_issue_first_Deterioration_of_the_quality_and_access_to_education)
table(UZ_data$important_issue_first_Construction_of_houses_residential_premises)
table(UZ_data$important_issue_first_Terrorism)
table(UZ_data$important_issue_first_War_conflict_in_other_countries)
table(UZ_data$important_issue_first_Increase_in_crime_violence_concerns_about_personal_safety)
table(UZ_data$important_issue_first_Corruption)
table(UZ_data$important_issue_first_Lack_of_opportunities)
table(UZ_data$important_issue_first_Discrimination_ethnic_or_religious_tensions)
table(UZ_data$important_issue_first_Infrastructure)
table(UZ_data$important_issue_first_Deterioration_of_the_environment)
table(UZ_data$important_issue_first_Unresolved_territorial_conflicts)
table(UZ_data$important_issue_first_Emigration)

# Frequency tables for the first important issue
table(UZ_data$important_issue_second_Political_instability)
table(UZ_data$important_issue_second_Unemployment)
table(UZ_data$important_issue_second_Inflation_Prices)
table(UZ_data$important_issue_second_Wages_pensions)
table(UZ_data$important_issue_second_Taxes)
table(UZ_data$important_issue_second_Access_to_basic_needs)
table(UZ_data$important_issue_second_General_economic_situation)
table(UZ_data$important_issue_second_Deterioration_of_the_quality_and_access_to_education)
table(UZ_data$important_issue_second_Construction_of_houses_residential_premises)
table(UZ_data$important_issue_second_Terrorism)
table(UZ_data$important_issue_second_War_conflict_in_other_countries)
table(UZ_data$important_issue_second_Increase_in_crime_violence_concerns_about_personal_safety)
table(UZ_data$important_issue_second_Corruption)
table(UZ_data$important_issue_second_Lack_of_opportunities)
table(UZ_data$important_issue_second_Discrimination_ethnic_or_religious_tensions)
table(UZ_data$important_issue_second_Infrastructure)
table(UZ_data$important_issue_second_Deterioration_of_the_environment)
table(UZ_data$important_issue_second_Unresolved_territorial_conflicts)
table(UZ_data$important_issue_second_Emigration)
# Frequency tables for social media use
table(UZ_data$sm_use_No)
table(UZ_data$sm_use_Yes)

# Frequency tables for VPN use
table(UZ_data$vpn_use_No)
table(UZ_data$vpn_use_Yes)

# Frequency tables for paid posters
table(UZ_data$paid_posters_never_heard)
table(UZ_data$paid_posters_heard_not_seen)
table(UZ_data$paid_posters_seen_posts)

#Scaling
library(scales)
UZ_data$age_n_sc = rescale(UZ_data$age_n, to = c(0, 1))
UZ_data$edu_n_sc = rescale(UZ_data$edu_n, to = c(0, 1))
UZ_data$inc_n_sc = rescale(UZ_data$inc_n, to = c(0, 1))
UZ_data$pol_interest_n_sc = rescale(UZ_data$pol_interest_n, to = c(1, 0))
UZ_data$system_capable_n_sc = rescale(UZ_data$system_capable_n, to = c(1, 0))
UZ_data$system_proud_n_sc = rescale(UZ_data$system_proud_n, to = c(1, 0))
UZ_data$system_live_n_sc = rescale(UZ_data$system_live_n, to = c(1, 0))
UZ_data$system_hurdles_participate_n_sc = rescale(UZ_data$system_hurdles_participate_n, to = c(1, 0))
UZ_data$tracking_local_n_sc = rescale(UZ_data$tracking_local_n, to = c(0, 1))
UZ_data$tracking_companies_n_sc = rescale(UZ_data$tracking_companies_n, to = c(0, 1))
UZ_data$democracy_elections_n_sc = rescale(UZ_data$democracy_elections_n, to = c(1, 0))
UZ_data$democracy_press_n_sc = rescale(UZ_data$democracy_press_n, to = c(1, 0))
UZ_data$facebook_n_sc = rescale(UZ_data$facebook_n, to = c(0, 1))
UZ_data$vkontakte_n_sc = rescale(UZ_data$vkontakte_n, to = c(0, 1))
UZ_data$instagram_n_sc = rescale(UZ_data$instagram_n, to = c(0, 1))
UZ_data$tiktok_n_sc = rescale(UZ_data$tiktok_n, to = c(0, 1))
UZ_data$twitter_n_sc = rescale(UZ_data$twitter_n, to = c(0, 1))
UZ_data$youtube_n_sc = rescale(UZ_data$youtube_n, to = c(0, 1))
UZ_data$whatsapp_n_sc = rescale(UZ_data$whatsapp_n, to = c(0, 1))
UZ_data$telegram_n_sc = rescale(UZ_data$telegram_n, to = c(0, 1))
UZ_data$pol_news_tv_n_sc = rescale(UZ_data$pol_news_tv_n, to = c(1, 0))
UZ_data$pol_news_facebook_n_sc = rescale(UZ_data$pol_news_facebook_n, to = c(1, 0))
UZ_data$pol_news_vkontakte_n_sc = rescale(UZ_data$pol_news_vkontakte_n, to = c(1, 0))
UZ_data$pol_news_tiktok_n_sc = rescale(UZ_data$pol_news_tiktok_n, to = c(1, 0))
UZ_data$pol_news_twitter_n_sc = rescale(UZ_data$pol_news_twitter_n, to = c(1, 0))
UZ_data$pol_news_odnoklassniki_n_sc = rescale(UZ_data$pol_news_odnoklassniki_n, to = c(1, 0))
UZ_data$trust_state_n_sc = rescale(UZ_data$trust_state_n, to = c(1, 0))
UZ_data$trust_russian_media_n_sc = rescale(UZ_data$trust_russian_media_n, to = c(1, 0))
UZ_data$trust_internet_n_sc = rescale(UZ_data$trust_internet_n, to = c(1, 0))
UZ_data$trust_facebook_n_sc = rescale(UZ_data$trust_facebook_n, to = c(1, 0))
UZ_data$trust_vkontakte_n_sc = rescale(UZ_data$trust_vkontakte_n, to = c(1, 0))
UZ_data$trust_western_n_sc = rescale(UZ_data$trust_western_n, to = c(1, 0))
UZ_data$news_balance_n_sc = rescale(UZ_data$news_balance_n, to = c(0, 1))
UZ_data$sm_positive_local_n_sc = rescale(UZ_data$sm_positive_local_n, to = c(1, 0))
UZ_data$sm_positive_central_n_sc = rescale(UZ_data$sm_positive_central_n, to = c(1, 0))
UZ_data$clickable_state_n_sc = rescale(UZ_data$clickable_state_n, to = c(1, 0))
UZ_data$clickable_russian_n_sc = rescale(UZ_data$clickable_russian_n, to = c(1, 0))
UZ_data$AI_n_sc = rescale(UZ_data$AI_n, to = c(1, 0))
UZ_data$echo_chamber_n_sc = rescale(UZ_data$echo_chamber_n, to = c(0, 1))
UZ_data$avoidance_blocking_n_sc = rescale(UZ_data$avoidance_blocking_n, to = c(1, 0))
UZ_data$avoidance_unfriending_n_sc = rescale(UZ_data$avoidance_unfriending_n, to = c(1, 0))
UZ_data$avoidance_leaving_group_n_sc = rescale(UZ_data$avoidance_leaving_group_n, to = c(1, 0))
UZ_data$avoidance_unsubscribing_n_sc = rescale(UZ_data$avoidance_unsubscribing_n, to = c(1, 0))
UZ_data$sm_disagreement_politics_n_sc = rescale(UZ_data$sm_disagreement_politics_n, to = c(0, 1))
UZ_data$sm_disagreement_news_n_sc = rescale(UZ_data$sm_disagreement_news_n, to = c(0, 1))
UZ_data$sm_disagreement_issues_n_sc = rescale(UZ_data$sm_disagreement_issues_n, to = c(0, 1))
UZ_data$network_breadth_n_sc = rescale(UZ_data$network_breadth_n, to = c(0, 1))
UZ_data$sm_engage_friends_n_sc = rescale(UZ_data$sm_engage_friends_n, to = c(1, 0))
UZ_data$sm_engage_groups_n_sc = rescale(UZ_data$sm_engage_groups_n, to = c(1, 0))
UZ_data$sm_engage_post_n_sc = rescale(UZ_data$sm_engage_post_n, to = c(1, 0))
UZ_data$sm_engage_offline_sc = rescale(UZ_data$sm_engage_offline_n, to = c(1, 0))
UZ_data$change_china_sc = rescale(UZ_data$change_china_n, to = c(1, 0))
UZ_data$change_russia_sc = rescale(UZ_data$change_russia_n, to = c(1, 0))
UZ_data$change_US_sc = rescale(UZ_data$change_US_n, to = c(1, 0))

# Add a new column to each dataset to identify its source
GA_data$country = 'GA'
KG_data$country = 'KG'
KZ_data$country = 'KZ'
TJ_data$country = 'TJ'
UZ_data$country = 'UZ'

#Single Dataframe - All Countries############################################################################################
library(data.table)
CAB_data = rbindlist(list(GA_data,KG_data,KZ_data,TJ_data,UZ_data), fill = TRUE)
table(CAB_data$country)

save.image("C:/Users/102142/Dropbox/Research/Internet Politics/Central Asia/Central Asia Grant Survey Project/GitHub-CAB-Issues/CAB_data_issues.RData")
