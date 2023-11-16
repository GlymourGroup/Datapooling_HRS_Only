## Complete Cases ----
# Find complete cases with regards to relevant data:
cc <- d$HRS %>% select(CASE_ID_HRS_RA,  ends_with("_RA"),
                       BMI_HRS_2,
                       #ends_with("_9"), 
                       SYSTOLIC_BP_HRS_14, GENHEALTH_HRS_14, DIABETES_HRS_14,
                       -ETH_HRS_RA,-RACE_HRS_RA,
                       # -starts_with("COG"), 
                       # -INTERVIEW_ENDDT_HRS_9, 
                       # -BMI_REPORTED_HRS_9, -ALCOHOL_DRINKSPERMO_HRS_9,
                       # -alc_nweek_HRS_9, -alc_ndrink_HRS_9, -alcohol_dpw_HRS_9,
                       # -alcohol_cat_HRS_9, -alcohol_bingedrink_HRS_9, 
                       # -alcohol_bingecat_HRS_9, -summed_recall_score_HRS_9,
                       # -BP_DISPUTE_HRS_9, -DM_DISPUTE_HRS_9, -CA_DISPUTE_HRS_9,
                       # -MI_DISPUTE_HRS_9
)

table(complete.cases(cc))

# very few complete cases overall, but we can check by outcome
# Diabetes:
table(complete.cases(cc %>% select(-SYSTOLIC_BP_HRS_14, -GENHEALTH_HRS_14)))
# Systolic BP:
table(complete.cases(cc %>% select(-GENHEALTH_HRS_14, -DIABETES_HRS_14)))
# Gen Health
table(complete.cases(cc %>% select(-DIABETES_HRS_14, -SYSTOLIC_BP_HRS_14)))

# Doesn't seem unreasonable to go with Gen Health ~ 2021 Obs
# Check in with others about this 
cc <- cc[complete.cases(cc %>% select(-DIABETES_HRS_14, -SYSTOLIC_BP_HRS_14)),]

d$HRS <- d$HRS %>% filter(CASE_ID_HRS_RA %in% cc$CASE_ID_HRS_RA)


# 03_CacluatateWeights ----
# 02_DataPrep.R
# Find complete cases with regards to relevant data:
cc <- d$old %>% select(CASE_ID_OLD_RA,  ends_with("_RA"),
                       #BMI_HRS_2,
                       ends_with("_9"), 
                       SYSTOLIC_BP_HRS_14, GENHEALTH_HRS_14, DIABETES_HRS_14,
                       -starts_with("COG"))

table(complete.cases(cc))
