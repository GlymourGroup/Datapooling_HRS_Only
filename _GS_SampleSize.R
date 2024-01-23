Diabetes <- complete.cases(hrs_tv_wide %>% 
                              select(CASE_ID_HRS_RA, BMI_HRS_2, 
                                     DIAB_MATCH_VAR, BMI_HRS_9,
                                     HbA1c,
                                     DIABETES_HRS_14))
Diabetes <- hrs_tv_wide[Diabetes,]



genhealth_v <- complete.cases(hrs_tv_wide %>% 
                             select(CASE_ID_HRS_RA, BMI_HRS_2, 
                                    BMI_HRS_9, GENHEALTH_HRS_9,
                                    GENHEALTH_HRS_14))
GENHEALTH <- hrs_tv_wide[genhealth_v,]







sbp_v <- complete.cases(hrs_tv_wide %>% 
                                select(CASE_ID_HRS_RA, BMI_HRS_2, 
                                       BMI_HRS_9, SYSTOLIC_BP_HRS_9,
                                       SYSTOLIC_BP_HRS_14))
SBP <- hrs_tv_wide[sbp_v,]
