# With the variables selected, clean up the data to make it easy to use

# Set-up ----

# Clear Environment
rm(list=ls())
gc()

## Libraries ----
library("tidyverse")

# Instructions
#instructions <- dget("Instructions/Instructions_01.R")

# Load Data ----

# Main dataset, mostly from RAND Longitudinal + Tracker: 
HRS <- read.csv("../../DP_HRS_Only/HRS_pulled.csv")

# Re-code ----
hrs <- HRS %>%
  mutate(
    ## Demographics ----

    # Birth Year - YYYY format
    # Birth Month - MM format, defaults to 7 if otherwise unknown
    # birthday (uses 15 as date, 7 as month if missing, NA if year is missing)
    BIRTHDAY_HRS     = as.Date(HRS$BIRTHDATE_HRS_RA,origin="1960-01-01"),

    # Interview date for the survey year 1996
    interviewdate01 = as.Date(INTERVIEW_BEGDT_HRS_1,origin="1960-01-01"),
    interviewdate02 = as.Date(INTERVIEW_BEGDT_HRS_2,origin="1960-01-01"),
    interviewdate03 = as.Date(INTERVIEW_BEGDT_HRS_3,origin="1960-01-01"),
    interviewdate04 = as.Date(INTERVIEW_BEGDT_HRS_4,origin="1960-01-01"),
    interviewdate05 = as.Date(INTERVIEW_BEGDT_HRS_5,origin="1960-01-01"),
    interviewdate06 = as.Date(INTERVIEW_BEGDT_HRS_6,origin="1960-01-01"),
    interviewdate07 = as.Date(INTERVIEW_BEGDT_HRS_7,origin="1960-01-01"),
    interviewdate08 = as.Date(INTERVIEW_BEGDT_HRS_8,origin="1960-01-01"),
    interviewdate09 = as.Date(INTERVIEW_BEGDT_HRS_9,origin="1960-01-01"),
    interviewdate10 = as.Date(INTERVIEW_BEGDT_HRS_10,origin="1960-01-01"),
    interviewdate11 = as.Date(INTERVIEW_BEGDT_HRS_11,origin="1960-01-01"),
    interviewdate12 = as.Date(INTERVIEW_BEGDT_HRS_12,origin="1960-01-01"),
    interviewdate13 = as.Date(INTERVIEW_BEGDT_HRS_13,origin="1960-01-01"),
    interviewdate14 = as.Date(INTERVIEW_BEGDT_HRS_14,origin="1960-01-01"),
    interviewdate15 = as.Date(INTERVIEW_BEGDT_HRS_15,origin="1960-01-01"),

    # Sex - 1 = male, female = 2
    FEMALE_HRS_RA = recode(SEX_HRS_RA,
                        "1" = 0,
                        "2" = 1),

    # Race/Ethnicity; Black = 2, White = 1, Other = 3
    # ETH_HRS_RA: Hispanic = 1, Non-Hispanic = 0
    ETH_HRS_RA = case_when(
      ETH_HRS_RA == 0 ~ "Non-Hispanic",
      ETH_HRS_RA == 1 ~ "Hispanic"),

    RACE_HRS_RA=case_when(
      RACEETH_HRS_RA==1 ~ "White",
      RACEETH_HRS_RA==2 ~ "Black",
      RACEETH_HRS_RA==3 ~ "Other"),
    
    RACE_ETH_HRS_RA = case_when(
      RACEETH_HRS_RA == 2 ~ "Black",
      ETH_HRS_RA == "Hispanic" ~ "Hispanic",
      RACEETH_HRS_RA == 1 ~ "White",
      TRUE ~ "Other"),

    # Birth Country
    USBIRTH_HRS_RA = case_when(
      BIRTHCOUNTRY_HRS_RA < 11 ~ "In the US",
      BIRTHCOUNTRY_HRS_RA ==11 ~ "In other country"), # Includes US Territories
    
    # Education coded as years of education (range: 0-17)
    ## ceilings in NLSY capped to 17 to match HRS
    # Create categorical education categories to exact match on
    EDU_NEW_HRS_RA = case_when(
      EDU_YEARS_HRS_RA <= 8 ~ 0, # Less than HS
      EDU_YEARS_HRS_RA <= 11~ 1, # Some HS
      EDU_YEARS_HRS_RA <= 15~ 2, # HS/Some higher
      EDU_YEARS_HRS_RA >  15~ 3),# College Grad +

    # Marriage - Coded below

    # Veteran status already coded as binary

    # Parental Education coded as years of education with ceiling of 17 years
    # If missing set to 0, otherwise use a similar schema to EDU_NEW
    MOM_EDU_HRS_RA = case_when(is.na(MOM_EDU_HRS_RA) ~ 0,
                               MOM_EDU_HRS_RA <= 8 ~ 1, # Less than HS
                               MOM_EDU_HRS_RA <= 11~ 2, # Some HS
                               MOM_EDU_HRS_RA <= 15~ 3, # HS/Some higher
                               MOM_EDU_HRS_RA >  15~ 4),
    DAD_EDU_HRS_RA = case_when(is.na(DAD_EDU_HRS_RA) ~ 0,
                               DAD_EDU_HRS_RA <= 8 ~ 1, # Less than HS
                               DAD_EDU_HRS_RA <= 11~ 2, # Some HS
                               DAD_EDU_HRS_RA <= 15~ 3, # HS/Some higher
                               DAD_EDU_HRS_RA >  15~ 4),
    
    

    # Religion - "at baseline"
    RELIGION_HRS_RA = recode(RELIGION_HRS_RA,
                             "1" = "Protestant",
                             "2" = "Catholic",
                             "3" = "Jewish",
                             "4" = "None",
                             "5" = "Other"),

    ## SES ----
    # HH income is for respondent and spouse, not necessarily entire hh
    # PP = "Per person"
    INCOME_PP_LOG10_HRS_1  = log10(HH_INCOME_HRS_1+1)/HHNUMBER_HRS_1,
    INCOME_PP_LOG10_HRS_2  = log10(HH_INCOME_HRS_2+1)/HHNUMBER_HRS_2,
    INCOME_PP_LOG10_HRS_3  = log10(HH_INCOME_HRS_3+1)/HHNUMBER_HRS_3,
    INCOME_PP_LOG10_HRS_4  = log10(HH_INCOME_HRS_4+1)/HHNUMBER_HRS_4,
    INCOME_PP_LOG10_HRS_5  = log10(HH_INCOME_HRS_5+1)/HHNUMBER_HRS_5,
    INCOME_PP_LOG10_HRS_6  = log10(HH_INCOME_HRS_6+1)/HHNUMBER_HRS_6,
    INCOME_PP_LOG10_HRS_7  = log10(HH_INCOME_HRS_7+1)/HHNUMBER_HRS_7,
    INCOME_PP_LOG10_HRS_8  = log10(HH_INCOME_HRS_8+1)/HHNUMBER_HRS_8,
    INCOME_PP_LOG10_HRS_9  = log10(HH_INCOME_HRS_9+1)/HHNUMBER_HRS_9,
    INCOME_PP_LOG10_HRS_10 = log10(HH_INCOME_HRS_10+1)/HHNUMBER_HRS_10,
    INCOME_PP_LOG10_HRS_11 = log10(HH_INCOME_HRS_11+1)/HHNUMBER_HRS_11,
    INCOME_PP_LOG10_HRS_12 = log10(HH_INCOME_HRS_12+1)/HHNUMBER_HRS_12,
    INCOME_PP_LOG10_HRS_13 = log10(HH_INCOME_HRS_13+1)/HHNUMBER_HRS_13,
    INCOME_PP_LOG10_HRS_14 = log10(HH_INCOME_HRS_14+1)/HHNUMBER_HRS_14,
    INCOME_PP_LOG10_HRS_15 = log10(HH_INCOME_HRS_15+1)/HHNUMBER_HRS_15,


    ## Biometrics ----
    # Physical Measurements available after Wave 7
    # Height - Use Physical Measure when available, otherwise self-report (m)
    HEIGHT_HRS_1 = HEIGHT_REPORTED_HRS_1,
    HEIGHT_HRS_2 = HEIGHT_REPORTED_HRS_2,
    HEIGHT_HRS_3 = HEIGHT_REPORTED_HRS_3,
    HEIGHT_HRS_4 = HEIGHT_REPORTED_HRS_4,
    HEIGHT_HRS_5 = HEIGHT_REPORTED_HRS_5,
    HEIGHT_HRS_6 = HEIGHT_REPORTED_HRS_6,
    HEIGHT_HRS_7 = HEIGHT_REPORTED_HRS_7,
    HEIGHT_HRS_8 = if_else(is.na(HEIGHT_HRS_8), HEIGHT_REPORTED_HRS_8, HEIGHT_HRS_8),
    HEIGHT_HRS_9 = if_else(is.na(HEIGHT_HRS_9), HEIGHT_REPORTED_HRS_9, HEIGHT_HRS_9),
    HEIGHT_HRS_10= if_else(is.na(HEIGHT_HRS_10),HEIGHT_REPORTED_HRS_10,HEIGHT_HRS_10),
    HEIGHT_HRS_11= if_else(is.na(HEIGHT_HRS_11),HEIGHT_REPORTED_HRS_11,HEIGHT_HRS_11),
    HEIGHT_HRS_12= if_else(is.na(HEIGHT_HRS_12),HEIGHT_REPORTED_HRS_12,HEIGHT_HRS_12),
    HEIGHT_HRS_13= if_else(is.na(HEIGHT_HRS_13),HEIGHT_REPORTED_HRS_13,HEIGHT_HRS_13),
    HEIGHT_HRS_14= if_else(is.na(HEIGHT_HRS_14),HEIGHT_REPORTED_HRS_14,HEIGHT_HRS_14),
    HEIGHT_HRS_15= HEIGHT_REPORTED_HRS_15,  # Measured height not recorded
    
    # Weight - Use Physical Measure when available, otherwise self-report (kg)
    WEIGHT_HRS_1 = WEIGHT_REPORTED_HRS_1,
    WEIGHT_HRS_2 = WEIGHT_REPORTED_HRS_2,
    WEIGHT_HRS_3 = WEIGHT_REPORTED_HRS_3,
    WEIGHT_HRS_4 = WEIGHT_REPORTED_HRS_4,
    WEIGHT_HRS_5 = WEIGHT_REPORTED_HRS_5,
    WEIGHT_HRS_6 = WEIGHT_REPORTED_HRS_6,
    WEIGHT_HRS_7 = WEIGHT_REPORTED_HRS_7,
    WEIGHT_HRS_8 = if_else(is.na(WEIGHT_HRS_8), WEIGHT_REPORTED_HRS_8, WEIGHT_HRS_8),
    WEIGHT_HRS_9 = if_else(is.na(WEIGHT_HRS_9), WEIGHT_REPORTED_HRS_9, WEIGHT_HRS_9),
    WEIGHT_HRS_10= if_else(is.na(WEIGHT_HRS_10),WEIGHT_REPORTED_HRS_10,WEIGHT_HRS_10),
    WEIGHT_HRS_11= if_else(is.na(WEIGHT_HRS_11),WEIGHT_REPORTED_HRS_11,WEIGHT_HRS_11),
    WEIGHT_HRS_12= if_else(is.na(WEIGHT_HRS_12),WEIGHT_REPORTED_HRS_12,WEIGHT_HRS_12),
    WEIGHT_HRS_13= if_else(is.na(WEIGHT_HRS_13),WEIGHT_REPORTED_HRS_13,WEIGHT_HRS_13),
    WEIGHT_HRS_14= if_else(is.na(WEIGHT_HRS_14),WEIGHT_REPORTED_HRS_14,WEIGHT_HRS_14),
    WEIGHT_HRS_15= WEIGHT_REPORTED_HRS_15, # Not physically measured
    
    # BMI - Use Physical Measure when available, otherwise self-report (kg/m2)
    BMI_HRS_1 = BMI_REPORTED_HRS_1,
    BMI_HRS_2 = BMI_REPORTED_HRS_2,
    BMI_HRS_3 = BMI_REPORTED_HRS_3,
    BMI_HRS_4 = BMI_REPORTED_HRS_4,
    BMI_HRS_5 = BMI_REPORTED_HRS_5,
    BMI_HRS_6 = BMI_REPORTED_HRS_6,
    BMI_HRS_7 = BMI_REPORTED_HRS_7,
    BMI_HRS_8 = if_else(is.na(BMI_HRS_8), BMI_REPORTED_HRS_8, BMI_HRS_8),
    BMI_HRS_9 = if_else(is.na(BMI_HRS_9), BMI_REPORTED_HRS_9, BMI_HRS_9),
    BMI_HRS_10= if_else(is.na(BMI_HRS_10),BMI_REPORTED_HRS_10,BMI_HRS_10),
    BMI_HRS_11= if_else(is.na(BMI_HRS_11),BMI_REPORTED_HRS_11,BMI_HRS_11),
    BMI_HRS_12= if_else(is.na(BMI_HRS_12),BMI_REPORTED_HRS_12,BMI_HRS_12),
    BMI_HRS_13= if_else(is.na(BMI_HRS_13),BMI_REPORTED_HRS_13,BMI_HRS_13),
    BMI_HRS_14= if_else(is.na(BMI_HRS_14),BMI_REPORTED_HRS_14,BMI_HRS_14),
    BMI_HRS_15= BMI_REPORTED_HRS_15, # Not physically measured
    
    ### CESD_New ----
    # use the same questions found in NLSY
    CESD_NEW6PT_HRS_2 = cesd_depressed_HRS_2+cesd_effort_HRS_2+cesd_restless_HRS_2+
                        cesd_lonely_HRS_2+cesd_sad_HRS_2+cesd_going_HRS_2,
    CESD_NEW6PT_HRS_3 = cesd_depressed_HRS_3+cesd_effort_HRS_3+cesd_restless_HRS_3+
                        cesd_lonely_HRS_3+cesd_sad_HRS_3+cesd_going_HRS_3,
    CESD_NEW6PT_HRS_4 = cesd_depressed_HRS_4+cesd_effort_HRS_4+cesd_restless_HRS_4+
                        cesd_lonely_HRS_4+cesd_sad_HRS_4+cesd_going_HRS_4,
    CESD_NEW6PT_HRS_5 = cesd_depressed_HRS_5+cesd_effort_HRS_5+cesd_restless_HRS_5+
                        cesd_lonely_HRS_5+cesd_sad_HRS_5+cesd_going_HRS_5,
    CESD_NEW6PT_HRS_6 = cesd_depressed_HRS_6+cesd_effort_HRS_6+cesd_restless_HRS_6+
                        cesd_lonely_HRS_6+cesd_sad_HRS_6+cesd_going_HRS_6,
    CESD_NEW6PT_HRS_7 = cesd_depressed_HRS_7+cesd_effort_HRS_7+cesd_restless_HRS_7+
                        cesd_lonely_HRS_7+cesd_sad_HRS_7+cesd_going_HRS_7,
    CESD_NEW6PT_HRS_8 = cesd_depressed_HRS_8+cesd_effort_HRS_8+cesd_restless_HRS_8+
                        cesd_lonely_HRS_8+cesd_sad_HRS_8+cesd_going_HRS_8,
    CESD_NEW6PT_HRS_9 = cesd_depressed_HRS_9+cesd_effort_HRS_9+cesd_restless_HRS_9+
                        cesd_lonely_HRS_9+cesd_sad_HRS_9+cesd_going_HRS_9,
    CESD_NEW6PT_HRS_10 = cesd_depressed_HRS_10+cesd_effort_HRS_10+cesd_restless_HRS_10+
                         cesd_lonely_HRS_10+cesd_sad_HRS_10+cesd_going_HRS_10,
    CESD_NEW6PT_HRS_11 = cesd_depressed_HRS_11+cesd_effort_HRS_11+cesd_restless_HRS_11+
                         cesd_lonely_HRS_11+cesd_sad_HRS_11+cesd_going_HRS_11,
    CESD_NEW6PT_HRS_12 = cesd_depressed_HRS_12+cesd_effort_HRS_12+cesd_restless_HRS_12+
                         cesd_lonely_HRS_12+cesd_sad_HRS_12+cesd_going_HRS_12,
    CESD_NEW6PT_HRS_13 = cesd_depressed_HRS_13+cesd_effort_HRS_13+cesd_restless_HRS_13+
                         cesd_lonely_HRS_13+cesd_sad_HRS_13+cesd_going_HRS_13,
    CESD_NEW6PT_HRS_14 = cesd_depressed_HRS_14+cesd_effort_HRS_14+cesd_restless_HRS_14+
                         cesd_lonely_HRS_14+cesd_sad_HRS_14+cesd_going_HRS_14,
    CESD_NEW6PT_HRS_15 = cesd_depressed_HRS_15+cesd_effort_HRS_15+cesd_restless_HRS_15+
                         cesd_lonely_HRS_15+cesd_sad_HRS_15+cesd_going_HRS_15,
    
    # No changes need to be made to: SBP, DBP, PULSE
    # Also have other tests available, but unsure if necessary. 

    ## Health Conditions ----
    # Per Scott: we will assume that once they report the condition, they will always have it
    # As Such, we will collapse the "ever" and current wave Variables
    
    ### Diabetes ----
    DIABETES_HRS_1 = case_when((DIABETES_HRS_1 == 1 |DIABETES_HRS_EVER_1 == 1) ~ 1, 
                               (DIABETES_HRS_1 == 0 |DIABETES_HRS_EVER_1 == 0) ~ 0, 
                               TRUE ~ as.numeric(DIABETES_HRS_EVER_1)),
    DIABETES_HRS_2 = case_when((DIABETES_HRS_2 == 1 |DIABETES_HRS_1 ==1|DIABETES_HRS_EVER_2 == 1) ~ 1, 
                               (DIABETES_HRS_2 == 0 |DIABETES_HRS_1 ==0|DIABETES_HRS_EVER_2 == 0) ~ 0,
                               TRUE ~ as.numeric(DIABETES_HRS_EVER_2)),
    DIABETES_HRS_3 = case_when((DIABETES_HRS_3 == 1 |DIABETES_HRS_2 ==1|DIABETES_HRS_EVER_3 == 1) ~ 1, 
                               (DIABETES_HRS_3 == 0 |DIABETES_HRS_2 ==0|DIABETES_HRS_EVER_3 == 0) ~ 0,
                               TRUE ~ as.numeric(DIABETES_HRS_EVER_3)),
    DIABETES_HRS_4 = case_when((DIABETES_HRS_4 == 1 |DIABETES_HRS_3 ==1|DIABETES_HRS_EVER_4 == 1) ~ 1, 
                               (DIABETES_HRS_4 == 0 |DIABETES_HRS_3 ==0|DIABETES_HRS_EVER_4 == 0) ~ 0, 
                               TRUE ~ as.numeric(DIABETES_HRS_EVER_4)),
    DIABETES_HRS_5 = case_when((DIABETES_HRS_5 == 1 |DIABETES_HRS_4 ==1|DIABETES_HRS_EVER_5 == 1) ~ 1, 
                               (DIABETES_HRS_5 == 0 |DIABETES_HRS_4 ==0|DIABETES_HRS_EVER_5 == 0) ~ 0, 
                               TRUE ~ as.numeric(DIABETES_HRS_EVER_5)),
    DIABETES_HRS_6 = case_when((DIABETES_HRS_6 == 1 |DIABETES_HRS_5 ==1|DIABETES_HRS_EVER_6 == 1) ~ 1, 
                               (DIABETES_HRS_6 == 0 |DIABETES_HRS_5 ==0|DIABETES_HRS_EVER_6 == 0) ~ 0, 
                               TRUE ~ as.numeric(DIABETES_HRS_EVER_6)),
    DIABETES_HRS_7 = case_when((DIABETES_HRS_7 == 1 |DIABETES_HRS_6 ==1|DIABETES_HRS_EVER_7 == 1) ~ 1, 
                               (DIABETES_HRS_7 == 0 |DIABETES_HRS_6 ==0|DIABETES_HRS_EVER_7 == 0) ~ 0, 
                               TRUE ~ as.numeric(DIABETES_HRS_EVER_7)),
    DIABETES_HRS_8 = case_when((DIABETES_HRS_8 == 1 |DIABETES_HRS_7 ==1|DIABETES_HRS_EVER_8 == 1) ~ 1, 
                               (DIABETES_HRS_8 == 0 |DIABETES_HRS_7 ==0|DIABETES_HRS_EVER_8 == 0) ~ 0,
                               TRUE ~ as.numeric(DIABETES_HRS_EVER_8)),
    DIABETES_HRS_9 = case_when((DIABETES_HRS_9 == 1 |DIABETES_HRS_8 ==1|DIABETES_HRS_EVER_9 == 1) ~ 1, 
                               (DIABETES_HRS_9 == 0 |DIABETES_HRS_8 ==0|DIABETES_HRS_EVER_9 == 0) ~ 0, 
                               TRUE ~ as.numeric(DIABETES_HRS_EVER_9)),
    DIABETES_HRS_10= case_when((DIABETES_HRS_10== 1 |DIABETES_HRS_9 ==1|DIABETES_HRS_EVER_10 == 1) ~ 1, 
                               (DIABETES_HRS_10== 0 |DIABETES_HRS_9 ==0|DIABETES_HRS_EVER_10 == 0) ~ 0,
                               TRUE ~ as.numeric(DIABETES_HRS_EVER_10)),
    DIABETES_HRS_11= case_when((DIABETES_HRS_11== 1 |DIABETES_HRS_10==1|DIABETES_HRS_EVER_11 == 1) ~ 1, 
                               (DIABETES_HRS_11== 0 |DIABETES_HRS_10==0|DIABETES_HRS_EVER_11 == 0) ~ 0, 
                               TRUE ~ as.numeric(DIABETES_HRS_EVER_11)),
    DIABETES_HRS_12= case_when((DIABETES_HRS_12== 1 |DIABETES_HRS_11==1|DIABETES_HRS_EVER_12 == 1) ~ 1, 
                               (DIABETES_HRS_12== 0 |DIABETES_HRS_11==0|DIABETES_HRS_EVER_12 == 0) ~ 0, 
                               TRUE ~ as.numeric(DIABETES_HRS_EVER_12)),
    DIABETES_HRS_13= case_when((DIABETES_HRS_13== 1 |DIABETES_HRS_12==1|DIABETES_HRS_EVER_13 == 1) ~ 1, 
                               (DIABETES_HRS_13== 0 |DIABETES_HRS_12==0|DIABETES_HRS_EVER_13 == 0) ~ 0, 
                               TRUE ~ as.numeric(DIABETES_HRS_EVER_13)),
    DIABETES_HRS_14= case_when((DIABETES_HRS_14== 1 |DIABETES_HRS_13==1|DIABETES_HRS_EVER_14 == 1) ~ 1, 
                               (DIABETES_HRS_14== 0 |DIABETES_HRS_13==0|DIABETES_HRS_EVER_14 == 0) ~ 0, 
                               TRUE ~ as.numeric(DIABETES_HRS_EVER_14)),  
    DIABETES_HRS_15= case_when((DIABETES_HRS_15== 1 |DIABETES_HRS_14==1|DIABETES_HRS_EVER_15 == 1) ~ 1, 
                               (DIABETES_HRS_15== 0 |DIABETES_HRS_14==0|DIABETES_HRS_EVER_15 == 0) ~ 0, 
                               TRUE ~ as.numeric(DIABETES_HRS_EVER_15)),    
    ### Hypertension ----
    HYPERTENSION_HRS_1 = case_when((HYPERTENSION_HRS_1 == 1 |HYPERTENSION_HRS_EVER_1 == 1) ~ 1, 
                                   TRUE ~ as.numeric(HYPERTENSION_HRS_EVER_1)),
    HYPERTENSION_HRS_2 = case_when((HYPERTENSION_HRS_2 == 1 |HYPERTENSION_HRS_1 ==1|HYPERTENSION_HRS_EVER_2 == 1) ~ 1, 
                                   TRUE ~ as.numeric(HYPERTENSION_HRS_EVER_2)),
    HYPERTENSION_HRS_3 = case_when((HYPERTENSION_HRS_3 == 1 |HYPERTENSION_HRS_2 ==1|HYPERTENSION_HRS_EVER_3 == 1) ~ 1, 
                                   TRUE ~ as.numeric(HYPERTENSION_HRS_EVER_3)),
    HYPERTENSION_HRS_4 = case_when((HYPERTENSION_HRS_4 == 1 |HYPERTENSION_HRS_3 ==1|HYPERTENSION_HRS_EVER_4 == 1) ~ 1, 
                                   TRUE ~ as.numeric(HYPERTENSION_HRS_EVER_4)),
    HYPERTENSION_HRS_5 = case_when((HYPERTENSION_HRS_5 == 1 |HYPERTENSION_HRS_4 ==1|HYPERTENSION_HRS_EVER_5 == 1) ~ 1, 
                                   TRUE ~ as.numeric(HYPERTENSION_HRS_EVER_5)),
    HYPERTENSION_HRS_6 = case_when((HYPERTENSION_HRS_6 == 1 |HYPERTENSION_HRS_5 ==1|HYPERTENSION_HRS_EVER_6 == 1) ~ 1, 
                                   TRUE ~ as.numeric(HYPERTENSION_HRS_EVER_6)),
    HYPERTENSION_HRS_7 = case_when((HYPERTENSION_HRS_7 == 1 |HYPERTENSION_HRS_6 ==1|HYPERTENSION_HRS_EVER_7 == 1) ~ 1, 
                                   TRUE ~ as.numeric(HYPERTENSION_HRS_EVER_7)),
    HYPERTENSION_HRS_8 = case_when((HYPERTENSION_HRS_8 == 1 |HYPERTENSION_HRS_7 ==1|HYPERTENSION_HRS_EVER_8 == 1) ~ 1, 
                                   TRUE ~ as.numeric(HYPERTENSION_HRS_EVER_8)),
    HYPERTENSION_HRS_9 = case_when((HYPERTENSION_HRS_9 == 1 |HYPERTENSION_HRS_8 ==1|HYPERTENSION_HRS_EVER_9 == 1) ~ 1, 
                                   TRUE ~ as.numeric(HYPERTENSION_HRS_EVER_9)),
    HYPERTENSION_HRS_10= case_when((HYPERTENSION_HRS_10== 1 |HYPERTENSION_HRS_9 ==1|HYPERTENSION_HRS_EVER_10== 1) ~ 1, 
                                   TRUE ~ as.numeric(HYPERTENSION_HRS_EVER_10)),
    HYPERTENSION_HRS_11= case_when((HYPERTENSION_HRS_11== 1 |HYPERTENSION_HRS_10==1|HYPERTENSION_HRS_EVER_11== 1) ~ 1, 
                                   TRUE ~ as.numeric(HYPERTENSION_HRS_EVER_11)),
    HYPERTENSION_HRS_12= case_when((HYPERTENSION_HRS_12== 1 |HYPERTENSION_HRS_11==1|HYPERTENSION_HRS_EVER_12== 1) ~ 1, 
                                   TRUE ~ as.numeric(HYPERTENSION_HRS_EVER_12)),
    HYPERTENSION_HRS_13= case_when((HYPERTENSION_HRS_13== 1 |HYPERTENSION_HRS_12==1|HYPERTENSION_HRS_EVER_13== 1) ~ 1, 
                                   TRUE ~ as.numeric(HYPERTENSION_HRS_EVER_13)),
    HYPERTENSION_HRS_14= case_when((HYPERTENSION_HRS_14== 1 |HYPERTENSION_HRS_13==1|HYPERTENSION_HRS_EVER_14== 1) ~ 1, 
                                   TRUE ~ as.numeric(HYPERTENSION_HRS_EVER_14)),  
    HYPERTENSION_HRS_15= case_when((HYPERTENSION_HRS_15== 1 |HYPERTENSION_HRS_14==1|HYPERTENSION_HRS_EVER_15== 1) ~ 1, 
                                   TRUE ~ as.numeric(HYPERTENSION_HRS_EVER_15)),    
    ### Cancer ----
    CANCER_HRS_1 = case_when((CANCER_HRS_1 == 1 |CANCER_HRS_EVER_1 == 1) ~ 1, TRUE ~ as.numeric(CANCER_HRS_EVER_1)),
    CANCER_HRS_2 = case_when((CANCER_HRS_2 == 1 |CANCER_HRS_1==1 |CANCER_HRS_EVER_2 == 1) ~ 1, 
                             TRUE ~ as.numeric(CANCER_HRS_EVER_2)),
    CANCER_HRS_3 = case_when((CANCER_HRS_3 == 1 |CANCER_HRS_2==1 |CANCER_HRS_EVER_3 == 1) ~ 1, 
                             TRUE ~ as.numeric(CANCER_HRS_EVER_3)),
    CANCER_HRS_4 = case_when((CANCER_HRS_4 == 1 |CANCER_HRS_3==1 |CANCER_HRS_EVER_4 == 1) ~ 1, 
                             TRUE ~ as.numeric(CANCER_HRS_EVER_4)),
    CANCER_HRS_5 = case_when((CANCER_HRS_5 == 1 |CANCER_HRS_4==1 |CANCER_HRS_EVER_5 == 1) ~ 1, 
                             TRUE ~ as.numeric(CANCER_HRS_EVER_5)),
    CANCER_HRS_6 = case_when((CANCER_HRS_6 == 1 |CANCER_HRS_5==1 |CANCER_HRS_EVER_6 == 1) ~ 1, 
                             TRUE ~ as.numeric(CANCER_HRS_EVER_6)),
    CANCER_HRS_7 = case_when((CANCER_HRS_7 == 1 |CANCER_HRS_6==1 |CANCER_HRS_EVER_7 == 1) ~ 1, 
                             TRUE ~ as.numeric(CANCER_HRS_EVER_7)),
    CANCER_HRS_8 = case_when((CANCER_HRS_8 == 1 |CANCER_HRS_7==1 |CANCER_HRS_EVER_8 == 1) ~ 1, 
                             TRUE ~ as.numeric(CANCER_HRS_EVER_8)),
    CANCER_HRS_9 = case_when((CANCER_HRS_9 == 1 |CANCER_HRS_8==1 |CANCER_HRS_EVER_9 == 1) ~ 1, 
                             TRUE ~ as.numeric(CANCER_HRS_EVER_9)),
    CANCER_HRS_10= case_when((CANCER_HRS_10== 1 |CANCER_HRS_9==1 |CANCER_HRS_EVER_10== 1) ~ 1, 
                             TRUE ~ as.numeric(CANCER_HRS_EVER_10)),
    CANCER_HRS_11= case_when((CANCER_HRS_11== 1 |CANCER_HRS_10==1|CANCER_HRS_EVER_11== 1) ~ 1, 
                             TRUE ~ as.numeric(CANCER_HRS_EVER_11)),
    CANCER_HRS_12= case_when((CANCER_HRS_12== 1 |CANCER_HRS_11==1|CANCER_HRS_EVER_12== 1) ~ 1, 
                             TRUE ~ as.numeric(CANCER_HRS_EVER_12)),
    CANCER_HRS_13= case_when((CANCER_HRS_13== 1 |CANCER_HRS_12==1|CANCER_HRS_EVER_13== 1) ~ 1, 
                             TRUE ~ as.numeric(CANCER_HRS_EVER_13)),
    CANCER_HRS_14= case_when((CANCER_HRS_14== 1 |CANCER_HRS_13==1|CANCER_HRS_EVER_14== 1) ~ 1, 
                             TRUE ~ as.numeric(CANCER_HRS_EVER_14)),
    CANCER_HRS_15= case_when((CANCER_HRS_15== 1 |CANCER_HRS_14==1|CANCER_HRS_EVER_15== 1) ~ 1, 
                             TRUE ~ as.numeric(CANCER_HRS_EVER_15)),
    
    ### Heart Problem ----
    HEARTPROB_HRS_1 = case_when((HEARTPROB_HRS_1 == 1 |HEARTPROB_HRS_EVER_1 == 1) ~ 1, TRUE ~ as.numeric(HEARTPROB_HRS_EVER_1)),
    HEARTPROB_HRS_2 = case_when((HEARTPROB_HRS_2 == 1 |HEARTPROB_HRS_1==1 |HEARTPROB_HRS_EVER_2 == 1) ~ 1, 
                                TRUE ~ as.numeric(HEARTPROB_HRS_EVER_2)),
    HEARTPROB_HRS_3 = case_when((HEARTPROB_HRS_3 == 1 |HEARTPROB_HRS_2==1 |HEARTPROB_HRS_EVER_3 == 1) ~ 1, 
                                TRUE ~ as.numeric(HEARTPROB_HRS_EVER_3)),
    HEARTPROB_HRS_4 = case_when((HEARTPROB_HRS_4 == 1 |HEARTPROB_HRS_3==1 |HEARTPROB_HRS_EVER_4 == 1) ~ 1, 
                                TRUE ~ as.numeric(HEARTPROB_HRS_EVER_4)),
    HEARTPROB_HRS_5 = case_when((HEARTPROB_HRS_5 == 1 |HEARTPROB_HRS_4==1 |HEARTPROB_HRS_EVER_5 == 1) ~ 1, 
                                TRUE ~ as.numeric(HEARTPROB_HRS_EVER_5)),
    HEARTPROB_HRS_6 = case_when((HEARTPROB_HRS_6 == 1 |HEARTPROB_HRS_5==1 |HEARTPROB_HRS_EVER_6 == 1) ~ 1, 
                                TRUE ~ as.numeric(HEARTPROB_HRS_EVER_6)),
    HEARTPROB_HRS_7 = case_when((HEARTPROB_HRS_7 == 1 |HEARTPROB_HRS_6==1 |HEARTPROB_HRS_EVER_7 == 1) ~ 1, 
                                TRUE ~ as.numeric(HEARTPROB_HRS_EVER_7)),
    HEARTPROB_HRS_8 = case_when((HEARTPROB_HRS_8 == 1 |HEARTPROB_HRS_7==1 |HEARTPROB_HRS_EVER_8 == 1) ~ 1, 
                                TRUE ~ as.numeric(HEARTPROB_HRS_EVER_8)),
    HEARTPROB_HRS_9 = case_when((HEARTPROB_HRS_9 == 1 |HEARTPROB_HRS_8==1 |HEARTPROB_HRS_EVER_9 == 1) ~ 1, 
                                TRUE ~ as.numeric(HEARTPROB_HRS_EVER_9)),
    HEARTPROB_HRS_10= case_when((HEARTPROB_HRS_10== 1 |HEARTPROB_HRS_9==1| HEARTPROB_HRS_EVER_10== 1) ~ 1, 
                                TRUE ~ as.numeric(HEARTPROB_HRS_EVER_10)),
    HEARTPROB_HRS_11= case_when((HEARTPROB_HRS_11== 1 |HEARTPROB_HRS_10==1|HEARTPROB_HRS_EVER_11== 1) ~ 1, 
                                TRUE ~ as.numeric(HEARTPROB_HRS_EVER_11)),
    HEARTPROB_HRS_12= case_when((HEARTPROB_HRS_12== 1 |HEARTPROB_HRS_11==1|HEARTPROB_HRS_EVER_12== 1) ~ 1, 
                                TRUE ~ as.numeric(HEARTPROB_HRS_EVER_12)),
    HEARTPROB_HRS_13= case_when((HEARTPROB_HRS_13== 1 |HEARTPROB_HRS_12==1|HEARTPROB_HRS_EVER_13== 1) ~ 1, 
                                TRUE ~ as.numeric(HEARTPROB_HRS_EVER_13)),
    HEARTPROB_HRS_14= case_when((HEARTPROB_HRS_14== 1 |HEARTPROB_HRS_13==1|HEARTPROB_HRS_EVER_14== 1) ~ 1, 
                                TRUE ~ as.numeric(HEARTPROB_HRS_EVER_14)),
    HEARTPROB_HRS_15= case_when((HEARTPROB_HRS_15== 1 |HEARTPROB_HRS_14==1|HEARTPROB_HRS_EVER_15== 1) ~ 1, 
                                TRUE ~ as.numeric(HEARTPROB_HRS_EVER_15)),
    
    # Self-reported Health: - recoded below
    
    ## Health Behaviours ----
    
    ### Vigorous Activity (waves 1-6) ----
    # Due to limitations from early waves on PA, more than 1x/week or not. 
    VIG_EXERCISE_HRS_1 = case_when(vig_activities_HRS_1 == 1 ~ 1, 
                                   vig_activities_HRS_1 == 0 ~ 0),
    VIG_EXERCISE_HRS_2 = case_when(vig_activities_HRS_2 == 1 ~ 1, 
                                   vig_activities_HRS_2 == 0 ~ 0),
    VIG_EXERCISE_HRS_3 = case_when(vig_activities_HRS_3 == 1 ~ 1, 
                                   vig_activities_HRS_3 == 0 ~ 0),
    VIG_EXERCISE_HRS_4 = case_when(vig_activities_HRS_4 == 1 ~ 1, 
                                   vig_activities_HRS_4 == 0 ~ 0),
    VIG_EXERCISE_HRS_5 = case_when(vig_activities_HRS_5 == 1 ~ 1, 
                                   vig_activities_HRS_5 == 0 ~ 0),
    VIG_EXERCISE_HRS_6 = case_when(vig_activities_HRS_6 == 1 ~ 1, 
                                   vig_activities_HRS_6 == 0 ~ 0),
    
    ### Light Exercise Wave 1 ----
    LIGHT_EXERCISE_HRS_1 = case_when(
      (lig_activities_housework_HRS_w1 < 3 | lig_activities_all_HRS_w1 < 3) ~ 1,
      (lig_activities_housework_HRS_w1 >=3 | lig_activities_all_HRS_w1 >=3) ~ 0),

    ### Physical Activity (waves 7-14) ----
    # Recode based to Vigorous/LitetoMod Activities
    # Vigorous Activities Recoded below
    # Lite to Mod:
    LIGHT_EXERCISE_HRS_7 = case_when(
      (mod_activities_HRS_7 < 3 | lig_activities_HRS_7 < 3) ~ 1,
      (mod_activities_HRS_7 >=3 | lig_activities_HRS_7 >=3) ~ 0),
    LIGHT_EXERCISE_HRS_8 = case_when(
      (mod_activities_HRS_8 < 3 | lig_activities_HRS_8 < 3) ~ 1,
      (mod_activities_HRS_8 >=3 | lig_activities_HRS_8 >=3) ~ 0),
    LIGHT_EXERCISE_HRS_9 = case_when(
      (mod_activities_HRS_9 < 3 | lig_activities_HRS_9 < 3) ~ 1,
      (mod_activities_HRS_9 >=3 | lig_activities_HRS_9 >=3) ~ 0),
    LIGHT_EXERCISE_HRS_10 = case_when(
      (mod_activities_HRS_10 < 3 | lig_activities_HRS_10 < 3) ~ 1,
      (mod_activities_HRS_10 >=3 | lig_activities_HRS_10 >=3) ~ 0),
    LIGHT_EXERCISE_HRS_11 = case_when(
      (mod_activities_HRS_11 < 3 | lig_activities_HRS_11 < 3) ~ 1,
      (mod_activities_HRS_11 >=3 | lig_activities_HRS_11 >=3) ~ 0),
    LIGHT_EXERCISE_HRS_12 = case_when(
      (mod_activities_HRS_12 < 3 | lig_activities_HRS_12 < 3) ~ 1,
      (mod_activities_HRS_12 >=3 | lig_activities_HRS_12 >=3) ~ 0),
    LIGHT_EXERCISE_HRS_13 = case_when(
      (mod_activities_HRS_13 < 3 | lig_activities_HRS_13 < 3) ~ 1,
      (mod_activities_HRS_13 >=3 | lig_activities_HRS_13 >=3) ~ 0),
    LIGHT_EXERCISE_HRS_14 = case_when(
      (mod_activities_HRS_14 < 3 | lig_activities_HRS_14 < 3) ~ 1,
      (mod_activities_HRS_14 >=3 | lig_activities_HRS_14 >=3) ~ 0),
    LIGHT_EXERCISE_HRS_15 = case_when(
      (mod_activities_HRS_15 < 3 | lig_activities_HRS_15 < 3) ~ 1,
      (mod_activities_HRS_15 >=3 | lig_activities_HRS_15 >=3) ~ 0),

    # Smoking, ever & now
    # For now as binary - consider cross walking with 3rd set

    ## Alcohol ----
    alc_ndrink_3 = ifelse(alc_ndrink_3 == 99, 50, alc_ndrink_3),

    ### Drinks per Week ----
    alcohol_dpw_HRS_3 = alc_nweek_3 *alc_ndrink_3,
    alcohol_dpw_HRS_4 = alc_nweek_4 *alc_ndrink_4,
    alcohol_dpw_HRS_5 = alc_nweek_5 *alc_ndrink_5,
    alcohol_dpw_HRS_6 = alc_nweek_6 *alc_ndrink_6,
    alcohol_dpw_HRS_7 = alc_nweek_7 *alc_ndrink_7,
    alcohol_dpw_HRS_8 = alc_nweek_8 *alc_ndrink_8,
    alcohol_dpw_HRS_9 = alc_nweek_9 *alc_ndrink_9,
    alcohol_dpw_HRS_10= alc_nweek_10*alc_ndrink_10,
    alcohol_dpw_HRS_11= alc_nweek_11*alc_ndrink_11,
    alcohol_dpw_HRS_12= alc_nweek_12*alc_ndrink_12,
    alcohol_dpw_HRS_13= alc_nweek_13*alc_ndrink_13,
    alcohol_dpw_HRS_14= alc_nweek_14*alc_ndrink_14,
    alcohol_dpw_HRS_15= alc_nweek_15*alc_ndrink_15,

    ### Drinking Frequency ----
    alcohol_cat_HRS_3 = as.factor(case_when(
      SEX_HRS_RA == 2 & alcohol_dpw_HRS_3 == 0 ~ "None",
      SEX_HRS_RA == 2 & (alcohol_dpw_HRS_3 > 0 & alcohol_dpw_HRS_3 <= 7) ~ "LiteToMod",
      SEX_HRS_RA == 2 & alcohol_dpw_HRS_3 > 7 ~ "Severe",
      SEX_HRS_RA == 1 & alcohol_dpw_HRS_3 == 0 ~ "None",
      SEX_HRS_RA == 1 & (alcohol_dpw_HRS_3 > 0 & alcohol_dpw_HRS_3 <= 14) ~ "LiteToMod",
      SEX_HRS_RA == 1 & alcohol_dpw_HRS_3 > 14 ~ "Severe",
      alc_ever_3 == 0 ~ "None")),

    alcohol_cat_HRS_4 = as.factor(case_when(
      SEX_HRS_RA == 2 & alcohol_dpw_HRS_4 == 0 ~ "None",
      SEX_HRS_RA == 2 & (alcohol_dpw_HRS_4 > 0 & alcohol_dpw_HRS_4 <= 7) ~ "LiteToMod",
      SEX_HRS_RA == 2 & alcohol_dpw_HRS_4 > 7 ~ "Severe",
      SEX_HRS_RA == 1 & alcohol_dpw_HRS_4 == 0 ~ "None",
      SEX_HRS_RA == 1 & (alcohol_dpw_HRS_4 > 0 & alcohol_dpw_HRS_4 <= 14) ~ "LiteToMod",
      SEX_HRS_RA == 1 & alcohol_dpw_HRS_4 > 14 ~ "Severe",
      alc_ever_4 == 0 ~ "None")),

    alcohol_cat_HRS_5 = as.factor(case_when(
      SEX_HRS_RA == 2 & alcohol_dpw_HRS_5 == 0 ~ "None",
      SEX_HRS_RA == 2 & (alcohol_dpw_HRS_5 > 0 & alcohol_dpw_HRS_5 <= 7) ~ "LiteToMod",
      SEX_HRS_RA == 2 & alcohol_dpw_HRS_5 > 7 ~ "Severe",
      SEX_HRS_RA == 1 & alcohol_dpw_HRS_5 == 0 ~ "None",
      SEX_HRS_RA == 1 & (alcohol_dpw_HRS_5 > 0 & alcohol_dpw_HRS_5 <= 14) ~ "LiteToMod",
      SEX_HRS_RA == 1 & alcohol_dpw_HRS_5 > 14 ~ "Severe",
      alc_ever_5 == 0 ~ "None")),

    alcohol_cat_HRS_6 = as.factor(case_when(
      SEX_HRS_RA == 2 & alcohol_dpw_HRS_6 == 0 ~ "None",
      SEX_HRS_RA == 2 & (alcohol_dpw_HRS_6 > 0 & alcohol_dpw_HRS_6 <= 7) ~ "LiteToMod",
      SEX_HRS_RA == 2 & alcohol_dpw_HRS_6 > 7 ~ "Severe",
      SEX_HRS_RA == 1 & alcohol_dpw_HRS_6 == 0 ~ "None",
      SEX_HRS_RA == 1 & (alcohol_dpw_HRS_6 > 0 & alcohol_dpw_HRS_6 <= 14) ~ "LiteToMod",
      SEX_HRS_RA == 1 & alcohol_dpw_HRS_6 > 14 ~ "Severe",
      alc_ever_6 == 0 ~ "None")),

    alcohol_cat_HRS_7 = as.factor(case_when(
      SEX_HRS_RA == 2 & alcohol_dpw_HRS_7 == 0 ~ "None",
      SEX_HRS_RA == 2 & (alcohol_dpw_HRS_7 > 0 & alcohol_dpw_HRS_7 <= 7) ~ "LiteToMod",
      SEX_HRS_RA == 2 & alcohol_dpw_HRS_7 > 7 ~ "Severe",
      SEX_HRS_RA == 1 & alcohol_dpw_HRS_7 == 0 ~ "None",
      SEX_HRS_RA == 1 & (alcohol_dpw_HRS_7 > 0 & alcohol_dpw_HRS_7 <= 14) ~ "LiteToMod",
      SEX_HRS_RA == 1 & alcohol_dpw_HRS_7 > 14 ~ "Severe",
      alc_ever_7 == 0 ~ "None")),

    alcohol_cat_HRS_8 = as.factor(case_when(
      SEX_HRS_RA == 2 & alcohol_dpw_HRS_8 == 0 ~ "None",
      SEX_HRS_RA == 2 & (alcohol_dpw_HRS_8 > 0 & alcohol_dpw_HRS_8 <= 7) ~ "LiteToMod",
      SEX_HRS_RA == 2 & alcohol_dpw_HRS_8 > 7 ~ "Severe",
      SEX_HRS_RA == 1 & alcohol_dpw_HRS_8 == 0 ~ "None",
      SEX_HRS_RA == 1 & (alcohol_dpw_HRS_8 > 0 & alcohol_dpw_HRS_8 <= 14) ~ "LiteToMod",
      SEX_HRS_RA == 1 & alcohol_dpw_HRS_8 > 14 ~ "Severe",
      alc_ever_8 == 0 ~ "None")),

    alcohol_cat_HRS_9 = as.factor(case_when(
      SEX_HRS_RA == 2 & alcohol_dpw_HRS_9 == 0 ~ "None",
      SEX_HRS_RA == 2 & (alcohol_dpw_HRS_9 > 0 & alcohol_dpw_HRS_9 <= 7) ~ "LiteToMod",
      SEX_HRS_RA == 2 & alcohol_dpw_HRS_9 > 7 ~ "Severe",
      SEX_HRS_RA == 1 & alcohol_dpw_HRS_9 == 0 ~ "None",
      SEX_HRS_RA == 1 & (alcohol_dpw_HRS_9 > 0 & alcohol_dpw_HRS_9 <= 14) ~ "LiteToMod",
      SEX_HRS_RA == 1 & alcohol_dpw_HRS_9 > 14 ~ "Severe",
      alc_ever_9 == 0 ~ "None")),

    alcohol_cat_HRS_10 = as.factor(case_when(
      SEX_HRS_RA == 2 & alcohol_dpw_HRS_10 == 0 ~ "None",
      SEX_HRS_RA == 2 & (alcohol_dpw_HRS_10 > 0 & alcohol_dpw_HRS_10 <= 7) ~ "LiteToMod",
      SEX_HRS_RA == 2 & alcohol_dpw_HRS_10 > 7 ~ "Severe",
      SEX_HRS_RA == 1 & alcohol_dpw_HRS_10 == 0 ~ "None",
      SEX_HRS_RA == 1 & (alcohol_dpw_HRS_10 > 0 & alcohol_dpw_HRS_10 <= 14) ~ "LiteToMod",
      SEX_HRS_RA == 1 & alcohol_dpw_HRS_10 > 14 ~ "Severe",
      alc_ever_10 == 0 ~ "None")),

    alcohol_cat_HRS_11 = as.factor(case_when(
      SEX_HRS_RA == 2 & alcohol_dpw_HRS_11 == 0 ~ "None",
      SEX_HRS_RA == 2 & (alcohol_dpw_HRS_11 > 0 & alcohol_dpw_HRS_11 <= 7) ~ "LiteToMod",
      SEX_HRS_RA == 2 & alcohol_dpw_HRS_11 > 7 ~ "Severe",
      SEX_HRS_RA == 1 & alcohol_dpw_HRS_11 == 0 ~ "None",
      SEX_HRS_RA == 1 & (alcohol_dpw_HRS_11 > 0 & alcohol_dpw_HRS_11 <= 14) ~ "LiteToMod",
      SEX_HRS_RA == 1 & alcohol_dpw_HRS_11 > 14 ~ "Severe",
      alc_ever_11 == 0 ~ "None")),

    alcohol_cat_HRS_12 = as.factor(case_when(
      SEX_HRS_RA == 2 & alcohol_dpw_HRS_12 == 0 ~ "None",
      SEX_HRS_RA == 2 & (alcohol_dpw_HRS_12 > 0 & alcohol_dpw_HRS_12 <= 7) ~ "LiteToMod",
      SEX_HRS_RA == 2 & alcohol_dpw_HRS_12 > 7 ~ "Severe",
      SEX_HRS_RA == 1 & alcohol_dpw_HRS_12 == 0 ~ "None",
      SEX_HRS_RA == 1 & (alcohol_dpw_HRS_12 > 0 & alcohol_dpw_HRS_12 <= 14) ~ "LiteToMod",
      SEX_HRS_RA == 1 & alcohol_dpw_HRS_12 > 14 ~ "Severe",
      alc_ever_12 == 0 ~ "None")),

    alcohol_cat_HRS_13 = as.factor(case_when(
      SEX_HRS_RA == 2 & alcohol_dpw_HRS_13 == 0 ~ "None",
      SEX_HRS_RA == 2 & (alcohol_dpw_HRS_13 > 0 & alcohol_dpw_HRS_13 <= 7) ~ "LiteToMod",
      SEX_HRS_RA == 2 & alcohol_dpw_HRS_13 > 7 ~ "Severe",
      SEX_HRS_RA == 1 & alcohol_dpw_HRS_13 == 0 ~ "None",
      SEX_HRS_RA == 1 & (alcohol_dpw_HRS_13 > 0 & alcohol_dpw_HRS_13 <= 14) ~ "LiteToMod",
      SEX_HRS_RA == 1 & alcohol_dpw_HRS_13 > 14 ~ "Severe",
      alc_ever_13 == 0 ~ "None")),

    alcohol_cat_HRS_14 = as.factor(case_when(
      SEX_HRS_RA == 2 & alcohol_dpw_HRS_14 == 0 ~ "None",
      SEX_HRS_RA == 2 & (alcohol_dpw_HRS_14 > 0 & alcohol_dpw_HRS_14 <= 7) ~ "LiteToMod",
      SEX_HRS_RA == 2 & alcohol_dpw_HRS_14 > 7 ~ "Severe",
      SEX_HRS_RA == 1 & alcohol_dpw_HRS_14 == 0 ~ "None",
      SEX_HRS_RA == 1 & (alcohol_dpw_HRS_14 > 0 & alcohol_dpw_HRS_14 <= 14) ~ "LiteToMod",
      SEX_HRS_RA == 1 & alcohol_dpw_HRS_14 > 14 ~ "Severe",
      alc_ever_14 == 0 ~ "None")),
    
    alcohol_cat_HRS_15 = as.factor(case_when(
      SEX_HRS_RA == 2 & alcohol_dpw_HRS_15 == 0 ~ "None",
      SEX_HRS_RA == 2 & (alcohol_dpw_HRS_15 > 0 & alcohol_dpw_HRS_15 <= 7) ~ "LiteToMod",
      SEX_HRS_RA == 2 & alcohol_dpw_HRS_15 > 7 ~ "Severe",
      SEX_HRS_RA == 1 & alcohol_dpw_HRS_15 == 0 ~ "None",
      SEX_HRS_RA == 1 & (alcohol_dpw_HRS_15 > 0 & alcohol_dpw_HRS_15 <= 14) ~ "LiteToMod",
      SEX_HRS_RA == 1 & alcohol_dpw_HRS_15 > 14 ~ "Severe",
      alc_ever_15 == 0 ~ "None")),

    ### Binge Drinking ----
    alcohol_bingedrink_HRS_3= case_when(
      SEX_HRS_RA == 2 & alc_ndrink_3 <  3 ~ 0,
      SEX_HRS_RA == 2 & alc_ndrink_3 >= 3 ~ 1,
      SEX_HRS_RA == 1 & alc_ndrink_3 <  4 ~ 0,
      SEX_HRS_RA == 1 & alc_ndrink_3 >= 4 ~ 1,
      alc_ever_3 == 0 ~ 0),
    alcohol_bingedrink_HRS_4= case_when(
      SEX_HRS_RA == 2 & alc_ndrink_4 <  3 ~ 0,
      SEX_HRS_RA == 2 & alc_ndrink_4 >= 3 ~ 1,
      SEX_HRS_RA == 1 & alc_ndrink_4 <  4 ~ 0,
      SEX_HRS_RA == 1 & alc_ndrink_4 >= 4 ~ 1,
      alc_ever_4 == 0 ~ 0),
    alcohol_bingedrink_HRS_5= case_when(
      SEX_HRS_RA == 2 & alc_ndrink_5 <  3 ~ 0,
      SEX_HRS_RA == 2 & alc_ndrink_5 >= 3 ~ 1,
      SEX_HRS_RA == 1 & alc_ndrink_5 <  4 ~ 0,
      SEX_HRS_RA == 1 & alc_ndrink_5 >= 4 ~ 1,
      alc_ever_5 == 0 ~ 0),
    alcohol_bingedrink_HRS_6= case_when(
      SEX_HRS_RA == 2 & alc_ndrink_6 <  3 ~ 0,
      SEX_HRS_RA == 2 & alc_ndrink_6 >= 3 ~ 1,
      SEX_HRS_RA == 1 & alc_ndrink_6 <  4 ~ 0,
      SEX_HRS_RA == 1 & alc_ndrink_6 >= 4 ~ 1,
      alc_ever_6 == 0 ~ 0),
    alcohol_bingedrink_HRS_7= case_when(
      SEX_HRS_RA == 2 & alc_ndrink_7 <  3 ~ 0,
      SEX_HRS_RA == 2 & alc_ndrink_7 >= 3 ~ 1,
      SEX_HRS_RA == 1 & alc_ndrink_7 <  4 ~ 0,
      SEX_HRS_RA == 1 & alc_ndrink_7 >= 4 ~ 1,
      alc_ever_7 == 0 ~ 0),
    alcohol_bingedrink_HRS_8= case_when(
      SEX_HRS_RA == 2 & alc_ndrink_8 <  3 ~ 0,
      SEX_HRS_RA == 2 & alc_ndrink_8 >= 3 ~ 1,
      SEX_HRS_RA == 1 & alc_ndrink_8 <  4 ~ 0,
      SEX_HRS_RA == 1 & alc_ndrink_8 >= 4 ~ 1,
      alc_ever_8 == 0 ~ 0),
    alcohol_bingedrink_HRS_9= case_when(
      SEX_HRS_RA == 2 & alc_ndrink_9 <  3 ~ 0,
      SEX_HRS_RA == 2 & alc_ndrink_9 >= 3 ~ 1,
      SEX_HRS_RA == 1 & alc_ndrink_9 <  4 ~ 0,
      SEX_HRS_RA == 1 & alc_ndrink_9 >= 4 ~ 1,
      alc_ever_9 == 0 ~ 0),
    alcohol_bingedrink_HRS_10= case_when(
      SEX_HRS_RA == 2 & alc_ndrink_10 <  3 ~ 0,
      SEX_HRS_RA == 2 & alc_ndrink_10 >= 3 ~ 1,
      SEX_HRS_RA == 1 & alc_ndrink_10 <  4 ~ 0,
      SEX_HRS_RA == 1 & alc_ndrink_10 >= 4 ~ 1,
      alc_ever_10 == 0 ~ 0),
    alcohol_bingedrink_HRS_11= case_when(
      SEX_HRS_RA == 2 & alc_ndrink_11 <  3 ~ 0,
      SEX_HRS_RA == 2 & alc_ndrink_11 >= 3 ~ 1,
      SEX_HRS_RA == 1 & alc_ndrink_11 <  4 ~ 0,
      SEX_HRS_RA == 1 & alc_ndrink_11 >= 4 ~ 1,
      alc_ever_11 == 0 ~ 0),
    alcohol_bingedrink_HRS_12= case_when(
      SEX_HRS_RA == 2 & alc_ndrink_12 <  3 ~ 0,
      SEX_HRS_RA == 2 & alc_ndrink_12 >= 3 ~ 1,
      SEX_HRS_RA == 1 & alc_ndrink_12 <  4 ~ 0,
      SEX_HRS_RA == 1 & alc_ndrink_12 >= 4 ~ 1,
      alc_ever_12 == 0 ~ 0),
    alcohol_bingedrink_HRS_13= case_when(
      SEX_HRS_RA == 2 & alc_ndrink_13 <  3 ~ 0,
      SEX_HRS_RA == 2 & alc_ndrink_13 >= 3 ~ 1,
      SEX_HRS_RA == 1 & alc_ndrink_13 <  4 ~ 0,
      SEX_HRS_RA == 1 & alc_ndrink_13 >= 4 ~ 1,
      alc_ever_13 == 0 ~ 0),
    alcohol_bingedrink_HRS_14= case_when(
      SEX_HRS_RA == 2 & alc_ndrink_14 <  3 ~ 0,
      SEX_HRS_RA == 2 & alc_ndrink_14 >= 3 ~ 1,
      SEX_HRS_RA == 1 & alc_ndrink_14 <  4 ~ 0,
      SEX_HRS_RA == 1 & alc_ndrink_14 >= 4 ~ 1,
      alc_ever_14 == 0 ~ 0),
    alcohol_bingedrink_HRS_15= case_when(
      SEX_HRS_RA == 2 & alc_ndrink_15 <  3 ~ 0,
      SEX_HRS_RA == 2 & alc_ndrink_15 >= 3 ~ 1,
      SEX_HRS_RA == 1 & alc_ndrink_15 <  4 ~ 0,
      SEX_HRS_RA == 1 & alc_ndrink_15 >= 4 ~ 1,
      alc_ever_15 == 0 ~ 0),

    ### Frequency*Binge ----
    alcohol_bingecat_HRS_3= case_when(
      alcohol_cat_HRS_3 == "None" & alcohol_bingedrink_HRS_3 == 0 ~ "None",
      alcohol_cat_HRS_3 == "None" & alcohol_bingedrink_HRS_3 == 1 ~ "NoneButBinge",
      alcohol_cat_HRS_3 == "LiteToMod" & alcohol_bingedrink_HRS_3 == 0 ~ "LiteNoBinge",
      alcohol_cat_HRS_3 == "LiteToMod" & alcohol_bingedrink_HRS_3 == 1 ~ "LiteButBinge",
      alcohol_cat_HRS_3 == "Severe" & alcohol_bingedrink_HRS_3 == 0 ~ "HeavyNoBinge",
      alcohol_cat_HRS_3 == "Severe" & alcohol_bingedrink_HRS_3 == 1 ~ "HeavyAndBinge"),
    alcohol_bingecat_HRS_4= case_when(
      alcohol_cat_HRS_4 == "None" & alcohol_bingedrink_HRS_4 == 0 ~ "None",
      alcohol_cat_HRS_4 == "None" & alcohol_bingedrink_HRS_4 == 1 ~ "NoneButBinge",
      alcohol_cat_HRS_4 == "LiteToMod" & alcohol_bingedrink_HRS_4 == 0 ~ "LiteNoBinge",
      alcohol_cat_HRS_4 == "LiteToMod" & alcohol_bingedrink_HRS_4 == 1 ~ "LiteButBinge",
      alcohol_cat_HRS_4 == "Severe" & alcohol_bingedrink_HRS_4 == 0 ~ "HeavyNoBinge",
      alcohol_cat_HRS_4 == "Severe" & alcohol_bingedrink_HRS_4 == 1 ~ "HeavyAndBinge"),
    alcohol_bingecat_HRS_5= case_when(
      alcohol_cat_HRS_5 == "None" & alcohol_bingedrink_HRS_5 == 0 ~ "None",
      alcohol_cat_HRS_5 == "None" & alcohol_bingedrink_HRS_5 == 1 ~ "NoneButBinge",
      alcohol_cat_HRS_5 == "LiteToMod" & alcohol_bingedrink_HRS_5 == 0 ~ "LiteNoBinge",
      alcohol_cat_HRS_5 == "LiteToMod" & alcohol_bingedrink_HRS_5 == 1 ~ "LiteButBinge",
      alcohol_cat_HRS_5 == "Severe" & alcohol_bingedrink_HRS_5 == 0 ~ "HeavyNoBinge",
      alcohol_cat_HRS_5 == "Severe" & alcohol_bingedrink_HRS_5 == 1 ~ "HeavyAndBinge"),
    alcohol_bingecat_HRS_6= case_when(
      alcohol_cat_HRS_6 == "None" & alcohol_bingedrink_HRS_6 == 0 ~ "None",
      alcohol_cat_HRS_6 == "None" & alcohol_bingedrink_HRS_6 == 1 ~ "NoneButBinge",
      alcohol_cat_HRS_6 == "LiteToMod" & alcohol_bingedrink_HRS_6 == 0 ~ "LiteNoBinge",
      alcohol_cat_HRS_6 == "LiteToMod" & alcohol_bingedrink_HRS_6 == 1 ~ "LiteButBinge",
      alcohol_cat_HRS_6 == "Severe" & alcohol_bingedrink_HRS_6 == 0 ~ "HeavyNoBinge",
      alcohol_cat_HRS_6 == "Severe" & alcohol_bingedrink_HRS_6 == 1 ~ "HeavyAndBinge"),
    alcohol_bingecat_HRS_7= case_when(
      alcohol_cat_HRS_7 == "None" & alcohol_bingedrink_HRS_7 == 0 ~ "None",
      alcohol_cat_HRS_7 == "None" & alcohol_bingedrink_HRS_7 == 1 ~ "NoneButBinge",
      alcohol_cat_HRS_7 == "LiteToMod" & alcohol_bingedrink_HRS_7 == 0 ~ "LiteNoBinge",
      alcohol_cat_HRS_7 == "LiteToMod" & alcohol_bingedrink_HRS_7 == 1 ~ "LiteButBinge",
      alcohol_cat_HRS_7 == "Severe" & alcohol_bingedrink_HRS_7 == 0 ~ "HeavyNoBinge",
      alcohol_cat_HRS_7 == "Severe" & alcohol_bingedrink_HRS_7 == 1 ~ "HeavyAndBinge"),
    alcohol_bingecat_HRS_8= case_when(
      alcohol_cat_HRS_8 == "None" & alcohol_bingedrink_HRS_8 == 0 ~ "None",
      alcohol_cat_HRS_8 == "None" & alcohol_bingedrink_HRS_8 == 1 ~ "NoneButBinge",
      alcohol_cat_HRS_8 == "LiteToMod" & alcohol_bingedrink_HRS_8 == 0 ~ "LiteNoBinge",
      alcohol_cat_HRS_8 == "LiteToMod" & alcohol_bingedrink_HRS_8 == 1 ~ "LiteButBinge",
      alcohol_cat_HRS_8 == "Severe" & alcohol_bingedrink_HRS_8 == 0 ~ "HeavyNoBinge",
      alcohol_cat_HRS_8 == "Severe" & alcohol_bingedrink_HRS_8 == 1 ~ "HeavyAndBinge"),
    alcohol_bingecat_HRS_9= case_when(
      alcohol_cat_HRS_9 == "None" & alcohol_bingedrink_HRS_9 == 0 ~ "None",
      alcohol_cat_HRS_9 == "None" & alcohol_bingedrink_HRS_9 == 1 ~ "NoneButBinge",
      alcohol_cat_HRS_9 == "LiteToMod" & alcohol_bingedrink_HRS_9 == 0 ~ "LiteNoBinge",
      alcohol_cat_HRS_9 == "LiteToMod" & alcohol_bingedrink_HRS_9 == 1 ~ "LiteButBinge",
      alcohol_cat_HRS_9 == "Severe" & alcohol_bingedrink_HRS_9 == 0 ~ "HeavyNoBinge",
      alcohol_cat_HRS_9 == "Severe" & alcohol_bingedrink_HRS_9 == 1 ~ "HeavyAndBinge"),
    alcohol_bingecat_HRS_10= case_when(
      alcohol_cat_HRS_10 == "None" & alcohol_bingedrink_HRS_10 == 0 ~ "None",
      alcohol_cat_HRS_10 == "None" & alcohol_bingedrink_HRS_10 == 1 ~ "NoneButBinge",
      alcohol_cat_HRS_10 == "LiteToMod" & alcohol_bingedrink_HRS_10 == 0 ~ "LiteNoBinge",
      alcohol_cat_HRS_10 == "LiteToMod" & alcohol_bingedrink_HRS_10 == 1 ~ "LiteButBinge",
      alcohol_cat_HRS_10 == "Severe" & alcohol_bingedrink_HRS_10 == 0 ~ "HeavyNoBinge",
      alcohol_cat_HRS_10 == "Severe" & alcohol_bingedrink_HRS_10 == 1 ~ "HeavyAndBinge"),
    alcohol_bingecat_HRS_11= case_when(
      alcohol_cat_HRS_11 == "None" & alcohol_bingedrink_HRS_11 == 0 ~ "None",
      alcohol_cat_HRS_11 == "None" & alcohol_bingedrink_HRS_11 == 1 ~ "NoneButBinge",
      alcohol_cat_HRS_11 == "LiteToMod" & alcohol_bingedrink_HRS_11 == 0 ~ "LiteNoBinge",
      alcohol_cat_HRS_11 == "LiteToMod" & alcohol_bingedrink_HRS_11 == 1 ~ "LiteButBinge",
      alcohol_cat_HRS_11 == "Severe" & alcohol_bingedrink_HRS_11 == 0 ~ "HeavyNoBinge",
      alcohol_cat_HRS_11 == "Severe" & alcohol_bingedrink_HRS_11 == 1 ~ "HeavyAndBinge"),
    alcohol_bingecat_HRS_12= case_when(
      alcohol_cat_HRS_12 == "None" & alcohol_bingedrink_HRS_12 == 0 ~ "None",
      alcohol_cat_HRS_12 == "None" & alcohol_bingedrink_HRS_12 == 1 ~ "NoneButBinge",
      alcohol_cat_HRS_12 == "LiteToMod" & alcohol_bingedrink_HRS_12 == 0 ~ "LiteNoBinge",
      alcohol_cat_HRS_12 == "LiteToMod" & alcohol_bingedrink_HRS_12 == 1 ~ "LiteButBinge",
      alcohol_cat_HRS_12 == "Severe" & alcohol_bingedrink_HRS_12 == 0 ~ "HeavyNoBinge",
      alcohol_cat_HRS_12 == "Severe" & alcohol_bingedrink_HRS_12 == 1 ~ "HeavyAndBinge"),
    alcohol_bingecat_HRS_13= case_when(
      alcohol_cat_HRS_13 == "None" & alcohol_bingedrink_HRS_13 == 0 ~ "None",
      alcohol_cat_HRS_13 == "None" & alcohol_bingedrink_HRS_13 == 1 ~ "NoneButBinge",
      alcohol_cat_HRS_13 == "LiteToMod" & alcohol_bingedrink_HRS_13 == 0 ~ "LiteNoBinge",
      alcohol_cat_HRS_13 == "LiteToMod" & alcohol_bingedrink_HRS_13 == 1 ~ "LiteButBinge",
      alcohol_cat_HRS_13 == "Severe" & alcohol_bingedrink_HRS_13 == 0 ~ "HeavyNoBinge",
      alcohol_cat_HRS_13 == "Severe" & alcohol_bingedrink_HRS_13 == 1 ~ "HeavyAndBinge"),
    alcohol_bingecat_HRS_14= case_when(
      alcohol_cat_HRS_14 == "None" & alcohol_bingedrink_HRS_14 == 0 ~ "None",
      alcohol_cat_HRS_14 == "None" & alcohol_bingedrink_HRS_14 == 1 ~ "NoneButBinge",
      alcohol_cat_HRS_14 == "LiteToMod" & alcohol_bingedrink_HRS_14 == 0 ~ "LiteNoBinge",
      alcohol_cat_HRS_14 == "LiteToMod" & alcohol_bingedrink_HRS_14 == 1 ~ "LiteButBinge",
      alcohol_cat_HRS_14 == "Severe" & alcohol_bingedrink_HRS_14 == 0 ~ "HeavyNoBinge",
      alcohol_cat_HRS_14 == "Severe" & alcohol_bingedrink_HRS_14 == 1 ~ "HeavyAndBinge"),
    alcohol_bingecat_HRS_15= case_when(
      alcohol_cat_HRS_15 == "None" & alcohol_bingedrink_HRS_15 == 0 ~ "None",
      alcohol_cat_HRS_15 == "None" & alcohol_bingedrink_HRS_15 == 1 ~ "NoneButBinge",
      alcohol_cat_HRS_15 == "LiteToMod" & alcohol_bingedrink_HRS_15 == 0 ~ "LiteNoBinge",
      alcohol_cat_HRS_15 == "LiteToMod" & alcohol_bingedrink_HRS_15 == 1 ~ "LiteButBinge",
      alcohol_cat_HRS_15 == "Severe" & alcohol_bingedrink_HRS_15 == 0 ~ "HeavyNoBinge",
      alcohol_cat_HRS_15 == "Severe" & alcohol_bingedrink_HRS_15 == 1 ~ "HeavyAndBinge"),

    ### Alcohol Drinks per Month (for matching) ----
    ALCOHOL_DRINKSPERMO_HRS_3 = (alc_nweek_3*4)*alc_ndrink_3,
    ALCOHOL_DRINKSPERMO_HRS_4 = (alc_nweek_4*4)*alc_ndrink_4,
    ALCOHOL_DRINKSPERMO_HRS_5 = (alc_nweek_5*4)*alc_ndrink_5,
    ALCOHOL_DRINKSPERMO_HRS_6 = (alc_nweek_6*4)*alc_ndrink_6,
    ALCOHOL_DRINKSPERMO_HRS_7 = (alc_nweek_7*4)*alc_ndrink_7,
    ALCOHOL_DRINKSPERMO_HRS_8 = (alc_nweek_8*4)*alc_ndrink_8,
    ALCOHOL_DRINKSPERMO_HRS_9 = (alc_nweek_9*4)*alc_ndrink_9,
    ALCOHOL_DRINKSPERMO_HRS_10= (alc_nweek_10*4)*alc_ndrink_10,
    ALCOHOL_DRINKSPERMO_HRS_11= (alc_nweek_11*4)*alc_ndrink_11,
    ALCOHOL_DRINKSPERMO_HRS_12= (alc_nweek_12*4)*alc_ndrink_12,
    ALCOHOL_DRINKSPERMO_HRS_13= (alc_nweek_13*4)*alc_ndrink_13,
    ALCOHOL_DRINKSPERMO_HRS_14= (alc_nweek_14*4)*alc_ndrink_14,
    ALCOHOL_DRINKSPERMO_HRS_15= (alc_nweek_15*4)*alc_ndrink_15,
    

    ## Imputed Cog Scores (outcome) ----
    
    # Preliminary cognitive cleaning
    
    # Add ceilings to variables that have maximum of 20
    COG_RECALL_HRS_1 = ifelse(COG_RECALL_HRS_W1 > 10, 
                               10,COG_RECALL_HRS_W1),
    COG_DELAYED_HRS_1= ifelse(COG_DELAYED_HRS_W1 > 10, 
                               10,COG_DELAYED_HRS_W1),
    
    COG_RECALL_HRS_2_HRS = ifelse(COG_RECALL_HRS_W2_HRS > 10, 
                                   10,COG_RECALL_HRS_W2_HRS),
    COG_DELAYED_HRS_2_HRS= ifelse(COG_DELAYED_HRS_W2_HRS> 10, 
                                   10,COG_DELAYED_HRS_W2_HRS),
    
    # Collapse Wave 2 AHD & HRS (if missing from one, use other)
    COG_RECALL_HRS_2 = ifelse(is.na(COG_RECALL_HRS_2_HRS),COG_RECALL_HRS_W2_AHD,
                               COG_RECALL_HRS_2_HRS),
    COG_DELAYED_HRS_2= ifelse(is.na(COG_DELAYED_HRS_2_HRS),COG_DELAYED_HRS_W2_AHD,
                               COG_DELAYED_HRS_2_HRS),
    
    
    # sum recall scores
    summed_recall_score_HRS_1 = COG_RECALL_HRS_1 + COG_DELAYED_HRS_1,
    # summed_recall_score_HRS_w2_AHD = COG_RECALL_HRS_W2_AHD + COG_DELAYED_HRS_W2_AHD,
    # summed_recall_score_HRS_w2_HRS = COG_RECALL_HRS_W2_HRS + COG_DELAYED_HRS_W2_HRS,
    summed_recall_score_HRS_2 = COG_RECALL_HRS_2 + COG_DELAYED_HRS_2,
    summed_recall_score_HRS_3 = COG_RECALL_HRS_3 + COG_DELAYED_HRS_3,
    summed_recall_score_HRS_4 = COG_RECALL_HRS_4 + COG_DELAYED_HRS_4,
    summed_recall_score_HRS_5 = COG_RECALL_HRS_5 + COG_DELAYED_HRS_5,
    summed_recall_score_HRS_6 = COG_RECALL_HRS_6 + COG_DELAYED_HRS_6,
    summed_recall_score_HRS_7 = COG_RECALL_HRS_7 + COG_DELAYED_HRS_7,
    summed_recall_score_HRS_8 = COG_RECALL_HRS_8 + COG_DELAYED_HRS_8,
    summed_recall_score_HRS_9 = COG_RECALL_HRS_9 + COG_DELAYED_HRS_9,
    summed_recall_score_HRS_10= COG_RECALL_HRS_10+ COG_DELAYED_HRS_10,
    summed_recall_score_HRS_11= COG_RECALL_HRS_11+ COG_DELAYED_HRS_11,
    summed_recall_score_HRS_12= COG_RECALL_HRS_12+ COG_DELAYED_HRS_12,
    summed_recall_score_HRS_13= COG_RECALL_HRS_13+ COG_DELAYED_HRS_13,
    
    # For some reason, the following are not found in the RAND longitudinal file
    # summed_recall_score_HRS_14= COG_RECALL_HRS_14+ COG_DELAYED_HRS_14,
    # summed_recall_score_HRS_15= COG_RECALL_HRS_15+ COG_DELAYED_HRS_15
    ) %>% 
  # Rename Alcohol Vars for Cross walk ----
  rename(
    alc_range_HRS_1 = alc_range_1,
    alc_range_HRS_2 = alc_range_2,
    alc_nweek_HRS_3 = alc_nweek_3,
    alc_nweek_HRS_4 = alc_nweek_4,
    alc_nweek_HRS_5 = alc_nweek_5,
    alc_nweek_HRS_6 = alc_nweek_6,
    alc_nweek_HRS_7 = alc_nweek_7,
    alc_nweek_HRS_8 = alc_nweek_8,
    alc_nweek_HRS_9 = alc_nweek_9,
    alc_nweek_HRS_10= alc_nweek_10,
    alc_nweek_HRS_11= alc_nweek_11,
    alc_nweek_HRS_12= alc_nweek_12,
    alc_nweek_HRS_13= alc_nweek_13,
    alc_nweek_HRS_14= alc_nweek_14,
    alc_nweek_HRS_15= alc_nweek_15,
    alc_ndrink_HRS_3= alc_ndrink_3,
    alc_ndrink_HRS_4= alc_ndrink_4,
    alc_ndrink_HRS_5= alc_ndrink_5,
    alc_ndrink_HRS_6= alc_ndrink_6,
    alc_ndrink_HRS_7= alc_ndrink_7,
    alc_ndrink_HRS_8= alc_ndrink_8,
    alc_ndrink_HRS_9= alc_ndrink_9,
    alc_ndrink_HRS_10=alc_ndrink_10,
    alc_ndrink_HRS_11=alc_ndrink_11,
    alc_ndrink_HRS_12=alc_ndrink_12,
    alc_ndrink_HRS_13=alc_ndrink_13,
    alc_ndrink_HRS_14=alc_ndrink_14,
    alc_ndrink_HRS_15=alc_ndrink_15
    ) %>%
  # Mutate-at ----
  # Marriage
  # Married, partnered, separated/divorced/absent, widowed, never
  mutate_at(grep("MARRIAGE", names(HRS), value=TRUE),
            recode, "1" = "married",
            "2" = "separated/divorced/absent",
            "3" = "partnered",
            "4" = "separated/divorced/absent",
            "5" = "separated/divorced/absent",
            "6" = "separated/divorced/absent",
            "7" = "widowed",
            "8" = "never"

  ) %>% # Health Flags
  mutate_at(grep("DISPUTE", names(HRS), value=TRUE),
            list(~ case_when(
              . == 0 ~ 0,
              . >  0 ~ 1))
  ) %>% # Self-Health
  mutate_at(grep("SELF_HEALTH", names(HRS), value=TRUE),
            recode, "1" = "Excellent",
            "2" = "Very Good",
            "3" = "Good",
            "4" = "Fair",
            "5" = "Poor"
  ) %>% # Vigorous Activities
  mutate_at(grep("vig_activities_freq", names(HRS), value=TRUE),
            recode, "1" = 1,
            "2" = 1,
            "3" = 0,
            "4" = 0,
            "5" = 0)

# Additional Cleaning ----

## Rename for consistency ----
hrs <- hrs %>% rename(
    GENHEALTH_HRS_1 = SELF_HEALTH_HRS_1,
    GENHEALTH_HRS_2 = SELF_HEALTH_HRS_2,
    GENHEALTH_HRS_3 = SELF_HEALTH_HRS_3,
    GENHEALTH_HRS_4 = SELF_HEALTH_HRS_4,
    GENHEALTH_HRS_5 = SELF_HEALTH_HRS_5,
    GENHEALTH_HRS_6 = SELF_HEALTH_HRS_6,
    GENHEALTH_HRS_7 = SELF_HEALTH_HRS_7,
    GENHEALTH_HRS_8 = SELF_HEALTH_HRS_8,
    GENHEALTH_HRS_9 = SELF_HEALTH_HRS_9,
    GENHEALTH_HRS_10= SELF_HEALTH_HRS_10,
    GENHEALTH_HRS_11= SELF_HEALTH_HRS_11,
    GENHEALTH_HRS_12= SELF_HEALTH_HRS_12,
    GENHEALTH_HRS_13= SELF_HEALTH_HRS_13,
    GENHEALTH_HRS_14= SELF_HEALTH_HRS_14,
    GENHEALTH_HRS_15= SELF_HEALTH_HRS_15,
    
    VIG_EXERCISE_HRS_7 = vig_activities_freq_HRS_7,
    VIG_EXERCISE_HRS_8 = vig_activities_freq_HRS_8,
    VIG_EXERCISE_HRS_9 = vig_activities_freq_HRS_9,
    VIG_EXERCISE_HRS_10= vig_activities_freq_HRS_10,
    VIG_EXERCISE_HRS_11= vig_activities_freq_HRS_11,
    VIG_EXERCISE_HRS_12= vig_activities_freq_HRS_12,
    VIG_EXERCISE_HRS_13= vig_activities_freq_HRS_13,
    VIG_EXERCISE_HRS_14= vig_activities_freq_HRS_14,
    VIG_EXERCISE_HRS_15= vig_activities_freq_HRS_15,
    
    # Do you ever drink alcohol?
    # NOT Have you ever drank alcohol?
    ALCOHOL_NOW_HRS_1 = alc_ever_1,
    ALCOHOL_NOW_HRS_2 = alc_ever_2,
    ALCOHOL_NOW_HRS_3 = alc_ever_3,
    ALCOHOL_NOW_HRS_4 = alc_ever_4,
    ALCOHOL_NOW_HRS_5 = alc_ever_5,
    ALCOHOL_NOW_HRS_6 = alc_ever_6,
    ALCOHOL_NOW_HRS_7 = alc_ever_7,
    ALCOHOL_NOW_HRS_8 = alc_ever_8,
    ALCOHOL_NOW_HRS_9 = alc_ever_9,
    ALCOHOL_NOW_HRS_10 = alc_ever_10,
    ALCOHOL_NOW_HRS_11 = alc_ever_11,
    ALCOHOL_NOW_HRS_12 = alc_ever_12,
    ALCOHOL_NOW_HRS_13 = alc_ever_13,
    ALCOHOL_NOW_HRS_14 = alc_ever_14,
    ALCOHOL_NOW_HRS_15 = alc_ever_15,
    
    MILITARY_HRS_RA = MILITARY_HRS
) 

hrs <- hrs %>% mutate(
  ALCOHOL_EVER_HRS_1 = case_when(hrs$ALCOHOL_NOW_HRS_1 == 1 ~ 1,
                                 hrs$ALCOHOL_NOW_HRS_1 == 0 ~ 0),
  ALCOHOL_EVER_HRS_2 = case_when(hrs$ALCOHOL_NOW_HRS_2 == 1 | ALCOHOL_EVER_HRS_1 == 1 ~ 1,
                                 hrs$ALCOHOL_NOW_HRS_2 == 0 | ALCOHOL_EVER_HRS_1 == 0 ~ 0),
  ALCOHOL_EVER_HRS_3 = case_when(hrs$ALCOHOL_NOW_HRS_3 == 1 | ALCOHOL_EVER_HRS_2 == 1 ~ 1,
                                (hrs$ALCOHOL_NOW_HRS_3 == 0 | ALCOHOL_EVER_HRS_2 == 0)~ 0),
  ALCOHOL_EVER_HRS_4 = case_when(hrs$ALCOHOL_NOW_HRS_4 == 1 | ALCOHOL_EVER_HRS_3 == 1 ~ 1,
                                (hrs$ALCOHOL_NOW_HRS_4 == 0 | ALCOHOL_EVER_HRS_3 == 0)|
                                (ALCOHOL_EVER_HRS_3 == 0 | is.na(ALCOHOL_EVER_HRS_3)) ~ 0),
  ALCOHOL_EVER_HRS_5 = case_when(hrs$ALCOHOL_NOW_HRS_5 == 1 | ALCOHOL_EVER_HRS_4 == 1 ~ 1,
                                (hrs$ALCOHOL_NOW_HRS_5 == 0 | ALCOHOL_EVER_HRS_4 == 0)|
                                (ALCOHOL_EVER_HRS_4 == 0 | is.na(ALCOHOL_EVER_HRS_4)) ~ 0),
  ALCOHOL_EVER_HRS_6 = case_when(hrs$ALCOHOL_NOW_HRS_6 == 1 | ALCOHOL_EVER_HRS_5 == 1 ~ 1,
                                (hrs$ALCOHOL_NOW_HRS_6 == 0 | ALCOHOL_EVER_HRS_5 == 0)|
                                (ALCOHOL_EVER_HRS_5 == 0 | is.na(ALCOHOL_EVER_HRS_5)) ~ 0),
  ALCOHOL_EVER_HRS_7 = case_when(hrs$ALCOHOL_NOW_HRS_7 == 1 | ALCOHOL_EVER_HRS_6 == 1 ~ 1,
                                (hrs$ALCOHOL_NOW_HRS_7 == 0 | ALCOHOL_EVER_HRS_6 == 0)|
                                (ALCOHOL_EVER_HRS_6 == 0 | is.na(ALCOHOL_EVER_HRS_6)) ~ 0),
  ALCOHOL_EVER_HRS_8 = case_when(hrs$ALCOHOL_NOW_HRS_8 == 1 | ALCOHOL_EVER_HRS_7 == 1 ~ 1,
                                (hrs$ALCOHOL_NOW_HRS_8 == 0 | ALCOHOL_EVER_HRS_7 == 0)|
                                (ALCOHOL_EVER_HRS_7 == 0 | is.na(ALCOHOL_EVER_HRS_7)) ~ 0),
  ALCOHOL_EVER_HRS_9 = case_when(hrs$ALCOHOL_NOW_HRS_9 == 1 | ALCOHOL_EVER_HRS_8 == 1 ~ 1,
                                (hrs$ALCOHOL_NOW_HRS_9 == 0 | ALCOHOL_EVER_HRS_8 == 0)|
                                (ALCOHOL_EVER_HRS_8 == 0 | is.na(ALCOHOL_EVER_HRS_8)) ~ 0),
  # switch to less than 12 alcoholic beverages in lifetime - much larger sample
  ALCOHOL_EVER_HRS_10= case_when(hrs$ALCOHOL_NOW_HRS_10== 1 | ALCOHOL_EVER_HRS_9 == 1 ~ 1,
                                (hrs$ALCOHOL_NOW_HRS_10== 0 | ALCOHOL_EVER_HRS_9 == 0)|
                                (ALCOHOL_EVER_HRS_9 == 0 | is.na(ALCOHOL_EVER_HRS_9)) ~ 0),
  ALCOHOL_EVER_HRS_11= case_when(hrs$ALCOHOL_NOW_HRS_11== 1 | ALCOHOL_EVER_HRS_10== 1 ~ 1,
                                (hrs$ALCOHOL_NOW_HRS_11== 1 | ALCOHOL_EVER_HRS_10== 0)|
                                (ALCOHOL_EVER_HRS_10== 0 | is.na(ALCOHOL_EVER_HRS_10))~ 0),
  ALCOHOL_EVER_HRS_12= case_when(hrs$ALCOHOL_NOW_HRS_12== 1 | ALCOHOL_EVER_HRS_11== 1 ~ 1,
                                (hrs$ALCOHOL_NOW_HRS_12== 0 | ALCOHOL_EVER_HRS_11== 0)| 
                                (ALCOHOL_EVER_HRS_11== 0 | is.na(ALCOHOL_EVER_HRS_11))~ 0),
  ALCOHOL_EVER_HRS_13= case_when(hrs$ALCOHOL_NOW_HRS_13== 1 | ALCOHOL_EVER_HRS_12== 1 ~ 1,
                                (hrs$ALCOHOL_NOW_HRS_13== 0 | ALCOHOL_EVER_HRS_12== 0)|
                                (ALCOHOL_EVER_HRS_12== 0 | is.na(ALCOHOL_EVER_HRS_12))~ 0),
  ALCOHOL_EVER_HRS_14= case_when(hrs$ALCOHOL_NOW_HRS_14== 1 | ALCOHOL_EVER_HRS_13== 1 ~ 1,
                                (hrs$ALCOHOL_NOW_HRS_14== 0 | ALCOHOL_EVER_HRS_13== 0)| 
                                (ALCOHOL_EVER_HRS_13== 0 | is.na(ALCOHOL_EVER_HRS_13))~ 0),
  ALCOHOL_EVER_HRS_15= case_when(hrs$ALCOHOL_NOW_HRS_15== 1 | ALCOHOL_EVER_HRS_14== 1 ~ 1,
                                (hrs$ALCOHOL_NOW_HRS_15== 0 | ALCOHOL_EVER_HRS_14== 0)| 
                                (ALCOHOL_EVER_HRS_14== 0 | is.na(ALCOHOL_EVER_HRS_14))~ 0)
  
)

## Remove intermediate variables ----
hrs <- hrs %>% select(
  -starts_with("DIABETES_HRS_EVER"),
  -starts_with("HYPERTENSION_HRS_EVER"),
  -starts_with("CANCER_HRS_EVER"),
  -starts_with("HEARTPROB_HRS_EVER"),
  -starts_with("HEIGHT_REPORTED"),
  -starts_with("WEIGHT_REPORTED")
)

## Order Factor Variables ----
hrs <- hrs %>% mutate(
  across(starts_with("GENHEALTH_HRS"), \(x) 
         factor(x, levels=c("Excellent","Very Good","Good","Fair","Poor"))))

hrs <- hrs %>% mutate(
  firstwave_HRS = case_when(FIRSTIW == 1992 ~ 1, FIRSTIW == 1993 ~ 2,
                            FIRSTIW == 1994 ~ 2, FIRSTIW == 1995 ~ 3,
                            FIRSTIW == 1996 ~ 3, FIRSTIW == 1998 ~ 4,
                            FIRSTIW == 2000 ~ 5, FIRSTIW == 2002 ~ 6,
                            FIRSTIW == 2004 ~ 7, FIRSTIW == 2006 ~ 8,
                            FIRSTIW == 2008 ~ 9, FIRSTIW == 2010 ~ 10,
                            FIRSTIW == 2012 ~ 11,FIRSTIW == 2014 ~ 12,
                            FIRSTIW == 2016 ~ 13,FIRSTIW == 2018 ~ 14,
                            FIRSTIW == 2020 ~ 15))

# Create a matching variable for diabetes
hrs <- hrs %>% mutate(
  DIAB_MATCH_VAR = case_when(DIABETES_HRS_2 == 1 ~ 1, # Prevalent (at exp)
                             DIABETES_HRS_9 == 1 ~ 2, # Incident
                             TRUE ~ 0) # Never
)

# UNSURE IF WE NEED THE FOLLOWING SECTION (GOOD FOR EVALUATING QUALITY?)


# # Missing Table ----
# # count non-missing (e.g. !is.na(x))
# # number of responses for each variable in a given wave (pre-carry forward)
# # This allows us to see if the question was completely not asked that wave
# # Repeat after carrying forward
# # What would the counts be if we required complete cases? 
# # (repeat for NLSY)
# 
# # Initiate a list of empty data frames
# NA_tables <- list()
# NA_tables$HRS_NA     <- data.frame(matrix(nrow = length(c(instructions$distVars, instructions$exact_timevarying))))
# NA_tables$HRS_N      <- data.frame(matrix(nrow = length(c(instructions$distVars, instructions$exact_timevarying))))
# NA_tables$HRS_NA_prop<- data.frame(matrix(nrow = length(c(instructions$distVars, instructions$exact_timevarying))))
# NA_tables$HRS_N_prop <- data.frame(matrix(nrow = length(c(instructions$distVars, instructions$exact_timevarying))))
# 
# # rows of variables (distance & exact-TV) and columns of waves
# HRS_names <- c(
#   "HEIGHT", "WEIGHT", "BMI",
#   "LIGHT_EXERCISE", "VIG_EXERCISE", 
#   "GENHEALTH", 
#   'INCOME_PP_LOG10',
#   'CESD_NEW6PT',
#   'DIABETES','HYPERTENSION','CANCER','HEARTPROB',
#   'SMOKE_EVER','SMOKE_NOW',
#   'ALCOHOL_NOW',
#   'ALCOHOL_DRINKSPERMO',
#   'ALCOHOL_BINGE2',
#   'CAGE_MORNING',
#   'RATE_MEM_COG','RATE_PAST_COG',
#   'IMMED_RECALL','DELAY_RECALL','SERIAL_7',
#   "ALCOHOL_EVER",  'MARRIAGE',
#   # The following are only captured once, but include just incase
#   "RELIGION", 'MILITARY',
#   'EDU_NEW',
#   'DAD_EDU','MOM_EDU')
# 
# # Name each row 
# row.names(NA_tables$HRS_NA)     <- HRS_names
# row.names(NA_tables$HRS_N)      <- HRS_names
# row.names(NA_tables$HRS_NA_prop)<- HRS_names
# row.names(NA_tables$HRS_N_prop) <- HRS_names
# 
# # For each variable,
# for(var in HRS_names){
#   # loop through each HRS wave (including RA, "Baseline")
#   for(wave in c(1:15, "RA")){
#     # If the variable was measured that wave
#     if(paste0(var,"_HRS_",wave) %in% names(hrs)){
#       # Calculate either the number missing/present
#       # or the proportion missing present
#       NA_tables$HRS_NA[var,paste0("wave_",wave)]     <- sum(is.na(hrs[[paste0(var,"_HRS_",wave)]]))
#       NA_tables$HRS_N[var,paste0("wave_",wave)]      <- sum(!is.na(hrs[[paste0(var,"_HRS_",wave)]]))
#       NA_tables$HRS_NA_prop[var,paste0("wave_",wave)]<- sum(is.na(hrs[[paste0(var,"_HRS_",wave)]]))/nrow(hrs)
#       NA_tables$HRS_N_prop[var,paste0("wave_",wave)] <- sum(!is.na(hrs[[paste0(var,"_HRS_",wave)]]))/nrow(hrs)
#     # If the variable wasn't recorded that wave, set to missing
#     }else if(!(paste0(var,"_HRS_",wave) %in% names(hrs))){
#       NA_tables$HRS_NA[var,paste0("wave_",wave)] <- NA
#       NA_tables$HRS_N[var,paste0("wave_",wave)] <- NA
#       NA_tables$HRS_NA_prop[var,paste0("wave_",wave)] <- NA
#       NA_tables$HRS_N_prop[var,paste0("wave_",wave)] <- NA
#     }
#   }
# }
# 
# NA_tables$HRS_NA <- NA_tables$HRS_NA %>% 
#   select(-matrix.nrow...length.c.instructions.distVars..instructions.exact_timevarying...)
# NA_tables$HRS_N <- NA_tables$HRS_N %>% 
#   select(-matrix.nrow...length.c.instructions.distVars..instructions.exact_timevarying...)
# NA_tables$HRS_NA_prop <- NA_tables$HRS_NA_prop %>% 
#   select(-matrix.nrow...length.c.instructions.distVars..instructions.exact_timevarying...)
# NA_tables$HRS_N_prop <- NA_tables$HRS_N_prop %>% 
#   select(-matrix.nrow...length.c.instructions.distVars..instructions.exact_timevarying...)
# 
# # Write-Out ----
# ## NA Tables ----
# write.csv(NA_tables$HRS_NA,
#           file.path("../../Alcohol_Cognition/NA_Tables/HRS_NA_table_initial.csv"))
# write.csv(NA_tables$HRS_N,
#           file.path("../../Alcohol_Cognition/NA_Tables/HRS_N_table_initial.csv"))
# write.csv(NA_tables$HRS_NA_prop,
#           file.path("../../Alcohol_Cognition/NA_Tables/HRS_NA_prop_initial.csv"))
# write.csv(NA_tables$HRS_N_prop,
#           file.path("../../Alcohol_Cognition/NA_Tables/HRS_N_prop_initial.csv"))


# Select Variables ----

# MAKE SURE TO INCLUDE THE NEWLY ADDED PHYSICAL MEASUREMENT VARIABLES
hrs <- hrs %>% select(
  CASE_ID_HRS_RA,
  FIRSTIW,
  firstwave_HRS,
  BIRTHDAY_HRS,
  BIRTHYEAR_HRS_RA,
  starts_with("INTERVIEW_BEGDT_"),
  starts_with("INTERVIEW_ENDDT_"),
  starts_with("AGEINTERVIEW_HRS"),
  starts_with("EDU_NEW"),
  starts_with("MOM"),
  starts_with("DAD"),
  FEMALE_HRS_RA,
  ETH_HRS_RA,
  RACE_HRS_RA,
  RACE_ETH_HRS_RA,
  USBIRTH_HRS_RA,
  RELIGION_HRS_RA,
  starts_with("MILITARY"),
  starts_with("INCOME_PP_LOG10_HRS_"),
  starts_with("HEIGHT"),
  starts_with("WEIGHT"),
  starts_with("BMI"),
  starts_with("CESD_NEW6PT"),
  starts_with("SYSTOLIC"),
  starts_with("DIASTOLIC"),
  starts_with("PULSE"),
  starts_with("DIAB"),
  starts_with("HYPERTENSION"),
  starts_with("CANCER"),
  starts_with("HEARTPROB"),
  starts_with("LIGHT_EXERCISE_"),
  starts_with("ALCOHOL_NOW"),
  starts_with("ALCOHOL_EVER"),
  starts_with("ALCOHOL_DRINKSPERMO"),
  starts_with("ALCOHOL_BINGE2"),
  starts_with("alc_"),
  starts_with("alcohol_dpw"),
  starts_with("alcohol_cat"),
  starts_with("alcohol_bingedrink"),
  starts_with("alcohol_bingecat"),
  starts_with("SMOKE"),
  starts_with("summed_recall_score_HRS"),
  starts_with("MARRIAGE", ignore.case=FALSE),
  grep("DISPUTE", names(HRS), value=TRUE),
  starts_with("GENHEALTH"),
  starts_with("VIG_EXERCISE"),
  starts_with("COG_"),
  starts_with("RATE_MEM_COG_"),
  starts_with("RATE_PAST_COG_"),
  starts_with("IMMED_RECALL_"),
  starts_with("DELAY_RECALL_"),
  starts_with("SERIAL_7_"),
  starts_with("CAGE"),
  starts_with("HbA1c"))

## Data sets ----
saveRDS(hrs, file.path("../../DP_HRS_Only/HRS_recoded.RDS"))
