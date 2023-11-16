# Set-up ----

# Clear Environment
rm(list=ls())
gc()

# Libraries
library("tidyverse")

# Load Data ----
d <- list()
d$HRS <- readRDS("../../DP_HRS_Only/HRS_recoded.RDS")
instructions <- dget("Instructions/Instructions_00.R")

# Show missingness in mediator and outcome
summary(d$HRS %>% select(SYSTOLIC_BP_HRS_9, SYSTOLIC_BP_HRS_14,
                         GENHEALTH_HRS_9, GENHEALTH_HRS_14, 
                         DIABETES_HRS_9, DIABETES_HRS_14))

# Missing Data ----

## Manually carry forward ----
## Physical Measurements are taken every other wave, 
## carry info forward from previous wave

d$HRS <- d$HRS %>% 
  mutate(
    # When the matching year is missing, use the year before
    SYSTOLIC_BP_HRS_9 = case_when(is.na(SYSTOLIC_BP_HRS_9) ~ SYSTOLIC_BP_HRS_8,
                                  TRUE ~ SYSTOLIC_BP_HRS_9),
    GENHEALTH_HRS_9   = case_when(is.na(GENHEALTH_HRS_9) ~ GENHEALTH_HRS_8,
                                  TRUE ~ GENHEALTH_HRS_9),
    # similarly, when the matching year is missing, use the year before
    SYSTOLIC_BP_HRS_14= case_when(is.na(SYSTOLIC_BP_HRS_14)~SYSTOLIC_BP_HRS_13,
                                  TRUE ~ SYSTOLIC_BP_HRS_14),
    GENHEALTH_HRS_14  = case_when(is.na(GENHEALTH_HRS_14) ~ GENHEALTH_HRS_13,
                                  TRUE ~ GENHEALTH_HRS_14)
    )

## Complete Cases ----
# MUST have the exposure AND outcome, not necessarily mediators
# Find complete cases with regards to relevant data:
cc <- d$HRS %>% select(CASE_ID_HRS_RA,  ends_with("_RA"),
                       BMI_HRS_2,
                       SYSTOLIC_BP_HRS_14, GENHEALTH_HRS_14, DIABETES_HRS_14,
                       -ETH_HRS_RA,-RACE_HRS_RA,
                       # -starts_with("COG")
)
names(cc) # to see if it has the co-variates of interest
table(complete.cases(cc)) 

# very few complete cases overall, but we can check by outcome
# Diabetes:
table(complete.cases(cc %>% select(-SYSTOLIC_BP_HRS_14, -GENHEALTH_HRS_14)))
# Systolic BP:
table(complete.cases(cc %>% select(-GENHEALTH_HRS_14, -DIABETES_HRS_14)))
# Gen Health
table(complete.cases(cc %>% select(-DIABETES_HRS_14, -SYSTOLIC_BP_HRS_14)))

# We should check all, but for proof of concept, let's start with one
cc <- cc[complete.cases(cc %>% select(-DIABETES_HRS_14, -SYSTOLIC_BP_HRS_14)),]
d$HRS <- d$HRS %>% filter(CASE_ID_HRS_RA %in% cc$CASE_ID_HRS_RA)

# Categorize matching vars ----

## Invariant ----
d$variables$invariant <- d$HRS %>% 
  select(CASE_ID_HRS_RA, BIRTHYEAR_HRS_RA,
         FEMALE_HRS_RA, USBIRTH_HRS_RA, RACE_ETH_HRS_RA,
         EDU_NEW_HRS_RA,
         DAD_EDU_HRS_RA, MOM_EDU_HRS_RA,
         RELIGION_HRS_RA, MILITARY_HRS_RA)

## Time-varying ----
d$variables$timevarying <- d$HRS %>%
  select(CASE_ID_HRS_RA,
         starts_with("INTERVIEW_BEGDT"),
         starts_with("AGEINTERVIEW"),
         starts_with("MARRIAGE"),
         starts_with("INCOME_PP_LOG10"),
         starts_with("HEIGHT_"),
         starts_with("WEIGHT_"),
         starts_with("BMI_"),
         -starts_with("BMI_REPORTED"),
         starts_with("CESD_NEW6PT"),
         starts_with("DIABETES"),
         starts_with("HYPERTENSION"),
         starts_with("CANCER"),
         starts_with("HEARTPROB"),
         starts_with("GENHEALTH"),
         starts_with("LIGHT_EXERCISE"),
         starts_with("VIG_EXERCISE"),
         starts_with('SMOKE_EVER'),
         starts_with("SMOKE_NOW"),
         starts_with("ALCOHOL_NOW"),
         starts_with("ALCOHOL_EVER"),
         starts_with("SYSTOLIC"),
         starts_with("DIASTOLIC"),
         starts_with("PULSE")
         ) 


# Make Long ----
hrs_tv_long <- d$variables$timevarying %>%
  pivot_longer(cols = -c(CASE_ID_HRS_RA),
               names_to = c(".value", "Wave"),
               names_sep = "_HRS_")


## Data Cleaning ----
hrs_tv_long <- hrs_tv_long %>%
  mutate(
    # Each wave corresponds to a year, recode for ease of comparison
    # (93 & 94) = wave 2; (95 & 96) = wave 3
    Year = recode(Wave,
                  `1` = 1992,`2` = 1994,`3` = 1996,`4` = 1998,
                  `5` = 2000,`6` = 2002,`7` = 2004,`8` = 2006,
                  `9` = 2008,`10`= 2010,`11`= 2012,`12`= 2014,
                  `13`= 2016,`14`= 2018,`15`= 2020),
    # Did not transform height and weight to avoid potential coding errors
    # HEIGHT = HEIGHT*39.3701, # converts m to in
    # WEIGHT = WEIGHT*2.20462, # converts kg to lbs
    Wave = as.double(Wave),
    
    GENHEALTH=recode(GENHEALTH,
                     "Excellent" = 4,
                     "Very Good" = 3,
                     "Good" = 2,
                     "Fair" = 1,
                     "Poor" = 0),
    
    SMK_STATUS = case_when(SMOKE_EVER == 0 ~ "Never",
                           SMOKE_EVER == 1 & SMOKE_NOW == 1 ~ "Current",
                           SMOKE_EVER == 1 ~ "Past"),
    
    ALC_STATUS = case_when(ALCOHOL_EVER == 0 ~ "Never",
                           ALCOHOL_EVER == 1 & ALCOHOL_NOW == 1 ~ "Current",
                           ALCOHOL_EVER == 1 ~ "Past")
  ) %>% select(-c(SMOKE_EVER, ALCOHOL_EVER, SMOKE_NOW, ALCOHOL_NOW, 
                  CANCER, LIGHT_EXERCISE))

## Count the number of of waves ----
wave_n <- hrs_tv_long %>%
  group_by(CASE_ID_HRS_RA) %>%
  filter(!is.na(INTERVIEW_BEGDT)) %>%
  summarise(nwaves_contributed = n())

# By design of complete cases, nearly all participants would have full f/u
table(wave_n$nwaves_contributed, useNA='ifany')

hrs_tv_long <- left_join(hrs_tv_long, wave_n)

# Split the cohort ----
# Both subsets should contain the matching wave: 2008
# Participants should be present in all three waves: 1994, 2008, 2018

# Randomly assign participants to the older and younger cohorts
set.seed(123)

cohort_assignment <-  d$HRS %>% 
  select(CASE_ID_HRS_RA) %>%
  mutate(cohort = rbinom(nrow(.), 1, .5))

# Merge in the cohort assignment
hrs_tv_long <- left_join(hrs_tv_long, cohort_assignment)

hrs_tv_wide <- hrs_tv_long %>% 
  select(-Year) %>%
  pivot_wider(
    names_from = Wave,
    names_sep  = "_HRS_",
    values_from=c(INTERVIEW_BEGDT, AGEINTERVIEW,
                  MARRIAGE ,INCOME_PP_LOG10,
                  HEIGHT,WEIGHT,BMI ,CESD_NEW6PT,
                  DIABETES, HYPERTENSION, HEARTPROB, GENHEALTH, #CANCER, 
                  VIG_EXERCISE, #LIGHT_EXERCISE, 
                  SMK_STATUS, #SMOKE_EVER,SMOKE_NOW,
                  ALC_STATUS, #ALCOHOL_EVER, ALCOHOL_NOW,
                  SYSTOLIC_BP, DIASTOLIC_BP, PULSE
    ))

hrs_tv_wide <- inner_join(d$variables$invariant, hrs_tv_wide) 

## Older ----

### Subset ----
hrs_old <- hrs_tv_long %>% 
  filter(Year >= 2008, cohort == 1) %>%
  rename(CASE_ID_OLD_RA = CASE_ID_HRS_RA)

# Can we carry forward information in the older subset?

### Standardize ----

for(distVar in instructions$distVars){
  hrs_old[[paste0(distVar,"_z")]] = scale(hrs_old[[distVar]])[,1]
}

## Younger  ----

### Subset ----
hrs_young <- hrs_tv_long %>% 
  filter(Year <=2008, cohort == 0)

### Carry forward ----
hrs_young <- hrs_young %>% 
  group_by(CASE_ID_HRS_RA) %>%
  fill(HEIGHT, WEIGHT, #LIGHT_EXERCISE, 
       VIG_EXERCISE,
       MARRIAGE,INCOME_PP_LOG10,
       CESD_NEW6PT,DIABETES,HYPERTENSION,HEARTPROB,GENHEALTH,#CANCER,
       SMK_STATUS, #SMOKE_EVER,SMOKE_NOW,
       ALC_STATUS, #ALCOHOL_EVER,ALCOHOL_NOW,
       SYSTOLIC_BP, DIASTOLIC_BP, PULSE) %>%
  ungroup() %>%
  # re-calculate BMI in HRS
  mutate(BMI = (WEIGHT/(HEIGHT^2))) %>%
  fill(BMI) %>%
  group_by(CASE_ID_HRS_RA) %>%
  fill(BMI) %>%
  ungroup() 

### Trim some extremes? ----
# Check in with Maria about this
summary(hrs_young %>% select(WEIGHT, HEIGHT, BMI))

summary(hrs_young %>%
  filter(WEIGHT > quantile(WEIGHT, 0.0001) | is.na(WEIGHT),
         HEIGHT > quantile(HEIGHT, 0.0001) | is.na(HEIGHT),
         HEIGHT < max(HEIGHT, na.rm=TRUE) | is.na(HEIGHT),
         BMI > min(BMI, na.rm=TRUE) | is.na(BMI),
         BMI < max(BMI, na.rm=TRUE) | is.na(BMI)) %>%
    select(WEIGHT, HEIGHT, BMI))

# hrs_young <- hrs_young %>%
#   filter(WEIGHT > quantile(WEIGHT, 0.0001) | is.na(WEIGHT),
#          HEIGHT > quantile(HEIGHT, 0.0001) | is.na(HEIGHT),
#          HEIGHT < max(HEIGHT, na.rm=TRUE) | is.na(HEIGHT),
#          BMI > min(BMI, na.rm=TRUE) | is.na(BMI),
#          BMI < max(BMI, na.rm=TRUE) | is.na(BMI)) 

### Standardize  ----


for(i in instructions$distVars){
  hrs_young[[paste0(i,"_z")]] = 
    (hrs_young[[i]]-mean(hrs_old[[i]],na.rm=T))/sd(hrs_old[[i]],na.rm=T)
}


# Recreate Wide Datasets ----

## OLDER ----

### Spread ----
HRS_old_wide <- hrs_old %>% 
  select(-Year, -ends_with("_z")) %>%
  pivot_wider(
    names_from = Wave,
    names_sep  = "_HRS_",
    values_from=c(INTERVIEW_BEGDT, AGEINTERVIEW,
                  MARRIAGE ,INCOME_PP_LOG10,
                  HEIGHT,WEIGHT,BMI ,CESD_NEW6PT,
                  DIABETES, HYPERTENSION, HEARTPROB, GENHEALTH, #CANCER, 
                  VIG_EXERCISE, #LIGHT_EXERCISE, 
                  SMK_STATUS, #SMOKE_EVER,SMOKE_NOW,
                  ALC_STATUS, #ALCOHOL_EVER, ALCOHOL_NOW,
                  SYSTOLIC_BP, DIASTOLIC_BP, PULSE
    ))

### Join Invariant ----
HRS_old_wide <- inner_join(d$variables$invariant, HRS_old_wide,
                           join_by(CASE_ID_HRS_RA == CASE_ID_OLD_RA)) %>%
  rename(CASE_ID_OLD_RA = CASE_ID_HRS_RA)




## younger ----


### Spread ----
HRS_young_wide <- hrs_young %>% 
  select(-INTERVIEW_BEGDT, -Year,
         -ends_with("_z")) %>%
  pivot_wider(
    names_from = Wave,
    names_sep  = "_HRS_",
    values_from=c(AGEINTERVIEW,
                  MARRIAGE ,INCOME_PP_LOG10,
                  HEIGHT,WEIGHT,BMI ,CESD_NEW6PT,
                  DIABETES, HYPERTENSION, HEARTPROB, GENHEALTH, #CANCER, 
                  VIG_EXERCISE, #LIGHT_EXERCISE, 
                  SMK_STATUS, #SMOKE_EVER,SMOKE_NOW,
                  ALC_STATUS, #ALCOHOL_EVER, ALCOHOL_NOW,
                  SYSTOLIC_BP, DIASTOLIC_BP, PULSE
    ))

### Complete ----
HRS_young_wide <- inner_join(d$variables$invariant, HRS_young_wide)


# Save ----
saveRDS(hrs_tv_wide,   "../../DP_HRS_Only/HRS_wide.rds")
saveRDS(hrs_tv_long,   "../../DP_HRS_Only/HRS_Full_OG.rds")
saveRDS(hrs_old,       "../../DP_HRS_Only/HRS_old.rds")
saveRDS(HRS_old_wide,  "../../DP_HRS_Only/HRS_old_wide.rds")
saveRDS(hrs_young,     "../../DP_HRS_Only/HRS_young.rds")
saveRDS(HRS_young_wide,"../../DP_HRS_Only/HRS_young_wide.rds")
