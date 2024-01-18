# Set-up ----

# Clear Environment
rm(list=ls())
gc()

# Libraries
library("tidyverse")

# Load Data ----
d <- list()

# Main Data set
d$HRS <- readRDS("../../DP_HRS_Only/HRS_recoded.RDS")

# Instruction set for each outcome
instructions <- list(
  GENHEALTH = dget("Instructions/GENHEALTH/Instructions_BOTH.R"),
  SYSTOLIC_BP=dget("Instructions/SYSTOLIC_BP/Instructions_BOTH.R"),
  DIABETES  = dget("Instructions/DIABETES/Instructions_BOTH.R") 
)
# the only instruction sets that actually differ are "BOTH" and "OUT"
# Don't  update DIABETES/Instructions_Both.R; DIAB_MATCH_VAR is not a dist var


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
         starts_with("PULSE"),
         DIAB_MATCH_VAR
         ) 


# Make Long ----
hrs_tv_long <- d$variables$timevarying %>%
  pivot_longer(cols = -c(CASE_ID_HRS_RA,
                         DIAB_MATCH_VAR),
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

## Carry Forward ----
hrs_tv_long <- hrs_tv_long %>% 
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

### Trim some extremes ----
# Check in with Maria about this
summary(hrs_tv_long %>% select(WEIGHT, HEIGHT, BMI))

summary(hrs_tv_long %>%
          filter(WEIGHT > quantile(WEIGHT, 0.0001) | is.na(WEIGHT),
                 HEIGHT > quantile(HEIGHT, 0.0001) | is.na(HEIGHT),
                 HEIGHT < max(HEIGHT, na.rm=TRUE) | is.na(HEIGHT),
                 BMI > min(BMI, na.rm=TRUE) | is.na(BMI),
                 BMI < max(BMI, na.rm=TRUE) | is.na(BMI)) %>%
          select(WEIGHT, HEIGHT, BMI))

hrs_tv_long <- hrs_tv_long %>%
  filter(WEIGHT > quantile(WEIGHT, 0.0001) | is.na(WEIGHT),
         HEIGHT > quantile(HEIGHT, 0.0001) | is.na(HEIGHT),
         HEIGHT < max(HEIGHT, na.rm=TRUE) | is.na(HEIGHT),
         BMI > min(BMI, na.rm=TRUE) | is.na(BMI),
         BMI < max(BMI, na.rm=TRUE) | is.na(BMI))

## Count the number of of waves ----
wave_n <- hrs_tv_long %>%
  group_by(CASE_ID_HRS_RA) %>%
  filter(!is.na(INTERVIEW_BEGDT)) %>%
  summarise(nwaves_contributed = n())

# By design of complete cases, nearly all participants would have full f/u
table(wave_n$nwaves_contributed, useNA='ifany')

hrs_tv_long <- left_join(hrs_tv_long, wave_n)

## Add HbA1c Data to long ----
hrs_tv_long <- left_join(hrs_tv_long,
                         d$HRS %>% select(CASE_ID_HRS_RA, HbA1c, HbA1c_YEAR))

# Split the cohort ----

# Participants should be present in: 1994, 2008, 2018
# having carried forward, we have many more complete cases

## Make the Data Wide ----
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
                  SYSTOLIC_BP, DIASTOLIC_BP, PULSE))

outcomes <- c("DIABETES_HRS_14", "GENHEALTH_HRS_14", "SYSTOLIC_BP_HRS_14")
# For each outcome
cc <- list()
d_long<-list()
d_wide<-list()
for(out in outcomes){
  
  cc[[out]] <- complete.cases(hrs_tv_wide %>% 
                                select(CASE_ID_HRS_RA, BMI_HRS_2, all_of(out)))
  
  ## Filter to complete cases ----
  temp <- hrs_tv_wide[cc[[out]],]
  
  # Randomly assign participants to the older and younger cohorts
  set.seed(123)
  
  cohort_assignment <-  temp %>% 
    select(CASE_ID_HRS_RA) %>%
    mutate(cohort = rbinom(nrow(.), 1, .5))
  
  # Merge in the cohort assignment
  d_long[[out]] <- left_join(hrs_tv_long, cohort_assignment)
  
  d_wide[[out]] <- inner_join(d$variables$invariant, temp) 
  
  ## Older ----
  
  ### Subset ----
  d_long[[paste0(out,"_old")]] <- d_long[[out]] %>% 
    filter(Year >= 2008, cohort == 1) %>%
    rename(CASE_ID_OLD_RA = CASE_ID_HRS_RA)
  
  ### Standardize ----
  instruction_set <- gsub("_HRS_14", "", out)
  
  for(distVar in instructions[[instruction_set]]$distVars){
    d_long[[paste0(out,"_old")]][[paste0(distVar,"_z")]] = scale(d_long[[paste0(out,"_old")]][[distVar]])[,1]
  }
  
  ## Younger  ----
  
  ### Subset ----
  d_long[[paste0(out,"_young")]] <- d_long[[out]] %>% 
    filter(Year <=2008, cohort == 0)
  
  ### Standardize  ----
  
  for(i in instructions[[instruction_set]]$distVars){
    d_long[[paste0(out,"_young")]][[paste0(i,"_z")]] = 
      (d_long[[paste0(out,"_young")]][[i]] - 
         mean(d_long[[paste0(out,"_old")]][[i]],na.rm=T))/sd(d_long[[paste0(out,"_old")]][[i]],na.rm=T)
  }
  
  
  # Recreate Wide Datasets ----
  
  ## OLDER ----
  
  ### Spread ----
  d_wide[[paste0(out,"_old")]] <- d_long[[paste0(out,"_old")]] %>% 
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
  d_wide[[paste0(out,"_old")]] <- inner_join(
    d$variables$invariant, 
    d_wide[[paste0(out,"_old")]],
    join_by(CASE_ID_HRS_RA == CASE_ID_OLD_RA)) %>%
    rename(CASE_ID_OLD_RA = CASE_ID_HRS_RA)
  
  
  ## younger ----

  ### Spread ----
  d_wide[[paste0(out,"_young")]] <- d_long[[paste0(out,"_young")]] %>% 
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
  d_wide[[paste0(out,"_young")]] <- inner_join(
    d$variables$invariant, d_wide[[paste0(out,"_young")]]
    )
  
}







# Save ----
# Full Datasets 
saveRDS(hrs_tv_long,   "../../DP_HRS_Only/HRS_Full_OG.rds") # Original -> long
saveRDS(hrs_tv_wide,   "../../DP_HRS_Only/HRS_wide.rds") # Long -> wide

# Subset by outcome
saveRDS(d_long, "../../DP_HRS_Only/Long_Data.rds")
saveRDS(d_wide, "../../DP_HRS_Only/Wide_Data.rds")
# saveRDS(hrs_old,       "../../DP_HRS_Only/HRS_old.rds")
# saveRDS(HRS_old_wide,  "../../DP_HRS_Only/HRS_old_wide.rds")
# saveRDS(hrs_young,     "../../DP_HRS_Only/HRS_young.rds")
# saveRDS(HRS_young_wide,"../../DP_HRS_Only/HRS_young_wide.rds")
