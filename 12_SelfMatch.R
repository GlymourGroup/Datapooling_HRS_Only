# The goal of this script is to prep the data to see if we can identify 
# participant's younger self via datapooling

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
                     "Poor" = 0)
  ) 

## Count the number of of waves ----
wave_n <- hrs_tv_long %>%
  group_by(CASE_ID_HRS_RA) %>%
  filter(!is.na(INTERVIEW_BEGDT)) %>%
  summarise(nwaves_contributed = n())


# Split the cohort ----
# Both subsets should contain the matching wave: 2006

## Older ----

### Subset ----
hrs_old <- hrs_tv_long %>% filter(Year >= 2006) %>%
  rename(CASE_ID_OLD_RA = CASE_ID_HRS_RA)

# Can we carry forward information in the older subset?

### Standardize ----

for(distVar in instructions$distVars){
  hrs_old[[paste0(distVar,"_z")]] = scale(hrs_old[[distVar]])[,1]
}

## Younger  ----

### Subset ----
hrs_young <- hrs_tv_long %>% filter(Year <=2006)

### Carry forward ----
hrs_young <- hrs_young %>% 
  group_by(CASE_ID_HRS_RA) %>%
  fill(HEIGHT, WEIGHT, LIGHT_EXERCISE, VIG_EXERCISE,
       MARRIAGE,INCOME_PP_LOG10,
       CESD_NEW6PT,DIABETES,HYPERTENSION,CANCER,HEARTPROB,GENHEALTH,
       SMOKE_EVER,SMOKE_NOW,
       ALCOHOL_EVER,ALCOHOL_NOW,
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
summary(hrs_young %>% select(WEIGHT, HEIGHT, BMI))

summary(hrs_young %>%
  filter(WEIGHT > quantile(WEIGHT, 0.0001) | is.na(WEIGHT),
         HEIGHT > quantile(HEIGHT, 0.0001) | is.na(HEIGHT),
         HEIGHT < max(HEIGHT, na.rm=TRUE) | is.na(HEIGHT),
         BMI > min(BMI, na.rm=TRUE) | is.na(BMI),
         BMI < max(BMI, na.rm=TRUE) | is.na(BMI)) %>%
    select(WEIGHT, HEIGHT, BMI))

hrs_young <- hrs_young %>%
  filter(WEIGHT > quantile(WEIGHT, 0.0001) | is.na(WEIGHT),
         HEIGHT > quantile(HEIGHT, 0.0001) | is.na(HEIGHT),
         HEIGHT < max(HEIGHT, na.rm=TRUE) | is.na(HEIGHT),
         BMI > min(BMI, na.rm=TRUE) | is.na(BMI),
         BMI < max(BMI, na.rm=TRUE) | is.na(BMI)) 

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
                  DIABETES, HYPERTENSION, CANCER, HEARTPROB, GENHEALTH,
                  LIGHT_EXERCISE, VIG_EXERCISE, SMOKE_EVER,SMOKE_NOW,
                  ALCOHOL_EVER, ALCOHOL_NOW,
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
    values_from=c(AGEINTERVIEW ,
                  MARRIAGE ,INCOME_PP_LOG10,
                  HEIGHT,WEIGHT,BMI ,CESD_NEW6PT,
                  DIABETES, HYPERTENSION, CANCER, HEARTPROB, GENHEALTH,
                  LIGHT_EXERCISE, VIG_EXERCISE, SMOKE_EVER,SMOKE_NOW,
                  ALCOHOL_EVER, ALCOHOL_NOW,
                  SYSTOLIC_BP, DIASTOLIC_BP, PULSE
    ))

### Complete ----
HRS_young_wide <- inner_join(d$variables$invariant, HRS_young_wide)


# Save ----
saveRDS(hrs_tv_long,   "../../DP_HRS_Only/HRS_Full_OG.rds")
saveRDS(hrs_old,       "../../DP_HRS_Only/HRS_old.rds")
saveRDS(HRS_old_wide,  "../../DP_HRS_Only/HRS_old_wide.rds")
saveRDS(hrs_young,     "../../DP_HRS_Only/HRS_young.rds")
saveRDS(HRS_young_wide,"../../DP_HRS_Only/HRS_young_wide.rds")
