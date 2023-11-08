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

# Data are harmonized because they come from the same source
 

# Variables Lists ----
# Create lists to store variables of interest

## Invariant ----
# Invariant variables: Case-ID, Birth year, Sex (Female), Birth Country, Race, Ethnicity

# HRS
d$variables$invariant <- d$HRS %>% 
  select(CASE_ID_HRS_RA,
         BIRTHYEAR_HRS_RA,
         FEMALE_HRS_RA, 
         USBIRTH_HRS_RA,
         RACE_ETH_HRS_RA,
         MOM_EDU_IND_HRS_RA, DAD_EDU_IND_HRS_RA)

## Time-varying ----
# Age at time of interview, Height, Weight, BMI, Self-Reported Health, Physical Activity

d$variables$timevarying <- d$HRS %>%
  select(CASE_ID_HRS_RA,
         starts_with("INTERVIEW_BEGDT"),
         starts_with("AGEINTERVIEW"),
         starts_with("EDU_NEW_HRS_RA"),
         starts_with('DAD_EDU_HRS_RA'), # not time varying
         starts_with("MOM_EDU_HRS_RA"), # not time varying
         starts_with("MARRIAGE"),
         starts_with("MILITARY_"),   # not time varying
         starts_with("INCOME_PP_LOG10"),
         starts_with("RELIGION"),
         starts_with("HEIGHT_"),
         starts_with("WEIGHT_"),
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

# Long Data ----
hrs_tv_long <- d$variables$timevarying %>%
  pivot_longer(cols = -c(CASE_ID_HRS_RA,
                        EDU_NEW_HRS_RA,
                        DAD_EDU_HRS_RA, MOM_EDU_HRS_RA,
                        RELIGION_HRS_RA,MILITARY_HRS_RA),
               names_to = c(".value", "Wave"),
               names_sep = "_HRS_")

hrs_tv_long <- hrs_tv_long %>%
  mutate(
    # Each wave corresponds to a year, recode for ease of comparison
    # CORE files had years as suffixes, recoded to avoid error
    # (93 & 94) = wave 2; (95 & 96) = wave 3
    Year = recode(Wave,
                  `1` = 1992,`2` = 1994,`3` = 1996,`4` = 1998,
                  `5` = 2000,`6` = 2002,`7` = 2004,`8` = 2006,
                  `9` = 2008,`10`= 2010,`11`= 2012,`12`= 2014,
                  `13`= 2016,`14`= 2018,`15`= 2020),
    # Make height and weight in same units as NLSY
    HEIGHT = HEIGHT*39.3701, # converts m to in
    WEIGHT = WEIGHT*2.20462, # converts kg to lbs
    Wave = as.double(Wave)
  ) 


# Carry forward ----
hrs_tv_long <- hrs_tv_long %>% 
  # Rename variables 
  rename(RELIGIONCURRENT = RELIGION_HRS_RA,
         EDU_NEW = EDU_NEW_HRS_RA,
         DAD_EDU = DAD_EDU_HRS_RA,
         MOM_EDU = MOM_EDU_HRS_RA,
         MILITARY = MILITARY_HRS_RA) %>%
  group_by(CASE_ID_HRS_RA) %>%
  fill(HEIGHT, WEIGHT, LIGHT_EXERCISE, VIG_EXERCISE, GENHEALTH,
       EDU_NEW,
       MARRIAGE,MILITARY,INCOME_PP_LOG10,RELIGIONCURRENT,
       CESD_NEW6PT,DIABETES,HYPERTENSION,CANCER,HEARTPROB,GENHEALTH,
       SMOKE_EVER,SMOKE_NOW,
       ALCOHOL_EVER,ALCOHOL_NOW,
       SYSTOLIC_BP, DIASTOLIC_BP, PULSE) %>%
  ungroup() %>%
  # re-calculate BMI in HRS
  mutate(BMI = (WEIGHT/(HEIGHT^2))*703) %>%
  fill(BMI) %>%
  group_by(CASE_ID_HRS_RA) %>%
  fill(BMI) %>%
  ungroup() 

# Trim some extremes ----
# Check in with Maria about this
# summary(hrs_tv_long %>% select(WEIGHT, HEIGHT, BMI, 
#                                SYSTOLIC_BP, DIASTOLIC_BP, PULSE))
# 
# summary(hrs_tv_long %>%
#   filter(WEIGHT > quantile(WEIGHT, 0.0001) | is.na(WEIGHT),
#          HEIGHT > quantile(HEIGHT, 0.0001) | is.na(HEIGHT),
#          HEIGHT < max(HEIGHT, na.rm=TRUE) | is.na(HEIGHT),
#          BMI > min(BMI, na.rm=TRUE) | is.na(BMI),
#          BMI < max(BMI, na.rm=TRUE) | is.na(BMI)) %>% 
#     select(WEIGHT, HEIGHT, BMI))

hrs_tv_long <- hrs_tv_long %>%
  filter(WEIGHT > quantile(WEIGHT, 0.0001) | is.na(WEIGHT),
         HEIGHT > quantile(HEIGHT, 0.0001) | is.na(HEIGHT),
         HEIGHT < max(HEIGHT, na.rm=TRUE) | is.na(HEIGHT),
         BMI > min(BMI, na.rm=TRUE) | is.na(BMI),
         BMI < max(BMI, na.rm=TRUE) | is.na(BMI)) 

# Count the number of of waves contributed by each HRS Participant
wave_n <- hrs_tv_long %>%
  group_by(CASE_ID_HRS_RA) %>%
  filter(!is.na(INTERVIEW_BEGDT)) %>%
  summarise(nwaves_contributed = n())
table(wave_n$nwaves_contributed)

# join back into long dataset. 
hrs_tv_long <- left_join(hrs_tv_long, wave_n)


# Some Data Cleaning ----
## Factor Variables ----
# first make categorical variables as ordinal for ease of analysis
hrs_tv_long <- hrs_tv_long %>% mutate(
  GENHEALTH=recode(GENHEALTH,
                       "Excellent" = 0,
                       "Very Good" = 1,
                       "Good" = 2,
                       "Fair" = 3,
                       "Poor" = 4))

# Older Dataset ----

## Subset ----
hrs_old <- hrs_tv_long %>% filter(Year > 2006) %>%
  rename(CASE_ID_OLD_RA = CASE_ID_HRS_RA)

## Spread ----
HRS_old_wide <- hrs_old %>% 
  select(-INTERVIEW_BEGDT, -Year, -nwaves_contributed,
         -ends_with("_z")) %>%
  pivot_wider(
    names_from = Wave,
    names_sep  = "_HRS_",
    values_from=c(AGEINTERVIEW ,
                  EDU_NEW,
                  DAD_EDU ,MOM_EDU,
                  MILITARY ,RELIGIONCURRENT, MARRIAGE ,INCOME_PP_LOG10,
                  HEIGHT,WEIGHT,BMI ,CESD_NEW6PT,
                  DIABETES, HYPERTENSION, CANCER, HEARTPROB, GENHEALTH,
                  LIGHT_EXERCISE, VIG_EXERCISE, SMOKE_EVER,SMOKE_NOW,
                  ALCOHOL_EVER, ALCOHOL_NOW,
                  SYSTOLIC_BP, DIASTOLIC_BP, PULSE
                  ))

## Standardize Variables ----
hrs_old <- hrs_old %>% mutate(
  AGEINTERVIEW_z = scale(AGEINTERVIEW)[,1],
  Year_z = scale(Year)[,1],
  HEIGHT_z=scale(HEIGHT)[,1],
  WEIGHT_z=scale(WEIGHT)[,1],
  BMI_z = scale(BMI)[,1],
  GENHEALTH_z=scale(GENHEALTH)[,1],
  MOM_EDU_z = scale(MOM_EDU)[,1],
  DAD_EDU_z = scale(DAD_EDU)[,1],
  INCOME_PP_LOG10_z = scale(INCOME_PP_LOG10)[,1],
  CESD_NEW6PT_z = scale(CESD_NEW6PT)[,1],
  
  SYSTOLIC_BP_z = scale(SYSTOLIC_BP)[,1],
  DIASTOLIC_BP_z= scale(DIASTOLIC_BP)[,1],
  PULSE_z = scale(PULSE)[,1]
)

## Complete ----
HRS_old_wide <- inner_join(d$variables$invariant, HRS_old_wide,
                           join_by(CASE_ID_HRS_RA == CASE_ID_OLD_RA)) %>%
  rename(CASE_ID_OLD_RA = CASE_ID_HRS_RA)

# Younger Dataset ----
## Subset ----
hrs_young <- hrs_tv_long %>% filter(Year <=2006)

## Spread ----
HRS_young_wide <- hrs_young %>% 
  select(-INTERVIEW_BEGDT, -Year, -nwaves_contributed,
         -ends_with("_z")) %>%
  pivot_wider(
    names_from = Wave,
    names_sep  = "_HRS_",
    values_from=c(AGEINTERVIEW ,
                  EDU_NEW,
                  DAD_EDU ,MOM_EDU,
                  MILITARY ,RELIGIONCURRENT, MARRIAGE ,INCOME_PP_LOG10,
                  HEIGHT,WEIGHT,BMI ,CESD_NEW6PT,
                  DIABETES, HYPERTENSION, CANCER, HEARTPROB, GENHEALTH,
                  LIGHT_EXERCISE, VIG_EXERCISE, SMOKE_EVER,SMOKE_NOW,
                  ALCOHOL_EVER, ALCOHOL_NOW,
                  SYSTOLIC_BP, DIASTOLIC_BP, PULSE
    ))

## Standardize to old ----
hrs_young <- hrs_young %>% mutate(
  AGEINTERVIEW_z = (AGEINTERVIEW - mean(hrs_old$AGEINTERVIEW, na.rm=TRUE))/sd(hrs_old$AGEINTERVIEW,na.rm=TRUE),
  Year_z         = (Year - mean(hrs_old$Year, na.rm=TRUE))/sd(hrs_old$Year,na.rm=TRUE),
  HEIGHT_z       = (HEIGHT - mean(hrs_old$HEIGHT, na.rm=TRUE))/sd(hrs_old$HEIGHT,na.rm=TRUE),
  WEIGHT_z       = (WEIGHT - mean(hrs_old$WEIGHT, na.rm=TRUE))/sd(hrs_old$WEIGHT,na.rm=TRUE),
  BMI_z          = (BMI - mean(hrs_old$BMI, na.rm=TRUE))/sd(hrs_old$BMI,na.rm=TRUE),
  GENHEALTH_z    = (GENHEALTH - mean(hrs_old$GENHEALTH, na.rm=TRUE))/sd(hrs_old$GENHEALTH,na.rm=TRUE),
  MOM_EDU_z      = (MOM_EDU - mean(hrs_old$MOM_EDU, na.rm=TRUE))/sd(hrs_old$MOM_EDU,na.rm=TRUE),
  DAD_EDU_z      = (DAD_EDU - mean(hrs_old$DAD_EDU, na.rm=TRUE))/sd(hrs_old$DAD_EDU,na.rm=TRUE),
  INCOME_PP_LOG10_z = (INCOME_PP_LOG10 - mean(hrs_old$INCOME_PP_LOG10, na.rm=TRUE))/sd(hrs_old$INCOME_PP_LOG10,na.rm=TRUE),
  CESD_NEW6PT_z  = (CESD_NEW6PT - mean(hrs_old$CESD_NEW6PT, na.rm=TRUE))/sd(hrs_old$CESD_NEW6PT,na.rm=TRUE),
  
  SYSTOLIC_BP_z  = (SYSTOLIC_BP - mean(hrs_old$SYSTOLIC_BP, na.rm=TRUE))/sd(hrs_old$SYSTOLIC_BP,na.rm=TRUE),
  DIASTOLIC_BP_z = (DIASTOLIC_BP - mean(hrs_old$DIASTOLIC_BP, na.rm=TRUE))/sd(hrs_old$DIASTOLIC_BP,na.rm=TRUE),
  PULSE_z        = (PULSE - mean(hrs_old$PULSE, na.rm=TRUE))/sd(hrs_old$PULSE,na.rm=TRUE)
)

## Complete ----
HRS_young_wide <- inner_join(d$variables$invariant, HRS_young_wide)

# Save ----
saveRDS(hrs_tv_long,   "../../DP_HRS_Only/HRS_Full_OG.rds")
saveRDS(hrs_old,       "../../DP_HRS_Only/HRS_old.rds")
saveRDS(HRS_old_wide,  "../../DP_HRS_Only/HRS_old_wide.rds")
saveRDS(hrs_young,     "../../DP_HRS_Only/HRS_young.rds")
saveRDS(HRS_young_wide,"../../DP_HRS_Only/HRS_young_wide.rds")
