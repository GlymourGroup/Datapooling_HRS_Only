# Set-up ----

## Session ----
rm(list=ls())
gc()
library("tidyverse")

## Data ----
d <- list()

# Full Data (prior to subset)
d$og_long <- readRDS("../../DP_HRS_Only/HRS_Full_OG.rds")
d$og_wide <- readRDS("../../DP_HRS_Only/HRS_wide.rds")

# Subset data
d$long <- readRDS("../../DP_HRS_Only/Long_Data.rds")
d$wide <- readRDS("../../DP_HRS_Only/Wide_Data.rds")

fullMatchedData <- readRDS("../../DP_HRS_Only/Full_MatchedData.RDS")

# Quick Look ----
table(fullMatchedData$DIABETES$Instructions_ALL3.R$DIAB_MATCH_VAR_OLD,
      fullMatchedData$DIABETES$Instructions_ALL3.R$DIAB_MATCH_VAR_young,
      useNA='ifany')

# Evaluate associations within each cohort ----

## X -> M in Young ----
lm(DIABETES_HRS_9 ~ BMI_HRS_2 + 
      AGEINTERVIEW_HRS_9 + FEMALE_HRS_RA + RACE_ETH_HRS_RA,
    data = d$wide$DIABETES_HRS_14_young)

## M -> Y In Old ----
lm(DIABETES_HRS_14 ~ BMI_HRS_9 + 
     AGEINTERVIEW_HRS_9 + FEMALE_HRS_RA + RACE_ETH_HRS_RA,
   data = d$wide$DIABETES_HRS_14_old)


# Evaluate accuracy of matching ----
young_all3_id <- fullMatchedData$DIABETES$Instructions_ALL3.R %>% 
  select(CASE_ID_HRS_RA) %>% # Pull out the young IDs
  unique() # Only the unique 

# From the full cohort, 
temp <- d$og_wide %>% 
  # pull just participants that were matched from the younger cohort
  filter(CASE_ID_HRS_RA %in% young_all3_id$CASE_ID_HRS_RA) %>% 
  # ID and Diabetes status in 2018
  select(CASE_ID_HRS_RA, DIABETES_HRS_14, BMI_HRS_2)

new <- left_join(fullMatchedData$DIABETES$Instructions_ALL3.R,
                 temp)

# Repeat for older
old_all3_id <- fullMatchedData$DIABETES$Instructions_ALL3.R %>% 
  select(CASE_ID_OLD_RA) %>% # Pull out the old IDs
  unique() # Only the unique 

# From the full cohort, 
temp_old <- d$og_wide %>% 
  # pull just participants that were matched from the younger cohort
  filter(CASE_ID_HRS_RA %in% old_all3_id$CASE_ID_OLD_RA) %>% 
  # ID and Diabetes status in 2018
  select(CASE_ID_HRS_RA, DIABETES_HRS_14, BMI_HRS_2) %>%
  rename(CASE_ID_OLD_RA = CASE_ID_HRS_RA,
         DIABETES_HRS_14_old = DIABETES_HRS_14,
         BMI_HRS_2_old = BMI_HRS_2)

new <- left_join(new,
              temp_old)


table(new$DIABETES_HRS_14,    # Observed 2018 outcome for the younger cohort
      new$DIABETES_HRS_14_old,# Observed 2018 outcome (used in regression) for older cohort
      new$DIAB_MATCH_VAR_OLD, # Matching 2008 variable
      useNA='ifany')
# Roughly 3000 pairs were discordant by 2018 when matching on 2008 values
# with ~11,000 + 4200 correctly matched 

# Does BMI differ among those who were correctly matched compared to those who weren't
new <- new %>% mutate(
  mismatch = case_when(DIABETES_HRS_14 != DIABETES_HRS_14_old ~ TRUE,
                       TRUE ~ FALSE)
)

# Among those who hadn't developed diabetes in 2008, are we doing anything additional to predict diabetes in 2018
new %>% group_by(mismatch) %>% summarise(mean_bmi = mean(BMI_HRS_2),
                                         mean_bmi_old = mean(BMI_HRS_2_old),
                                         diff_bmi = mean(BMI_HRS_2 - BMI_HRS_2_old))

new <- left_join()

# Find X->Y ----
## Longitudinal Old ----
glm(DIABETES_HRS_14_old ~ BMI_HRS_2_old + 
     AGEINTERVIEW_OLD,
   data = new,
   family = "binomial")

## Young ----
lm(DIABETES_HRS_14 ~ BMI_HRS_2 + 
     AGEINTERVIEW_young,
   data = new)

lm(DIABETES_HRS_14 ~ BMI_HRS_2 + 
     AGEINTERVIEW_HRS_14,
   data = d$og_wide)
