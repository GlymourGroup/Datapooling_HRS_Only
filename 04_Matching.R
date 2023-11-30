# Data Matching

# Options: Self-Rated Health, Body Weight, Depressive Symptoms

# Set-up ----

## Session ----
rm(list=ls())
gc()
library("tidyverse")

## Data ----
d <- list()

### Full (wide) Datasets ----
d$old <- readRDS("../../DP_HRS_Only/HRS_old_wide.rds")
d$young<-readRDS("../../DP_HRS_Only/HRS_young_wide.rds")

### Harmonized Datasets ----
d$old_long <- readRDS("../../DP_HRS_Only/HRS_old.rds")
d$young_long<-readRDS("../../DP_HRS_Only/HRS_young.rds")

### Instructions ----
instructions <- dget("Instructions/Instructions_02.R")

### Weights ----
weights <- readRDS("../../DP_HRS_Only/Weights.RDS")
weights <- weights$total$GENHEALTH_HRS_14

## QC ----
nTracker <- list()
nTracker$OLD_start <- length(unique(d$old_long$CASE_ID_OLD_RA))
nTracker$young_start<-length(unique(d$young_long$CASE_ID_HRS_RA))

# Base-line exact matching ----

## Get first interview attended ----
# Everyone's "baseline wave" 2008 or later
old_firstobs <- d$old_long %>% 
  filter(!is.na(INTERVIEW_BEGDT)) %>% 
  group_by(CASE_ID_OLD_RA) %>% 
  arrange(Year, .by_group = TRUE) %>% # unsure if needed, keep in case
  slice_head(n=1) %>%
  ungroup()
  
# create data-set with exact variables from instructions
old_timeinvariant <- d$old %>% 
  select(CASE_ID_OLD_RA, paste0(instructions$exact, "_HRS_RA"))

# Remove suffix from variables
old_timeinvariant <- old_timeinvariant %>% rename_with(~gsub("_HRS_RA", "", .x))


# Repeat for Young
young_timeinvariant <- d$young %>% 
  select(CASE_ID_HRS_RA, paste0(instructions$exact, "_HRS_RA"))

young_timeinvariant<- young_timeinvariant %>%
  rename_with(~ gsub("_HRS_RA", "", .x)) %>%
  rename(CASE_ID_HRS_RA = CASE_ID)

# Exact match based on: Sex, Race/Ethnicity, US Nativity, and Parent's Edu Ind
ID_LookUp_Exact_BL <- left_join(old_timeinvariant, young_timeinvariant,
                                relationship = "many-to-many") %>%
  select(CASE_ID_OLD_RA, CASE_ID_HRS_RA) # Subset to only their IDs

## Track sample size ----
nTracker$exact_match_baseline <- nrow(ID_LookUp_Exact_BL)



# Exact Match on TV Young Variables ----

## Select TV variables from OLD baseline ----
old_exact_timevarying <- old_firstobs %>% 
  select(CASE_ID_OLD_RA, Year,
         all_of(instructions$exact_timevarying))

# Join to ID_LookUp 
ID_LookUp_Exact_TV <- left_join(ID_LookUp_Exact_BL, old_exact_timevarying)

## Repeat for young ----
young_exact_timevarying <- d$young_long %>% 
  filter(Year == 2008) %>% # mediators only from 2008
  select(CASE_ID_HRS_RA,
         Year,
         all_of(instructions$exact_timevarying)) %>%
  # We do not want to match on year, so rename
  rename(Year_young = Year)

## ID young participant-wave:old pairs----
ID_LookUp_Exact_TV <- inner_join(ID_LookUp_Exact_TV, young_exact_timevarying,
                                 relationship = "many-to-many")
ID_LookUp_Exact_TV <- ID_LookUp_Exact_TV %>% 
  select(CASE_ID_OLD_RA, Year, CASE_ID_HRS_RA, Year_young)

## Track Sample Size ----
nTracker$exact_match_tv <- nrow(ID_LookUp_Exact_TV)



# Exact Match on Age ----

## Age "OLD" joined ----
ID_LookUp_Age <- left_join(ID_LookUp_Exact_TV, 
                           old_firstobs %>% 
                             select(CASE_ID_OLD_RA, Year,
                                    names(instructions$exact_HRS_timevarying)))

## Matched YOUNG age ----
ID_LookUp_Age<-left_join(ID_LookUp_Age, 
                         d$young_long %>% 
                           filter(Year == 2008) %>% 
                           select(CASE_ID_HRS_RA, Year,
                                  names(instructions$exact_HRS_timevarying)) %>%
                           rename(Year_young = Year,
                                  AGEINTERVIEW_young = AGEINTERVIEW),
                         relationship = "many-to-many")

for(var in names(instructions$exact_HRS_timevarying)){
  ID_LookUp_Age[,paste0(var,"_dif")] <- 
    abs(ID_LookUp_Age[,var] - ID_LookUp_Age[,paste0(var,"_young")])
  ID_LookUp_Age[["flag"]] <- ID_LookUp_Age[,paste0(var,"_dif")] <= 5
  ID_LookUp_Age <- ID_LookUp_Age %>% filter(flag == 1)
}

## Age-Restricted Pairs ----
ID_LookUp_Age <- ID_LookUp_Age %>% 
  select(CASE_ID_OLD_RA, CASE_ID_HRS_RA, Year, Year_young)

nTracker$exact_match_HRS_TV <- nrow(ID_LookUp_Age)



# Distance Matching ----

## Start with OLD ----
ids = unique(ID_LookUp_Age$CASE_ID_OLD_RA)

## Merge in exact matched counterparts ----
matched <- ID_LookUp_Age %>% select(CASE_ID_OLD_RA, CASE_ID_HRS_RA, Year_young)

## Append "Baseline" wave from OLD ----
matched <- left_join(matched, old_firstobs)

## Clean names to match  ----
matched <- matched %>% 
  rename_with(~ paste0(.x, "_OLD")) %>% 
  rename(CASE_ID_OLD_RA = CASE_ID_OLD_RA_OLD,
         CASE_ID_HRS_RA = CASE_ID_HRS_RA_OLD,
         Year_young = Year_young_OLD)

temp_young <- d$young_long %>% 
  filter(Year == 2008) %>%
  rename_with(~ paste0(.x, "_young")) %>% 
  rename(CASE_ID_HRS_RA = CASE_ID_HRS_RA_young)

## Merge in YOUNG data ----
matched <- left_join(matched, temp_young) 

## Calculate Distances ----
for(var in instructions$distVars){
  matched[[paste0(var,"_dist")]] = 
    abs(matched[[paste0(var,"_OLD")]] - matched[[paste0(var,"_young")]])
  matched[[paste0(var,"_z_dist")]] = 
    abs(matched[[paste0(var,"_z_OLD")]] - matched[[paste0(var,"_z_young")]])
  
  # For QC purposes, save out actual distances
  matched[[paste0(var,"_dist_QC")]] = 
    matched[[paste0(var,"_OLD")]] - matched[[paste0(var,"_young")]]
  matched[[paste0(var,"_z_dist_QC")]] = 
    matched[[paste0(var,"_z_OLD")]] - matched[[paste0(var,"_z_young")]]
}

## Calculate Weights ----
for(var in instructions$distVars){
  matched[[paste0(var,"_dist_weighted")]] = 
    1/weights[[var]]*matched[[paste0(var,"_dist")]]
  matched[[paste0(var,"_z_dist_weighted")]] = 
    1/weights[[var]]*matched[[paste0(var,"_z_dist")]]
}

## Total the distances ----

# Initiate Total Scores
matched$dist_z_weighted = 0
matched$dist_z_unweighted = 0
matched$dist_weighted = 0
matched$dist_unweighted = 0

for(var in instructions$distVars){
  matched$dist_z_weighted = 
    matched$dist_z_weighted + matched[[paste0(var,"_z_dist_weighted")]]
  matched$dist_z_unweighted=
    matched$dist_z_unweighted+matched[[paste0(var,"_z_dist")]]
  matched$dist_weighted   = 
    matched$dist_weighted + matched[[paste0(var,"_dist_weighted")]]
  matched$dist_unweighted = 
    matched$dist_unweighted+ matched[[paste0(var,"_dist")]]
}

# For some reason, can't directly manipulate weight of interest
# assign to `new` variable
matched$new <- matched[["dist_z_weighted"]]

## Find top matches ----
out <- matched %>%
  group_by(CASE_ID_OLD_RA, CASE_ID_HRS_RA) %>% 
  arrange(new, .by_group = TRUE) %>%  
  slice_head(n=1) %>%
  ungroup() %>% 
  group_by(CASE_ID_OLD_RA) %>%
  arrange(new, .by_group = TRUE) %>%  
  slice_head(n=10) %>%
  ungroup()

# Track Sample Size
nTracker$distance_match <- nrow(out)

# Quality Control and Trimming ----
QC_info <- read.csv("../../DP_HRS_Only/MatchQC_info.csv")
row.names(QC_info) <- QC_info$Variable
cut_off = 0.25

## Establish Cut-offs -----

# Loop through each possible distance variable
for(var in instructions$distVars){
  cat(paste0("Calculating Threshold: ", var, "\n"))
  
  # Calculate the threshold from the HRS wave joined dataset
  # Threshold is the cut_off * SD of the variable
  threshold = cut_off*sd(old_firstobs[[var]],na.rm=TRUE) 
  
  # If the variable is a score,
  if(QC_info[var,"type"] == "score"){
    # round to get the threshold
    threshold = ceiling(threshold)
  }
  QC_info[var,paste0("threshold_",cut_off)] <- threshold
}


# Get number of HRS participants pre-trim
nTracker$OLD_precutoff <- length(unique(out$CASE_ID_OLD_RA))

## Filtering ----
cat("Trimming observations\n")

# Create trim_tracker to track how many observations are dropped at each step
trim_tracker <- list()
trim_tracker[["pre-trim"]] <- nrow(out)

# create a list to store the data
data_qc <- out

# For each distance variable:
for(var in instructions$distVars){
  
  # Look up threshold
  threshold <- QC_info[var,paste0("threshold_",cut_off)]
  
  # Subset to distances less than the threshold
  data_qc <- data_qc %>% filter(.data[[paste0(var,"_dist")]] <= threshold)
  
  # Track change in sample size
  trim_tracker[[var]] <- nrow(data_qc)
}

# Track the sample size
nTracker[[paste0("threshold_",cut_off)]] <- nrow(data_qc)
# Quality of matches are poor at this point. 

# Save ----
saveRDS(out, file="../../DP_HRS_Only/Full_MatchedData.RDS") # why keep?
saveRDS(data_qc,file="../../DP_HRS_Only/QCd_MatchedData.RDS")
saveRDS(data_qc %>% select(CASE_ID_OLD_RA, Wave_OLD, 
                       CASE_ID_HRS_RA, Year_young, 
                       new),
        file = "../../DP_HRS_Only/QCd_MatchedData_IDs.RDS")
saveRDS(nTracker,file="../../DP_HRS_Only/nTracker.RDS")
saveRDS(trim_tracker,file="../../DP_HRS_Only/qc_step_tracker.RDS")
