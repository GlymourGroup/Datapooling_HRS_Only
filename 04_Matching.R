# Data Matching

# Possible outcomes: Self-Rated Health, SBP, DM

# Instruction Sets Key:
  # 00: Both Mediators matched on
  # 01: Only Matches on Outcome
  # 02: Only Matches on Exposure
  # 03: Matches on neither mediator, just: sex, race, age

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

### Instructions ----

outcomes <- c("DIABETES", "GENHEALTH", "SYSTOLIC_BP")

# Import different instruction sets
instruction_sets <- list()
for(i in outcomes){
  for(ii in list.files(paste0("Instructions/",i))){
    instruction_sets[[i]][[ii]] <- 
      dget(paste0("Instructions/",i,"/",ii))
  }
}

### Weights ----
weights <- readRDS("../../DP_HRS_Only/Weights.RDS")
weights <- weights$scaled_exp

# Matching ----

# Initiate list to store results
out <- list()
data_qc <- list()
trim_tracker <- list()
nTracker <- list()

for(outcome in outcomes){
  out_hrs_14 <- paste0(outcome,"_HRS_14")
  
  ## Sample Size Tracker ----
  nTracker[[outcome]] <- 
    data.frame(matrix(ncol=length(instruction_sets[[outcome]]), nrow=0))
  names(nTracker[[outcome]]) <- names(instruction_sets[[outcome]])
  
  nTracker[[outcome]]["OLD_start",] <- 
    length(unique(d[["long"]][[paste0(out_hrs_14,"_old")]][["CASE_ID_OLD_RA"]]))
  nTracker[[outcome]]["young_start",] <-
    length(unique(d[["long"]][[paste0(out_hrs_14,"_young")]][['CASE_ID_HRS_RA']]))
  
  # Loop through each instruction set
  for(instructions in names(instruction_sets[[outcome]])){
    
    # Base-line exact matching ----
    
    ## Get first interview attended ----
    # Everyone's "baseline wave" should be 2008 by selection restriction in 01
    old_firstobs <- d$long[[paste0(out_hrs_14,"_old")]] %>% 
      filter(!is.na(INTERVIEW_BEGDT)) %>% 
      group_by(CASE_ID_OLD_RA) %>% 
      arrange(Year, .by_group = TRUE) %>% # unsure if needed, keep in case
      slice_head(n=1) %>%
      ungroup()
    
    # create data-set with exact variables from instructions
    old_timeinvariant <- d$wide[[paste0(out_hrs_14,"_old")]] %>% 
      select(CASE_ID_OLD_RA, 
             paste0(instruction_sets[[outcome]][[instructions]]$exact,"_HRS_RA"))
    
    # Remove suffix from variables
    old_timeinvariant <- old_timeinvariant %>% 
      rename_with(~gsub("_HRS_RA", "", .x))
    
    # Repeat for Young
    young_timeinvariant <- d$wide[[paste0(out_hrs_14,"_young")]] %>% 
      select(CASE_ID_HRS_RA, 
             paste0(instruction_sets[[outcome]][[instructions]]$exact,"_HRS_RA"))
    
    young_timeinvariant<- young_timeinvariant %>%
      rename_with(~ gsub("_HRS_RA", "", .x)) %>%
      rename(CASE_ID_HRS_RA = CASE_ID)
    
    # Exact match based on: Sex, Race/Ethnicity
    ID_LookUp_Exact_BL <- left_join(old_timeinvariant, young_timeinvariant,
                                    relationship = "many-to-many") %>%
      select(CASE_ID_OLD_RA, CASE_ID_HRS_RA) # Subset to only their IDs
    
    ## Track sample size ----
    nTracker[[outcome]]["exact_match_baseline",instructions] <- 
      nrow(ID_LookUp_Exact_BL)
    
    
    # Exact Match on TV Young Variables ----
    
    ## Select TV variables from OLD baseline ----
    old_exact_timevarying <- old_firstobs %>% 
      select(CASE_ID_OLD_RA, Year,
             all_of(instruction_sets[[outcome]][[instructions]]$exact_timevarying))
    
    # Join to ID_LookUp 
    ID_LookUp_Exact_TV <- left_join(ID_LookUp_Exact_BL, old_exact_timevarying)
    
    ## Repeat for young ----
    young_exact_timevarying <- d$long[[paste0(out_hrs_14,"_young")]] %>% 
      filter(Year == 2008) %>% # mediators only from 2008
      select(CASE_ID_HRS_RA,
             Year,
             all_of(instruction_sets[[outcome]][[instructions]]$exact_timevarying)) %>%
      # We do not want to match on year, so rename
      rename(Year_young = Year)
    
    ## ID young participant-wave:old pairs----
    ID_LookUp_Exact_TV <- inner_join(ID_LookUp_Exact_TV, young_exact_timevarying,
                                     relationship = "many-to-many")
    ID_LookUp_Exact_TV <- ID_LookUp_Exact_TV %>% 
      select(CASE_ID_OLD_RA, Year, CASE_ID_HRS_RA, Year_young)
    
    ## Track Sample Size ----
    nTracker[[outcome]]["exact_match_tv",instructions] <- nrow(ID_LookUp_Exact_TV)
    
    
    
    # Exact Match on Age ----
    
    ## Age "OLD" joined ----
    ID_LookUp_Age <- left_join(ID_LookUp_Exact_TV, 
                               old_firstobs %>% 
                                 select(CASE_ID_OLD_RA, Year,
                                        names(instruction_sets[[outcome]][[instructions]]$exact_HRS_timevarying)))
    
    ## Matched YOUNG age ----
    ID_LookUp_Age<-left_join(ID_LookUp_Age, 
                             d$long[[paste0(out_hrs_14,"_young")]] %>% 
                               filter(Year == 2008) %>% 
                               select(CASE_ID_HRS_RA, Year,
                                      names(instruction_sets[[outcome]][[instructions]]$exact_HRS_timevarying)) %>%
                               rename(Year_young = Year,
                                      AGEINTERVIEW_young = AGEINTERVIEW),
                             relationship = "many-to-many")
    
    for(var in names(instruction_sets[[outcome]][[instructions]]$exact_HRS_timevarying)){
      ID_LookUp_Age[,paste0(var,"_dif")] <- 
        abs(ID_LookUp_Age[,var] - ID_LookUp_Age[,paste0(var,"_young")])
      ID_LookUp_Age[["flag"]] <- ID_LookUp_Age[,paste0(var,"_dif")] <= 5
      ID_LookUp_Age <- ID_LookUp_Age %>% filter(flag == 1)
    }
    
    ## Age-Restricted Pairs ----
    ID_LookUp_Age <- ID_LookUp_Age %>% 
      select(CASE_ID_OLD_RA, CASE_ID_HRS_RA, Year, Year_young)
    
    nTracker[[outcome]]["exact_match_HRS_TV",instructions]<-nrow(ID_LookUp_Age)
    
    
    
    # Distance Matching ----
    
    ## Start with OLD ----
    ids = unique(ID_LookUp_Age$CASE_ID_OLD_RA)
    
    ## Merge in exact matched counterparts ----
    matched <- ID_LookUp_Age %>% 
      select(CASE_ID_OLD_RA, CASE_ID_HRS_RA, Year_young)
    
    ## Append "Baseline" wave from OLD ----
    matched <- left_join(matched, old_firstobs)
    
    ## Clean names to match  ----
    matched <- matched %>% 
      rename_with(~ paste0(.x, "_OLD")) %>% 
      rename(CASE_ID_OLD_RA = CASE_ID_OLD_RA_OLD,
             CASE_ID_HRS_RA = CASE_ID_HRS_RA_OLD,
             Year_young = Year_young_OLD)
    
    temp_young <- d$long[[paste0(out_hrs_14,"_young")]] %>% 
      filter(Year == 2008) %>%
      rename_with(~ paste0(.x, "_young")) %>% 
      rename(CASE_ID_HRS_RA = CASE_ID_HRS_RA_young)
    
    ## Merge in YOUNG data ----
    matched <- left_join(matched, temp_young) 
    
    ## Calculate Distances ----
    for(var in instruction_sets[[outcome]][[instructions]]$distVars){
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
    cat("CALCULATING WEIGHTS\n")

    for(var in instruction_sets[[outcome]][[instructions]]$distVars){
      matched[[paste0(var,"_dist_weighted")]] =
        1/weights[[outcome]][[instructions]][[var]]*matched[[paste0(var,"_dist")]]
      matched[[paste0(var,"_z_dist_weighted")]] =
        1/weights[[outcome]][[instructions]][[var]]*matched[[paste0(var,"_z_dist")]]
    }
    
    ## Total the distances ----
    
    # Initiate Total Scores
    matched$dist_z_weighted = 0
    matched$dist_z_unweighted = 0
    matched$dist_weighted = 0
    matched$dist_unweighted = 0
    
    for(var in instruction_sets[[outcome]][[instructions]]$distVars){
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
    out[[outcome]][[instructions]] <- matched %>%
      group_by(CASE_ID_OLD_RA, CASE_ID_HRS_RA) %>% 
      arrange(new, .by_group = TRUE) %>%  
      slice_head(n=1) %>%
      ungroup() %>% 
      group_by(CASE_ID_OLD_RA) %>%
      arrange(new, .by_group = TRUE) %>%  
      slice_head(n=10) %>%
      ungroup()
    
    # Track Sample Size
    nTracker[[outcome]]["distance_match",instructions] <- 
      nrow(out[[outcome]][[instructions]])
    
    # Quality Control and Trimming ----
    QC_info <- read.csv("../../DP_HRS_Only/MatchQC_info.csv")
    row.names(QC_info) <- QC_info$Variable
    cut_off = 0.25
    
    ## Establish Cut-offs -----
    
    # Loop through each possible distance variable
    for(var in instruction_sets[[outcome]][[instructions]]$distVars){
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
    
    # Get number of OLD participants pre-trim
    nTracker[[outcome]]["OLD_precutoff",instructions] <- 
      length(unique(out[[outcome]][[instructions]]$CASE_ID_OLD_RA))
    
    ## Filtering ----
    cat("Trimming observations\n")
    
    # Create trim_tracker to track how many observations are dropped at each step
    trim_tracker[[outcome]][[instructions]][["pre-trim"]] <- 
      nrow(out[[outcome]][[instructions]])
    
    # Duplicate distance matched data to QC
    data_qc[[outcome]][[instructions]] <- out[[outcome]][[instructions]]
    
    # For each distance variable:
    for(var in instruction_sets[[outcome]][[instructions]]$distVars){
      
      # Look up threshold
      threshold <- QC_info[var,paste0("threshold_",cut_off)]
      
      # Subset to distances less than the threshold
      data_qc[[outcome]][[instructions]] <- 
        data_qc[[outcome]][[instructions]] %>% 
        filter(.data[[paste0(var,"_dist")]] <= threshold)
      
      # Track change in sample size
      trim_tracker[[outcome]][[instructions]][[var]] <- 
        nrow(data_qc[[outcome]][[instructions]])
    }
    
    # Track the sample size
    nTracker[[outcome]][paste0("threshold_",cut_off),instructions] <- 
      nrow(data_qc[[outcome]][[instructions]])
    nTracker[[outcome]][paste0("OLD_postcutoff"), instructions] <- 
      length(unique(data_qc[[outcome]][[instructions]]$CASE_ID_OLD_RA)) 
  }
}

# Save ----
saveRDS(out, file="../../DP_HRS_Only/Full_MatchedData.RDS") 
saveRDS(data_qc,file="../../DP_HRS_Only/QCd_MatchedData.RDS")

# Create a minimal set of data
QCd_MatchedData_IDs <- list()

for(outcome in outcomes){
  for(instructions in names(instruction_sets[[outcome]])){
    QCd_MatchedData_IDs[[outcome]][[instructions]] <- 
      data_qc[[outcome]][[instructions]] %>% select(CASE_ID_OLD_RA, Wave_OLD, 
                                         CASE_ID_HRS_RA, Year_young, 
                                         new)
  }
}

saveRDS(QCd_MatchedData_IDs,file = "../../DP_HRS_Only/QCd_MatchedData_IDs.RDS")

saveRDS(nTracker,file="../../DP_HRS_Only/nTracker.RDS")
saveRDS(trim_tracker,file="../../DP_HRS_Only/qc_step_tracker.RDS")

