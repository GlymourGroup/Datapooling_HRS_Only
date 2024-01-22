# Calculate Weights

# Options: Self-Rated Health, Body Weight, Depressive Symptoms
# Currently going with BMI

# Set-up ----

## Session ----
rm(list=ls())
gc()

library("tidyverse")
library(pscl)

## Data ----
d <- list()

# Full Data (prior to subset)
d$og_long <- readRDS("../../DP_HRS_Only/HRS_Full_OG.rds")
d$og_wide <- readRDS("../../DP_HRS_Only/HRS_wide.rds")

# Subset data
d$long <- readRDS("../../DP_HRS_Only/Long_Data.rds")
d$wide <- readRDS("../../DP_HRS_Only/Wide_Data.rds")

# Parameters ----
model_info <- read.csv("../../DP_HRS_Only/outcome_info.csv")
row.names(model_info) <- model_info$outcome

outcomes <- c("DIABETES", "GENHEALTH", "SYSTOLIC_BP")

# Set Exposure:
exposure <- "BMI_HRS_2"

# Define Outcome Wave
w_out <- "_HRS_14"

## Instructions ----

# Import different instruction sets
instruction_sets <- list()
for(i in outcomes){
  for(ii in list.files(paste0("Instructions/",i))){
    instruction_sets[[i]][[ii]] <- 
      dget(paste0("Instructions/",i,"/",ii))
  }
}

# Calculate Weights ----

# Initiate Lists
exp_weights <- list()
out_weights <- list()
total_exposure_weight <- list()
total_outcome_weight <- list()
scaled_exp <- list()
scaled_out <- list()
total_weights <-list()
total_unscaled<-list()

# For each outcome,
for(out in outcomes){
  out_hrs_14 <- paste0(out,"_HRS_14")
  # run through each set of instructions 
  for(instructions in names(instruction_sets[[out]])){
    
    ## Exposure Weights ----
    
    ### Pull exposure and Baseline co-vars ----
    exp_temp <- d$wide[[paste0(out_hrs_14,"_young")]] %>% 
      select(CASE_ID_HRS_RA, BMI_HRS_2,
             paste0(instruction_sets[[out]][[instructions]]$exact, "_HRS_RA"))
    
    ### Append to overlap wave (2008) ----
    exp_temp <- left_join(d$long[[paste0(out_hrs_14,"_young")]] %>% 
                            filter(Year == 2008), 
                          exp_temp)
    
    ### Continuous Variables ----
    
    # Loop through each matching variable
    for(V in c(instruction_sets[[out]][[instructions]]$distVars,
               paste0(instruction_sets[[out]][[instructions]]$exact,"_HRS_RA"), 
               instruction_sets[[out]][[instructions]]$exact_timevarying)){ 
      # Construct formula
      aFormula <- as.formula(paste0(exposure," ~ ",V)) 
      # Run SLR
      Amodel <- lm(aFormula, data=exp_temp) 
      # Store results
      exp_weights[[out]][[instructions]][[V]] <- summary(Amodel)$r.squared 
    }
    
    # assign some weight to age based off r2 for exposure
    # exp_weights$AGEINTERVIEW = exp_weights$BMI
    # scale all the weights by 100
    exp_weights[[out]][[instructions]] = 
      lapply(exp_weights[[out]][[instructions]], function(x){x*100})
    
    # ## Outcome Weights ----
    # # 2018 is the year we are pullling the outcome from 
    # 
    # ### Get the Outcome ----
    # # Previously thought that we would be able to include all outcomes in one go
    # # However, variable selection leads to different matching sets samples
    # out_temp <- d$wide[[paste0(out_hrs_14,"_old")]] %>% 
    #   select(CASE_ID_OLD_RA, all_of(out_hrs_14),
    #          paste0(instruction_sets[[out]][[instructions]]$exact,"_HRS_RA"))
    # 
    # out_temp <- left_join(d$long[[paste0(out_hrs_14, "_old")]] %>% 
    #                         filter(Year == 2008), 
    #                       out_temp)
    # 
    # 
    # 
    # # set parameters
    # type <- model_info[out,"type"]
    # print(type)
    # ## Linear Regression
    # if(type %in% c("score", "cont")){
    #   for(V in c(instruction_sets[[out]][[instructions]]$distVars,
    #             paste0(instruction_sets[[out]][[instructions]]$exact,"_HRS_RA"), 
    #              instruction_sets[[out]][[instructions]]$exact_timevarying)){
    #     Aform = as.formula(paste0(out_hrs_14," ~ ", V))
    #     Amodel <- lm(Aform, data=out_temp)
    #     out_weights[[out]][[instructions]][[V]] <- summary(Amodel)$r.squared
    #   }
    #   ## Logistic Regression
    # }else if(type == "binary"){
    #   for(V in c(instruction_sets[[out]][[instructions]]$distVars,
    #             paste0(instruction_sets[[out]][[instructions]]$exact,"_HRS_RA"), 
    #              instruction_sets[[out]][[instructions]]$exact_timevarying)){
    #     Bform = as.formula(paste0(out,"~", V))
    #     Bmodel <- glm(Bform, data=out_temp, family = "binomial") 
    #     out_weights[[out]][[instructions]][[V]] <- pR2(Bmodel)['McFadden']
    #   }
    # }else{cat("Unrecognized type \n")}
    # 
    # #out_weights[[out]][["AGEINTERVIEW"]] <- max(unlist(out_weights[[out]]))
    # 
    # 
    # ## Sum weights ----
    ### Exposure ----
    total_exposure_weight[[out]][[instructions]] <- 
      sum(as.data.frame(exp_weights[[out]][[instructions]]))
    
    # ### Outcome ----
    # total_outcome_weight[[out]][[instructions]]<- 
    #   sum(as.data.frame(out_weights[[out]][[instructions]]))
    # 
    ## Calculate total weights ----

    # For each varirable,
    for(V in c(instruction_sets[[out]][[instructions]]$distVars,
               paste0(instruction_sets[[out]][[instructions]]$exact,"_HRS_RA"),
               instruction_sets[[out]][[instructions]]$exact_timevarying)){

      ### Scale exposure weights ----
      scaled_exp[[out]][[instructions]][[V]] <-
        exp_weights[[out]][[instructions]][[V]]/
        total_exposure_weight[[out]][[instructions]]
    #   
    #   ## Scale Outcome weights ----
    #   scaled_out[[out]][[instructions]][[V]] <- 
    #     out_weights[[out]][[instructions]][[V]]/
    #     total_outcome_weight[[out]][[instructions]]
    #   
    #   ## Sum Total Scaled Weights ----
    #   total_weights[[out]][[instructions]][[V]] <- 
    #     (scaled_exp[[out]][[instructions]][[V]] +
    #        scaled_out[[out]][[instructions]][[V]])/2
    #   
    #   ## Sum Total Unscaled Weights ----
    #   total_unscaled[[out]][[instructions]][[V]] <-
    #     (exp_weights[[out]][[instructions]][[V]] + 
    #        out_weights[[out]][[instructions]][[V]])/2
    #   
    #   # define weight for age at interview
    #   total_weights[[out]][[instructions]][["AGEINTERVIEW"]] <- 
    #     max(unlist(total_weights[[out]][[instructions]]))
    }
    # 
    # 
    # total_unscaled[[out]][[instructions]][order(unlist(total_unscaled[[out]][[instructions]]))]
    # total_weights[[out]][[instructions]][order(unlist(total_weights[[out]][[instructions]]))]
    # 
  }
  
}

weights <- list(exposure  = exp_weights, 
                scaled_exp= scaled_exp)#,
                # outcome   = out_weights, 
                # scaled_out= scaled_out,
                # total     = total_weights)

saveRDS(weights, file="../../DP_HRS_Only/Weights.RDS")
