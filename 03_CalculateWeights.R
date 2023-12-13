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

### Full (wide) Datasets ----
d$old <- readRDS("../../DP_HRS_Only/HRS_old_wide.rds")
d$young<-readRDS("../../DP_HRS_Only/HRS_young_wide.rds")

### Harmonized Datasets ----
d$old_long <- readRDS("../../DP_HRS_Only/HRS_old.rds")
d$young_long<-readRDS("../../DP_HRS_Only/HRS_young.rds")

### Instructions ----
#instructions <- dget("Instructions/Instructions_02.R")

# Import different instruction sets
instruction_sets <- list()
for(i in list.files("Instructions/")){
  instruction_sets[[i]] <- dget(paste0("Instructions/",i))
}

# Parameters ----
model_info <- read.csv("../../DP_HRS_Only/outcome_info.csv")
row.names(model_info) <- model_info$outcome

# Set Exposure:
exposure <- "BMI_HRS_2"

# Define wave
w_out <- "_HRS_14"

# Define outcomes
# Categorical, continuous, binary
outcomes <- c("GENHEALTH"#, "SYSTOLIC_BP", "DIABETES"
              )
#summary(d$old %>% select(paste0(outcomes, w_out)))
#nrow(d$old) # huge missing rate

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

# run through each set of instructions 
for(instructions in names(instruction_sets)){
  
  ## Exposure Weights ----
  
  
  ### Get some exposure ----
  exp_temp <- d$young %>% select(CASE_ID_HRS_RA, BMI_HRS_2,
                                 paste0(instruction_sets[[instructions]]$exact,
                                        "_HRS_RA"))
  
  ### Append to overlap wave (2008) ----
  exp_temp <- left_join(d$young_long %>% filter(Year == 2008), 
                        exp_temp)
  
  ### Continuous Variables ----
  
  # Loop through each matching variable
  for(Var in c(instruction_sets[[instructions]]$distVars,
               paste0(instruction_sets[[instructions]]$exact,"_HRS_RA"), 
               instruction_sets[[instructions]]$exact_timevarying)){ 
    # Construct formula
    aFormula <- as.formula(paste0(exposure," ~ ",Var)) 
    # Run SLR
    Amodel <- lm(aFormula, data=exp_temp) 
    # Store results
    exp_weights[[instructions]][[Var]] <- summary(Amodel)$r.squared 
  }
  
  ### IMPORTANT BUT ARBITRARY DECISIONS: ----
  # assign some weight to age based off r2 for exposure
  #exp_weights$AGEINTERVIEW = exp_weights$BMI
  # scale all the weights by 100
  exp_weights[[instructions]] = lapply(exp_weights[[instructions]], 
                                       function(x){x*100})
  
  ## Outcome Weights ----
  # Unlike the alcohol paper, we are not too interested in change over time
  # We can look at 2018 (or some other year) for a particular outcome
  
  
  ### Get the Outcomes ----
  out_temp <- d$old %>% select(CASE_ID_OLD_RA, paste0(outcomes, w_out),
                               paste0(instruction_sets[[instructions]]$exact,
                                      "_HRS_RA"))
  
  out_temp <- left_join(d$old_long %>% filter(Year == 2008), 
                        out_temp)
  
  
  for(out in outcomes){
    # set parameters
    type <- model_info[out,"type"]
    print(type)
    out <- paste0(out,w_out)
    print(out)
    ## Linear Regression
    if(type %in% c("score", "cont")){
      for(distVar in c(instruction_sets[[instructions]]$distVars,
                       paste0(instruction_sets[[instructions]]$exact,"_HRS_RA"), 
                       instruction_sets[[instructions]]$exact_timevarying)){
        Aform = as.formula(paste0(out," ~ ", distVar))
        Amodel <- lm(Aform, data=out_temp)
        out_weights[[instructions]][[out]][[distVar]]<-summary(Amodel)$r.squared
      }
      ## Logistic Regression
    }else if(type == "binary"){
      for(distVar in c(instruction_sets[[instructions]]$distVars,
                       paste0(instruction_sets[[instructions]]$exact,"_HRS_RA"), 
                       instruction_sets[[instructions]]$exact_timevarying)){
        Bform = as.formula(paste0(out,"~", distVar))
        Bmodel <- glm(Bform, data=out_temp, family = "binomial") 
        out_weights[[instructions]][[out]][[distVar]] <- pR2(Bmodel)['McFadden']
      }
    }else{cat("Unrecognized type \n")}
    
    #out_weights[[out]][["AGEINTERVIEW"]] <- max(unlist(out_weights[[out]]))
  }
  
  ## Sum weights ----
  ### Exposure ----
  total_exposure_weight[[instructions]] <- 
    sum(as.data.frame(exp_weights[[instructions]]))
  
  ### Outcome ----
  for(out in paste0(outcomes,w_out)){
    total_outcome_weight[[instructions]][[out]] <- 
      sum(as.data.frame(out_weights[[instructions]][[out]]))
  }
  
  ## Calculate total weights ----
  
  # For each varirable,
  for(distVar in c(instruction_sets[[instructions]]$distVars,
                   paste0(instruction_sets[[instructions]]$exact,"_HRS_RA"), 
                   instruction_sets[[instructions]]$exact_timevarying)){
    
    ### Scale exposure weights ----
    scaled_exp[[instructions]][[distVar]] <- 
      exp_weights[[instructions]][[distVar]]/
      total_exposure_weight[[instructions]]
    
    for(out in paste0(outcomes,w_out)){
      
      ## Scale Outcome weights ----
      scaled_out[[instructions]][[out]][[distVar]] <- 
        out_weights[[instructions]][[out]][[distVar]]/
        total_outcome_weight[[instructions]][[out]]
      
      ## Sum Total Scaled Weights ----
      total_weights[[instructions]][[out]][[distVar]] <- 
        (scaled_exp[[instructions]][[distVar]]+
           scaled_out[[instructions]][[out]][[distVar]])/2
      
      ## Sum Total Unscaled Weights ----
      total_unscaled[[instructions]][[out]][[distVar]] <-
        (exp_weights[[instructions]][[distVar]] + 
           out_weights[[instructions]][[out]][[distVar]])/2
      
      # define weight for age at interview
      total_weights[[instructions]][[out]][["AGEINTERVIEW"]] <- 
        max(unlist(total_weights[[instructions]][[out]]))
    }
  }
  
  
  total_unscaled[[instructions]]$GENHEALTH_HRS_14[order(unlist(total_unscaled[[instructions]]$GENHEALTH_HRS_14))]
  total_weights[[instructions]]$GENHEALTH_HRS_14[order(unlist(total_weights[[instructions]]$GENHEALTH_HRS_14))]
  
}
weights <- list(exposure  = exp_weights, 
                scaled_exp= scaled_exp,
                outcome   = out_weights, 
                scaled_out= scaled_out,
                total     = total_weights)

saveRDS(weights, file="../../DP_HRS_Only/Weights.RDS")
