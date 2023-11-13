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
instructions <- dget("Instructions/Instructions_00.R")

### Outcome info
model_info <- read.csv("../../DP_HRS_Only/outcome_info.csv")
row.names(model_info) <- model_info$outcome

# Calculate Exposure Weights ----
# Set Exposure:
exposure <- "BMI_HRS_2"

## Get some exposure ----
exp_temp <- d$young %>% select(CASE_ID_HRS_RA, BMI_HRS_2)

## Append to overlap wave (2006) ----
exp_temp <- left_join(d$young_long %>% filter(Year == 2006), 
                      exp_temp)

exp_weights <- list() # initiate empty list to store results

for(distVar in instructions$distVars){ #loop through each matching variable
  aFormula <- as.formula(paste0(exposure,"~",distVar)) # Construct formula
  Amodel <- lm(aFormula, data=exp_temp) # Run SLR
  exp_weights[[distVar]] <- summary(Amodel)$r.squared # Store results
}

### IMPORTANT BUT ARBITRARY DECISIONS: ----
# assign some weight to age based off r2 for exposure
exp_weights$AGEINTERVIEW = exp_weights$BMI
# scale all the weights by 100
exp_weights = lapply(exp_weights, function(x){x*100})


# Calculate Outcome Weights ----
# Unlike the alcohol paper, we are not too interested in change over time
# We can look at 2018 (or some other year) for a particular outcome

## Define wave ----
w_out <- "_HRS_14"

## Define outcomes ----
# Categorical, continuous, binary
outcomes <- c("GENHEALTH", "SYSTOLIC_BP", "DIABETES")
summary(d$old %>% select(paste0(outcomes, w_out)))
nrow(d$old) # huge missing rate

# Because we didn't carry forward, >=1/2 observations will be NA for BP

## Get the Outcomes ----
out_temp <- d$old %>% select(CASE_ID_OLD_RA, paste0(outcomes, w_out))

out_temp <- left_join(d$old_long %>% filter(Year == 2006), 
                      out_temp)

out_weights <- list()

for(out in outcomes){
  # set parameters
  type <- model_info[out,"type"]
  print(type)
  out <- paste0(out,w_out)
  print(out)
  ## Linear Regression
  if(type %in% c("score", "cont")){
    for(distVar in instructions$distVars){
      Aform = as.formula(paste0(out," ~ ", distVar))
      Amodel <- lm(Aform, data=out_temp)
      out_weights[[out]][[distVar]] <- summary(Amodel)$r.squared
      }
    ## Logistic Regression
  }else if(type == "binary"){
    for(distVar in instructions$distVars){
      Bform = as.formula(paste0(out,"~", distVar))
      Bmodel <- glm(Bform, data=out_temp, family = "binomial") 
      out_weights[[out]][[distVar]] <- pR2(Bmodel)['McFadden']
      }
  }else{cat("Unrecognized type \n")}
  
  out_weights[[out]][["AGEINTERVIEW"]] <- max(unlist(out_weights[[out]]))
}

# Calculating total weight ----

# Take the sum of the weights
total_exposure_weight <- sum(as.data.frame(exp_weights))

total_outcome_weight <- list()
for(out in paste0(outcomes,w_out)){
  total_outcome_weight[[out]] <- sum(as.data.frame(out_weights[[out]]))
}

scaled_exp <- list()
scaled_out <- list()
total_weights <- list()

for(distVar in instructions$distVars){
  scaled_exp[[distVar]] <- exp_weights[[distVar]]/total_exposure_weight
  
  for(out in paste0(outcomes,w_out)){
    scaled_out[[out]][[distVar]]<-out_weights[[out]][[distVar]]/total_outcome_weight[[out]]
    
    total_weights[[out]][[distVar]]<-(scaled_exp[[distVar]]+scaled_out[[out]][[distVar]])/2
    
    total_weights[[out]][["AGEINTERVIEW"]] <- max(unlist(total_weights[[out]]))
  }
}

weights <- list(exposure  = exp_weights, 
                scaled_exp=scaled_exp,
                outcome   = out_weights, 
                scaled_out=scaled_out,
                total     = total_weights)

saveRDS(weights, file="../../DP_HRS_Only/Weights.RDS")
