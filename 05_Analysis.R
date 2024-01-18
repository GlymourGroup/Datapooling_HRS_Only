# Analysis Script

# Set-up ----

# Clear Environment
rm(list=ls())
gc()

# Libraries
library("tidyverse")

## Data ----
d <- list()

# Full Data (prior to subset)
d$og_long <- readRDS("../../DP_HRS_Only/HRS_Full_OG.rds")
d$og_wide <- readRDS("../../DP_HRS_Only/HRS_wide.rds")

# Subset data
d$long <- readRDS("../../DP_HRS_Only/Long_Data.rds")
d$wide <- readRDS("../../DP_HRS_Only/Wide_Data.rds")

# matched data set
matched_sets<-readRDS("../../DP_HRS_Only/QCd_MatchedData_IDs.RDS")


## Instructions ----
outcomes <- c("DIABETES", "GENHEALTH", "SYSTOLIC_BP")

# Import different instruction sets
instruction_sets <- list()
for(i in outcomes){
  for(ii in list.files(paste0("Instructions/",i))){
    instruction_sets[[i]][[ii]] <- 
      dget(paste0("Instructions/",i,"/",ii))
  }
}
rm(i)
rm(ii)

## Var Order ----
var_weights <- readRDS("../../DP_HRS_Only/Weights.RDS")

# var_order <- list()
# # Loop through each outcome and instruction set
# for(out in outcomes){
#   for(instructions in names(instruction_sets[[out]])){
#     # Order variables by magnitude 
#     var_weights$total[[out]][[instructions]] <-
#       var_weights$total[[out]][[instructions]][order(unlist(var_weights$total[[out]][[instructions]]),
#                                                      decreasing = TRUE)]
#     
#     # Remove AGEINTERVIEW
#     var_weights$total[[out]][[instructions]]$AGEINTERVIEW  <-NULL
# 
#     var_order[[out]][[instructions]] <- 
#       names(var_weights$total[[out]][[instructions]])
#   }
# }



# Data Prep ----

# For each outcome,
for(out in outcomes){
  out_hrs_14 <- paste0(out,"_HRS_14")
  
  # Prep the "OLD" long data to easily merge with other datasets
  d[["cleaned"]][[out]] <- 
    d$long[[paste0(out_hrs_14,"_old")]] %>%
    select(CASE_ID_OLD_RA, Wave, AGEINTERVIEW, nwaves_contributed) %>%
    rename(Wave_OLD = Wave,
           AGEINTERVIEW_OLD = AGEINTERVIEW)
  
  # and create the outcome set
  d[["temp"]][[out]] <- 
    d$wide[[paste0(out_hrs_14,"_old")]] %>%
    select(CASE_ID_OLD_RA, all_of(out_hrs_14))
  
}

# Analysis ----

models <- list()
rubin <- list()

for(out in outcomes){
  out_hrs_14 <- paste0(out,"_HRS_14")
  for(matched_set in names(matched_sets[[out]])){
    
    ## For each outcome and matched set ----
    
    ## Create permutation groups ----
    set.seed(123) # may not be needed as not random?
    matched_sets[[out]][[matched_set]] <- 
      matched_sets[[out]][[matched_set]] %>%
      # Group by HRS Participant
      group_by(CASE_ID_OLD_RA) %>%
      # and randomly select one of their matched NLSY Pair
      mutate(permutation_id = sample(1:10,size = n()),
             # count how many nlsy pairs they have
             n= n()) %>%
      ungroup() 
    
    ## Pull 1994 BMI from "Young" ----
    exp_temp <- d$wide[[paste0(out_hrs_14,"_young")]] %>% 
      select(CASE_ID_HRS_RA, BMI_HRS_2)
    
    # Join in Matched Data
    exp_temp <- left_join(matched_sets[[out]][[matched_set]], 
                          exp_temp)
    
    
    ## Create analytic data set ----
    # Join in  outcome
    analytic <- left_join(exp_temp %>% rename(firstwave_OLD = Wave_OLD), 
                          d$temp[[out]],
                          relationship = "many-to-many")
    
    ### merge in cleaned long data ----
    # only really need age & number of waves participated
    analytic <- left_join(analytic,
                          d$cleaned[[out]] %>% 
                            rename(firstwave_OLD = Wave_OLD),
                          relationship = "many-to-many") 
    
    # Center age and set units to decades
    analytic$age_dec50 = (analytic$AGEINTERVIEW_OLD-50)/10
    
    ### merge in the "static" variables ----
    # Sex, US Birth, Race/Ethnicity, (confounders)
    analytic <- 
      left_join(analytic, 
                d$wide[[paste0(out_hrs_14,"_old")]] %>% 
                  select(CASE_ID_OLD_RA, all_of(
                    paste0(instruction_sets[[out]][[matched_set]]$exact,"_HRS_RA"))),
                relationship = "many-to-many")
    
    ## Models ----
    
    # Remove NAs from outcome
    analytic <- analytic %>% filter(!is.na(out_hrs_14))
    
    ## Loop through each permutation ----
    for (i in 1:10){
      cat(paste0("running permutation #", i,"\n"))
      
      # Subset data to permutation
      d_permutation <- analytic %>% 
        filter(permutation_id == i)
      
      form <- as.formula(paste0(out_hrs_14, " ~ ", 
                                "BMI_HRS_2 + 
                                age_dec50 + FEMALE_HRS_RA + RACE_ETH_HRS_RA"))
      
      ## Run models ----
      models[[out]][[matched_set]][[paste0("p_",i)]] <- 
        lm(form, 
           data=d_permutation)
    }
    
    # Apply rubin's rules across permutations ----
    # (formula for calculating variance across multiple-imputed data sets)
    
    ## Rubin's rules ----
    # initiate empty list
    
    cat("Running Rubin's Rules\n")
    # extract coefs (should be the same for all outcomes)
    
    # Start by initiating a data frame with model terms
    # round-about way because by chance some cells might be too small
    # otherwise, could just lapply(models[[out]][[matched_set]], coef)
    coefficients<-data.frame(rowname=c("(Intercept)", "BMI_HRS_2", "age_dec50",
                                       "FEMALE_HRS_RA", 
                                       "RACE_ETH_HRS_RAHispanic",
                                       "RACE_ETH_HRS_RAOther", 
                                       "RACE_ETH_HRS_RAWhite"))
    
    for(i in names(models[[out]][[matched_set]])){
      coefficients <- 
        full_join(as.data.frame(coef(models[[out]][[matched_set]][[i]])) %>% 
                    rownames_to_column() %>% 
                    rename(!!i := `coef(models[[out]][[matched_set]][[i]])`), 
                  coefficients)
    }
    
    # Clean up coefficients
    row.names(coefficients) <- coefficients$rowname
    coefficients$rowname <- NULL
    
    # find mean coef; check to see whether this is okay
    rubin[[out]][[matched_set]]$coef_mean <- 
      apply(coefficients, 1, mean, na.rm=TRUE) 
    
    # calculate variance of coefs
    coef_variance <- apply(coefficients, 1, var, na.rm = TRUE) 
    
    # pull standard errors & square to store variance
    
    variance<-data.frame(rowname=c("(Intercept)", "BMI_HRS_2", "age_dec50",
                                   "FEMALE_HRS_RA", "RACE_ETH_HRS_RAHispanic",
                                   "RACE_ETH_HRS_RAOther", 
                                   "RACE_ETH_HRS_RAWhite"))
    
    for(i in names(models[[out]][[matched_set]])){
      variance <- 
        full_join(as.data.frame(summary(models[[out]][[matched_set]][[i]])$coefficients) %>%
                                  select(`Std. Error`) %>%
                    rownames_to_column() %>%
                    rename(!!i := `Std. Error`), 
                  variance,
                  by = join_by(rowname))
    }
    
    # Clean up coefficients
    row.names(variance) <- variance$rowname
    variance$rowname <- NULL
    
    # Transform Standard Error to Variance
    variance <- variance^2
    
    # Find the average Variance 
    average_var <- apply(variance, 1, mean, na.rm=TRUE)
    
    # sum variances
    rubin[[out]][[matched_set]]$final_variance <- coef_variance + average_var
  }
}

# Save Output ----
saveRDS(models, 
        "../../DP_HRS_Only/Results/Analytic_Models.RDS")
saveRDS(rubin,
        file.path("../../DP_HRS_Only/Results/RubinsRules.RDS"))
