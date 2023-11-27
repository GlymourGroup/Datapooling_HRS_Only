# Tables for the paper and appendix
# Set-up ----

# Clear Environment
rm(list=ls())
gc()

# Libraries
library("tidyverse")

## Options ----
n_dec <- 2

# Load Data ----

## Sample Size Tracker ----
nTracker <- readRDS("../../DP_HRS_Only/nTracker.RDS")


## Var Order ----
var_wts <- readRDS("../../DP_HRS_Only/Weights.RDS")
var_wts$total$GENHEALTH_HRS_14 <- 
  var_wts$total$GENHEALTH_HRS_14[order(unlist(var_wts$total$GENHEALTH_HRS_14),
                                           decreasing=TRUE)]
# Remove AGEINTERVIEW
var_wts$total$GENHEALTH_HRS_14<- 
  var_wts$total$GENHEALTH_HRS_14[names(var_wts$total$GENHEALTH_HRS_14) 
                                     != "AGEINTERVIEW"]
var_order <- names(var_wts$total$GENHEALTH_HRS_14)

## Instruction Sets ----
## Instructions ----
instructions <- dget("Instructions/Instructions_01.R")

## Matched Sets ----
matched_data <- readRDS("../../DP_HRS_Only/Full_MatchedData.RDS")

## Results ----

# Get the coefficients from rubin 
rubin<- readRDS("../../DP_HRS_Only/Results/RubinsRules.RDS")

# TABLES ----
## UNIVARIATE TABLE(s) ----
# Get the mean and standard deviation for each matching variable within each set

# Count the number of observations for this dataset
n_exact <- nrow(matched_data)

# Initiate lists to store the cohort-specific means
tbl_means<-as.data.frame(matrix(nrow=0,
                                ncol=2))
names(tbl_means) <- c("_OLD", "_young")

# Duplicate for the SD
tbl_sds <- tbl_means

# Calculate descriptive statistics for each variable
for(cohort in (c("_OLD","_young"))){
  for(var in instructions$distVars){
    # If the variable is an exact variable, calc the n & proportion
    if(var %in% instructions$exact_timevarying){
      # Go through each level of the exact variable
      var_cohort <- paste0(var,cohort)
      for(varValue in unique(matched_data[[var_cohort]])){
        # Count the number of observations at that level
        n_value <- ifelse(is.na(varValue),
                          sum(is.na(matched_data)),
                          sum(matched_data[[var_cohort]]==varValue,
                              na.rm=TRUE))
        # And the proportion of the observations with that level
        p <- n_value/n_exact
        
        # Save Results to Exact_tracker
        tbl_means[paste0(var,": ",varValue),cohort]<- round(n_value,n_dec)
        tbl_sds[paste0(var,": ",varValue),cohort]  <- round(p,n_dec)
      }
    }
    
    else if(var %in% instructions$distVars){
      var_cohort <- paste0(var,cohort)
      tbl_means[var,cohort]<- round(mean(matched_data[[var_cohort]],na.rm=TRUE),
                                    n_dec)
      tbl_sds[var,cohort]  <- round(sd(matched_data[[var_cohort]],  na.rm=TRUE),
                                    n_dec)
    }
  }
}

# Repeat for the distances between the distance variables

# Create a table to hold distances
tbl_dist <- as.data.frame(matrix(nrow=length(instructions$distVars),ncol=2))
names(tbl_dist) <- c("mean", "sd")
row.names(tbl_dist) <- instructions$distVars

# Loop over distance variables 
for(var in var_order[var_order %in% instructions$distVars]){
  # And calculate the mean and SD
  var_dist <- paste0(var,"_dist")
  tbl_dist[var,"mean"]<-round(mean(matched_data[[var_dist]], na.rm=TRUE),
                              n_dec)
  tbl_dist[var,"sd"]  <-round(sd(matched_data[[var_dist]],   na.rm=TRUE),
                              n_dec)
}



# Write out CSV
write.csv(tbl_dist,"../../DP_HRS_Only/Tables/DistanceTable.csv")
write.csv(tbl_means,"../../DP_HRS_Only/Tables/Matched_Cohort_Means.csv")
write.csv(tbl_sds,"../../DP_HRS_Only/Tables/Matched_Cohort_SDs.csv")


## Matched Sample Size Tracker ----

nTracker <- as.data.frame(t(as.data.frame(nTracker)))
names(nTracker) <- "n"

write.csv(nTracker, "../../DP_HRS_Only/Tables/nTracker.csv")

## Regression Tables ----

### format data ----

# Extract the coefficient and variance 
temp_coef <- data.frame(rubin$coef_mean)
temp_var  <- data.frame(rubin$final_variance)
rubin_cleaned   <- cbind(temp_coef, temp_var)
# Name columns
colnames(rubin_cleaned) <- c("coef","var")

### Set-up  Tables ----
# Initiate an empty list of data frames to hold the results

m0 = as.data.frame(matrix(nrow = length(rubin$coef_mean), 
                          ncol = 2))
names(m0) <- c("Matched", "Gold Standard")

# rename rows of each list to match the outcomes
row.names(m0) <- names(rubin$coef_mean)


# pull the coefficient and variance
temp_est <- rubin_cleaned$coef
temp_var <- rubin_cleaned$var

# Calculate the CI
temp_lb <- temp_est - 1.96*(sqrt(temp_var))
temp_ub <- temp_est + 1.96*(sqrt(temp_var))

m0$Matched <- paste0(round(temp_est,2)," (",
                     round(temp_lb, 2),", ",
                     round(temp_ub, 2),")")

## GOLD STANDARD ----
d_gold <- readRDS("../../DP_HRS_Only/HRS_wide.rds")
d_gold <- readRDS("../../DP_HRS_Only/")
# Standardize age
d_gold$age_dec50 = (d_gold$AGEINTERVIEW_HRS_14-50)/10

d_gold_subset<-d_gold%>%filter(CASE_ID_HRS_RA %in% matched_data$CASE_ID_OLD_RA)


mG <- as.data.frame(coef(summary(lm(GENHEALTH_HRS_14 ~BMI_HRS_2 +
                   age_dec50 + FEMALE_HRS_RA + RACE_ETH_HRS_RA, 
                 data=d_gold_subset))))

m0$`Gold Standard` <- paste0(round(mG$Estimate,2)," (",
                             round(mG$Estimate-1.96*(mG$`Std. Error`), 2),", ",
                             round(mG$Estimate+1.96*(mG$`Std. Error`), 2),")")


### Write results to .csv ----

# Combined
write.csv(m0, "../../DP_HRS_Only/Tables/MainResults.csv")

# TABLE 1 ----
summary(d_gold_subset %>% 
          select(c(paste0(c(#instructions$distVars, 
                            instructions$exact_timevarying),
                          "_HRS_9")#,
                   #paste0(instructions$exact, "_HRS_RA")
                   )))

d_gold_unmatched <- d_gold%>%
  filter(!(CASE_ID_HRS_RA %in% matched_data$CASE_ID_OLD_RA))

summary(d_gold_subset %>% 
          select(c(paste0(c(instructions$distVars, 
                            instructions$exact_timevarying),
                          "_HRS_9"),
                   paste0(instructions$exact, "_HRS_RA"))))


exact_uniquevals <- list()
# Calculate descriptive statistics for each variable
for(var in c(paste0(instructions$exact_timevarying, "_HRS_9"),
             paste0(instructions$exact, "_HRS_RA"))){
  # Go through each level of the exact variable
  exact_uniquevals[[var]] <- unique(d_gold_subset[[var]])
}
exact_TV_combs <-  expand.grid(exact_uniquevals)

exact_TV_combs$comb_index <- row_number(exact_TV_combs)

combo <- left_join(d_gold_unmatched, exact_TV_combs)

temp <- combo %>% 
  group_by(comb_index) %>%
  summarise(n = n()) %>%
  ungroup()

exact_TV_combs <- left_join(exact_TV_combs, temp)

combo_matched <- left_join(d_gold_subset, exact_TV_combs)
temp_matched <- combo_matched%>% 
  group_by(comb_index) %>%
  summarise(n_matched = n()) %>%
  ungroup()

exact_TV_combs <- left_join(exact_TV_combs, temp_matched)
