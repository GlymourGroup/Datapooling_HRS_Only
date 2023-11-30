# Analysis Script

# Set-up ----

# Clear Environment
rm(list=ls())
gc()

# Libraries
library("tidyverse")

## Data ----
d <- list()
d$old <- readRDS("../../DP_HRS_Only/HRS_old_wide.rds")
d$young<-readRDS("../../DP_HRS_Only/HRS_young_wide.rds")

# long:
d$old_tv_long  <-readRDS("../../DP_HRS_Only/HRS_old.rds")
d$young_tv_long<-readRDS("../../DP_HRS_Only/HRS_young.rds")

#

## Instructions ----
instructions <- dget("Instructions/Instructions_02.R")


## Var Order ----
var_weights <- readRDS("../../DP_HRS_Only/Weights.RDS")
var_weights$total$GENHEALTH_HRS_14 <- 
  var_weights$total$GENHEALTH_HRS_14[order(unlist(var_weights$total$GENHEALTH_HRS_14),
                                            decreasing=TRUE)]
# Remove AGEINTERVIEW
var_weights$total$GENHEALTH_HRS_14<-
  var_weights$total$GENHEALTH_HRS_14[names(var_weights$total$GENHEALTH_HRS_14) 
                                     != "AGEINTERVIEW"]
var_order <- names(var_weights$total$GENHEALTH_HRS_14)


# Data Prep ----

## Prep the old_tv_long data to easily merge with other datasets
d$old_tv_long_cleaned <- d$old_tv_long %>%
  select(CASE_ID_OLD_RA, Wave, AGEINTERVIEW, nwaves_contributed) %>%
  rename(Wave_OLD = Wave,
         AGEINTERVIEW_OLD = AGEINTERVIEW)

## Create outcome set
out_temp <- d$old %>% 
  select(CASE_ID_OLD_RA, GENHEALTH_HRS_14)

# Analysis ----

## Load in the matched data set  ----

matched_set<-readRDS("../../DP_HRS_Only/QCd_MatchedData_IDs.RDS")

# # Subset to match what would have been the distance scores only
# matched_set <- matched_set %>% 
#   select(CASE_ID_OLD_RA,Wave_OLD,CASE_ID_HRS_RA,
#          Year_young,dist_z_weighted)

# Where new = dist_z_weighted

## create permutation group ----
set.seed(123) # may not be needed as not random?
matched_set <- matched_set %>%
  # Group by HRS Participant
  group_by(CASE_ID_OLD_RA) %>%
  # and randomly select one of their matched NLSY Pair
  mutate(permutation_id = sample(1:10,size = n()),
         # count how many nlsy pairs they have
         n= n()) %>%
  ungroup() 


## Exposure Variable: BMI in 1994 ----
exp_temp <- d$young %>% select(CASE_ID_HRS_RA, BMI_HRS_2)

# now join with young
exp_temp <- left_join(matched_set, exp_temp)


## Create analytic data set ----
# Join in  outcome
analytic <- left_join(exp_temp %>% rename(firstwave_OLD = Wave_OLD), 
                      out_temp,
                      relationship = "many-to-many")

### merge in cleaned long data ----
# only really need age & number of waves participated
analytic <- left_join(analytic,
                      d$old_tv_long_cleaned %>% 
                        rename(firstwave_OLD = Wave_OLD),
                      relationship = "many-to-many")

# Center age and set units to decades
analytic$age_dec50 = (analytic$AGEINTERVIEW_OLD-50)/10

### merge in the "static" variables ----
# Sex, US Birth, Race/Ethnicity, (confounders)
analytic <- left_join(analytic, 
                      d$old %>% select(CASE_ID_OLD_RA, all_of(
                        paste0(instructions$exact,"_HRS_RA"))),
                      relationship = "many-to-many")

## Models ----
models <- list()

# Remove NAs from outcome
# *~~~*~*Doesn't matter for the mediator, though we should probably limit mediator?*~~~*~*
analytic <- analytic %>% filter(!is.na(GENHEALTH_HRS_14))

## Loop through each permutation ----
for (i in 1:10){
  cat(paste0("running permutation #", i,"\n"))
  
  # Subset data to permutation
  d_permutation <- analytic %>% 
    filter(permutation_id == i)
  
  ## Run models ----
  models[[paste0("p_",i)]] <- lm(GENHEALTH_HRS_14 ~ BMI_HRS_2 + age_dec50 + 
                                   FEMALE_HRS_RA + RACE_ETH_HRS_RA, 
                                   data=d_permutation)
  }
## Save Output ----
cat("Saving models\n")
saveRDS(models, "../../DP_HRS_Only/Results/Analytic_Models.RDS")

# Apply rubin's rules across permutations ----
# (formula for calculating variance across multiple-imputed data sets)

## Rubin's rules ----
# initiate empty list
rubin <- list()

cat("Running Rubin's Rules\n")
# extract coefs
coefficients<- as.data.frame(lapply(models, coef))
# find mean coef
rubin$coef_mean <- apply(coefficients, 1, mean) 
# calculate variance of coefs
coef_variance <- apply(coefficients, 1, var) 

# pull standard errors & square to store variance
variance <-as.data.frame(
  lapply(models, 
         function(x){(summary(x)$coefficients[,2])^2}))
average_var <- apply(variance, 1, mean)

# sum variances
rubin$final_variance <- coef_variance + average_var


### Save Rubin's output ----
cat("Saving Rubin's Rules \n")
saveRDS(rubin,
        file.path("../../DP_HRS_Only/Results/RubinsRules.RDS"))
