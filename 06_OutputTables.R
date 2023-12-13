# Tables for the paper and appendix
# Set-up ----

# Clear Environment
rm(list=ls())
gc()

# Libraries
library("tidyverse")
library("ggplot2")

## Options ----
n_dec <- 4

# Load Data ----

## Wide Data ----
d <- list()
d$old <- readRDS("../../DP_HRS_Only/HRS_old_wide.rds")
d$young<-readRDS("../../DP_HRS_Only/HRS_young_wide.rds")

## Sample Size Tracker ----
nTracker <- readRDS("../../DP_HRS_Only/nTracker.RDS")


## Var Order ----
var_weights <- readRDS("../../DP_HRS_Only/Weights.RDS")
var_weights$total$Instructions_00.R$GENHEALTH_HRS_14 <- 
  var_weights$total$Instructions_00.R$GENHEALTH_HRS_14[order(unlist(var_weights$total$Instructions_00.R$GENHEALTH_HRS_14),
                                                             decreasing=TRUE)]
# Remove AGEINTERVIEW
var_weights$total$Instructions_00.R$GENHEALTH_HRS_14<-
  var_weights$total$Instructions_00.R$GENHEALTH_HRS_14[names(var_weights$total$Instructions_00.R$GENHEALTH_HRS_14) 
                                                       != "AGEINTERVIEW"]
var_order <- names(var_weights$total$Instructions_00.R$GENHEALTH_HRS_14)

## Instruction Sets ----
## Instructions ----
instruction_sets <- list()
for(instructions in list.files("Instructions/")){
  instruction_sets[[instructions]] <- dget(paste0("Instructions/",instructions))
}

## Matched Sets ----
matched_data <- readRDS("../../DP_HRS_Only/QCd_MatchedData.RDS")

## Results ----

# Get the coefficients from rubin 
rubin<- readRDS("../../DP_HRS_Only/Results/RubinsRules.RDS")

# TABLES ----

## UNIVARIATE TABLE(s) ----

# Initiate lists to store the cohort-specific means
tbl_means<-as.data.frame(matrix(nrow=0, 
                                ncol=2*(length(matched_data))))
names(tbl_means) <- c("Instructions_00.R_OLD", "Instructions_00.R_young",
                      "Instructions_01.R_OLD", "Instructions_01.R_young",
                      "Instructions_02.R_OLD", "Instructions_02.R_young",
                      "Instructions_03.R_OLD", "Instructions_03.R_young")

# Duplicate for the SD
tbl_sds <- tbl_means

# Get the mean and standard deviation for each matching variable within each set

for(set in names(matched_data)[1:length(matched_data)]){
  # Count the number of observations for this dataset
  n_exact <- nrow(matched_data[[set]])
  # Calculate descriptive statistics for each variable
  for(cohort in (c("_OLD","_young"))){
    for(var in instruction_sets$Instructions_00.R$distVars){
      # If the variable is an exact variable, calc the n & proportion
      if(var %in% instruction_sets$Instructions_00.R$exact_timevarying){
        # Go through each level of the exact variable
        var_cohort <- paste0(var,cohort)
        for(varValue in unique(matched_data[[set]][[var_cohort]])){
          # Count the number of observations at that level
          n_value <- ifelse(is.na(varValue),
                            sum(is.na(matched_data[[set]])),
                            sum(matched_data[[set]][[var_cohort]]==varValue,
                                na.rm=TRUE))
          # And the proportion of the observations with that level
          p <- n_value/n_exact
          
          # Save Results to Exact_tracker
          tbl_means[paste0(var,": ",varValue),paste0(set,cohort)]<- round(n_value,n_dec)
          tbl_sds[paste0(var,": ",varValue),paste0(set,cohort)]  <- round(p,n_dec)
        }
      }
      
      else if(var %in% instruction_sets$Instructions_00.R$distVars){
        var_cohort <- paste0(var,cohort)
        tbl_means[var,paste0(set,cohort)]<- round(mean(matched_data[[set]][[var_cohort]],na.rm=TRUE),
                                      n_dec)
        tbl_sds[var,paste0(set,cohort)]  <- round(sd(matched_data[[set]][[var_cohort]],  na.rm=TRUE),
                                      n_dec)
      }
    }
  }
}
# Repeat for the distances between the distance variables

# Create a table to hold distances
tbl_dist <- as.data.frame(matrix(nrow=length(instruction_sets$Instructions_00.R$distVars),
                                 ncol=length(matched_data) - 1)) # last set has none
names(tbl_dist) <- names(matched_data)[1:length(matched_data)-1]
row.names(tbl_dist) <- instruction_sets$Instructions_00.R$distVars

# for each matched set, 
for(set in names(matched_data)[1:length(matched_data)-1]){
  # Loop over distance variables 
  for(var in var_order[var_order %in% instruction_sets$Instructions_00.R$distVars]){
    # And calculate the mean and SD
    var_dist <- paste0(var,"_dist_QC")
    tbl_dist[var,set]<-round(mean(matched_data[[set]][[var_dist]], na.rm=TRUE),
                                n_dec)
    tbl_dist[var,set]  <-round(sd(matched_data[[set]][[var_dist]], na.rm=TRUE),
                                n_dec)
  }
}



# Write out CSV
write.csv(tbl_dist,"../../DP_HRS_Only/Tables/DistanceTable.csv")
write.csv(tbl_means,"../../DP_HRS_Only/Tables/Matched_Cohort_Means.csv")
write.csv(tbl_sds,"../../DP_HRS_Only/Tables/Matched_Cohort_SDs.csv")


## Matched Sample Size Tracker ----
# ADD THIS IN THE MATCHING SCRIPT
# nTracker <- as.data.frame(t(as.data.frame(nTracker)))
# names(nTracker) <- "n"
# nTracker[nrow(nTracker)+1,"n"] <- length(unique(matched_data$CASE_ID_OLD_RA))
# rownames(nTracker)[rownames(nTracker) == "9"] <- "OLD_postcutoff" 
# 
# write.csv(nTracker, "../../DP_HRS_Only/Tables/nTracker.csv")

## Regression Tables ----

### Initiate table ----
rubin_means<-data.frame(matrix(nrow=length(rubin$Instructions_00.R$coef_mean),
                               ncol=length(matched_data)))
colnames(rubin_means) <- names(matched_data)
rownames(rubin_means) <- names(rubin$Instructions_00.R$coef_mean)

rubin_sds <- rubin_means

### Extract the coefficient and sds ----
for(set in names(matched_data)){
  rubin_means[[set]] <- round(rubin[[set]]$coef_mean, n_dec)
  rubin_sds[[set]] <- round(sqrt(rubin[[set]]$final_variance), n_dec)
}

### GOLD STANDARD ----
d_gold <- readRDS("../../DP_HRS_Only/HRS_wide.rds")
# Standardize age
d_gold$age_dec50 = (d_gold$AGEINTERVIEW_HRS_14-50)/10

d_gold_sets <- list()
m_gold <- list()

for(set in names(matched_data)){
  d_gold_sets[[set]] <- d_gold %>% 
    filter(CASE_ID_HRS_RA %in% matched_data[[set]]$CASE_ID_OLD_RA)
  
  m_gold[[set]] <- as.data.frame(coef(summary(lm(GENHEALTH_HRS_14 ~ BMI_HRS_2 +
                                                   age_dec50 + FEMALE_HRS_RA + 
                                                   RACE_ETH_HRS_RA, 
                                                 data=d_gold_sets[[set]]))))
}


### Final Results Table ----
rubin_cleaned<-data.frame(matrix(nrow=length(rubin$Instructions_00.R$coef_mean),
                               ncol=length(matched_data)*2))
rownames(rubin_cleaned) <- names(rubin$Instructions_00.R$coef_mean)
colnames(rubin_cleaned) <- c(names(matched_data), paste0(names(matched_data),"_TRUTH"))

lb <- rubin_cleaned
ub <- rubin_cleaned

for(set in names(matched_data)){
  rubin_cleaned[[set]] <- 
    paste0(rubin_means[[set]]," (",
           round(rubin_means[[set]]-1.96*(rubin_sds[[set]]), n_dec), ",",
           round(rubin_means[[set]]+1.96*(rubin_sds[[set]]), n_dec), ")")
  
  rubin_cleaned[[paste0(set,"_TRUTH")]] <- 
    paste0(round(m_gold[[set]]$Estimate, n_dec), " (",
           round(m_gold[[set]]$Estimate-1.96*m_gold[[set]]$`Std. Error`,n_dec),
           ",",
           round(m_gold[[set]]$Estimate+1.96*m_gold[[set]]$`Std. Error`,n_dec),
           ")")
  
  lb[[set]] <- round(rubin_means[[set]]-1.96*(rubin_sds[[set]]), n_dec)
  ub[[set]] <- round(rubin_means[[set]]+1.96*(rubin_sds[[set]]), n_dec)
  
  lb[[paste0(set,"_TRUTH")]] <- round(m_gold[[set]]$Estimate-1.96*(m_gold[[set]]$`Std. Error`), n_dec)
  ub[[paste0(set,"_TRUTH")]] <- round(m_gold[[set]]$Estimate+1.96*(m_gold[[set]]$`Std. Error`), n_dec)
}

### Write results to .csv ----

# Combined
write.csv(rubin_cleaned, "../../DP_HRS_Only/Tables/MainResults.csv")




## Table 1: Matched vs Unmatched ----

# Import original data before splitting
d$OG <- readRDS("../../DP_HRS_Only/HRS_recoded.RDS")
# append BMI from 1994 to old data
d$old <- left_join(d$old,
                   d$OG %>% 
                     select(CASE_ID_HRS_RA, BMI_HRS_2) %>%
                     rename(CASE_ID_OLD_RA = CASE_ID_HRS_RA)
)

tbl1_cat <- c("FEMALE_HRS_RA", "RACE_ETH_HRS_RA")
tbl1_cont<- c("AGEINTERVIEW_HRS_9", "BMI_HRS_2", "GENHEALTH_HRS_14")

# Create placeholders for categorical variables and their values
# initiate an empty vector
catVarNames_byCat <- c()
# Then loop through each categorical variable
for(cat in tbl1_cat){
  # and identify the unique values of that variable
  uniqueCats <- setdiff(unique(d$old[,cat]),NA)
  # store a combination of "variable|value" in catVarNames_byCat
  catVarNames_byCat <- c(catVarNames_byCat,paste0(cat,"|",uniqueCats))
}

get_univariate_table <- function(d){
  # Initiate data frame of the numeric and categorical vars
  table_univariate <- data.frame(matrix(nrow=length(c(tbl1_cont,
                                                      catVarNames_byCat)),
                                        ncol=2))
  # The columns are the mean/count & SD/Proportion
  colnames(table_univariate) <- c("Mean or Count","SD or Proportion")
  rownames(table_univariate) <- c(tbl1_cont,catVarNames_byCat)
  for(var in tbl1_cont){
    table_univariate[var,]<-c(mean(d[[var]],na.rm=TRUE),sd(d[[var]],na.rm=TRUE))
  }
  for(var in tbl1_cat){
    n_not_NA <- sum(!is.na(d[[var]]))
    for(varValue in unique(d[,var])){
      n_value <- sum(d[[var]]==varValue,na.rm=TRUE)
      p <- n_value/n_not_NA
      table_univariate[paste0(var,"|",varValue),] <- c(n_value,p)
    }
  }
  return(table_univariate)
}

# Create a "table 1" for each matched set
tbl1_list <- list()
for(set in names(matched_data)){
  
  # create `matched` indicator
  temp <- d$old %>% mutate(
    matched=case_when(CASE_ID_OLD_RA %in% matched_data[[set]]$CASE_ID_OLD_RA ~1,
                      !(CASE_ID_OLD_RA%in%matched_data[[set]]$CASE_ID_OLD_RA)~0)
  )
  
  # pull results for matched/unmatched
  temp_matched <- round(get_univariate_table(temp %>% filter(matched==1)),n_dec)
  temp_unmatch <- round(get_univariate_table(temp %>% filter(matched==0)),n_dec)
  
  tbl1_list[[set]] <- data.frame(matrix(nrow=length(c(tbl1_cont,
                                                      catVarNames_byCat)),
                                        ncol=2))
  
  tbl1_list[[set]]$X1 <- paste0(temp_matched[[1]]," (",temp_matched[[2]],")")
  tbl1_list[[set]]$X2 <- paste0(temp_unmatch[[1]]," (",temp_unmatch[[2]],")")
  
  colnames(tbl1_list[[set]]) <- c("Matched","Unmatched")
  rownames(tbl1_list[[set]]) <- c(tbl1_cont,catVarNames_byCat)
}

saveRDS(tbl1_list, "../../DP_HRS_Only/Tables/Table1_List.RDS")

# Use in case people want to talk about discrepancy
# ageplot <- d$old %>%
#   ggplot(aes(x = AGEINTERVIEW_HRS_9, group = matched, fill = matched)) + 
#   geom_density(adjust=1.5, alpha=0.4) 
# 
# saveRDS(ageplot, "../../DP_HRS_Only/Tables/agebymatched.RDS")


# Line Plot with Error Bars ----

## Create data frame ----
fig1 <- data.frame(matrix(nrow=8,
                          ncol=5))
names(fig1) <- c("Source", "Model", "Estimate", "LB", "UB")
fig1$Source <- rep(c("Pooled", "Truth"), 4)
fig1$Model  <- c(rep("No Mediators", 2),
                 rep("BMI", 2),
                 rep("GENHEALTH", 2),
                 rep("Both Mediators", 2))
fig1$Model <- factor(fig1$Model, 
                     levels = c("No Mediators",
                                "BMI",
                                "GENHEALTH",
                                "Both Mediators"))
fig1$Estimate <- c(rubin_means$Instructions_03.R[2],
                   m_gold$Instructions_03.R$Estimate[2],
                   rubin_means$Instructions_02.R[2],
                   m_gold$Instructions_02.R$Estimate[2],
                   rubin_means$Instructions_01.R[2],
                   m_gold$Instructions_01.R$Estimate[2],
                   rubin_means$Instructions_00.R[2],
                   m_gold$Instructions_00.R$Estimate[2])
fig1$LB <- c(lb$Instructions_03.R[2],
             lb$Instructions_03.R_TRUTH[2],
             lb$Instructions_02.R[2],
             lb$Instructions_02.R_TRUTH[2],
             lb$Instructions_01.R[2],
             lb$Instructions_01.R_TRUTH[2],
             lb$Instructions_00.R[2],
             lb$Instructions_00.R_TRUTH[2])
fig1$UB <- c(ub$Instructions_03.R[2],
             ub$Instructions_03.R_TRUTH[2],
             ub$Instructions_02.R[2],
             ub$Instructions_02.R_TRUTH[2],
             ub$Instructions_01.R[2],
             ub$Instructions_01.R_TRUTH[2],
             ub$Instructions_00.R[2],
             ub$Instructions_00.R_TRUTH[2])

plot1 <- fig1 %>% ggplot(aes(x = Model, y = Estimate, group = Source, color = Source)) +
  geom_point(position = position_dodge(width = 0.2)) +
  geom_errorbar(aes(ymin=LB, ymax=UB), 
                width=0.2,
                position =position_dodge(width = 0.2))

saveRDS(plot1, "../../DP_HRS_Only/Tables/Figure1.RDS")
