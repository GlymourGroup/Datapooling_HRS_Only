# Tables for the paper and appendix
# Set-up ----

# Clear Environment
rm(list=ls())
gc()

# Libraries
library("tidyverse")

## Options ----
n_dec <- 4

# Load Data ----
d <- list()

# Full Data (prior to subset)
d$og_long <- readRDS("../../DP_HRS_Only/HRS_Full_OG.rds")
d$og_wide <- readRDS("../../DP_HRS_Only/HRS_wide.rds")

# Subset data
d$long <- readRDS("../../DP_HRS_Only/Long_Data.rds")
d$wide <- readRDS("../../DP_HRS_Only/Wide_Data.rds")

## Gold Standard ----
d$gold <- readRDS("../../DP_HRS_Only/Wide_Data.rds")

### Filter for mediators ----

# Diabetes
d$gold$DIABETES_HRS_14 <- d$gold$DIABETES_HRS_14 %>%
  filter(!is.na(BMI_HRS_2), 
         !is.na(DIAB_MATCH_VAR), !is.na(BMI_HRS_9), !is.na(HbA1c),
         !is.na(DIABETES_HRS_14))

# General health
d$gold$GENHEALTH_HRS_14 <- d$gold$GENHEALTH_HRS_14 %>% 
  filter(!is.na(BMI_HRS_2), 
         !is.na(BMI_HRS_9), !is.na(GENHEALTH_HRS_9),
         !is.na(GENHEALTH_HRS_14))

# SBP
d$gold$SYSTOLIC_BP_HRS_14 <- d$gold$SYSTOLIC_BP_HRS_14 %>% 
  filter(!is.na(BMI_HRS_2), 
         !is.na(BMI_HRS_9), !is.na(SYSTOLIC_BP_HRS_9),
         !is.na(SYSTOLIC_BP_HRS_14))

## matched sets ----
matched_sets<-readRDS("../../DP_HRS_Only/QCd_MatchedData.RDS")

## Results ----

# Get the coefficients from rubin 
rubin<- readRDS("../../DP_HRS_Only/Results/RubinsRules.RDS")

# Parameters ----
model_info <- read.csv("../../DP_HRS_Only/outcome_info.csv")
row.names(model_info) <- model_info$outcome

outcomes <- c("DIABETES", "GENHEALTH", "SYSTOLIC_BP")

# Instructions ----
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

# Var Order ----
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

# Functions ----
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

# UNIVARIATE TABLE(s) ----

list_means<- list()
list_sds  <- list()
list_dist <- list()

# Loop through each outcome,
for(out in outcomes){
  # Initiate lists to store the cohort-specific means
  tbl_means<-as.data.frame(matrix(nrow=0, 
                                  ncol=2*(length(matched_sets[[out]]))))
  names(tbl_means) <- c(paste0(names(instruction_sets[[out]]), "_OLD"),
                        paste0(names(instruction_sets[[out]]), "_young"))
  
  # Duplicate for the SD
  tbl_sds <- tbl_means
  
  # Get the mean & SD for each matching variable within each set
  for(set in names(matched_sets[[out]])){
    # Count the number of observations for this dataset
    n_exact <- nrow(matched_sets[[out]][[set]])
    # Calculate descriptive statistics for each variable
    for(cohort in (c("_OLD","_young"))){
      if(out!="DIABETES"){
        for(var in instruction_sets[[out]]$Instructions_BOTH.R$distVars){
          # If the variable is an exact variable, calc the n & proportion
          var_cohort <- paste0(var,cohort)
          
          tbl_means[var,paste0(set,cohort)] <- 
            round(mean(matched_sets[[out]][[set]][[var_cohort]],na.rm=TRUE),n_dec)
          
          tbl_sds[var,paste0(set,cohort)]  <- 
            round(sd(matched_sets[[out]][[set]][[var_cohort]],  na.rm=TRUE),n_dec)
        }
      }else{
        for(var in instruction_sets[[out]]$Instructions_ALL3.R$distVars){
          # If the variable is an exact variable, calc the n & proportion
          var_cohort <- paste0(var,cohort)
          
          tbl_means[var,paste0(set,cohort)] <- 
            round(mean(matched_sets[[out]][[set]][[var_cohort]],na.rm=TRUE),n_dec)
          
          tbl_sds[var,paste0(set,cohort)]  <- 
            round(sd(matched_sets[[out]][[set]][[var_cohort]],  na.rm=TRUE),n_dec)
        }
      }
        
      }
    }
  # Repeat for the distances between the distance variables
  
  # Create a table to hold distances
  tbl_dist <- 
    as.data.frame(matrix(nrow=length(instruction_sets[[out]]$Instructions_BOTH.R$distVars),
                         ncol=length(matched_sets[[out]]) - 1)) # ignore 'none'
  names(tbl_dist) <- 
    names(matched_sets[[out]])[names(matched_sets[[out]]) != "Instructions_NONE.R"]
  row.names(tbl_dist) <- instruction_sets[[out]]$Instructions_BOTH.R$distVars
  
  # for each matched set, 
  for(set in names(matched_sets[[out]])[names(matched_sets[[out]]) != "Instructions_NONE.R"]){
    # Loop over distance variables 
    for(var in instruction_sets[[out]]$Instructions_BOTH.R$distVars){
      # And calculate the mean and SD
      var_dist <- paste0(var,"_dist")
      tbl_dist[var,set]<-round(mean(matched_sets[[out]][[set]][[var_dist]], 
                                    na.rm=TRUE),
                               n_dec)
      # tbl_dist[var,set]<-round(sd(matched_sets[[out]][[set]][[var_dist]], 
      #                               na.rm=TRUE),
      #                            n_dec)
    }
  }
  list_means[[out]]<- tbl_means
  list_sds[[out]]  <- tbl_sds
  list_dist[[out]] <- tbl_dist
}


# Write out CSV
saveRDS(list_dist,"../../DP_HRS_Only/Tables/DistanceTable.rds")
saveRDS(list_means,"../../DP_HRS_Only/Tables/Matched_Cohort_Means.rds")
saveRDS(list_sds,"../../DP_HRS_Only/Tables/Matched_Cohort_SDs.rds")

# Regression Tables ----
rubin_cleaned  <- list()
rubin_means_lt <- list()
rubin_sds_list <- list()
rubin_lb_list <- list()
rubin_ub_list <- list()

m_gold <- list()

for(out in outcomes){
  out_hrs_14 <- paste0(out,"_HRS_14")
  ### Initiate table ----
  if(out != "DIABETES"){
    rubin_means <- 
      data.frame(matrix(nrow=length(rubin[[out]]$Instructions_BOTH.R$coef_mean),
                        ncol=length(matched_sets[[out]])))
    rownames(rubin_means) <- names(rubin[[out]]$Instructions_BOTH.R$coef_mean)
    
  }else{
    rubin_means <- 
      data.frame(matrix(nrow=length(rubin[[out]]$Instructions_ALL3.R$coef_mean),
                        ncol=length(matched_sets[[out]])))
    rownames(rubin_means) <- names(rubin[[out]]$Instructions_ALL3.R$coef_mean)
  }
  
  colnames(rubin_means) <- names(matched_sets[[out]])
  
  rubin_sds <- rubin_means
  
  ### Extract the coefficient and sds ----
  for(set in names(matched_sets[[out]])){
    rubin_means[[set]]<- round(rubin[[out]][[set]]$coef_mean, n_dec)
    rubin_sds[[set]]  <- round(sqrt(rubin[[out]][[set]]$final_variance), n_dec)
  }
  
  ### GOLD STANDARD ----
  # The "Gold Standard" should be the super population we are drawing from
  # with restrictions to be able to be in the older or younger cohort
  # (e.g. complete cases of matching variables and the outcome)
  # Standardize age
  d$gold[[out_hrs_14]]$age_dec50 = 
    (d$gold[[out_hrs_14]]$AGEINTERVIEW_HRS_14-50)/10
  
  
  # We only need the gold standard for the most restrcted case
  # can hold onto others just in case
  d_gold_sets <- list()
  
  for(set in names(matched_sets[[out]])){
    
    # Do NOT Subset TRUE data to those who were matched in this set
    d_gold_sets[[set]] <- d$gold[[out_hrs_14]] #%>% 
      #filter(CASE_ID_HRS_RA %in% matched_sets[[out]][[set]]$CASE_ID_OLD_RA)
    
    # Construct a regression formula 
    type <- model_info[out,"type"]
    
    ## Linear Regression
    #if(type %in% c("score", "cont")){
      form <- as.formula(paste0(out_hrs_14," ~ ", "BMI_HRS_2 +
                                 age_dec50 + FEMALE_HRS_RA + 
                                 RACE_ETH_HRS_RA"))
      m_gold[[out]][[set]] <- as.data.frame(coef(summary(lm(form, 
                                                     data=d_gold_sets[[set]]))))
      
    ## Logistic Regression
    # }else if(type == "binary"){
    #   form <- as.formula(paste0(out_hrs_14," ~ ", "BMI_HRS_2 +
    #                              age_dec50 + FEMALE_HRS_RA + 
    #                              RACE_ETH_HRS_RA"))
    #   m_gold[[out]][[set]] <- as.data.frame(coef(summary(glm(form, 
    #                                                   data=d_gold_sets[[set]], 
    #                                                   family = "binomial"))))
    # }else{cat("Unrecognized type \n")}
  }
  
  
  ### Final Results Table ----
  rubin_cleaned[[out]] <- 
    data.frame(matrix(nrow=length(rubin[[out]]$Instructions_NONE.R$coef_mean),
                                   ncol=length(matched_sets[[out]])*2))
  rownames(rubin_cleaned[[out]]) <- 
    names(rubin[[out]]$Instructions_NONE.R$coef_mean)
  colnames(rubin_cleaned[[out]]) <- 
    c(names(matched_sets[[out]]), 
      paste0(names(matched_sets[[out]]),"_TRUTH"))
  
  lb <- rubin_cleaned[[out]]
  ub <- rubin_cleaned[[out]]
  
  for(set in names(matched_sets[[out]])){
    rubin_cleaned[[out]][[set]] <- 
      paste0(rubin_means[[set]]," (",
             round(rubin_means[[set]]-1.96*(rubin_sds[[set]]), n_dec), ",",
             round(rubin_means[[set]]+1.96*(rubin_sds[[set]]), n_dec), ")")
    
    rubin_cleaned[[out]][[paste0(set,"_TRUTH")]] <- 
      paste0(round(m_gold[[out]][[set]]$Estimate, n_dec), " (",
             round(m_gold[[out]][[set]]$Estimate-1.96*m_gold[[out]][[set]]$`Std. Error`,n_dec),
             ",",
             round(m_gold[[out]][[set]]$Estimate+1.96*m_gold[[out]][[set]]$`Std. Error`,n_dec),
             ")")
    
    lb[[set]] <- round(rubin_means[[set]]-1.96*(rubin_sds[[set]]), n_dec)
    ub[[set]] <- round(rubin_means[[set]]+1.96*(rubin_sds[[set]]), n_dec)

    rubin_means[[paste0(set,"_TRUTH")]] <- round(m_gold[[out]][[set]]$Estimate, n_dec)
    lb[[paste0(set,"_TRUTH")]] <- round(m_gold[[out]][[set]]$Estimate-1.96*(m_gold[[out]][[set]]$`Std. Error`), n_dec)
    ub[[paste0(set,"_TRUTH")]] <- round(m_gold[[out]][[set]]$Estimate+1.96*(m_gold[[out]][[set]]$`Std. Error`), n_dec)
    
    rubin_means_lt[[out]] <- rubin_means
    rubin_sds_list[[out]] <- rubin_sds
    rubin_lb_list[[out]]  <- lb
    rubin_ub_list[[out]]  <- ub
  }
}
### Write results to .csv ----
saveRDS(rubin_cleaned, "../../DP_HRS_Only/Tables/MainResults.rds")





# Use in case people want to talk about discrepancy
# ageplot <- d$old %>%
#   ggplot(aes(x = AGEINTERVIEW_HRS_9, group = matched, fill = matched)) + 
#   geom_density(adjust=1.5, alpha=0.4) 
# 
# saveRDS(ageplot, "../../DP_HRS_Only/Tables/agebymatched.RDS")


# Line Plot with Error Bars ----

fig1 <- list()
plot1<- list()

for(out in outcomes){
  ## Create data frame ----
  fig1[[out]] <- data.frame(matrix(nrow=length(matched_sets[[out]])*2,
                                   ncol=4)) # See next line
  names(fig1[[out]]) <- c("Model", "Estimate", "LB", "UB")
  #fig1[[out]]$Source <- rep(c("Pooled", "Truth"), length(matched_sets[[out]]))
  fig1[[out]]$Model  <- names(rubin_means_lt[[out]])
  # fig1[[out]]$Model <- factor(fig1[[out]]$Model, 
  #                             levels = c("No Mediators",
  #                                        "BMI",
  #                                        out,
  #                                        "Both Mediators"))
  fig1[[out]]$Estimate <- as.numeric(rubin_means_lt[[out]][2,])
  fig1[[out]]$LB <- as.numeric(rubin_lb_list[[out]][2,])
  fig1[[out]]$UB <- as.numeric(rubin_ub_list[[out]][2,])
  
  if(out != "DIABETES"){
    plot1[[out]] <- fig1[[out]] %>% 
      filter(!grepl("_TRUTH", Model)) %>%
      mutate(Model = gsub("Instructions_", "", Model)) %>%
      mutate(Model = gsub(".R", "", Model)) %>%
      ggplot(aes(x = Model, y = Estimate)) +
      #geom_point(position = position_dodge(width = 0.2)) +
      geom_errorbar(aes(ymin=LB, ymax=UB), 
                    width=0.2,
                    position =position_dodge(width = 0.2)) +
      geom_hline(yintercept=fig1[[out]][grep("_BOTH.R_TRUTH",fig1[[out]]$Model),
                                          "Estimate"])+
      geom_hline(yintercept=fig1[[out]][grep("_BOTH.R_TRUTH",fig1[[out]]$Model),
                                          "LB"],
                 linetype="dashed")+
      geom_hline(yintercept=fig1[[out]][grep("_BOTH.R_TRUTH",fig1[[out]]$Model),
                                          "UB"],
                 linetype="dashed")
  }else{
    plot1[[out]] <- fig1[[out]] %>% 
      filter(!grepl("_TRUTH", Model)) %>%
      mutate(Model = gsub("Instructions_", "", Model)) %>%
      mutate(Model = gsub(".R", "", Model)) %>%
      ggplot(aes(x = Model, y = Estimate)) +
      #geom_point(position = position_dodge(width = 0.2)) +
      geom_errorbar(aes(ymin=LB, ymax=UB), 
                    width=0.2,
                    position =position_dodge(width = 0.2)) +
      geom_hline(yintercept=fig1[[out]][grep("_ALL3.R_TRUTH",fig1[[out]]$Model),
                                          "Estimate"])+
      geom_hline(yintercept=fig1[[out]][grep("_ALL3.R_TRUTH",fig1[[out]]$Model),
                                          "LB"],
                 linetype="dashed")+
      geom_hline(yintercept=fig1[[out]][grep("_ALL3.R_TRUTH",fig1[[out]]$Model),
                                          "UB"],
                 linetype="dashed")
  }

  
}

saveRDS(plot1, "../../DP_HRS_Only/Tables/Figure1.RDS")


# Table 1: Matched vs Unmatched ----
tbl1_list <- list()

# Import original data before splitting
d$OG <- readRDS("../../DP_HRS_Only/HRS_recoded.RDS")

for(out in outcomes){
  out_hrs_14 <- paste0(out,"_HRS_14")
  # append BMI from 1994 to old data
  d$wide[[paste0(out_hrs_14, "_old")]] <- 
    left_join(d$wide[[paste0(out_hrs_14, "_old")]],
              d$OG %>% 
                select(CASE_ID_HRS_RA, BMI_HRS_2) %>%
                rename(CASE_ID_OLD_RA = CASE_ID_HRS_RA)
    )
  
  tbl1_cat <- c("FEMALE_HRS_RA", "RACE_ETH_HRS_RA")
  tbl1_cont<- c("AGEINTERVIEW_HRS_9", "BMI_HRS_2", out_hrs_14)
  
  # Create placeholders for categorical variables and their values
  # initiate an empty vector
  catVarNames_byCat <- c()
  # Then loop through each categorical variable
  for(cat in tbl1_cat){
    # and identify the unique values of that variable
    uniqueCats <- setdiff(unique(d$wide[[paste0(out_hrs_14, "_old")]][,cat]),NA)
    # store a combination of "variable|value" in catVarNames_byCat
    catVarNames_byCat <- c(catVarNames_byCat,paste0(cat,"|",uniqueCats))
  }
  
  # Create a "table 1" for each matched set
  for(set in names(matched_sets[[out]])){
    
    # create `matched` indicator
    temp <- d$wide[[paste0(out_hrs_14, "_old")]] %>% mutate(
      matched=case_when(CASE_ID_OLD_RA %in% matched_sets[[out]][[set]]$CASE_ID_OLD_RA ~1,
                        !(CASE_ID_OLD_RA%in%matched_sets[[out]][[set]]$CASE_ID_OLD_RA)~0)
    )
    
    # pull results for matched/unmatched
    temp_matched <- round(get_univariate_table(temp %>% filter(matched==1)),n_dec)
    temp_unmatch <- round(get_univariate_table(temp %>% filter(matched==0)),n_dec)
    
    tbl1_list[[out]][[set]] <- data.frame(matrix(nrow=length(c(tbl1_cont,
                                                               catVarNames_byCat)),
                                                 ncol=2))
    
    tbl1_list[[out]][[set]]$X1 <- paste0(temp_matched[[1]]," (",temp_matched[[2]],")")
    tbl1_list[[out]][[set]]$X2 <- paste0(temp_unmatch[[1]]," (",temp_unmatch[[2]],")")
    
    colnames(tbl1_list[[out]][[set]]) <- c("Matched","Unmatched")
    rownames(tbl1_list[[out]][[set]]) <- c(tbl1_cont,catVarNames_byCat)
  }
  
}

saveRDS(tbl1_list, "../../DP_HRS_Only/Tables/Table1_List.RDS")
