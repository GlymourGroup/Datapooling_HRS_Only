# Check what is happening when include vs exclude weights. 

# Set-up ----

# Clear Environment
rm(list=ls())
gc()

# Libraries
library("tidyverse")
library(ggpubr)

## Options ----
n_dec <- 4

# Data ----
d <- list()

## Original ----
d$og_long <- readRDS("../../DP_HRS_Only/HRS_Full_OG.rds")
d$og_wide <- readRDS("../../DP_HRS_Only/HRS_wide.rds")

## Old and young cohorts ----
d$long <- readRDS("../../DP_HRS_Only/Long_Data.rds")
d$wide <- readRDS("../../DP_HRS_Only/Wide_Data.rds")

## Gold Standard ----
d$gold <- readRDS("../../DP_HRS_Only/Wide_Data.rds")

### Cleaning ----

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

## Matched Sets ----
matched_sets<-readRDS("../../DP_HRS_Only/QCd_MatchedData.RDS")

## Unmatched Sets ----
unmatched_sets <- list(
  DIABETES_HRS_14 = d$gold$DIABETES_HRS_14 %>% 
    filter(!(CASE_ID_HRS_RA %in% 
               matched_sets$DIABETES$Instructions_ALL3.R$CASE_ID_OLD_RA)),
  
  GENHEALTH_HRS_14= d$gold$GENHEALTH_HRS_14 %>% 
    filter(!(CASE_ID_HRS_RA %in% 
               matched_sets$GENHEALTH$Instructions_BOTH.R$CASE_ID_OLD_RA)),
  
  SYSTOLIC_BP_HRS_14= d$gold$SYSTOLIC_BP_HRS_14 %>% 
    filter(!(CASE_ID_HRS_RA %in% 
               matched_sets$SYSTOLIC_BP$Instructions_BOTH.R$CASE_ID_OLD_RA))
)


# Data Cleaning ----

# rbind to compare matched and unmatched data
d_plot <- list(
  DIABETES_HRS_14 = rbind(
    # Select only relevant variables and create an indicator
    unmatched_sets$DIABETES_HRS_14 %>% 
      select(CASE_ID_HRS_RA, BMI_HRS_9, HbA1c, DIABETES_HRS_9) %>%
      mutate(matched = 0),
    # Have to rename to rbind
    matched_sets$DIABETES$Instructions_ALL3.R %>% 
      select(CASE_ID_HRS_RA, BMI_OLD, HbA1c_OLD, DIABETES_OLD) %>% 
      rename(BMI_HRS_9 = BMI_OLD,
             HbA1c = HbA1c_OLD, 
             DIABETES_HRS_9 = DIABETES_OLD) %>%
      mutate(matched = 1)),
  
  SYSTOLIC_BP_HRS_14 = rbind(
    unmatched_sets$SYSTOLIC_BP_HRS_14 %>% 
      select(CASE_ID_HRS_RA, BMI_HRS_9, SYSTOLIC_BP_HRS_9) %>%
      mutate(matched = 0),
    matched_sets$SYSTOLIC_BP$Instructions_BOTH.R   %>% 
      select(CASE_ID_HRS_RA, BMI_OLD, SYSTOLIC_BP_OLD) %>% 
      rename(BMI_HRS_9 = BMI_OLD,
             SYSTOLIC_BP_HRS_9 = SYSTOLIC_BP_OLD) %>%
      mutate(matched = 1))
)

# Visualizations ----

## Scatter Plots ----

# Diabetes
# Show BMI vs HbA1c by matched status and diabetes
ggarrange(
  d_plot$DIABETES_HRS_14 %>% 
    filter(DIABETES_HRS_9 == 1) %>% 
    ggplot(aes(x=BMI_HRS_9, y = HbA1c, color=as.factor(matched))) +
    geom_point()+
    xlim(10, 65)+
    ylim(4, 13),
  d_plot$DIABETES_HRS_14 %>% 
    filter(DIABETES_HRS_9 == 0) %>% 
    ggplot(aes(x=BMI_HRS_9, y = HbA1c, color=as.factor(matched))) +
    geom_point()+
    xlim(10, 65)+
    ylim(4, 13),
  ncol=1, nrow=2,
  legend = "right",
  common.legend = TRUE
)

# Alternatively (preferred)

ggarrange(
  d_plot$DIABETES_HRS_14 %>% 
    filter(matched == 1) %>% 
    ggplot(aes(x=BMI_HRS_9, y = HbA1c, color=as.factor(DIABETES_HRS_9))) +
    geom_point()+
    xlim(10, 65)+
    ylim(4, 13),
  d_plot$DIABETES_HRS_14 %>% 
    filter(matched == 0) %>% 
    ggplot(aes(x=BMI_HRS_9, y = HbA1c, color=as.factor(DIABETES_HRS_9))) +
    geom_point()+
    xlim(10, 65)+
    ylim(4, 13),
  ncol=1, nrow=2,
  legend = "right",
  common.legend = TRUE
)

### Systolic Blood Pressure

d_plot$SYSTOLIC_BP_HRS_14 %>% 
  ggplot(aes(x=BMI_HRS_9, y = SYSTOLIC_BP_HRS_9, colour = as.factor(matched))) +
  geom_point() 

ggarrange(
  d_plot$SYSTOLIC_BP_HRS_14 %>% 
    filter(matched == 1) %>% 
    ggplot(aes(x=BMI_HRS_9, y = SYSTOLIC_BP_HRS_9)) +
    geom_point() +
    xlim(10, 65) +
    ylim(85, 225),
  d_plot$SYSTOLIC_BP_HRS_14 %>% 
    filter(matched == 0) %>% 
    ggplot(aes(x=BMI_HRS_9, y = SYSTOLIC_BP_HRS_9)) +
    geom_point() +
    xlim(10, 65) +
    ylim(85, 225),
  ncol=1, nrow=2,
  legend = "right",
  common.legend = TRUE
)

## Histograms ----

# Show distributions of Old vs Young matching variables 
ggarrange(
  # BMI
  matched_sets$DIABETES$Instructions_ALL3.R %>%
    ggplot() +
    geom_histogram(aes(x=BMI_z_OLD, fill="Old"),  alpha = 0.25) +
    geom_histogram(aes(x=BMI_z_young,fill="Young"),  alpha = 0.25) + 
    scale_fill_manual(values = c("blue","red")),
  # HbA1c
  matched_sets$DIABETES$Instructions_ALL3.R %>%
    ggplot() +
    geom_histogram(aes(x=HbA1c_z_OLD, fill="Old"),  alpha = 0.25) +
    geom_histogram(aes(x=HbA1c_z_young,fill="Young"),  alpha = 0.25) + 
    scale_fill_manual(values = c("blue","red")),
  ncol=1, nrow=2,
  legend = "right",
  common.legend = TRUE
)

# SYS BP
ggarrange(
  matched_sets$DIABETES$Instructions_ALL3.R %>%
    ggplot() +
    geom_histogram(aes(x=BMI_z_OLD, fill="Old"),  alpha = 0.25) +
    geom_histogram(aes(x=BMI_z_young,fill="Young"),  alpha = 0.25) + 
    scale_fill_manual(values = c("blue","red"))+
    xlim(-2, 4),
  # Systolic Blood Pressures
  matched_sets$SYSTOLIC_BP$Instructions_BOTH.R %>%
    ggplot() +
    geom_histogram(aes(x=BMI_z_OLD, fill="Old"),  alpha = 0.25) +
    geom_histogram(aes(x=BMI_z_young,fill="Young"),  alpha = 0.25) + 
    scale_fill_manual(values = c("blue","red"))+
    xlim(-2, 4),
  ncol=1, nrow=2,
  legend = "right",
  common.legend = TRUE
)
