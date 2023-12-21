# Pull data from HRS for pooling proof of concept
# We will use the 1992 cohort that participated in 1994
# Split the cohort at 2006

# Set-up ----

# Clear Environment
rm(list=ls())
gc()

## Libraries ----
library("tidyverse")

## Helper Functions ----

user <- dget(file.path("../../Alcohol_Cognition/Helper Code/Users/peter_buto.R"))
dirs <- dget(file.path("../../Alcohol_Cognition/Helper Code/dirs.R"))(user)
fr   <- dget(file.path("../../Alcohol_Cognition/Helper Code/fileReader.R"))(dirs)

# Wu ----

# Import wu-file and merge on to this file from RAW HRS PUBLIC Only
# https://pubmed.ncbi.nlm.nih.gov/22992720/
wu <- fr$load$raw$HRS$wu()

# Create a concatenated hhidpn variable
wu$hhidpn <- as.double(paste0(wu$hhid,wu$pn))
wu <- wu %>%
  select(-hhid, -pn)

# RAND ----
rand<- fr$load$raw$HRS$longitudinal2020() # Large file, takes some time to load

rand <- left_join(rand, wu)

# Pull vars from RAND for all waves available 
desiredVars <- list(
  ## Interview ----
  "CASE_ID_HRS_RA" = "hhidpn",        # Case ID
  "HHID_HRS_RA" = "hhid",             # Household ID
  "PN_HRS_RA" = "pn",                 # Individual ID
  "INTERVIEW_BEGDT_HRS" = "r#iwbeg",  # Interview start date
  "INTERVIEW_ENDDT_HRS" = "r#iwend",  # Interview Date
  "INTERVIEW_ENDMO_HRS" = "r#iwendm", # Interview Month
  "INTERVIEW_ENDYR_HRS" = "r#iwendy", # Interview Year
  "HHNUMBER_HRS" = "h#hhres",         # Number of people in household

  ## Demographics ----
  "AGEINTERVIEW_HRS" = "r#agey_e",    # Age at interview in years
  "AGEINTERVIEW_MO_HRS" = "r#agem_e", # Age at interview in months
  "BIRTHYEAR_HRS_RA" = "rabyear",     # Birth Year
  "BIRTHMONTH_HRS_RA"= "rabmonth",    # Birth Month
  "BIRTHDATE_HRS_RA" = "rabdate",     # Birthdate (default date 15, month 7 if missing, NA if year is missing)
  "SEX_HRS_RA" = "ragender",          # Gender, 1:Male, 2: Female
  "RACEETH_HRS_RA" = "raracem",       # 1: White, 2: Black, 3: Other
  "ETH_HRS_RA" = "rahispan",          # 0: NH, 1: H

  ## Education ----
  "EDU_YEARS_HRS_RA" = "raedyrs",
  "EDU_HIGHEST_HRS_RA" = "raedegrm",
  "EDU_CAT_HRS_RA" = "raeduc",        # 1: lt-hs, 2: GED, 3: HS, 4: Some college, 5: College+

  ## Parent's Education ----
  "MOM_EDU_HRS_RA" = "rameduc",       # Mother's years of education
  "DAD_EDU_HRS_RA" = "rafeduc",       # Father's years of education

  ## Geography ----
  "BIRTHCOUNTRY_HRS_RA" = "rabplace", # US regions, 11: "Not US/inc US Terr" option
  "censusregion_HRS" = "r#cenreg",    # Census Region: NE, MW, S, W, Other
  "censusdivision_HRS" = "r#cendiv",  # Census Division:

  ## Marriage ----
  "MARRIAGE_HRS" = "r#mstat",         # 1:married, 2:spouse absent, 3:partnered, 4:separated, 5:divorced, 6:sep/divorced, 7:widowed, 8:never
  "partnership_HRS"="r#mpart",        # 0:No,1:Yes
  "marriage_wo_part"="r#mstath",      # Marriage without partnership? unsure how it's different from mstat

  ## Religion & Military ----
  "RELIGION_HRS_RA"="rarelig",        # Religion (likely current) 1:Protestant,2:Catholic,3:Jewish,4:None,5:Other
  "MILITARY_HRS" = 'ravetrn',         # Veteran status 0:No,1:yes - have you ever served? (includes people currently serving)

  ## Biometrics ----
  "BMI_REPORTED_HRS"   ="r#bmi",   # Using reported data
  "BMI_HRS"            ="r#pmbmi", # Physical measure of BMI from waves 8-14
  "HEIGHT_REPORTED_HRS"="r#height",# Self-reported height, all waves (m) 
  "HEIGHT_HRS"         ="r#pmhght",# Physical measure of height from waves 8-14 (m)
  "WEIGHT_REPORTED_HRS"="r#weight",# Self-reported weight, all waves (kg) 
  "WEIGHT_HRS"         ="r#pmwght",# Physical measure of weight from waves 8-14 (kg)
  "SYSTOLIC_BP_HRS"    ="r#bpsys", # Physical measure of SBP from waves 8-14
  "DIASTOLIC_BP_HRS"   ="r#bpdia", # Physical measure of DBP from waves 8-14
  "PULSE_HRS"          ="r#bppuls",# Physical measure of pulse
  "BP_POSITION_HRS"    ="r#bppos", # Position during BP measurement
  
  ## Physical Tests ----
  "BREATHING_TEST_HRS" ="r#puff",  # Breathing Test (puff)
  "BREATHING_POS_HRS"  ="r#puffpos",#Position during breathing test
  "GRIP_STRENGTH_HRS"  ="r#grp",   # Hand grip strength max measurement
  "DOMINANT_HAND_HRS"  ="r#grpdom",# Dominant hand
  "GRIP_STR_LEFT_HRS"  ="r#grpl",  # hand grip strength - left
  "GRIP_STR_RIGHT_HRS" ="r#grpr",  # hand grip strength - right
  "BAL_SEMI_TIME_HRS"  ="r#balsemi",# semi tandem bal test time
  "BAL_SEMI_MVMT_HRS"  ="r#balsemic",#semi tandem bal test - comp mvmts
  "BAL_SIDE_TIME_HRS"  ="r#balsbs",# side-by-side balance test time
  "BAL_SIDE_MVMT_HRS"  ="r#balsbsc",#side-by-side balance test comp mvmts
  "BAL_FULL_TIME_HRS"  ="r#balful",# full tandem bal test time
  "BAL_FULL_MVMT_HRS"  ="r#balfulc",#full tandem bal test - comp mvmts
  "BAL_FULL_COMP_HRS"  ="r#balfult",#full tandem bal test - completed
  "WALK_TEST_TIME_HRS" ="r#timwlk",# timed walk test time
  "WALK_TEST_AID_HRS"  ="r#timwlka",#timed walk test aid used
  
  ## Mental health ----
  "CESD_HRS" = "r#cesd",
  "cesd_depressed_HRS" = "r#depres",
  "cesd_effort_HRS" = "r#effort",
  "cesd_restless_HRS"= "r#sleepr",
  "cesd_happy_HRS" = "r#whappy",
  "cesd_lonely_HRS" = "r#flone",
  "cesd_enjoyed_HRS" = "r#enlife",
  "cesd_sad_HRS" = "r#fsad",
  "cesd_going_HRS" = "r#going",

  ## Health ----
  "SELF_HEALTH_HRS" = "r#shlt",  # self-reported health status (bad, excellent)
  "HYPERTENSION_HRS" = "r#hibp", # self-report high bp
  "DIABETES_HRS" = "r#diab",     # self-report diabetes
  "CANCER_HRS" = "r#cancr",      # self-report cancer
  "HEARTPROB_HRS" = "r#heart",   # self-report heart problem

  "Memory_problems_HRS" = "r#memry",
  "Alzheimers_HRS" = "r#alzhe",
  "Dementia_HRS" = "r#demen",

  # Health conditions - ever
  "HYPERTENSION_HRS_EVER" = "r#hibpe",
  "DIABETES_HRS_EVER" = "r#diabe",
  "CANCER_HRS_EVER" = "r#cancre",
  "HEARTPROB_HRS_EVER" = "r#hearte",

  # included dispute flags for above to examine difference by dispute status
  "BP_DISPUTE_HRS" = "r#hibpf",
  "DM_DISPUTE_HRS" = "r#diabf",
  "CA_DISPUTE_HRS" = "r#cancrf",
  "MI_DISPUTE_HRS" = "r#heartf",

  "Memory_problems_ever_HRS" = "r#memrye",
  "Alzheimers_ever_HRS" = "r#alzhee",
  "Dementia_ever_HRS" = "r#demene",

  ## SES ----

  # WOULD BE INTERESTED IN CHILDHOOD SES & PRE-RETIREMENT INCOME (TO TRY TO MATCH NLSY INCOME POST-PARENT)
  "HH_INCOME_HRS" = "h#itot",  # Total household income (respondent + Spouse)
  "IND_EARNING_HRS"="r#iearn", # Individual Earnings
  "POVERTY_STAT_HRS"="h#inpov",# Poverty Status (waves 6 and on)
  "HH_RESIDENTS_HRS"="h#hhres",# Number of people in Household

  # Consider other variables for future analysis:
  ## Father's income in childhood


  ### Behaviors ----
  "vig_activities_HRS" = "r#vigact", #3+ times a week, waves 1-6; 0:no,1:yes
  "vig_activities_old_freq_HRS_w1" = "r1vgactf", # w1 only; 1: 3+/wk, 2: 1-2/wk, 3: 1-3/mo, 4: <1/mo, 5: never
  "vig_activities_freq_HRS" = "r#vgactx", # Frequency of vig activities (waves7-14); 1:everyday, 2:>1/week,3:1/week,4:1-3/month,5:never
  "mod_activities_HRS" = "r#mdactx", # freq of moderate activities (waves 7-14); 1:everyday, 2:>1/week,3:1/week,4:1-3/month,5:never
  "lig_activities_HRS" = "r#ltactx", # freq of light activities (Waves 7-14); 1:everyday, 2:>1/week,3:1/week,4:1-3/month,5:never
  
  "lig_activities_housework_HRS_w1" = "r1hswrkf", # freq of heavy house work (light act), w1, 1: 3+/wk, 2: 1-2/wk, 3: 1-3/mo, 4: <1/mo, 5: never
  "lig_activities_all_HRS_w1" = "r1ltactf", # freq of lite pa, w1 only, 1: 3+/wk, 2: 1-2/wk, 3: 1-3/mo, 4: <1/mo, 5: never
  
  "vig_pa_freq_HRS_w2" = "r2vgactn", # num of pa, wave2 only, continuous
  "vig_pa_time_HRS_w2" = "r2vgactp", # period for pa for previous question, 02: per wk, 04: per mo, 06: per yr, 07: other, 11: per day
  
  "lig_pa_freq_HRS_w2" = "r2ltactn", # num of pa, wave2 only, cont.
  "lig_pa_time_HRS_w2" = "r2ltactp", # period for PA for previous question, 02: per wk, 04: per mo, 06: per yr, 07: other, 11: per day

  "SMOKE_EVER_HRS" = "r#smokev", # ever smoke 0:no,1:yes
  "SMOKE_NOW_HRS"  = "r#smoken", # Smoke now  0:no,1:yes

  ### Alcohol: ----
  "alc_ever" = "r#drink", # do you ever drink alcohol? 0:No, 1:Yes
  "alc_range"= "r#drinkr",# range of drinks per day, waves 1-2; 0:0, 1:<1, 2:1-2, 3:3-4, 4:5+
  "alc_nweek"= "r#drinkd",# number of days/week, waves 3-14, (1-7)
  "alc_ndrink"="r#drinkn",# number of drink/day (when drink), waves 3-14 (continuous, 99 drinks all day)


  ## Cognition ----

  # Admin
  # indicate whether and/or which cognitive functioning measures are provided
  "COG_IMP_STATUS_HRS" = "r#status", # Imputation eligibility status
  "COG_TICS_ASKED_HRS" = "r#notics", # not asked tics items
  "COG_VOC_ASKED_HRS"  = "r#novoc",  # not asked vocabulary

  # Self Reported Memory
  "COG_SELF_REPORT_HRS"= "r#slfmem", # self rated memory (range 1:5)
  "COG_SELF_REPORT_imp_HRS" = "r#fslfme", # self-rated memory imputation flag (0/1)

  "COG_COMPAR_PAST_HRS"= "r#pstmem", # memory compared to past (range 1:3)
  "COG_COMPAR_PAST_imp_HRS"="r#fpstme", # Imputation flag for mem compared to past

  # Number Series Score
  # No corresponding imputation flag variable, though standard error is available
  "COG_NUM_SERIES_HRS" = "r#nsscre", # Calculated number series score (waves 10-13)

  # Immediate word Recall:
  # counts of the number of words  recalled correctly
  # 20 nouns, recalled in any order:
  "COG_RECALL_HRS_W1" = "r1imrc20",  # immediate word recall, wave 1 only
  "COG_RECALL_HRS_W2_AHD" = "r2aimr10",  # IWC -AHD93, wave 2 only
  "COG_RECALL_HRS_W2_HRS" = "r2himr20",  # IWC -hrs94, wave 2 only

  # Randomly assigned TO a list, with a different assignment each interview
  "COG_RECALL_HRS"    = "r#imrc",    # IWC, waves 3-13
  "COG_RECALL_imp_HRS"= "r#fimrc",   # Imputation flag for immediate word recall

  # Delayed Word Recall
  # See comments for immediate word recall
  # Delay is about 5 minutes with survey questions
  "COG_DELAYED_HRS_W1" = "r1dlrc20",
  "COG_DELAYED_HRS_W2_AHD" = "r2adlr10",
  "COG_DELAYED_HRS_W2_HRS" = "r2hdlr20",

  "COG_DELAYED_HRS" = "r#dlrc",
  "COG_DELAYED_imp_HRS" = "r#fdlrc", # Imputation flag for dlrc

  # Serial 7s, number of correct subtractions in the serial 7s test (0-5)
  # correct subtractions are based on the prior number given,
  "COG_SERIAL7_HRS" = "r#ser7", # Waves 2-13
  "COG_SERIAL7_imp_HRS" = "r#fser7", # imp flag for serial 7

  # Backwards Counting
  # Two points are given if successful on the first try, 
  # one if successful on the second, and zero if not successful on either try.
  "COG_BACKWARDS_HRS" = "r#bwc20", # 0:incorrect, 1:correct 2nd, 2: correct 1st
  "COG_BACKWARDS_imp_HRS"="r#fbwc20", # imp flag

  "COG_BACKWARDS_86_HRS"="r#bwc86", # w3-6 0:incorrect, 1: 2nd, 2: 1st
  "COG_BACKWARDS_86_imp_HRS"="r#bwc86", # imp flag

  # Date-naming
    # able to report today’s date correctly
    # coded "1" for a correct answer and "0" for an incorrect
  "COG_MONTH_HRS" = "r#mo", # month, waves 2-13
  "COG_DATE_HRS" = "r#dy", # day of month, waves 2-13
  "COG_YEAR_HRS" = "r#yr", # Year, waves 2-13
  "COG_DAY_HRS"  = "r#dw", # day of week, waves 2-13

    # Corresponding imputation flags for above
  "COG_MONTH_imp_HRS" = "r#fmo",
  "COG_DATE_imp_HRS" = "r#fdy",
  "COG_YEAR_imp_HRS" = "r#yr",
  "COG_DAY_imp_HRS"  = "r#dw",

  # Object Naming
  #  correctly name cactus and scissors based on a verbal description
  "COG_OBJECT_SCISSORS_HRS" = 'r#scis', # waves 2-13
  "COG_OBJECT_CACTUS_HRS" = 'r#cact',
  #  imputation flags:
  "COG_OBJECT_SCISSORS_imp_HRS" = 'r#fscis',
  "COG_OBJECT_CACTUS_imp_HRS" = 'r#fcact',

  # Pres/vice pres
  # correctly name the current president and vice-president
  "COG_PRESIDENT_HRS" = 'r#pres', # waves 2-13
  "COG_VICEPRES_HRS"  = 'r#vp',
  #  Imputation flags:
  "COG_PRESIDENT_imp_HRS" = 'r#fpres',
  "COG_VICEPRES_imp_HRS"  = 'r#fvp',

  # Vocabulary
  # scores are perfectly correct (2), partially correct (1), and incorrect (0)
  "COG_VOCAB_HRS" = "r#vocab", # waves 3-13, score 0-10
  "COG_VOCAB_imp_HRS"="r#fvocab", # imputation flag


  # Cognition Summary Scores
  "COG_WORD_TOTALw1_HRS" = "r1tr40", # wave 1 total word recall summary score
  "COG_WORD_TOTALw2_HRS" = "r2atr20",# wave 2 total word recall
  "COG_WORD_TOTALw2h_HRS"= "r2htr40",# Wave 2h total word recall
  "COG_WORD_TOTAL_HRS"   = "r#tr20", # waves 3-13 total word recall

  "COG_MENTALSTATUSw2_HRS"="r2amstot",# Wave 2 AHD MENTAL STATUS SUMMARY SCORE
  "COG_SCOREw2_HRS"      = "r2acgtot",# Wave 2 AHD Total cognition score
  "COG_MENTAL_TOTAL_HRS" = "r#mstot", # Waves 3-13
  "COG_COG_TOTAL_HRS"    = "r#cogtot",# Waves 3-13

  # Proxy
  "PROXY_INTERV_HRS" = "r#proxy",  # 1 if proxy
  "PROXY_RATING_HRS" = "r#prmem",  # (likert 1(excellent)-5(poor), missing w1)
  "PROXY_CHANGE_HRS" = "r#prchmem",# (likert 1(better)-3(Worse), no wave 1)

  # Wu-Measures
  "WU_SCORE_HRS" = "memimp:",
  "WU_DEMENTIA_HRS"="dementpimp:"
)

# Extract Variable Names from rand file
cols <- colnames(rand)

# initiate empty data frame to store old and new variable names
varMap <- data.frame(old=character(),new=character(),stringsAsFactors = FALSE)

wu_years <- c("1995", "1996", "1998", "2000", "2002", "2004",
              "2006", "2008", "2010", "2012", "2014", "2016")

wave_years<-c("1992", "1993", "1995", "1998",
              "2000", "2002", "2004", "2006", "2008", 
              "2010", "2012", "2014", "2016", "2018",
              "2020")

# Cycle through each variable of interest
for(newVar in names(desiredVars)){
  # Start a count:
  any <- 0

  # If variable name contains '#':
  if(str_detect(desiredVars[[newVar]], "#")){
    for(i in 1:15){                               # Cycle through waves (1-15)
      oldVar <- gsub("#",i,desiredVars[[newVar]]) # and substitute '#' with wave

      # If the newly created variable (replacing # with i) is found,
      if(oldVar %in% cols){
        varMap[nrow(varMap)+1,] <- c(oldVar,paste0(newVar,"_",i)) # rename
        any <- any + 1                                            # increment
      }
    }

  # Otherwise, if variable name contains ':'
  } else if(str_detect(desiredVars[[newVar]], ":")) {
    for(j in wu_years){
      oldVar <- gsub(":",j,desiredVars[[newVar]]) # and substitute ':' with year

      # If the newly created variable (replacing # with j) is found,
      if(oldVar %in% cols){
        varMap[nrow(varMap)+1,] <- c(oldVar,paste0(newVar,"_",j)) # rename
        any <- any + 1                                            # increment
      }
  }
    }else{
    varMap[nrow(varMap)+1,] <- c(desiredVars[[newVar]],newVar) # rename
    any <- any + 1
  }
  #Make sure the variable was found
  if(any == 0){
    stop(paste0("Error: variable not found for ",newVar))
  }
}

d_out <- rand[,varMap$old]
colnames(d_out) <- varMap$new

# Tracker Data ----
tracker <- fr$load$raw$HRS$tracker2020()
tracker <- tracker %>% 
  select(HHID, PN, FIRSTIW) %>% 
  mutate(CASE_ID_HRS_RA = as.double(paste0(HHID,PN))) %>%
  select(CASE_ID_HRS_RA, FIRSTIW)

d_out <- left_join(d_out, tracker)

# Biomarker Data ----
biomarker06 <- haven::read_sav("~/Documents/HRS/biomkr06/biomk06bl_r.sav")
biomarker06 <- biomarker06 %>% 
  mutate(CASE_ID_HRS_RA = as.numeric(paste0(HHID, PN)),
         HbA1c_YEAR = 2006) %>%
  rename(HbA1c = KA1C_ADJ) %>% 
  select(CASE_ID_HRS_RA, HbA1c, HbA1c_YEAR)

biomarker08 <- haven::read_sav("~/Documents/HRS/biomkr08/biomk08bl_r.sav")
biomarker08 <- biomarker08 %>% 
  mutate(CASE_ID_HRS_RA = as.numeric(paste0(HHID, PN)),
         HbA1c_YEAR = 2008) %>%
  rename(HbA1c = LA1C_ADJ) %>% 
  select(CASE_ID_HRS_RA, HbA1c, HbA1c_YEAR)

biomarker <- rbind(biomarker06, biomarker08)

d_out <- left_join(d_out, biomarker)

# Subset Data ----
d_out <- d_out %>% 
  # Participants must have started in 1992
  filter(FIRSTIW == 1992,
         # and have 1994 wave data
         !is.na(INTERVIEW_BEGDT_HRS_2), # exposure first measured
         !is.na(INTERVIEW_BEGDT_HRS_8), # Mediator wave
         !is.na(INTERVIEW_BEGDT_HRS_14))# Outcome wave

# Write-out ----
# If we save as RDS, the variables remain as haven type :(
write.csv(d_out, "../../DP_HRS_Only/HRS_pulled.csv", row.names=FALSE)
