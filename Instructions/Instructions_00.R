# Instructions file
# Contains three lists of variables to match at different stages of analysis

list(
  distVars = c(
    #"Year",
    "HEIGHT", "WEIGHT", "BMI",
    "GENHEALTH", # factors
    'INCOME_PP_LOG10',
    'CESD_NEW6PT',
    'SYSTOLIC_BP', 'DIASTOLIC_BP', 'PULSE'
    ),
  
  exact = c(
    "FEMALE", 
    "RACE_ETH",
    "USBIRTH",
    "RELIGION",
    "MILITARY",
    "EDU_NEW",'DAD_EDU','MOM_EDU'),
  
  exact_timevarying = c(
    #"RELIGIONCURRENT", 
    "ALCOHOL_EVER",'ALCOHOL_NOW',#'MILITARY', 
    'MARRIAGE',
    # The following were moved from distance matching (all binary)
    "LIGHT_EXERCISE", "VIG_EXERCISE", 
    'DIABETES','HYPERTENSION','CANCER','HEARTPROB',
    'SMOKE_EVER','SMOKE_NOW'
    ),
  
  exact_HRS_timevarying = list(
    AGEINTERVIEW = 5
  )
  
)



 