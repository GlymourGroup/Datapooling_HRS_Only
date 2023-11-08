# Instructions file
# Contains three lists of variables to match at different stages of analysis

list(
  distVars = c(
    #"Year",
    "HEIGHT", "WEIGHT", "BMI",
    "GENHEALTH", # factors
    'DAD_EDU','MOM_EDU', 
    'INCOME_PP_LOG10',
    'CESD_NEW6PT',
    'SYSTOLIC_BP', 'DIASTOLIC_BP', 'PULSE'
    ),
  
  exact = c(
    "FEMALE", 
    "RACE_ETH",
    "USBIRTH",
    "MOM_EDU_IND",
    "DAD_EDU_IND"),
  
  exact_timevarying = c(
    "RELIGIONCURRENT", "ALCOHOL_EVER",'ALCOHOL_NOW','MILITARY', 'MARRIAGE',
    "EDU_NEW",
    # The following were moved from distance matching (all binary)
    "LIGHT_EXERCISE", "VIG_EXERCISE", 
    'DIABETES','HYPERTENSION','CANCER','HEARTPROB',
    'SMOKE_EVER','SMOKE_NOW'
    ),
  
  exact_HRS_timevarying = list(
    AGEINTERVIEW = 5
  )
  
)



 