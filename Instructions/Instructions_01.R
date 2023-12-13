# Instructions file
# Start with a minimal set of variables to match on
# Do not match on exposure

list(
  distVars = c(
    #"Year",
    #"HEIGHT", "WEIGHT", 
    #"BMI",
    "GENHEALTH"#,  factors
    # 'INCOME_PP_LOG10',
    # 'CESD_NEW6PT',
    # 'SYSTOLIC_BP'#, 'PULSE' 'DIASTOLIC_BP',
  ),
  
  exact = c(
    "FEMALE", 
    "RACE_ETH"#,
    #"USBIRTH",
    # "RELIGION",
    # #"MILITARY",
    # "EDU_NEW",'DAD_EDU','MOM_EDU'
  ),
  
  exact_timevarying = c(
    # "ALC_STATUS", # collpased from ever and now
    # 'MARRIAGE',
    # # The following were moved from distance matching (all binary)
    # # Much more missing in light exercise than vig exercise
    # "VIG_EXERCISE", 
    # 'DIABETES','HYPERTENSION','HEARTPROB', # removed CANCER
    # 'SMK_STATUS' # collapsed from ever and now
  ),
  
  exact_HRS_timevarying = list(
    AGEINTERVIEW = 5
  )
  
)



