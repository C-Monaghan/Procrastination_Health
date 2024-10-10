# Using 2020 HRS data to create a cross sectional dataset of procrastination and
# health screening data
# ------------------------------------------------------------------------------
rm(list=ls()) # Clearing work space

library(haven)
library(here)
library(dplyr)

path_data <- "./01__Data/01__Raw/"

# Reading in Data --------------------------------------------------------------
# Tracker file containing information on each participant
tracker               <- read_sav(here(path_data, "Tracker.sav"))

# HRS data files (2020 data)
health                <- read_sav(here(path_data, "Health.sav"))
services              <- read_sav(here(path_data, "Health_Services.sav"))
cognition             <- read_sav(here(path_data, "Cognition.sav"))
employment            <- read_sav(here(path_data, "Employement.sav"))
# leave_behind        <- read_sav(here(path_data, "Leave_Behind.sav"))
mod_v                 <- read_sav(here(path_data, "Module_V.sav"))

# Data manipulation ------------------------------------------------------------
# Filtering to procrastination participants and getting their scores
mod_v_filtered  <- mod_v %>%
  filter(!is.na(RV155)) %>%
  select(HHID, PN, RV156:RV167) %>%
  rename(
    P_1 = "RV156",
    P_2 = "RV157",
    P_3 = "RV158",
    P_4 = "RV159",
    P_5 = "RV160",
    P_6 = "RV161",
    P_7 = "RV162",
    P_8 = "RV163",
    P_9 = "RV164",
    P_10 = "RV165",
    P_11 = "RV166",
    P_12 = "RV167"
  )

# Filtering health data
health_filtered <- health %>%
  semi_join(mod_v_filtered, by = c("HHID", "PN")) %>%
  select(HHID, PN, RC114, RC112, RC110, RC113, RC109,
         RC005, RC010, RC018, RC036, RC053,
         RC001) %>%
  rename(
    # Preventative behaviours
    Prostate_exam = "RC114",
    Mammogram = "RC112",
    C_screening = "RC110",
    Pap_smear = "RC113",
    Flu_shot = "RC109",
    # Chronic illnesses
    Hypertension = "RC005",
    Diabetes = "RC010",
    Heart_disease = "RC018",
    Cancer = "RC036",
    Stroke = "RC053",
    Health_assessment = "RC001"
  )

# Filtering services data
services_filtered <- services %>%
  semi_join(mod_v_filtered, by = c("HHID", "PN")) %>%
  select(HHID, PN, RN164, RN001) %>%
  rename(Dental_visit = "RN164", Medicare_coverage = "RN001")

# Filtering cognition data
cognition_filtered <- cognition %>%
  semi_join(mod_v_filtered, by = c("HHID", "PN")) %>%
  select(HHID, PN, RD110:RD117) %>%
  rename(
    D_1 = "RD110",
    D_2 = "RD111",
    D_3 = "RD112",
    D_4 = "RD113",
    D_5 = "RD114",
    D_6 = "RD115",
    D_7 = "RD116",
    D_8 = "RD117",
  )
  
# Filtering employment data 
employement_filtered <- employment %>%
  semi_join(mod_v_filtered, by = c("HHID", "PN")) %>%
  select(HHID, PN, RJ005M1) %>%
  rename(Job_status = "RJ005M1")

# Filtering demographic data
tracker_filtered <- tracker %>%
  semi_join(mod_v_filtered, by = c("HHID", "PN")) %>%
  select(HHID, PN, GENDER, RAGE, DEGREE, RMARST) %>%
  rename(
    Gender = "GENDER",
    Age = "RAGE",
    Education = "DEGREE",
    Marital_status = "RMARST")

# Joining data together --------------------------------------------------------
health_data <- list(
  tracker_filtered, health_filtered, services_filtered,
  cognition_filtered, employement_filtered, mod_v_filtered) %>%
  purrr::reduce(full_join, by = c("HHID", "PN")) %>%
  mutate(ID = seq(1:nrow(mod_v_filtered)), .before = HHID) %>%
  select(!c(HHID, PN))

# Removing other datasets
rm(tracker, tracker_filtered, health, health_filtered, 
   services, services_filtered, cognition, cognition_filtered, 
   employment, employement_filtered, mod_v, mod_v_filtered)

# Handling missing values ------------------------------------------------------
health_data <- health_data %>%
  mutate(
    # Certain values correspond to "Don't know" or "Refused to answer"
    # These values will be changed to be NA
    # Covariates
    Education         = replace(Education, Education == 9, NA),
    Marital_status    = replace(Marital_status, Marital_status == 5, NA),
    Health_assessment = replace(Health_assessment, Health_assessment %in% c(8, 9), NA),
    Medicare_coverage = replace(Medicare_coverage, Medicare_coverage %in% c(8, 9), NA),
    Job_status        = replace(Job_status, Job_status %in% c(98, 99), NA),
    
    # Chronic Illness
    Hypertension      = replace(Hypertension, Hypertension %in% c(8, 9), NA),
    Diabetes          = replace(Diabetes, Diabetes %in% c(8, 9), NA),
    Heart_disease     = replace(Heart_disease, Heart_disease %in% c(8, 9), NA),
    Cancer            = replace(Cancer, Cancer %in% c(8, 9), NA),
    Stroke            = replace(Stroke, Stroke %in% c(8, 9), NA),
    
    # Preventative Behaviours
    Prostate_exam     = replace(Prostate_exam, Prostate_exam %in% c(-8, 8, 9), NA),
    Mammogram         = replace(Mammogram, Mammogram %in% c(-8, 8, 9), NA),
    C_screening       = replace(C_screening, C_screening %in% c(-8, 8, 9), NA),
    Pap_smear         = replace(Pap_smear, Pap_smear %in% c(-8, 8, 9), NA),
    Flu_shot          = replace(Flu_shot, Flu_shot %in% c(-8, 8, 9), NA),
    Dental_visit      = replace(Dental_visit, Dental_visit %in% c(-8, 8, 9), NA),
    
    # Procrastination and depression
    across(c(
      starts_with("P_"), 
      starts_with("D_")), ~ ifelse(. %in% c(-8, 8, 9), NA, .))
  )

# Reversing scoring, recoding and collapsing some items ------------------------
health_data <- health_data %>%
  mutate(
    # Covariates
    Gender              = ifelse(Gender == 1, 0, 1),
    Health_assessment   = recode(Health_assessment, '1' = 5, '2' = 4, '3' = 3, '4' = 2, '5' = 1),
    Medicare_coverage   = ifelse(Medicare_coverage == 5, 0, 1),
    
    # Chronic illnesses
    Hypertension        = ifelse(Hypertension %in% c(4, 5, 6), 0, 1),
    Diabetes            = ifelse(Diabetes %in% c(4, 5, 6), 0, 1),
    Heart_disease       = ifelse(Heart_disease %in% c(4, 5, 6), 0, 1),
    Cancer              = ifelse(Cancer %in% c(4, 5, 6), 0, 1),
    Stroke              = ifelse(Stroke %in% c(4, 5, 6), 0, 1),
    
    # Preventative Behaviours
    Prostate_exam       = ifelse(Prostate_exam == 5, 0, 1),
    Mammogram           = ifelse(Mammogram == 5, 0, 1),
    C_screening         = ifelse(C_screening == 5, 0, 1),
    Pap_smear           = ifelse(Pap_smear == 5, 0, 1),
    Flu_shot            = ifelse(Flu_shot == 5, 0, 1),
    Dental_visit        = ifelse(Dental_visit == 5, 0, 1),
    
    # Depression
    D_1                 = ifelse(D_1 == 5, 0, 1),
    D_2                 = ifelse(D_2 == 5, 0, 1),
    D_3                 = ifelse(D_3 == 5, 0, 1),
    D_4                 = ifelse(D_4 == 5, 1, 0),
    D_5                 = ifelse(D_5 == 5, 0, 1),
    D_6                 = ifelse(D_6 == 5, 1, 0),
    D_7                 = ifelse(D_7 == 5, 0, 1),
    D_8                 = ifelse(D_8 == 5, 0, 1)
  ) %>%
  # Adding some binary dummy coded variables
  # Does the person have a college education
  mutate(
    College          = ifelse(Education %in% c(0, 1, 2), 0, 1), .after = Education) %>%
  # Is the person married
  mutate(
    Married          = ifelse(Marital_status == 1, 1, 0), .after = Marital_status) %>%
  # Is the person widowed
  mutate(
    Widowed          = ifelse(Marital_status == 2, 1, 0), .after = Married) %>%
  # Is the person working
  mutate(Working     = ifelse(Job_status == 1, 1, 0), .after = Job_status) %>%
  # Is the person retired
  mutate(Retired     = ifelse(Job_status == 5, 1, 0), .after = Working) %>%
  # Is the person unable to work because of a disability
  mutate(Diability   = ifelse(Job_status == 4, 1, 0), .after = Retired) %>%
  # Does the person consider their health to be good
  mutate(Good_health = ifelse(Health_assessment %in% c(3, 4, 5), 1, 0), .after = Health_assessment)

# Creating total columns -------------------------------------------------------
health_data <- health_data %>%
  mutate(
    # How many chronic illnesses does someone have
    Total_ci              = rowSums(select(., c(Hypertension:Stroke)), na.rm = TRUE),
    
    # Total Depression and procrastination
    Total_depression      = rowSums(select(., starts_with("D_")), na.rm = TRUE),
    Total_procrastination = rowSums(select(., starts_with("P_")), na.rm = TRUE),
  ) %>%
  # Changing zeros to NA values
  mutate(across(c("Total_procrastination"), ~ ifelse(. %in% 0, NA, .)))

# Exporting --------------------------------------------------------------------
export_path <- "./01__Data/02__Processed/"

writexl::write_xlsx(health_data, path = file.path(export_path, "Health_HRS.xlsx"))
