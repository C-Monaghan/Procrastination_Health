rm(list=ls()) # Clearing work space

library(dplyr)

path_data <- "./01__Data/01__Raw/"

# Reading in Data -------------------------------------------------------------
# Tracker file containing information on each participant
tracker <- haven::read_sav(file.path(path_data, "Tracker.sav"))

# HRS(2020 data)
health <- haven::read_sav(file.path(path_data, "Health_2020.sav"))
cognition <- haven::read_sav(file.path(path_data, "Cognition_2020.sav"))
leave_behind <- haven::read_sav(file.path(path_data, "Leave_Behind_2020.sav"))
mod_v <- haven::read_sav(file.path(path_data, "Module_V_2020.sav"))

# Filtration -------------------------------------------------------------------
# Selecting procrastination participants
mod_v <- mod_v %>%
  filter(!is.na(RV155))

# Matching participants across data files
tracker <- semi_join(tracker, mod_v, by = c("HHID", "PN"))
health <- semi_join(health, mod_v, by = c("HHID", "PN"))
cognition <- semi_join(cognition, mod_v, by = c("HHID", "PN"))
leave_behind <- semi_join(leave_behind, mod_v, by = c("HHID", "PN"))

# Creating singular data -------------------------------------------------------
health_data <- cbind(
  tracker[, c("HHID", "PN", "GENDER", "BIRTHYR", "RAGE", "DEGREE", "RMARST")],
  health[, c(
    # General Health Problems
    "RC001", 
    # Disabling Pain(s)
    "RC146","RC147", "RC148",
    # Health behaviour - Smoking and Alcohol
    "RC128", "RC129", "RC116", "RC117", "RC118",
    # Health behaviour - Physical Activity
    "RC225", "RC224", "RC223", "RC301", "RC302", "RC303", "RC304",
    
    "RC001", "RC005", "RC010", "RC283","RC030", "RC031", "RC036", "RC039", 
             "RC038", "RC040", "RC041", "RC260", "RC053", "RC070", "RC272", "RC273", 
             "RC240", "Rc237",
             "RC146", "RC147", "RC148")],
  cognition[, c(paste0("RD11", 0:7))],
  leave_behind[, c(paste0("RLB035B", 1:10), paste0("RLB035C", 1:5))],
  mod_v[, c(paste0("RV", 156:167))]
)


# Renaming
health_data <- health_data %>%
  rename(
    # TRACKER ------------------------------------------------------------------
    HHID = "HHID", 
    ID = "PN",
    Gender = "GENDER",
    Birth_year = "BIRTHYR",
    Education = "DEGREE",
    Marital_status = "RMARST",
    Age = "RAGE",
    # HEALTH QUESTIONS ---------------------------------------------------------
    # General Health
    Health_assessment = "RC001",
    # Disabling Pain(s)
    Back_pain = "RC146",
    Headache = "RC147",
    Fatigue = "RC148",
    # Health behavior - Smoking and Alcohol
    Alcohol = "RC128",
    Alcohol_number = "RC129",
    Smoker_ever = "RC116",
    Smoker_current = "RC117",
    Smoker_num = "RC118",
    # Health behaviour - Physical Activity
    Activity_mild = "RC225",
    Activity_moderate = "RC224",
    Activity_vigorous = "RC223",
    On_feet_min = "RC301",
    On_feet_hr = "RC302",
    Sitting_min = "RC303",
    Sitting_hr = "RC304",
    
    
    
    Blood_pressure = "RC005",
    Diabetes = "RC010",
    Cholesterol = "RC283",
    Lung_disease = "RC030",
    Lung_disease_status = "RC031",
    Heart_condition = "RC036",
    Heart_condition_status = "RC039",
    Seen_doctor_heart_condition = "RC038",
    Heart_attack_recent = "RC040", # HA in last 2 years
    Seen_doctor_heart_attack = "RC041",
    Angia = "RC260",
    Stroke = "RC053",
    Arthritis = "RC270",
    Alzheimers = "RC272",
    Dementia = "RC273",
    Shingles = "RC240",
    # MENTAL HEALTH ------------------------------------------------------------
    # Depression
    Depression_1 = "RD110",
    Depression_2 = "RD111",
    Depression_3 = "RD112",
    Depression_4 = "RD113",
    Depression_5 = "RD114",
    Depression_6 = "RD115",
    Depression_7 = "RD116",
    Depression_8 = "RD117",
    # Stress
    Stress_1 = "RLB035B1",
    Stress_2 = "RLB035B2",
    Stress_3 = "RLB035B3",
    Stress_4 = "RLB035B4",
    Stress_5 = "RLB035B5",
    Stress_6 = "RLB035B6",
    Stress_7 = "RLB035B7",
    Stress_8 = "RLB035B8",
    Stress_9 = "RLB035B9",
    Stress_10 = "RLB035B10",
    # Anxiety
    Anxiety_1 = "RLB035C1",
    Anxiety_2 = "RLB035C2",
    Anxiety_3 = "RLB035C3",
    Anxiety_4 = "RLB035C4",
    Anxiety_5 = "RLB035C5",
    # Procrastination
    Procras_1 = "RV156",
    Procras_2 = "RV157",
    Procras_3 = "RV158",
    Procras_4 = "RV159",
    Procras_5 = "RV160",
    Procras_6 = "RV161",
    Procras_7 = "RV162",
    Procras_8 = "RV163",
    Procras_9 = "RV164",
    Procras_10 = "RV165",
    Procras_11 = "RV166",
    Procras_12 = "RV167"
  )

# Handling Missing Values ------------------------------------------------------
health_data <- health_data %>%
  mutate(
    Education = replace(Education, Education == 9, NA),
    Marital_status = replace(Marital_status, Marital_status == 5, NA),
    Health_assessment = replace(Health_assessment, Health_assessment %in% c(8, 9), NA),
    Blood_pressure = replace(Blood_pressure, Blood_pressure == 8, NA),
    Diabetes = replace(Diabetes, Diabetes == 8, NA),
    Cholesterol = replace(Cholesterol, Cholesterol == 8, NA),
    Lung_disease = replace(Lung_disease, Lung_disease %in% c(-8, 8), NA),
    Lung_disease_status = replace(Lung_disease_status, Lung_disease_status %in% c(4, 6), NA),
    Heart_condition = replace(Heart_condition, Heart_condition %in% c(8, 9), NA),
    Heart_condition_status = replace(Heart_condition_status, Heart_condition_status == 8, NA),
    Heart_attack_recent = replace(Heart_attack_recent, Heart_attack_recent == 8, NA),
    Seen_doctor_heart_attack = replace(Seen_doctor_heart_attack, Seen_doctor_heart_attack == 8, NA),
    Alzheimers = replace(Alzheimers, Alzheimers %in% c(-8, 8), NA),
    Dementia = replace(Dementia, Dementia == 8, NA),
    Back_pain = replace(Back_pain, Back_pain %in% c(8, 9), NA),
    Headache = replace(Headache, Headache %in% c(8, 9), NA),
    Fatigue = replace(Fatigue, Fatigue %in% c(-8, 8), NA),
    across(c(starts_with("Procras_")), ~ ifelse(. %in% c(-8, 8, 9), NA, .))
  )

# Reversing scoring and collapsing some items ----------------------------------
health_data <- health_data %>%
  mutate(
    Gender = ifelse(Gender == 1, 0, 1),
    Education = ifelse(Education %in% c(0, 1, 2), 0, 1),
    Marital_status = ifelse(Marital_status == 1, 1, 0),
    Health_assessment = ifelse(Health_assessment %in% c(1, 2, 3, 4), 1, 0),
    Blood_pressure = ifelse(Blood_pressure %in% c(4, 5, 6), 0, 1),
    Diabetes = ifelse(Diabetes %in% c(4, 5, 6), 0, 1),
    Cholesterol = ifelse(Cholesterol == 5, 0, 1),
    Lung_disease = ifelse(Lung_disease %in% c(4, 5, 6), 0, 1),
    Heart_condition = ifelse(Heart_condition %in% c(4, 5, 6), 0, 1),
    Seen_doctor_heart_condition = ifelse(Seen_doctor_heart_condition == 5, 0, 1),
    Heart_attack_recent = ifelse(Heart_attack_recent == 5, 0, 1),
    Seen_doctor_heart_attack = ifelse(Seen_doctor_heart_attack == 5, 0, 1),
    Alzheimers = ifelse(Alzheimers == 5, 0, 1),
    Dementia = ifelse(Dementia == 5, 0, 1),
    Back_pain = ifelse(Back_pain == 5, 0, 1),
    Headache = ifelse(Headache == 5, 0, 1),
    Fatigue = ifelse(Fatigue == 5, 0, 1),
    Depression_1 = ifelse(Depression_1 == 5, 0, 1),
    Depression_2 = ifelse(Depression_2 == 5, 0, 1),
    Depression_3 = ifelse(Depression_3 == 5, 0, 1),
    Depression_4 = ifelse(Depression_4 == 5, 1, 0),
    Depression_5 = ifelse(Depression_5 == 5, 0, 1),
    Depression_6 = ifelse(Depression_6 == 5, 1, 0),
    Depression_7 = ifelse(Depression_7 == 5, 0, 1),
    Depression_8 = ifelse(Depression_8 == 5, 0, 1),
    Stress_4 = recode(Stress_4, '1' = 5, '2' = 4, '3' = 3, '4'= 2, '5' = 1),
    Stress_5 = recode(Stress_5, '1' = 5, '2' = 4, '3' = 3, '4'= 2, '5' = 1),
    Stress_7 = recode(Stress_7, '1' = 5, '2' = 4, '3' = 3, '4'= 2, '5' = 1),
    Stress_8 = recode(Stress_8, '1' = 5, '2' = 4, '3' = 3, '4'= 2, '5' = 1),
  )

# Creating total columns -------------------------------------------------------
health_data <- health_data %>%
  mutate(
    Total_procrastination = rowSums(select(., starts_with("Procras")), na.rm = TRUE),
    Total_stress = rowSums(select(., starts_with("Stress")), na.rm = TRUE),
    Total_anxiety = rowSums(select(., starts_with("Anxiety")), na.rm = TRUE),
    Total_depression = rowSums(select(., starts_with("Depression")), na.rm = TRUE),
    HRP_count = rowSums(select(., starts_with(c(
      "Blood_pressure", "Diabetes", "Cholesterol", "Lung_disease", 
      "Heart_condition", "Back_pain", "Headache", "Fatigue"))), na.rm = TRUE)
  ) %>%
  # Changing zeros to NA values
  mutate(
    across(c("Total_procrastination", "Total_stress", "Total_anxiety"), ~ ifelse(. %in% 0, NA, .))
  )

# Exporting --------------------------------------------------------------------
export_path <- "./01__Data/02__Processed/"

writexl::write_xlsx(health_data, path = file.path(export_path, "Health_HRS.xlsx"))
