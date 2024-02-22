# Using 2020 HRS data to create a cross sectional dataset of procrastination and
# health data
# ------------------------------------------------------------------------------
rm(list=ls()) # Clearing work space

library(dplyr)

path_data <- "./01__Data/01__Raw/"

# Reading in Data --------------------------------------------------------------
# Tracker file containing information on each participant
tracker <- haven::read_sav(file.path(path_data, "Tracker.sav"))

# HRS(2020 data)
health <- haven::read_sav(file.path(path_data, "Health.sav"))
services <- haven::read_sav(file.path(path_data, "Health_Services.sav"))
cognition <- haven::read_sav(file.path(path_data, "Cognition.sav"))
leave_behind <- haven::read_sav(file.path(path_data, "Leave_Behind.sav"))
mod_v <- haven::read_sav(file.path(path_data, "Module_V.sav"))

# Filtration -------------------------------------------------------------------
# Selecting procrastination participants
mod_v <- mod_v %>%
  filter(!is.na(RV155))

# Matching participants across data files
tracker <- semi_join(tracker, mod_v, by = c("HHID", "PN"))
health <- semi_join(health, mod_v, by = c("HHID", "PN"))
services <- semi_join(services, mod_v, by = c("HHID", "PN"))
cognition <- semi_join(cognition, mod_v, by = c("HHID", "PN"))
leave_behind <- semi_join(leave_behind, mod_v, by = c("HHID", "PN"))

# Creating singular data -------------------------------------------------------
health_data <- cbind(
  tracker[, c(
    "GENDER", "BIRTHYR", "RAGE", "DEGREE", "RMARST")],
  health[, c(
    # General health assessment
    "RC001", 
    # Disabling Pain - Back, Headache, and Fatigue
    "RC146","RC147", "RC148",
    # Lifestyle - Smoking and Alcohol
    "RC128", "RC129", "RC130", "RC117", "RC118",
    # Lifestyle - Physical Activity
    "RC225", "RC224", "RC223",
    # Health Markers - Biological
    "RC005", "RC010", "RC283", "RC036",
    # Health Behaviors - Protective
    "RC114", "RC112", "RC110", "RC113", "RC109")],
  services[, c("RN164")],
  cognition[, c(paste0("RD11", 0:7))],
  leave_behind[, c(paste0("RLB035B", 1:10), paste0("RLB035C", 1:5))],
  mod_v[, c(paste0("RV", 156:167))]
)

# Renaming and creating ID
health_data <- health_data %>%
  rename(
    # TRACKER ------------------------------------------------------------------
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
    # Lifestyle - Smoking and Alcohol
    Alcohol = "RC128",
    Alcohol_week = "RC129",
    Alcohol_amount = "RC130",
    Smoker_current = "RC117",
    Smoker_num = "RC118",
    # Lifestyle - Physical Activity
    Activity_mild = "RC225",
    Activity_moderate = "RC224",
    Activity_vigorous = "RC223",
    # Health Markers - Biological
    Blood_pressure = "RC005",
    Diabetes = "RC010",
    Cholesterol = "RC283",
    Heart_condition = "RC036",
    # Health Behaviours - Protective
    Prostate_exam = "RC114",
    Mammogram = "RC112",
    Cholesterol_screening = "RC110",
    Pap_smear = "RC113",
    Flu_shot = "RC109",
    # Health services
    Dental_visit_2_years = "RN164",
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
    # PROCRASTINATION ----------------------------------------------------------  
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
  ) %>%
  mutate(ID = seq(1:nrow(health_data)), .before = Gender)

# Handling Missing Values ------------------------------------------------------
health_data <- health_data %>%
  mutate(
    Education = replace(Education, Education == 9, NA),
    Marital_status = replace(Marital_status, Marital_status == 5, NA),
    Health_assessment = replace(Health_assessment, Health_assessment %in% c(8, 9), NA),
    Back_pain = replace(Back_pain, Back_pain %in% c(8, 9), NA),
    Headache = replace(Headache, Headache %in% c(8, 9), NA),
    Fatigue = replace(Fatigue, Fatigue %in% c(-8, 8), NA),
    Alcohol = replace(Alcohol, Alcohol %in% c(-8, 3, 8, 9), NA),
    Alcohol_week = replace(Alcohol_week, is.na(Alcohol_week), 0), # Those who don't drink at all have NA for their number of drinks - this is changed to 0
    Alcohol_week = replace(Alcohol_week, Alcohol_week %in% c(-8, 8, 9), NA), # We can then change those who actually drink values to NA
    Alcohol_amount = replace(Alcohol_amount, is.na(Alcohol_amount), 0),
    Alcohol_amount = replace(Alcohol_amount, Alcohol_amount %in% c(98), NA),
    Smoker_num = ifelse(Smoker_current == 5, 0, Smoker_num), # Those who don't smoke at all have NA for their number of cigarettes - this is changed to 0
    Activity_mild = replace(Activity_mild, Activity_mild %in% c(-8, 8, 9), NA),
    Activity_moderate = replace(Activity_moderate, Activity_moderate %in% c(-8, 8, 9), NA),
    Activity_vigorous = replace(Activity_vigorous, Activity_vigorous %in% c(-8, 8, 9), NA),
    Blood_pressure = replace(Blood_pressure, Blood_pressure == 8, NA),
    Diabetes = replace(Diabetes, Diabetes == 8, NA),
    Cholesterol = replace(Cholesterol, Cholesterol == 8, NA),
    Prostate_exam = replace(Prostate_exam, Prostate_exam %in% c(-8, 8, 9), NA),
    Mammogram = replace(Mammogram, Mammogram %in% c(-8, 8, 9), NA),
    Cholesterol_screening = replace(Cholesterol_screening, Cholesterol_screening %in% c(-8, 8, 9), NA),
    Pap_smear = replace(Pap_smear, Pap_smear %in% c(-8, 8, 9), NA),
    Flu_shot = replace(Flu_shot, Flu_shot %in% c(-8, 8, 9), NA),
    Dental_visit_2_years = replace(Dental_visit_2_years, Dental_visit_2_years %in% c(-8, 8, 9), NA),
    across(c(
      starts_with("Procras_"), 
      starts_with("Depression")), ~ ifelse(. %in% c(-8, 8, 9), NA, .))
  )

# Reversing scoring, recoding and collapsing some items ------------------------
health_data <- health_data %>%
  mutate(
    Gender = ifelse(Gender == 1, 0, 1),
    College = ifelse(Education %in% c(0, 1, 2), 0, 1), .after = Education,
    Health_assessment = recode(Health_assessment, '1' = 5, '2' = 4, '3' = 3, '4' = 2, '5' = 1),
    Back_pain = ifelse(Back_pain == 5, 0, 1),
    Headache = ifelse(Headache == 5, 0, 1),
    Fatigue = ifelse(Fatigue == 5, 0, 1),
    Alcohol = ifelse(Alcohol == 5, 0, 1),
    Smoker_current = ifelse(Smoker_current == 5, 0, 1),
    Activity_mild = recode(Activity_mild, '1' = 4, '2' = 3, '3' = 2, '4' = 1, '7' = 5),
    Activity_moderate = recode(Activity_moderate, '1' = 4, '2' = 3, '3' = 2, '4' = 1, '7' = 5),
    Activity_vigorous = recode(Activity_vigorous, '1' = 4, '2' = 3, '3' = 2, '4' = 1, '7' = 5),
    Blood_pressure = ifelse(Blood_pressure %in% c(4, 5, 6), 0, 1),
    Diabetes = ifelse(Diabetes %in% c(4, 5, 6), 0, 1),
    Cholesterol = ifelse(Cholesterol == 5, 0, 1),
    Heart_condition = ifelse(Heart_condition %in% c(4, 5, 6), 0, 1),
    Prostate_exam = ifelse(Prostate_exam == 5, 0, 1),
    Mammogram = ifelse(Mammogram == 5, 0, 1),
    Cholesterol_screening = ifelse(Cholesterol_screening == 5, 0, 1),
    Pap_smear = ifelse(Pap_smear == 5, 0, 1),
    Flu_shot = ifelse(Flu_shot == 5, 0, 1),
    Dental_visit_2_years = ifelse(Dental_visit_2_years == 5, 0, 1),
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
  ) %>%
  mutate(
    Married = ifelse(Marital_status == 1, 1, 0), .after = Marital_status
  )

# Creating total columns -------------------------------------------------------
health_data <- health_data %>%
  mutate(
    Total_depression = rowSums(select(., starts_with("Depression")), na.rm = TRUE),
    Total_stress = rowSums(select(., starts_with("Stress")), na.rm = TRUE),
    Total_anxiety = rowSums(select(., starts_with("Anxiety")), na.rm = TRUE),
    Total_procrastination = rowSums(select(., starts_with("Procras")), na.rm = TRUE),
    Health_problems = rowSums(select(., starts_with(c(
      "Back_pain", "Headache", "Fatigue", "Alcohol", "Smoker_current",
      "Blood_pressure", "Diabetes", "Cholesterol", "Heart_condition"))), na.rm = TRUE),
    Health_behaviours = rowSums(select(., starts_with(c(
      "Prostate_exam", "Mammogram", "Cholesterol_screening", 
      "Pap_smear", "Flu_shot", "Dental_visit_2_years"))), na.rm = TRUE)
  ) %>%
  # Changing zeros to NA values
  mutate(
    across(c(
      "Total_stress", 
      "Total_anxiety", 
      "Total_procrastination"), ~ ifelse(. %in% 0, NA, .))
  )

# Exporting --------------------------------------------------------------------
export_path <- "./01__Data/02__Processed/"

writexl::write_xlsx(health_data, path = file.path(export_path, "Health_HRS.xlsx"))
