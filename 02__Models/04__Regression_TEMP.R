rm(list = ls())

library(dplyr)

# Custom functions
source(file.path("./02__Models/00__Functions.R"))

path_data <- "./01__Data/02__Processed/"

# Reading in data
health_data <- readxl::read_xlsx(file.path(path_data, "Health_HRS.xlsx"))

# Fitting binary logistic regressions (with no control) ------------------------
# Defining a vector of health problems
health_problems <- c("Back_pain", "Headache", "Fatigue", 
                     "Alcohol", "Smoker_current", "Blood_pressure", 
                     "Diabetes", "Cholesterol", "Heart_condition")

# Defining a vector of health protective behaviors
health_protection <- c("Prostate_exam", "Mammogram", "Cholesterol_screening",
                       "Pap_smear", "Flu_shot", "Dental_visit_2_years")

# Fitting (health problems)
problem_models_base <- lapply(health_problems, logit_model, 
                                predictor = "Total_procrastination", 
                                data = health_data, type = "base")

# Fitting (health protection)
protection_models_base <- lapply(health_protection, logit_model, 
                                   predictor = "Total_procrastination", 
                                   data = health_data, type = "base")

# health_problems_model[[i]] where i = 1, 2, 3, ..., 9

# health_protection_models[[i]] where i = 1, 2, 3, ..., 6

# Fitting binary logistic regressions (with control) ---------------------------
problem_models_control <- lapply(health_problems, logit_model, 
                                 predictor = "Total_procrastination", 
                                 data = health_data, type = "control")

protection_models_control <- lapply(health_problems, logit_model, 
                                    predictor = "Total_procrastination", 
                                    data = health_data, type = "control")


# Exporting --------------------------------------------------------------------
export_path <- "./02__Models/Results/"

save(health_problem_models, file = file.path(export_path, "RData/01__Problem_Models_BASE.RData"))
save(health_protection_models, file = file.path(export_path, "RData/02__Protection_Models_BASE.RData"))

# OLD CODE
# model <- glm(Blood_pressure ~ Total_procrastination * Heart_condition, family = "binomial", data = health_data)
# summary(model)
# 
# anova(model, test = "Chisq")
