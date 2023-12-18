rm(list = ls())

library(dplyr)

# Custom functions
source(file.path("./02__Models/00__Functions.R"))

path_data <- "./01__Data/02__Processed/"

# Reading in data
health_data <- readxl::read_xlsx(file.path(path_data, "Health_HRS.xlsx"))

# Fitting binary logistic regression -------------------------------------------
# Defining a vector of health problems
health_problems <- c("Back_pain", "Headache", "Fatigue", 
                     "Alcohol", "Smoker_current", "Blood_pressure", 
                     "Diabetes", "Cholesterol", "Heart_condition")

# Fitting a logistic regression for each health problem
health_problem_models <- lapply(health_problems, logit_model, predictor = "Total_procrastination", 
                                data = health_data)

# Models can be viewed by running code such as:
# health_problems_model[[i]] where i = 1, 2, 3, ..., 9

# Defining a vector of health protective behaviors
health_protection <- c("Prostate_exam", "Mammogram", "Cholesterol_screening",
                       "Pap_smear", "Flu_shot", "Dental_visit_2_years")

# Fitting a logistic regression for each health protective behaviour
health_protection_models <- lapply(health_protection, logit_model, predictor = "Total_procrastination", 
                                   data = health_data)

# Models can be viewed by running code such as:
# health_protection_models[[i]] where i = 1, 2, 3, ..., 6

# Exporting --------------------------------------------------------------------
export_path <- "./02__Models/Results/"

save(health_problem_models, file = file.path(export_path, "RData/01__Problem_Models.RData"))
save(health_protection_models, file = file.path(export_path, "RData/02__Protection_Models.RData"))

# OLD CODE
# Running seperate linear regressions (find better way)
# model <- manova(cbind(
#   Back_pain, Headache, Fatigue, Alcohol, Blood_pressure, Diabetes, Cholesterol, Heart_condition) ~ Total_procrastination,
#   data = health_data)
# 
# summary(model)
# 
# model <- glm(Diabetes ~ Total_procrastination, family = "binomial", data = health_data)
# 
# model <- glm(Blood_pressure ~ Total_procrastination, family = "binomial", data = health_data)
# summary(model)
# 
# model <- glm(Blood_pressure ~ Total_procrastination * Heart_condition, family = "binomial", data = health_data)
# summary(model)
# 
# anova(model, test = "Chisq")
