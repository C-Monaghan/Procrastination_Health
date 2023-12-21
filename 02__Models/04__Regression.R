rm(list = ls())

library(dplyr)

# Custom functions
source(file.path("./02__Models/00__Functions.R"))

path_data <- "./01__Data/02__Processed/"

# Reading in data
health_data <- readxl::read_xlsx(file.path(path_data, "Health_HRS.xlsx"))

# Defining a vector of health problems
health_problems <- c("Back_pain", "Headache", "Fatigue", 
                     "Alcohol", "Smoker_current", "Blood_pressure", 
                     "Diabetes", "Cholesterol", "Heart_condition")

# Defining a vector of health protective behaviors
health_protection <- c("Prostate_exam", "Mammogram", "Cholesterol_screening",
                       "Pap_smear", "Flu_shot", "Dental_visit_2_years")

# Fitting binary logistic regressions (with no covariate) ----------------------
# Fitting (health problems)
problem_models_base <- lapply(health_problems, logit_model,
                              predictor = "Total_procrastination",
                              data = health_data, type = "base")

# Fitting (health protection)
protection_models_base <- lapply(health_protection, logit_model,
                                 predictor = "Total_procrastination",
                                 data = health_data, type = "base")

# Fitting binary logistic regressions (with covariate) -------------------------
problem_models_control <- lapply(health_problems, logit_model, 
                                 predictor = "Total_procrastination", 
                                 data = health_data, type = "control")

protection_models_control <- lapply(health_protection, logit_model, 
                                    predictor = "Total_procrastination", 
                                    data = health_data, type = "control")

# Plotting model performance
mod_per <- DHARMa::simulateResiduals(problem_models_base[[2]]$model)
plot(mod_per)


# Exporting --------------------------------------------------------------------
export_path <- "./02__Models/Results/"

save(problem_models_base, file = file.path(export_path, "RData/01__Problem_Models_BASE.RData"))
save(protection_models_base, file = file.path(export_path, "RData/02__Protection_Models_BASE.RData"))
save(problem_models_control, file = file.path(export_path, "RData/02__Protection_Models_BASE.RData"))
save(protection_models_control, file = file.path(export_path, "RData/02__Protection_Models_BASE.RData"))


# OLD CODE
# model <- glm(Blood_pressure ~ Total_procrastination * Heart_condition, family = "binomial", data = health_data)
# summary(model)
# 
# anova(model, test = "Chisq")
