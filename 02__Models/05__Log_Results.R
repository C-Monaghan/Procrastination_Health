rm(list = ls())

library(sjPlot)
library(ggplot2)

# Custom functions
source(file.path("./02__Models/00__Functions.R"))

# Loading regression results
load(file = "02__Models/Results/RData/01__Problem_Models_BASE.RData")
load(file = "02__Models/Results/RData/02__Protection_Models_BASE.RData")
load(file = "02__Models/Results/RData/03__Problem_Models_CONTROL.RData")
load(file = "02__Models/Results/RData/04__Protection_Models_CONTROL.RData")

# Defining a vector of health problems
health_problems <- c(
  "Back Pain", "Headache", "Fatigue", "Alcohol", "Smoker Current", 
  "Blood Pressure", "Diabetes", "Cholesterol", "Heart Condition")

# Defining a vector of health protective behaviors
health_protection <- c(
  "Prostate Exam", "Mammogram", "Cholesterol Screening",
  "Pap Smear", "Flu Shot", "Dental Visit (2yrs)")

# Extracting relevant GLM results ----------------------------------------------
health_problem_base <- process_glm_results(problem_models_base)
health_problem_control <- process_glm_results(problem_models_control)
health_protection_base <- process_glm_results(protection_models_base)
health_protection_control <- process_glm_results(protection_models_control)

# Changing to dataframe
health_problem_base <- as.data.frame(health_problem_base)
health_problem_control <- as.data.frame(health_problem_control)
health_protection_base <- as.data.frame(health_protection_base)
health_protection_control <- as.data.frame(health_protection_control)

# Assigning rownames
rownames(health_problem_base) <- health_problems
rownames(health_problem_control) <- health_problems
rownames(health_protection_base) <- health_protection
rownames(health_protection_control) <- health_protection

# Creating HTML Tables ---------------------------------------------------------
# Base models
tab_model(
  problem_models_base[[1]]$model, problem_models_base[[2]]$model,
  problem_models_base[[3]]$model, problem_models_base[[4]]$model,
  problem_models_base[[5]]$model, problem_models_base[[6]]$model,
  problem_models_base[[7]]$model, problem_models_base[[8]]$model,
  problem_models_base[[9]]$model,
  pred.labels = c("(Intercept)", "Procrastination"),
  title = "Binary Logistic Regression Analysis Predicting Likelihood of Health Problems",
  dv.labels = health_problems, 
  collapse.ci = TRUE, p.style = "stars",
  file = file.path("./02__Models/Results/Tables/01__BASE_Problem_Table.html")
)

tab_model(
  protection_models_base[[1]]$model, protection_models_base[[2]]$model,
  protection_models_base[[3]]$model, protection_models_base[[4]]$model,
  protection_models_base[[5]]$model, protection_models_base[[6]]$model,
  pred.labels = c("(Intercept)", "Procrastination"),
  dv.labels = health_protection, 
  title = "Binary Logistic Regression Analysis Predicting Likelihood of Health Protective Behaviours",
  collapse.ci = TRUE, p.style = "stars",
  file = file.path("./02__Models/Results/Tables/02__BASE_Protection_Table.html")
)

# Control Models
tab_model(
  problem_models_control[[1]]$model, problem_models_control[[2]]$model,
  problem_models_control[[3]]$model, problem_models_control[[4]]$model,
  problem_models_control[[5]]$model, problem_models_control[[6]]$model,
  problem_models_control[[7]]$model, problem_models_control[[8]]$model,
  problem_models_control[[9]]$model,
  pred.labels = c("Intercept", "Procrastination", "Depression", "Procrastination x Depression"),
  title = "Binary Logistic Regression Analysis Predicting Likelihood of Health Problems (with Control)",
  dv.labels = health_problems, 
  collapse.ci = TRUE, p.style = "stars",
  file = file.path("./02__Models/Results/Tables/03__CONTROL_Problem_Table.html")
)

tab_model(
  protection_models_control[[1]]$model, protection_models_control[[2]]$model,
  protection_models_control[[3]]$model, protection_models_control[[4]]$model,
  protection_models_control[[5]]$model, protection_models_control[[6]]$model,
  pred.labels = c("Intercept", "Procrastination", "Depression", "Procrastination x Depression"),
  title = "Binary Logistic Regression Analysis Predicting Likelihood of Health Protective Behaviours (with Control)",
  dv.labels = health_protection, 
  collapse.ci = TRUE, p.style = "stars",
  file = file.path("./02__Models/Results/Tables/04__CONTROL_Protection_Table.html")
)

# Plotting log odds -----------------------------------------------------------
log_odds_plot(health_problem_base)
log_odds_plot(health_problem_control)
log_odds_plot(health_protection_base)
log_odds_plot(health_protection_control)
