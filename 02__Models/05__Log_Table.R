rm(list = ls())

library(sjPlot)

# Loading regression results
load(file = "02__Models/Results/RData/01__Problem_Models_BASE.RData")
load(file = "02__Models/Results/RData/02__Protection_Models_BASE.RData")


# Defining a vector of health problems
health_problems <- c("Back Pain", "Headache", "Fatigue", 
                     "Alcohol", "Smoker Current", "Blood Pressure", 
                     "Diabetes", "Cholesterol", "Heart Condition")

# Defining a vector of health protective behaviors
health_protection <- c("Prostate Exam", "Mammogram", "Cholesterol Screening",
                       "Pap Smear", "Flu Shot", "Dental Visit (2yrs)")

problem_table <- tab_model(
  problem_models_base[[1]]$model, problem_models_base[[2]]$model,
  problem_models_base[[3]]$model, problem_models_base[[4]]$model,
  problem_models_base[[5]]$model, problem_models_base[[6]]$model,
  problem_models_base[[7]]$model, problem_models_base[[8]]$model,
  problem_models_base[[9]]$model,
  pred.labels = c("(Intercept)", "Procrastination"),
  title = "Binary Logistic Regression Analysis Predicting Likelihood of Health Problems",
  dv.labels = health_problems, 
  collapse.ci = TRUE, p.style = "stars",
  file = file.path("./02__Models/Results/Tables/01__Health_Problem_Log_Table.html")
)

tab_model(
  protection_models_base[[1]]$model, protection_models_base[[2]]$model,
  protection_models_base[[3]]$model, protection_models_base[[4]]$model,
  protection_models_base[[5]]$model, protection_models_base[[6]]$model,
  pred.labels = c("(Intercept)", "Procrastination"),
  dv.labels = health_protection, 
  title = "Binary Logistic Regression Analysis Predicting Likelihood of Health Protective Behaviours",
  collapse.ci = TRUE, p.style = "stars",
  file = file.path("./02__Models/Results/Tables/02__Health_Protection_Log_Table.html")
)



