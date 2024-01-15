rm(list = ls())

library(dplyr)
library(sjPlot)

# Custom functions
source(file.path("./02__Models/00__Functions.R"))

# Loading regression results
load(file = "02__Models/Results/RData/01__Problem_Models_BASE.RData")
load(file = "02__Models/Results/RData/02__Protection_Models_BASE.RData")
load(file = "02__Models/Results/RData/03__Problem_Models_CONTROL.RData")
load(file = "02__Models/Results/RData/04__Protection_Models_CONTROL.RData")

# Defining a vector of health problems
health_problems <- c(
  "Back Pain", "Headache", "Fatigue", "Smoking", 
  "Blood Pressure", "Diabetes", "Cholesterol", "Heart Condition")

# Defining a vector of health protective behaviors
health_protection <- c(
  "Prostate Exam", "Mammogram", "Cholesterol Screening",
  "Pap Smear", "Flu Shot", "Dental Visit (2yrs)")

# Define a vector of predictors
predictors <- c(
  "Procrastination", "Depression", "Education","Age"
)

# Extracting GLM results -------------------------------------------------------
# BASE MODELS
health_problem_base <- problem_models_base %>%
  process_glm_results(type = "base") %>%
  as.data.frame() %>%
  mutate(response = health_problems, .before = odds)
  
health_protection_base <- protection_models_base %>%
  process_glm_results(type = "base") %>%
  as.data.frame() %>%
  mutate(response = health_protection, .before = odds)

# COVARIATE MODELS
health_problem_control <- problem_models_control %>%
  process_glm_results(type = "control") %>%
  mutate(response = rep(health_problems, each = 4), .before = odds) %>%
  mutate(predictor = rep(predictors, times = (nrow(.) / 4)), .before = odds)

health_protection_control <- protection_models_control %>%
    process_glm_results(type = "control") %>%
  mutate(response = rep(health_protection, each = 4), .before = odds) %>%
  mutate(predictor = rep(predictors, times = (nrow(.) / 4)), .before = odds)

# Creating HTML Tables ---------------------------------------------------------
# BASE MODELS
tab_model(
  problem_models_base[[1]]$model, problem_models_base[[2]]$model,
  problem_models_base[[3]]$model, problem_models_base[[4]]$model,
  problem_models_base[[5]]$model, problem_models_base[[6]]$model,
  problem_models_base[[7]]$model, problem_models_base[[8]]$model,
  show.intercept = FALSE,
  pred.labels = "Procrastination", 
  title = "Binary Logistic Regression Analysis Predicting Likelihood of Health Problems",
  dv.labels = health_problems, 
  collapse.ci = TRUE, p.style = "numeric_stars",
  file = file.path("./02__Models/Results/Tables/01__BASE_Problem_Table.html")
)

tab_model(
  protection_models_base[[1]]$model, protection_models_base[[2]]$model,
  protection_models_base[[3]]$model, protection_models_base[[4]]$model,
  protection_models_base[[5]]$model, protection_models_base[[6]]$model,
  show.intercept = FALSE,
  pred.labels = "Procrastination", 
  dv.labels = health_protection, 
  title = "Binary Logistic Regression Analysis Predicting Likelihood of Health Protective Behaviours",
  collapse.ci = TRUE, p.style = "numeric_stars",
  file = file.path("./02__Models/Results/Tables/02__BASE_Protection_Table.html")
)

# COVARIATE MODELS
tab_model(
  problem_models_control[[1]]$model, problem_models_control[[2]]$model,
  problem_models_control[[3]]$model, problem_models_control[[4]]$model,
  problem_models_control[[5]]$model, problem_models_control[[6]]$model,
  problem_models_control[[7]]$model, problem_models_control[[8]]$model,
  show.intercept = FALSE,
  pred.labels = predictors,
  title = "Binary Logistic Regression Analysis Predicting Likelihood of Health Problems (with Control)",
  dv.labels = health_problems, 
  collapse.ci = TRUE, p.style = "numeric_stars",
  file = file.path("./02__Models/Results/Tables/Test/01__CONTROL_Problem_Table.html")
)

tab_model(
  protection_models_control[[1]]$model, protection_models_control[[2]]$model,
  protection_models_control[[3]]$model, protection_models_control[[4]]$model,
  protection_models_control[[5]]$model, protection_models_control[[6]]$model,
  show.intercept = FALSE,
  pred.labels = predictors,
  title = "Binary Logistic Regression Analysis Predicting Likelihood of Health Protective Behaviours (with Control)",
  dv.labels = health_protection, 
  collapse.ci = TRUE, p.style = "numeric_stars",
  file = file.path("./02__Models/Results/Tables/Test/02__CONTROL_Protection_Table.html")
)

# Plotting log odds ------------------------------------------------------------
# BASE PLOTS
odds_problem_base <- health_problem_base %>%
  log_odds_plot(title = "Risk of Experiencing a Health Problem")

odds_protection_base <- health_protection_base %>%
  log_odds_plot(title = "Chance of Engaging in Health Protective Behaviours")

# COVARIATE PLOTS
odds_problem_control <- list()
odds_protection_control <- list()

for(i in 1:length(predictors)){
  odds_problem_control[[i]] <- health_problem_control %>%
    filter(predictor == predictors[i]) %>%
    log_odds_plot(title = paste("Risk of Experiencing a Health Problem -", predictors[i]))
  
  odds_protection_control[[i]] <- health_protection_control %>%
    filter(predictor == predictors[i]) %>%
    log_odds_plot(title = paste("Chance of Engaging in Health Protective Behaviours -", predictors[i]))
}


# Exporting --------------------------------------------------------------------
export_path <- "./02__Models/Results/Figures/02__GLM_Plots/02__Odds_Plots/"

# BASE PLOTS
cowplot::save_plot(filename = file.path(export_path, "01__Base/01__Odds_Problem_Base.png"), 
                   plot = odds_problem_base)
cowplot::save_plot(filename = file.path(export_path, "01__Base/02__Odds_Protection_Base.png"), 
                   plot = odds_protection_base)

# COVARIATE PLOTS
for(i in 1:length(odds_problem_control)){
  # Saving problem plots
  cowplot::save_plot(
    filename = file.path(export_path, sprintf("02__Covariates/01__Problems/0%d__Odds_%s.png", i, predictors[i])),
    plot = odds_problem_control[[i]])
  
  # Saving protection plots
  cowplot::save_plot(
    filename = file.path(export_path, sprintf("02__Covariates/02__Protection/0%d__Odds_%s.png", i, predictors[i])),
    plot = odds_protection_control[[i]])
}

# Plotting log odds on one graph (test code) -----------------------------------
# health_problems_combined <- rbind(health_problem_base, health_problem_control) %>%
#   tibble::rownames_to_column("Health_problem") %>%
#   mutate(Health_problem = rep(health_problems, times = 2),
#          Depression_control = factor(rep(c("No", "Yes"), each = 8)))
# 
# health_protection_combined <- rbind(health_protection_base, health_protection_control) %>%
#   tibble::rownames_to_column("Health_protection") %>%
#   mutate(Health_problem = rep(health_protection, times = 2),
#          Depression_control = factor(rep(c("No", "Yes"), each = 6)))
# 
# problem_odds_combined <- ggplot(health_problems_combined, aes(y = Health_problem, colour = Depression_control)) +
#   geom_point(aes(x = log_odds), size = 3, alpha = 0.8) +
#   geom_errorbarh(aes(xmin = ci_lower, xmax = ci_upper), 
#                  height = 0.5, linewidth = 1, alpha = 0.4) +
#   geom_vline(xintercept = 1, color = "red", linetype = "dashed") +
#   xlim(0.9, 1.1) +
#   labs(x = "Odds (95% CI)", y = "", title = "Risk of Experiencing a Health Problem") +
#   theme_bw() +
#   ggeasy::easy_center_title() +
#   ggeasy::easy_add_legend_title("Covariates") +
#   ggeasy::easy_move_legend(to = "bottom")
# 
# protection_odds_combined <- ggplot(health_protection_combined, aes(y = Health_problem, colour = Depression_control)) +
#   geom_point(aes(x = log_odds), size = 3, alpha = 0.8) +
#   geom_errorbarh(
#     aes(xmin = ci_lower, xmax = ci_upper), height = 0.5, 
#     linewidth = 1, alpha = 0.4) +
#   geom_vline(xintercept = 1, color = "red", linetype = "dashed") +
#   xlim(0.9, 1.1) +
#   labs(x = "Odds (95% CI)", y = "", title = "Chance of Engaging in Health Protective Behaviours") +
#   theme_bw() +
#   ggeasy::easy_center_title() +
#   ggeasy::easy_add_legend_title("Covariates") +
#   ggeasy::easy_move_legend(to = "bottom")
# 
# cowplot::save_plot(
#   filename = file.path(export_path, "01__Combined/01__Health_Problems_Odds.png"), 
#   plot = problem_odds_combined)
# 
# cowplot::save_plot(
#   filename = file.path(export_path, "01__Combined/02__Health_Protection_Odds.png"), 
#   plot = protection_odds_combined)
