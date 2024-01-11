rm(list = ls())

library(sjPlot)
library(dplyr)

# Custom functions
source(file.path("./02__Models/00__Functions.R"))

# Loading regression results
load(file = "02__Models/Results/RData/01__Problem_Models_BASE.RData")
load(file = "02__Models/Results/RData/02__Protection_Models_BASE.RData")
load(file = "02__Models/Results/RData/03__Problem_Models_CONTROL.RData")
load(file = "02__Models/Results/RData/04__Protection_Models_CONTROL.RData")

# Defining a vector of health problems
health_problems <- c(
  "Back Pain", "Headache", "Fatigue", "Drinking", "Smoking", 
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
  pred.labels = c("Intercept", "Procrastination", "Depression", "Education", 
                  "Procrastination x Depression", "Procrastination x Education"),
  title = "Binary Logistic Regression Analysis Predicting Likelihood of Health Problems (with Control)",
  dv.labels = health_problems, 
  collapse.ci = TRUE, p.style = "stars",
  file = file.path("./02__Models/Results/Tables/Test/01__CONTROL_Problem_Table.html")
)

tab_model(
  protection_models_control[[1]]$model, protection_models_control[[2]]$model,
  protection_models_control[[3]]$model, protection_models_control[[4]]$model,
  protection_models_control[[5]]$model, protection_models_control[[6]]$model,
  pred.labels = c("Intercept", "Procrastination", "Depression", "Education", 
                  "Procrastination x Depression", "Procrastination x Education"),
  title = "Binary Logistic Regression Analysis Predicting Likelihood of Health Protective Behaviours (with Control)",
  dv.labels = health_protection, 
  collapse.ci = TRUE, p.style = "stars",
  file = file.path("./02__Models/Results/Tables/Test/02__CONTROL_Protection_Table.html")
)

# Plotting log odds -----------------------------------------------------------
odds_problem_base <- log_odds_plot(health_problem_base, title = "Risk of Experiencing a Health Problem")
odds_problem_control <- log_odds_plot(health_problem_control, title = "Risk of Experiencing a Health Problems (Controlling for Depression)")
odds_protection_base <- log_odds_plot(health_protection_base, title = "Chance of Engaging in Health Protective Behaviours")
odds_protection_control <- log_odds_plot(health_protection_control, title = "Chance of Engaging in Health Protective Behaviours (Controlling for Depression)", size_font = 7)

odds_plots_combined <- cowplot::plot_grid(odds_problem_base, odds_problem_control,
                                          odds_protection_base, odds_protection_control,
                                          nrow = 2, ncol = 2)
# Exporting --------------------------------------------------------------------
export_path <- "./02__Models/Results/Figures/02__GLM_Plots/02__Odds_Plots/"

cowplot::save_plot(filename = file.path(export_path, "01__Odds_Problem_Base.png"), plot = odds_problem_base)
cowplot::save_plot(filename = file.path(export_path, "02__Odds_Problem_Control.png"), plot = odds_problem_control)
cowplot::save_plot(filename = file.path(export_path, "03__Odds_Protection_Base.png"), plot = odds_protection_base)
cowplot::save_plot(filename = file.path(export_path, "04__Odds_Protection_Control.png"), plot = odds_protection_control)
cowplot::save_plot(filename = file.path(export_path, "05__Odds_Combined.png"), plot = odds_plots_combined, base_height = 7)


# Plotting log odds on one graph (test code) -----------------------------------
health_problems_combined <- rbind(health_problem_base, health_problem_control) %>%
  tibble::rownames_to_column("Health_problem") %>%
  mutate(Health_problem = rep(health_problems, times = 2),
         Depression_control = factor(rep(c("No", "Yes"), each = 9)))

health_protection_combined <- rbind(health_protection_base, health_protection_control) %>%
  tibble::rownames_to_column("Health_protection") %>%
  mutate(Health_problem = rep(health_protection, times = 2),
         Depression_control = factor(rep(c("No", "Yes"), each = 6)))

problem_odds_combined <- ggplot(health_problems_combined, aes(y = Health_problem, colour = Depression_control)) +
  geom_point(aes(x = log_odds), size = 3, alpha = 0.8) +
  geom_errorbarh(aes(xmin = ci_lower, xmax = ci_upper), 
                 height = 0.5, linewidth = 1, alpha = 0.4) +
  geom_vline(xintercept = 1, color = "red", linetype = "dashed") +
  xlim(0.9, 1.1) +
  labs(x = "Odds (95% CI)", y = "", title = "Risk of Experiencing a Health Problem") +
  theme_bw() +
  ggeasy::easy_center_title() +
  ggeasy::easy_add_legend_title("Controlling for Depression") +
  ggeasy::easy_move_legend(to = "bottom")

protection_odds_combined <- ggplot(health_protection_combined, aes(y = Health_problem, colour = Depression_control)) +
  geom_point(aes(x = log_odds), size = 3, alpha = 0.8) +
  geom_errorbarh(aes(xmin = ci_lower, xmax = ci_upper), 
                 height = 0.5, linewidth = 1, alpha = 0.4) +
  geom_vline(xintercept = 1, color = "red", linetype = "dashed") +
  xlim(0.9, 1.1) +
  labs(x = "Odds (95% CI)", y = "", title = "Chance of Engaging in Health Protective Behaviours") +
  theme_bw() +
  ggeasy::easy_center_title() +
  ggeasy::easy_add_legend_title("Controlling for Depression") +
  ggeasy::easy_move_legend(to = "bottom")

cowplot::save_plot(
  filename = file.path(export_path, "01__Combined/01__Health_Problems_Odds.png"), 
  plot = problem_odds_combined)

cowplot::save_plot(
  filename = file.path(export_path, "01__Combined/02__Health_Protection_Odds.png"), 
  plot = protection_odds_combined)



