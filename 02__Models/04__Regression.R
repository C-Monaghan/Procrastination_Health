rm(list = ls())

library(dplyr)
library(ggplot2)
library(modelr)

# Custom functions
source(file.path("./02__Models/00__Functions.R"))

path_data <- "./01__Data/02__Processed/"

# Reading in data
health_data <- readxl::read_xlsx(file.path(path_data, "Health_HRS.xlsx"))

# Defining a vector of health problems
health_problems <- c(
  "Back_pain", "Headache", "Fatigue", "Smoker_current", "Blood_pressure", 
  "Diabetes", "Cholesterol", "Heart_condition")

# Defining a vector of health protective behaviors
health_protection <- c(
  "Prostate_exam", "Mammogram", "Cholesterol_screening", 
  "Pap_smear", "Flu_shot", "Dental_visit_2_years")

# Linear Fitting
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

# Plotting ---------------------------------------------------------------------
# Create a list to store plots
problem_list <- list()
protection_list <- list()

# Generate individual plots
for (i in 1:length(health_problems)) {
  problem_list[[i]] <- generate_log_plot(
    !!as.name(health_problems[i]), 
    data = health_data, title = gsub("_", " ", health_problems[i]))
  
  if(i <= length(health_protection)){
    protection_list[[i]] <- generate_log_plot(
      !!as.name(health_protection[i]), 
      data = health_data, title = gsub("_", " ", health_protection[i]))
  }
}

# Plotting as a grouped plot
problems_plot <- cowplot::plot_grid(plotlist = problem_list, nrow = 2, ncol = 4)
protection_plot <- cowplot::plot_grid(plotlist = protection_list, ncol = 3)

# Adding a title to each using ggdraw()
problem_title <- cowplot::ggdraw() +
  cowplot::draw_label(
    "Logistic Regression Curves for Health Problems",
    fontface = 'bold', x = 0.5, hjust = 0.5) +
  theme_bw()

protection_title <- cowplot::ggdraw() +
  cowplot::draw_label(
    "Logistic Regression Curves for Health Protection",
    fontface = 'bold', x = 0.5, hjust = 0.5) +
  theme_bw()

# Combining the title and the plot
problems_plot <- cowplot::plot_grid(
  problem_title, problems_plot, nrow = 2, rel_heights = c(0.1, 0.9))

protection_plot <- cowplot::plot_grid(
  protection_title, protection_plot, nrow = 2, rel_heights = c(0.1, 0.9))


# Exporting --------------------------------------------------------------------
export_path <- "./02__Models/Results/"

# R Data
save(problem_models_base, file = file.path(export_path, "RData/01__Problem_Models_BASE.RData"))
save(protection_models_base, file = file.path(export_path, "RData/02__Protection_Models_BASE.RData"))
save(problem_models_control, file = file.path(export_path, "RData/03__Problem_Models_CONTROL.RData"))
save(protection_models_control, file = file.path(export_path, "RData/04__Protection_Models_CONTROL.RData"))

# Plots
cowplot::save_plot(
  filename = file.path(export_path, "Figures/02__GLM_Plots/01__Logit_Plots/01__Health_Problems_logit.png"),
  plot = problems_plot, base_height = 10)

cowplot::save_plot(
  filename = file.path(export_path, "Figures/02__GLM_Plots/01__Logit_Plots/02__Health_Protection_logit.png"),
  plot = protection_plot, base_height = 6)


# Quadratic fit ----------------------------------------------------------------
# Base model 
fit_1 <- glm(
  formula = Alcohol ~ Total_procrastination + I(Total_procrastination^2),
  data = health_data,
  family = binomial(link = "logit"))

# Covariate Model
fit_2 <- glm(
  formula = Alcohol ~ Total_procrastination + I(Total_procrastination^2) + Total_depression + Education + Age, 
  data = health_data,
  family = binomial(link = "logit"))

# Outputting model summary
summary(fit_1)
summary(fit_2)

# Creating a function to extract the odds ratio
odds_base <- function(x){
  # ax^2 + bx + c
  exp((coef(fit_1)[3] * x^2) + (coef(fit_1)[2] * x) + coef(fit_1)[1])
}

odds_covariate <- function(x){
  # ax^2 + bx + c1 + c2 + ... + cn
  
  c1 <- coef(fit_2)[1]
  c2 <- coef(fit_2)[4] * mean(health_data$Total_depression, na.rm = TRUE)
  c3 <- coef(fit_2)[5] * mean(health_data$Education, na.rm = TRUE)
  c4 <- coef(fit_2)[6] * mean(health_data$Age, na.rm = TRUE)
  
  exp((coef(fit_2)[3] * x^2) + (coef(fit_2)[2] * x) + c1 + c2 + c3 + c4)
}

# Plotting ---------------------------------------------------------------------
# Base Plot
alcohol_base <- health_data %>%
  ggplot(aes(x = Total_procrastination, y = Alcohol)) +
  stat_function(fun = odds_base, geom = "line", colour = "blue", linewidth = 1) +
  scale_x_continuous(breaks = seq(0, 60, by = 5)) +
  scale_y_continuous(breaks = seq(0.4, 1.8, by = 0.2)) +
  geom_hline(yintercept = 1, linetype = "dashed", linewidth = .5, colour = "red") + 
  labs(x = "Procrastination", y = "Predicted Odds", 
       title = "Odds of Alcohol Consumption - Procrastination") +
  theme_bw() +
  ggeasy::easy_center_title()

# Covariate Plot
alcohol_covaiate <- health_data %>%
  ggplot(aes(x = Total_procrastination, y = Alcohol)) +
  stat_function(fun = odds_covariate, geom = "line", colour = "blue", linewidth = 1) +
  scale_x_continuous(breaks = seq(0, 60, by = 5)) +
  scale_y_continuous(breaks = seq(0.75, 1.75, by = 0.25), limits = c(0.75, 1.75)) +
  geom_hline(yintercept = 1, linetype = "dashed", linewidth = .5, colour = "red") + 
  labs(x = "Procrastination", y = "Predicted Odds", 
       title = "Odds of Alcohol Consumption - Procrastination") +
  theme_bw() +
  ggeasy::easy_center_title()

# Exporting --------------------------------------------------------------------
cowplot::save_plot(filename = file.path(
  export_path, "Figures/02__GLM_Plots/02__Odds_Plots/01__Base/03__Alcohol_Plot.png"),
  plot = alcohol_base)

cowplot::save_plot(filename = file.path(
  export_path, "Figures/02__GLM_Plots/02__Odds_Plots/02__Covariates/01__Alcohol_Plot.png"),
  plot = alcohol_covaiate)