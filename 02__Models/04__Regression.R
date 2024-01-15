rm(list = ls())

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
fit <- glm(
  formula = Alcohol ~ Total_procrastination + I(Total_procrastination^2),
  data = health_data,
  family = binomial(link = "logit"))

summary(fit)

health_data %>%
  select(Alcohol, Total_procrastination) %>%
  filter(complete.cases(.)) %>%
  mutate(predicted_probs = predict(fit, type = "response")) %>%
  ggplot(aes(x = Total_procrastination)) +
  geom_jitter(aes(y = Alcohol), width = 0, height = 0.1) +
  geom_line(aes(y = predicted_probs), colour = "red", linewidth = 1) +
  scale_x_continuous(breaks = seq(0, 60, by = 10)) +
  ylim(c(0, 1)) +
  labs(x = "Procrastination", y = "Predicted Probability") +
  theme_bw()


