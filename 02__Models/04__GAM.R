rm(list = ls())

set.seed(2468) # Reproducibility

library(dplyr)
library(purrr) # For using the %||% operator
library(mgcv) # For working with GAMs
library(visreg)
library(ggplot2)
library(cowplot)
library(patchwork)

# Predictions Plot Function ----------------------------------------------------
create_health_plot <- function(model, data, x_var, y_var, x_label, y_label) {
  
  # Setting up -----------------------------------------------------------------
  # Defining x-axis details and sub caption for different x variables
  axis_details <- list(
    Total_procrastination = list(subtitle = "Controlling for depression and age", min = 0, max = 60, step = 10),
    Total_depression = list(subtitle = "Controlling for procrastination and age", min = 0, max = 8, step = 1),
    Age = list(subtitle = "Controlling for procrastination and depression", min = 50, max = 100, step = 10)
  )
  
  # Define non-significant result combinations for highlighting in red
  non_significant_combinations <- list(
    Total_procrastination = c("Cholesterol", "Heart condition", "Pap smears", "Flu shots"),
    Total_depression = c("Diabetes", "Mammograms", "Flu shots", "Dental visits"),
    Age = c("Back pain", "Fatigue", "Cholesterol")
  )
  
  # Retrieve axis details for the current x variable
  details <- axis_details[[x_var]] %||% list(subtitle = NULL)
  
  # Plotting GAM results -------------------------------------------------------
  gam_plot <- visreg(fit = model, xvar = x_var,
                     gg = TRUE, scale = "response", rug = FALSE) +
    geom_jitter(data = data, aes_string(x = x_var, y = y_var),
                height = 0.05, alpha = 0.5, size = 0.8) +
    scale_x_continuous(breaks = seq(details$min, details$max, by = details$step)) +
    labs(title = paste(y_label, "and", x_label),
         subtitle = details$subtitle,
         x = x_label, 
         y = paste("Prob(", y_label, ")")) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5))
  
  # Highlight the title and subtitle in red if the combination of X and Y is non-significant
  if (y_label %in% non_significant_combinations[[x_var]]) {
    gam_plot <- gam_plot +
      theme(plot.title = element_text(colour = "red"),
            plot.subtitle = element_text(colour = "red"))
  }
  
  # Returning plot
  gam_plot
}

# Data Importing ---------------------------------------------------------------
path_data <- "./01__Data/02__Processed/"

# Reading in data
health_data <- readxl::read_xlsx(file.path(path_data, "Health_HRS.xlsx"))

health_data <- health_data %>%
  filter(Age >= 50) %>%
  filter(!is.na(Total_procrastination))

# Fitting a GAM ----------------------------------------------------------------
# Health Problems --------------------------------------------------------------
# Creating a vector of health problems (and tidy names)
health_problems <- c(
  "Back_pain", "Headache", "Fatigue", "Smoker_current",
  "Blood_pressure", "Diabetes", "Cholesterol", "Heart_condition")

health_problems_tidy <- c(
  "Back pain", "Headaches", "Fatigue", "Smoking",
  "Blood pressure", "Diabetes", "Cholesterol", "Heart condition")

# Apply the GAM model to each problem variable and store it in the list
problem_fit <- lapply(health_problems, function(x){
  formula <- as.formula(paste(x, "~ s(Total_procrastination) + s(Total_depression, k = 9) + s(Age)"))
  
  gam(formula = formula, data = health_data,
      family = "binomial", method = "REML")
})

# Creating a dataset of results
gam_results_problems <- data.frame(
  Health_problem = rep(health_problems_tidy, each = 3),
  Predictor = rep(c("Procrastination", "Depression", "Age"), times = length(health_problems)), # times = 8
  edf = numeric(length(health_problems) *3),
  ref_df = numeric(length(health_problems) *3),
  chi_sq = numeric(length(health_problems) *3),
  p_val = numeric(length(health_problems) *3)
)

# Initial index value
index <- 1

# Filling in dataset
for(fit in problem_fit) {
  
  fit_sum <- summary(fit)
  num_rows <- length(fit_sum$edf)
  
  # Extract the effective degrees of freedom, reference degrees of freedom,
  # chi-squared value, and p-values from the model summary
  gam_results_problems[index:(index + num_rows - 1), c("edf", "ref_df", "chi_sq", "p_val")] <- 
    cbind(fit_sum$edf, fit_sum$s.table[, 2], fit_sum$chi.sq, fit_sum$s.pv)
  
  # Increment the index to move to the next set of rows
  index <- index + num_rows
}

# Rounding to 5 decimal places
gam_results_problems <- gam_results_problems %>%
  mutate(across(!c(Health_problem, Predictor), round, digits = 5))

# Health Protection ------------------------------------------------------------
# Creating a vector of health protection (and tidy names)
health_protection <- c(
  "Prostate_exam", "Mammogram", "Cholesterol_screening",
  "Pap_smear", "Flu_shot", "Dental_visit_2_years")

health_protection_tidy <- c(
  "Prostate exams", "Mammograms", "Cholesterol screenings",
  "Pap smears", "Flu shots", "Dental visits")

# Different formulas for different protective behaviors
formulas <- list(
  Mammogram = "~ s(Total_procrastination) + s(Total_depression, k = 9) + s(Age)",
  Flu_shot = "~ s(Total_procrastination) + s(Total_depression, k = 9) + s(Age)",
  Pap_smear = "~ s(Total_procrastination) + te(Total_depression, Age)",
  Cholesterol_screening = "~ s(Total_procrastination) + te(Total_depression, Age)",
  Prostate_exam = "~ s(Age) + te(Total_procrastination, Total_depression)",
  Dental_visit_2_years = "~ s(Total_depression, k = 9) + te(Total_procrastination, Age)"
)

# Apply the GAM model to each protection variable and store it in the list
protection_fit <- lapply(health_protection, function(x) {
  # Getting relevant formula for each protective behavior
  formula <- as.formula(paste(x, formulas[[x]]))
  
  # Fitting GAM
  gam(formula = formula, data = health_data, 
      family = "binomial", method = "REML")
})

# Creating a dataset (I can't think of a non-hard code way)
gam_results_protection <- data.frame(
  health_protection = c("Prostate Exams", "Prostate Exams", "Mammograms", "Mammograms", "Mammograms",
                        "Cholesterol Screening", "Cholesterol Screening", "Pap Smears", "Pap Smears",
                        "Flu Shots", "Flu Shots", "Flu Shots", "Dental Visit", "Dental Visit"),
  predictor = c("Age", "Procrastination x Depression", "Procrastination", "Depression", "Age",
                "Procrastination", "Depression x Age", "Procrastination", "Depression x Age",
                "Procrastination", "Depression", "Age", "Depression", "Procrastination x Age"),
  edf = numeric(14),
  ref_df = numeric(14),
  chi_sq = numeric(14),
  p_val = numeric(14)
)

# Initial index value
index <- 1

# Filling in dataset
for(fit in protection_fit) {
  
  fit_sum <- summary(fit)
  num_rows <- length(fit_sum$edf)
  
  # Extract the effective degrees of freedom, reference degrees of freedom,
  # chi-squared value, and p-values from the model summary
  gam_results_protection[index:(index + num_rows - 1), c("edf", "ref_df", "chi_sq", "p_val")] <- 
    cbind(fit_sum$edf, fit_sum$s.table[, 2], fit_sum$chi.sq, fit_sum$s.pv)
  
  # Increment the index to move to the next set of rows
  index <- index + num_rows
}

# Rounding to 5 decimal places
gam_results_protection <- gam_results_protection %>%
  mutate(across(!c(health_protection, predictor), round, digits = 5))

# Plotting ---------------------------------------------------------------------
# Health Problems --------------------------------------------------------------
# Initial empty lists to store plots for each factor
problem_p_plots <- list()
problem_d_plots <- list()
problem_a_plots <- list()

# Loop through each fitted GAM model to create plots for each health problem
for(i in seq_along(problem_fit)){
  # Procrastination
  problem_p_plots[[i]] <- create_health_plot(
    model = problem_fit[[i]], data = health_data, 
    x_var = "Total_procrastination", y_var = health_problems[i], 
    x_label = "Procrastination", y_label = health_problems_tidy[i])
  
  # Depression
  problem_d_plots[[i]] <- create_health_plot(
    model = problem_fit[[i]], data = health_data, 
    x_var = "Total_depression", y_var = health_problems[i], 
    x_label = "Depression", y_label = health_problems_tidy[i])
  
  # Age
  problem_a_plots[[i]] <- create_health_plot(
    model = problem_fit[[i]], data = health_data, 
    x_var = "Age", y_var = health_problems[i], 
    x_label = "Age", y_label = health_problems_tidy[i])
}

# Combine each individual plot into a grid
problem_p_grid <- plot_grid(plotlist = problem_p_plots, nrow = 2, ncol = 4)
problem_d_grid <- plot_grid(plotlist = problem_d_plots, nrow = 2, ncol = 4)
problem_a_grid <- plot_grid(plotlist = problem_a_plots, nrow = 2, ncol = 4)

# Making titles
problem_p_title <- ggdraw() + draw_label("Procrastination and Health Problems",
                                         fontface = 'bold', x = 0.5, hjust = 0.5, size = 14)
problem_d_title <- ggdraw() + draw_label("Depression and Health Problems", 
                                         fontface = 'bold', x = 0.5, hjust = 0.5, size = 14)
problem_a_title <- ggdraw() + draw_label("Age and Health Problems",
                                         fontface = 'bold', x = 0.5, hjust = 0.5, size = 14)

# Adding titles to each grid
problem_p_grid <- plot_grid(problem_p_title, problem_p_grid, ncol = 1, rel_heights = c(0.1, 1))
problem_d_grid <- plot_grid(problem_d_title, problem_d_grid, ncol = 1, rel_heights = c(0.1, 1))
problem_a_grid <- plot_grid(problem_a_title, problem_a_grid, ncol = 1, rel_heights = c(0.1, 1))

# Making a big grid (probably a bad idea)
problem_full_grid <- plot_grid(problem_p_grid, problem_d_grid, problem_a_grid, nrow = 3)

# Health Protection ------------------------------------------------------------
# Initial empty lists to store plots for each factor
protection_p_plots <- list()
protection_d_plots <- list()
protection_a_plots <- list()

# Loop through each fitted GAM model to create plots for each health protective behavior
for(i in seq_along(protection_fit)){
  # Procrastination
  protection_p_plots[[i]] <- create_health_plot(
    model = protection_fit[[i]], data = health_data, 
    x_var = "Total_procrastination", y_var = health_protection[i], 
    x_label = "Procrastination", y_label = health_protection_tidy[i])
  
  # Depression
  protection_d_plots[[i]] <- create_health_plot(
    model = protection_fit[[i]], data = health_data, 
    x_var = "Total_depression", y_var = health_protection[i], 
    x_label = "Depression", y_label = health_protection_tidy[i])
  
  # Age
  protection_a_plots[[i]] <- create_health_plot(
    model = protection_fit[[i]], data = health_data, 
    x_var = "Age", y_var = health_protection[i], 
    x_label = "Age", y_label = health_protection_tidy[i])
}

# Combine each individual plot into a grid
# Indexing because I want the plots without the interaction effects
protection_p_grid <- plot_grid(plotlist = protection_p_plots[2:5], nrow = 2, ncol = 2)
protection_d_grid <- plot_grid(plotlist = protection_d_plots[c(2, 5, 6)], ncol = 3)
protection_a_grid <- plot_grid(plotlist = protection_a_plots[c(1, 2, 5)], ncol = 3)

# 3D Contour Plots -------------------------------------------------------------
prostate_results <- wrap_elements(panel = ~ vis.gam(
  protection_fit[[1]], view = c("Total_procrastination", "Total_depression"),
  type = "response", plot.type = 'persp', phi = 30,
  theta = 120, n.grid = 50,
  main = "Predicted probability of getting a prostate exam by procrastination and depression",
  xlab = "Total Depression (0 - 8)", ylab = "Total Procrastination (0 - 60)", 
  zlab = "Predicted Probability (0 - 1)"
))

cholesterol_results <- wrap_elements(panel = ~ vis.gam(
  protection_fit[[3]], view = c("Total_depression", "Age"),
  type = "response", plot.type = 'persp', phi = 30, 
  theta = 120, n.grid = 50, r = 50,
  main = "Predicted probability of getting a cholesterol screening by depression and age",
  xlab = "Total Depression (0 - 8)", ylab = "Age (Years)", 
  zlab = "Predicted Probability (0 - 1)"
))

pap_results <- wrap_elements(panel = ~ vis.gam(
  protection_fit[[4]], view = c("Total_depression", "Age"),
  type = "response", plot.type = 'persp', phi = 30, 
  theta = 120, n.grid = 50, r = 50,
  main = "Predicted probability of getting a pap smear by depression and age",
  xlab = "Total Depression (0 - 8)", ylab = "Age (Years)", 
  zlab = "Predicted Probability (0 - 1)"
))

dental_results <- wrap_elements(panel = ~ vis.gam(
  protection_fit[[6]], view = c("Total_procrastination", "Age"),
  type = "response", plot.type = 'persp', phi = 30, 
  theta = 120, n.grid = 50, r = 50,
  main = "Predicted probability of visiting the dentist by depression and age",
  xlab = "Total Procrastination (0 - 60)", ylab = "Age (Years)", 
  zlab = "Predicted Probability (0 - 1)"
))

# Using patchwork to plot as a grid
interaction_grid <- prostate_results + pap_results + cholesterol_results + dental_results +
  plot_layout(nrow = 2, ncol = 2)

# Exporting --------------------------------------------------------------------
export_path_data <- "./02__Models/Results/"
export_path_graphics <- "./02__Models/Results/Figures/03__GAM/"

# GAM Results
writexl::write_xlsx(path = file.path(export_path_data, "01__GAM_Problems.xlsx"), 
                    x = gam_results_problems, col_names = TRUE)
writexl::write_xlsx(path = file.path(export_path_data, "02__GAM_Protection.xlsx"), 
                    x = gam_results_protection, col_names = TRUE)
# Main Effects
# Problems
save_plot(filename = file.path(export_path_graphics, "01__Problem/01__p_grid.png"), 
          plot = problem_p_grid, base_height = 10)
save_plot(filename = file.path(export_path_graphics, "01__Problem/02__d_grid.png"), 
          plot = problem_d_grid, base_height = 10)
save_plot(filename = file.path(export_path_graphics, "01__Problem/03__a_grid.png"), 
          plot = problem_a_grid, base_height = 10)
save_plot(filename = file.path(export_path_graphics, "01__Problem/04__full_grid.png"), 
          plot = problem_full_grid, base_height = 12, base_aspect_ratio = 1.5)

# Protection
save_plot(filename = file.path(export_path_graphics, "02__Protection/01__p_grid.png"),
          plot = protection_p_grid, base_height = 10)
save_plot(filename = file.path(export_path_graphics, "02__Protection/02__d_grid.png"),
          plot = protection_d_grid, base_height = 10)
save_plot(filename = file.path(export_path_graphics, "02__Protection/03__a_grid.png"),
          plot = protection_a_grid, base_height = 10)

# 3D Plots ---------------------------------------------------------------------
save_plot(filename = file.path(export_path_graphics, "02__Protection/04__Prostate_GAM.png"),
          plot = prostate_results, base_height = 10)
save_plot(filename = file.path(export_path_graphics, "02__Protection/05__Pap_Smear_GAM.png"),
          plot = pap_results, base_height = 10)
save_plot(filename = file.path(export_path_graphics, "02__Protection/06__Dentist_GAM.png"),
          plot = dental_results, base_height = 10)
save_plot(filename = file.path(export_path_graphics, "02__Protection/07__Cholesterol_GAM.png"),
          plot = cholesterol_results, base_height = 10)
save_plot(filename = file.path(export_path_graphics, "02__Protection/08__Interaction_grid.png"),
          plot = interaction_grid, base_height = 10)


