rm(list = ls())

set.seed(2468) # Reproducibility

library(dplyr)
library(mgcv) # For working with GAMs
library(visreg)
library(ggplot2)
library(cowplot)

# Predictions Plot Function ----------------------------------------------------
create_health_plot <- function(model, data, x_var, y_var, x_label, y_label) {
  
  if(x_var == "Total_procrastination"){
    subtitle <- "Controlling for depression and age"
    min <- 0; max <- 60; step <- 10
  } else if(x_var == "Total_depression"){
    subtitle <- "Controlling for procrastination and age"
    min <- 0; max <- 8; step <- 1
  } else if(x_var == "Age"){
    subtitle <- "Controlling for procrastination and depression"
    min <- 30; max <- 100; step <- 10
  } else{
    subtitle <- NULL
  }
  
  visreg(fit = model, xvar = x_var,
         gg = TRUE, scale = "response", rug = FALSE) +
    geom_jitter(data = data, aes_string(x = x_var, y = y_var),
                height = 0.05, alpha = 0.5, size = 0.8) +
    scale_x_continuous(breaks = seq(min, max, by = step)) +
    labs(title = paste(y_label, "and", x_label),
         subtitle = subtitle,
         x = x_label, 
         y = paste("Prob(", y_label, ")")) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5))
}

# Data Importing ---------------------------------------------------------------
path_data <- "./01__Data/02__Processed/"

# Reading in data
health_data <- readxl::read_xlsx(file.path(path_data, "Health_HRS.xlsx"))

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
  Predictor = rep(c("Procrastination", "Depression", "Age"), times = 8),
  edf = numeric(24),
  ref_df = numeric(24),
  chi_sq = numeric(24),
  p_val = numeric(24)
)

# Initialising values
min_row <- 1
max_row <- 3

# Filling in dataset
for(i in 1:length(problem_fit)){
  
  fit_sum <- summary(problem_fit[[i]])
  
  gam_results_problems$edf[min_row:max_row] <- fit_sum$edf
  gam_results_problems$ref_df[min_row:max_row] <- fit_sum$s.table[, 2]
  gam_results_problems$chi_sq[min_row:max_row] <- fit_sum$chi.sq
  gam_results_problems$p_val[min_row:max_row] <- fit_sum$s.pv
  
  min_row <- min_row + 3
  max_row <- max_row + 3
}

# Health Protection ------------------------------------------------------------
# Creating a vector of health protection (and tidy names)
health_protection <- c(
  "Prostate_exam", "Mammogram", "Cholesterol_screening",
  "Pap_smear", "Flu_shot", "Dental_visit_2_years"
)

health_protection_tidy <- c(
  "Prostate exams", "Mammograms", "Cholesterol screenings",
  "Pap smears", "Flue shots", "Dental visits"
)

protection_fit <- lapply(health_protection, function(x){
  
  if(x == "Mammogram" || x == "Flu_shot"){
    # Base Formula
    formula <- as.formula(paste(x, "~ s(Total_procrastination) + s(Total_depression, k = 9) + s(Age)"))
    
    gam(formula = formula, data = health_data,
        family = "binomial", method = "REML")
    
  } else if(x == "Pap_smear" || x == "Cholesterol_screening"){
    # Tensor Product Smooth (Depression and Age)
    formula <- as.formula(paste(x, "~ s(Total_procrastination) + te(Total_depression, Age)"))
    
    gam(formula = formula, data = health_data,
        family = "binomial", method = "REML")
    
  } else if(x == "Prostate_exam"){
    # Tensor Product Smooth (Procrastination and Depression)
    formula <- as.formula(paste(x, "~ s(Age) + te(Total_procrastination, Total_depression)"))
    
    gam(formula = formula, data = health_data,
        family = "binomial", method = "REML")
    
  } else if(x == "Dental_visit_2_years"){
    # Tensor Product Smooth (Procrastination and Age)
    formula <- as.formula(paste(x, "~ s(Total_depression, k = 9) + te(Total_procrastination, Age)"))
    
    gam(formula = formula, data = health_data,
        family = "binomial", method = "REML")
  } else{
    
    break
  }
})

# Creating a dataset
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

min_row <- 1
max_row <- 2

for(i in 1:length(protection_fit)){
  
  fit_sum <- summary(protection_fit[[i]])
  
  gam_results_protection$edf[min_row:max_row] <- fit_sum$edf
  gam_results_protection$ref_df[min_row:max_row] <- fit_sum$s.table[, 2]
  gam_results_protection$chi_sq[min_row:max_row] <- fit_sum$chi.sq
  gam_results_protection$p_val[min_row:max_row] <- fit_sum$s.pv
  
  # Probably a better way to do this
  if(i == 1){
    min_row <- 3; max_row <- 5
  } else if(i == 2){
    min_row <- 6; max_row <- 7 
  } else if(i == 3){
    min_row <- 8; max_row <- 9 
  } else if(i == 4){
    min_row <- 10; max_row <- 12 
  } else if(i == 5){
    min_row <- 13; max_row <- 14 
  } else {
    break
  }
}

# Plotting ---------------------------------------------------------------------
# Health Problems --------------------------------------------------------------
problem_p_plots <- list()
problem_d_plots <- list()
problem_a_plots <- list()

for(i in 1:length(problem_fit)){
  problem_p_plots[[i]] <- create_health_plot(
    model = problem_fit[[i]], data = health_data, 
    x_var = "Total_procrastination", y_var = health_problems[i], 
    x_label = "Procrastination", y_label = health_problems_tidy[i])
  
  problem_d_plots[[i]] <- create_health_plot(
    model = problem_fit[[i]], data = health_data, 
    x_var = "Total_depression", y_var = health_problems[i], 
    x_label = "Depression", y_label = health_problems_tidy[i])
  
  problem_a_plots[[i]] <- create_health_plot(
    model = problem_fit[[i]], data = health_data, 
    x_var = "Age", y_var = health_problems[i], 
    x_label = "Age", y_label = health_problems_tidy[i])
}

problem_p_grid <- plot_grid(plotlist = problem_p_plots, nrow = 2, ncol = 4)
problem_d_grid <- plot_grid(plotlist = problem_d_plots, nrow = 2, ncol = 4)
problem_a_grid <- plot_grid(plotlist = problem_a_plots, nrow = 2, ncol = 4)

# Titles
problem_p_title <- ggdraw() + draw_label("Procrastination and Health Problems",
                                         fontface = 'bold', x = 0.5, hjust = 0.5, size = 14)
problem_d_title <- ggdraw() + draw_label("Depression and Health Problems", 
                                         fontface = 'bold', x = 0.5, hjust = 0.5, size = 14)
problem_a_title <- ggdraw() + draw_label("Age and Health Problems",
                                         fontface = 'bold', x = 0.5, hjust = 0.5, size = 14)

problem_p_grid <- plot_grid(problem_p_title, problem_p_grid, ncol = 1, rel_heights = c(0.1, 1))
problem_d_grid <- plot_grid(problem_d_title, problem_d_grid, ncol = 1, rel_heights = c(0.1, 1))
problem_a_grid <- plot_grid(problem_a_title, problem_a_grid, ncol = 1, rel_heights = c(0.1, 1))

# Health Protection ------------------------------------------------------------
# Individual Plots
protection_p_plot <- list()
protection_a_plot <- list()

# Procrastination Plots
protection_p_plot[[1]] <- create_health_plot(
  model = protection_fit[[2]], data = health_data, 
  x_var = "Total_procrastination", y_var = "Mammogram",
  x_label = "Procrastination", y_label = "Mammograms")

# Age Plots
protection_a_plot[[1]] <- create_health_plot(
  model = protection_fit[[1]], data = health_data,
  x_var = "Age", y_var = "Prostate_exam",
  x_label = "Age", y_label = "Prostate Exams")

protection_a_plot[[2]] <- create_health_plot(
  model = protection_fit[[2]], data = health_data,
  x_var = "Age", y_var = "Mammogram",
  x_label = "Age", y_label = "Mammograms")

protection_a_plot[[3]] <- create_health_plot(
  model = protection_fit[[5]], data = health_data,
  x_var = "Age", y_var = "Flu_shot",
  x_label = "Age", y_label = "Flu Shots")

protection_a_grid <- plot_grid(plotlist = protection_a_plot, ncol = 3)

protection_a_title <- ggdraw() + draw_label("Age and Health Protection",
                                         fontface = 'bold', x = 0.5, hjust = 0.5, size = 14)

protection_a_grid <- plot_grid(protection_a_title, protection_a_grid, ncol = 1, rel_heights = c(0.1, 1))

# Exporting --------------------------------------------------------------------
export_path <- "./02__Models/Results/Figures/03__GAM/Test"

# Problems
save_plot(filename = file.path(export_path, "01__Problem/01__p_grid.png"), 
          plot = problem_p_grid, base_height = 10)
save_plot(filename = file.path(export_path, "01__Problem/02__d_grid.png"), 
          plot = problem_d_grid, base_height = 10)
save_plot(filename = file.path(export_path, "01__Problem/03__a_grid.png"), 
          plot = problem_a_grid, base_height = 10)

# Protection
save_plot(filename = file.path(export_path, "02__Protection/01__a_grid.png"),
          plot = protection_a_grid, base_height = 10)
save_plot(filename = file.path(export_path, "02__Protection/02__p_plot.png"),
          plot = protection_p_plot[[1]], base_height = 10)

