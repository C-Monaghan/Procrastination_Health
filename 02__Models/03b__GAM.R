rm(list = ls())

set.seed(2468) # Reproducibility

library(dplyr)
library(purrr) # For using the %||% operator
library(mgcv) # For working with GAMs
library(visreg)
library(ggplot2)
library(cowplot)
library(scales)

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
    Total_procrastination = c("Mammograms", "Cholesterol screenings"),
    Total_depression = c("Mammograms", "Cholesterol screenings", "Pap smears", "Flu shots", "Dental visits"),
    Age = c("Dental visits")
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
    theme_bw(base_size = 12) +
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5))
  
  # Highlight the title and subtitle in red if the combination of X and Y is non-significant
  if (y_label %in% non_significant_combinations[[x_var]]) {
    gam_plot <- gam_plot +
      theme(plot.title = element_text(colour = "#D2042D"), # Cherry red
            plot.subtitle = element_text(colour = "#D2042D")) # Cherry Red
  }
  
  if(y_label == "Cholesterol screenings" & x_label == "Procrastination"){
    gam_plot <- gam_plot +
      theme(plot.title = element_text(colour = "#8B8000"), # Amber yellow
            plot.subtitle = element_text(colour = "#8B8000")) # Amber yellow
  }
  
  # Returning plot
  gam_plot
}

# Creating a heat map (with alpha blending)
create_heatmap <- function(data, preds, se, title) {
  ggplot(data, aes(x = Total_procrastination, y = Total_depression)) +
    # Heat map layer (with alpha blending)
    geom_raster(aes(fill = rescale(!!sym(preds)), alpha = (1/!!sym(se))^2)) +
    # Contour lines
    geom_contour(aes(z = !!sym(preds)), color = "black", size = 0.3) +
    # Adding data points
    geom_jitter(data = health_data, aes(x = Total_procrastination, y = Total_depression),
                height = 0.08, width = 0.2, alpha = 0.7, size = 1) +
    # Colour scheme
    scale_fill_viridis_c(option = "plasma") +
    # Adjusting axis
    scale_x_continuous(breaks = seq(0, 60, by = 10)) +
    scale_y_continuous(breaks = seq(0, 8, by = 1)) +
    # Plot title and labels
    labs(title = title, x = "Total Procrastination", y = "Total Depression", fill = expression(hat(p))) +
    # Setting themes and legends
    theme_classic() +
    theme(
      title = element_text(size = 8),
      axis.title = element_text(size = 8),
      axis.text = element_text(size = 8),
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 8),
      plot.title = element_text(hjust = 0.5)
    ) +
    guides(alpha = "none")
}

# Exporting Plot Function ------------------------------------------------------
save_gam_plot <- function(filename, plot, height = 10, aspect_ratio = NULL){
  # Getting file path
  file_path <- file.path(export_path_graphics, filename)
  
  # Seperate saving if the aspect ratio value is present
  if(!is.null(aspect_ratio)){
    save_plot(filename = file_path, plot = plot, base_height = height, base_aspect_ratio = aspect_ratio)
  } else{
    save_plot(filename = file_path, plot = plot, base_height = height)
  }
}

# Data Importing ---------------------------------------------------------------
path_data <- "./01__Data/02__Processed/"

# Reading in data
health_data <- readxl::read_xlsx(file.path(path_data, "Health_HRS.xlsx"))

health_data <- health_data %>%
  filter(Age >= 50) %>%
  filter(!is.na(Total_procrastination))

# Health Protection ------------------------------------------------------------
# Creating a vector of health protection (and tidy names)
health_protection <- c(
  "Prostate_exam", "Mammogram", "Cholesterol_screening", "Pap_smear", 
  "Flu_shot", "Dental_visit_2_years")

health_protection_tidy <- c(
  "Prostate exams", "Mammograms", "Cholesterol screenings",
  "Pap smears", "Flu shots", "Dental visits")

# Different formulas for different protective behaviors
formulas <- list(
  Mammogram = "~ s(Total_procrastination) + s(Total_depression, k = 9) + s(Age)",
  Flu_shot = "~ s(Total_procrastination) + s(Total_depression, k = 9) + s(Age)",
  Pap_smear = "~ s(Total_procrastination) + s(Total_depression, k = 9) + s(Age)",
  Cholesterol_screening = "~ s(Total_procrastination) + s(Total_depression, k = 9) + s(Age)",
  Prostate_exam = "~ s(Age) + te(Total_procrastination, Total_depression)",
  Dental_visit_2_years = "~ s(Age) + te(Total_procrastination, Total_depression)"
)

# Apply the GAM model to each protection variable and store it in the list
protection_fit <- lapply(health_protection, function(x) {
  # Getting relevant formula for each protective behavior
  formula <- as.formula(paste(x, formulas[[x]]))
  
  # Fitting GAM
  gam(formula = formula, data = health_data, 
      family = "binomial", method = "REML")
})

# Running k.check on each model ------------------------------------------------
diagnostic <- lapply(protection_fit, function(x){
  k.check(x)
})

# Creating a dataset (I can't think of a non-hard code way)
gam_results_protection <- data.frame(
  health_protection = c(
    "Prostate Exams", "Prostate Exams", "Mammograms", "Mammograms", "Mammograms",
    "Cholesterol Screening", "Cholesterol Screening", "Cholesterol Screening",
    "Pap Smears", "Pap Smears", "Pap Smears", "Flu Shots", "Flu Shots", "Flu Shots", 
    "Dental Visit", "Dental Visit"),
  predictor = c(
    "Age", "Procrastination x Depression", "Procrastination", "Depression", "Age",
    "Procrastination", "Depression", "Age", "Procrastination", "Depression", "Age",
    "Procrastination", "Depression", "Age", "Age", "Procrastination x Depression"),
  edf = numeric(16),
  ref_df = numeric(16),
  chi_sq = numeric(16),
  p_val = numeric(16)
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

# Rounding to 3 decimal places
gam_results_protection <- gam_results_protection %>%
  mutate(across(!c(health_protection, predictor), round, digits = 3))

# Plotting ---------------------------------------------------------------------
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
# Indexing because I only want main effects
protection_p_grid <- plot_grid(plotlist = protection_p_plots[c(2, 3, 4, 5)])
protection_d_grid <- plot_grid(plotlist = protection_d_plots[c(2, 3, 4, 5)])
protection_a_grid <- plot_grid(plotlist = protection_a_plots)

# Interaction Effects ----------------------------------------------------------
# Making predictions datasets
new_data <- expand.grid(
  Total_procrastination = seq(0, 60, length = 200),
  Total_depression = seq(0, 8, length = 200),
  Age = median(health_data$Age))

# Making predictions
prostate_preds <- predict(protection_fit[[1]], newdata = new_data, type = "response", se.fit = TRUE)
dental_preds <- predict(protection_fit[[6]], newdata = new_data, type = "response", se.fit = TRUE)

# Adding predictions
new_data$prostate_preds <- prostate_preds$fit
new_data$prostate_se <- prostate_preds$se.fit

new_data$dental_preds <- dental_preds$fit
new_data$dental_se <- dental_preds$se.fit

# Plotting ---------------------------------------------------------------------
# Using heat map function
prostate_map <- create_heatmap(
  data = new_data, preds = "prostate_preds", se = "prostate_se", 
  title = "Predicted probability of prostate exam\nby procrastination and depression")

dental_map <- create_heatmap(
  data = new_data, preds = "dental_preds", se = "dental_se", 
  title = "Predicted probability of dental visit\nby procrastination and depression")


map_grid <- ggpubr::ggarrange(
  prostate_map, dental_map,
  common.legend = TRUE, legend = "right", 
  ncol = 2
)

# Exporting --------------------------------------------------------------------
export_path_data <- "./02__Models/Results/"
export_path_graphics <- "./02__Models/Results/Figures/02__GAM/"

# GAM Results
writexl::write_xlsx(
  path = file.path(export_path_data, "01__GAM_Protection.xlsx"),
  x = gam_results_protection, col_names = TRUE)

# Saving Main Effects Plots
# Protection
save_gam_plot("01__p_grid.png", protection_p_grid)
save_gam_plot("02__d_grid.png", protection_d_grid)
save_gam_plot("03__a_grid.png", protection_a_grid)

# Heat Maps
save_gam_plot("04__prostate_map.png", prosate_2d)
save_gam_plot("05__dental_map.png", dental_2d)
save_gam_plot("06b__map.png", map_grid, height = 5)

