rm(list = ls())

set.seed(2468) # Reproducibility

library(dplyr)
library(purrr) # For using the %||% operator
library(mgcv) # For working with GAMs
library(visreg)
library(ggplot2)
library(cowplot)
library(patchwork)
library(scales)
library(gtable)

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
    theme_bw(base_size = 12) +
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

# Exporting Plot Function ------------------------------------------------------
save_gam_plot <- function(subfolder, filename, plot, height = 10, aspect_ratio = NULL){
  # Getting file path
  file_path <- file.path(export_path_graphics, subfolder, filename)
  
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
  health_protection = c(
    "Prostate Exams", "Prostate Exams", "Mammograms", "Mammograms", "Mammograms",
    "Cholesterol Screening", "Cholesterol Screening", "Pap Smears", "Pap Smears",
    "Flu Shots", "Flu Shots", "Flu Shots", "Dental Visit", "Dental Visit"),
  predictor = c(
    "Age", "Procrastination x Depression", "Procrastination", "Depression", "Age",
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
  mutate(across(!c(health_protection, predictor), round, digits = 3))


# Plotting ---------------------------------------------------------------------
# Health Protection
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
protection_p_grid <- plot_grid(
  protection_p_plots[[2]], protection_p_plots[[3]],
  protection_p_plots[[4]], protection_p_plots[[5]],
  nrow = 2, ncol = 2)

protection_d_grid <- plot_grid(plotlist = protection_d_plots[c(2, 5, 6)], ncol = 3)
protection_a_grid <- plot_grid(plotlist = protection_a_plots[c(1, 2, 5)], ncol = 3)

# Interaction Effects ----------------------------------------------------------
# Making predictions datasets
# Prostate Exams
prostate_data <- expand.grid(
  Total_procrastination = seq(0, 60, length = 200),
  Total_depression = seq(0, 8, length = 200),
  Age = median(health_data$Age))

# Making predictions for prostate exams
preds <- predict(protection_fit[[1]], newdata = prostate_data, type = "response", se.fit = TRUE)

# Adding prostate predictions
prostate_data$preds <- preds$fit
prostate_data$se <- preds$se.fit

# Cholesterol and Pap Smear ----------------------------------------------------
c_and_p_data <- expand.grid(
  Age = seq(50, 95, length = 200),
  Total_depression = seq(0, 8, length = 200),
  Total_procrastination = median(health_data$Total_procrastination))

# Making predictions for cholesterol and pap smears
c_preds <- predict(protection_fit[[3]], newdata = c_and_p_data, type = "response", se.fit = TRUE)
p_preds <- predict(protection_fit[[4]], newdata = c_and_p_data, type = "response", se.fit = TRUE)

# Adding cholesterol predictions
c_and_p_data$c_preds <- c_preds$fit
c_and_p_data$c_se <- c_preds$se.fit

# Adding pap predictions
c_and_p_data$p_preds <- p_preds$fit
c_and_p_data$p_se <- p_preds$se.fit

# Dental Visits ----------------------------------------------------------------
dental_data <- expand.grid(
  Total_procrastination = seq(0, 60, length = 200),
  Age = seq(50, 95, length = 200),
  Total_depression = median(health_data$Total_depression))

# Making predictions for dental visits
preds <- predict(protection_fit[[6]], newdata = dental_data, type = "response", se.fit = TRUE)

# Adding dental predictions
dental_data$preds <- preds$fit
dental_data$se <- preds$se.fit

# Plotting ---------------------------------------------------------------------
# Using 2D heat map with alpha blending
# Prostate Exams
prosate_2d <- prostate_data %>%
  ggplot(aes(x = Total_procrastination, y = Total_depression)) +
  # Heat map layer (with alpha blending)
  geom_raster(aes(fill = rescale(preds), alpha = (1/se)^2 )) +
  # Contour lines
  geom_contour(aes(z = preds), col = 1, lwd = 0.2) +
  # Colour scheme
  scale_fill_viridis_c(option = "plasma") +
  # Adjusting axis
  scale_x_continuous(breaks = seq(0, 60, by = 10)) +
  scale_y_continuous(breaks = seq(0, 8, by = 1)) +
  # Plot title and labels
  labs(title = "Predicted probability of getting a prostate exam by procrastination and depression",
       x = "Total Procrastination", y = "Total Depression", fill = "p̂") +
  # Setting themes and legends
  theme_classic(base_size = 12) +
  guides(alpha = "none") +
  ggeasy::easy_center_title()

# Cholesterol Screening
cholesterol_2d <- c_and_p_data %>%
  ggplot(aes(x = Age, y = Total_depression)) +
  # Heat map layer (with alpha blending)
  geom_raster(aes(fill = rescale(c_preds), alpha = (1/c_se)^2 )) +
  # Contour lines
  geom_contour(aes(z = c_preds), col = 1, lwd = 0.2) +
  # Colour scheme
  scale_fill_viridis_c(option = "plasma") +
  # Adjusting axis
  scale_y_continuous(breaks = seq(0, 8, by = 1)) +
  # Plot title and labels
  labs(title = "Predicted probability of getting a cholesterol screening by depression and age",
       x = "Age", y = "Total Depression", fill = "p̂") +
  # Setting themes and legend
  theme_classic(base_size = 12) + 
  guides(alpha = "none") +
  ggeasy::easy_center_title()

# Pap Smears
pap_2d <- c_and_p_data %>%
  ggplot(aes(x = Age, y = Total_depression)) +
  # Heat map layer (with alpha blending)
  geom_raster(aes(fill = rescale(p_preds), alpha = (1/p_se)^2 )) +
  # Contour lines
  geom_contour(aes(z = p_preds), col = 1, lwd = 0.2) +
  # Colour scheme
  scale_fill_viridis_c(option = "plasma") +
  # Adjusting axis
  scale_y_continuous(breaks = seq(0, 8, by = 1)) +
  # Plot title and labels
  labs(title = "Predicted probability of getting a pap smear by depression and age",
       x = "Age", y = "Total Depression", fill = "p̂") +
  # Setting themes and legend
  theme_classic(base_size = 12) +
  guides(alpha = "none") +
  ggeasy::easy_center_title()

# Dental Visits
dental_2d <- dental_data %>%
  ggplot(aes(x = Total_procrastination, y = Age)) +
  # Heat map layer (with alpha blending)
  geom_raster(aes(fill = rescale(preds), alpha = (1/se)^2 )) +
  # Contour lines
  geom_contour(aes(z = preds), col = 1, lwd = 0.2) +
  # Colour scheme
  scale_fill_viridis_c(option = "plasma") +
  # Adjusting axis
  scale_x_continuous(breaks = seq(0, 60, by = 10)) +
  # Plot title and labels
  labs(title = "Predicted probability of visiting the dentist by procrastination and age",
       x = "Total Procrastination", y = "Age", fill = "p̂") +
  # Setting themes and legend
  theme_classic(base_size = 12) +
  guides(alpha = "none") +
  ggeasy::easy_center_title()

map_grid <- ggpubr::ggarrange(
  prosate_2d,dental_2d,
  cholesterol_2d, pap_2d, 
  common.legend = TRUE, legend = "right", 
  ncol = 2, nrow = 2
)

# Exporting --------------------------------------------------------------------
export_path_data <- "./02__Models/Results/"
export_path_graphics <- "./02__Models/Results/Figures/02__GAM/"

# GAM Results
writexl::write_xlsx(path = file.path(export_path_data, "01__GAM_Problems.xlsx"), 
                    x = gam_results_problems, col_names = TRUE)
writexl::write_xlsx(path = file.path(export_path_data, "02__GAM_Protection.xlsx"), 
                    x = gam_results_protection, col_names = TRUE)

# Saving Main Effects Plots
# Protection
save_gam_plot("02__Protection", "01__p_grid.pdf", protection_p_grid)
save_gam_plot("02__Protection", "02__d_grid.png", protection_d_grid)
save_gam_plot("02__Protection", "03__a_grid.png", protection_a_grid)

# Heat Maps
save_gam_plot("02__Protection", "04__prostate_map.png", prosate_2d)
save_gam_plot("02__Protection", "05__cholesterol_map.png", cholesterol_2d)
save_gam_plot("02__Protection", "06__pap_map.png", pap_2d)
save_gam_plot("02__Protection", "07__dental_map.png", dental_2d)
save_gam_plot("02__Protection", "08__map.png", map_grid)