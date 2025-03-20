rm(list = ls())

library(dplyr)
library(ggplot2)

# Custom functions -------------------------------------------------------------
source(here::here(file.path("./R/00a__Functions.R")))

path_data <- "./data"

# Reading in data
health_data <- readxl::read_xlsx(file.path(path_data, "Health_HRS.xlsx"))

# Visualizing missing data -----------------------------------------------------
# Whole dataset
naniar::vis_miss(health_data)

# For ONLY total columns
missing_percentage <- health_data %>%
  select(c(starts_with("Total"), "Health_problems", "Health_behaviours")) %>%
  rename("Stress" = Total_stress, 
         "Anxiety" = Total_anxiety,
         "Procrastination" = Total_procrastination, 
         "Depression" = Total_depression,
         "Health Problems" = Health_problems,
         "Health Behaviours" = Health_behaviours) %>%
  naniar::gg_miss_var(show_pct = TRUE) +
  labs(y = "% Missing Data", title = "So much missing data!!") +
  theme_bw(base_size = 12) +
  ggeasy::easy_center_title()

# Visualizing relationships with procrastination -------------------------------
depression_scatter <- health_data %>%
  create_scatter_plot(y_variable = Total_depression, y_label = "Depression")

anxiety_scatter <- health_data %>%
  create_scatter_plot(y_variable = Total_anxiety, y_label = "Anxiety")

stress_scatter <- health_data %>%
  create_scatter_plot(y_variable = Total_stress, y_label = "Stress")

health_problem_scatter <- health_data %>%
  create_scatter_plot(y_variable = Health_problems, y_label = "Health Problems")

health_protection_scatter <- health_data %>%
  create_scatter_plot(y_variable = Health_behaviours, y_label = "Health Protection Behaviours")

# Visualizing distributions ----------------------------------------------------
# Procrastination
procrastination_distribution <- create_histogram(
  data = health_data, x_variable = Total_procrastination, 
  x_label = "Total Procrastination", binwidth = 5)

# Depression
depression_distribution <- create_histogram(
  data = health_data, x_variable = Total_depression, 
  x_label = "Total Depression", binwidth = 1)

# Anxiety
anxiety_distribution <- create_histogram(
  data = health_data, x_variable = Total_anxiety, 
  x_label = "Total Anxiety", binwidth = 1)

# Stress
stress_distribution <- create_histogram(
  data = health_data, x_variable = Total_stress,
  x_label = "Total Stress", binwidth = 2)

# Health Problems
illness_distribution <- create_histogram(
  data = health_data, x_variable = Health_problems,
  x_label = "Number of Health Problems", binwidth = 1)

# Health protection
protection_distribution <- create_histogram(
  data = health_data, x_variable = Health_behaviours,
  x_label = "Number of Health Protective Behaviours", binwidth = 1)

# Frequency Analysis -----------------------------------------------------------
# Health Problems
problems_frequency <- process_health_data(
  data = health_data, variables = c(
  "Back_pain", "Headache", "Fatigue", "Alcohol", "Smoker_current", 
  "Blood_pressure", "Diabetes", "Cholesterol", "Heart_condition"), 
  type = "problems_frequency")

# Health Protection (Males)
protection_frequency_male <- health_data %>%
  filter(Gender == 0) %>%
  process_health_data(variables = c(
    "Prostate_exam", "Cholesterol_screening",
    "Flu_shot", "Dental_visit_2_years"), gender_title = "Male",
    type = "protection_frequency")

# Health Protection (Females)
protection_frequency_female <- health_data %>%
  filter(Gender == 1) %>%
  process_health_data(variables = c(
    "Cholesterol_screening", "Flu_shot", "Dental_visit_2_years",
    "Mammogram", "Pap_smear"), gender_title = "Female",
    type = "protection_frequency")

# Health and Procrastination ---------------------------------------------------
health_procrastination_male <- generate_health_plot(
  data = health_data, gender = 0, gender_title = "Male", 
  variables = c("Prostate_exam", "Cholesterol_screening", 
                "Flu_shot", "Dental_visit_2_years"))

health_procrastination_female <- generate_health_plot(
  data = health_data, gender = 1, gender_title = "Female", 
  variables = c("Mammogram", "Pap_smear", "Cholesterol_screening",
                "Flu_shot", "Dental_visit_2_years"))

# Grouping everything together -------------------------------------------------
# Scatter Plots
scatter_grouped <- cowplot::plot_grid(
  depression_scatter, 
  anxiety_scatter, 
  health_problem_scatter, 
  health_protection_scatter, 
  stress_scatter,
  NULL,
  nrow = 2, ncol = 3)

# Distributions
distributions_grouped <- cowplot::plot_grid(
  procrastination_distribution, 
  depression_distribution, 
  anxiety_distribution, 
  stress_distribution, 
  illness_distribution, 
  protection_distribution,
  nrow = 3, ncol = 2)

# Health Protection Frequency
protection_frequency_grouped <- cowplot::plot_grid(
  protection_frequency_male, 
  protection_frequency_female, 
  ncol = 2, nrow = 1)

# Health Procrastination
health_procrastination_grouped <- cowplot::plot_grid(
  health_procrastination_male,
  health_procrastination_female,
  ncol = 2, nrow = 1
)

# Exporting --------------------------------------------------------------------
export_path <- "./R/Results/Figures/01__Exploratory"

cowplot::save_plot(filename = file.path(export_path, "01__Distributions.png"),
                   plot = distributions_grouped, base_height = 10)
cowplot::save_plot(filename = file.path(export_path, "02__Health_problem_frequency.png"),
                   plot = problems_frequency, base_height = 14)
cowplot::save_plot(filename = file.path(export_path, "03__Health_protection_frequency.png"),
                   plot = protection_frequency_grouped, base_height = 14)
cowplot::save_plot(filename = file.path(export_path, "04__Health_procrastination.png"),
                   plot = health_procrastination_grouped, base_height = 14)
cowplot::save_plot(filename = file.path(export_path, "05__Scatter_plots.png"),
                   plot = scatter_grouped, base_height = 14)
cowplot::save_plot(filename = file.path(export_path, "06__Missing_percentage.png"),
                   plot = missing_percentage, base_height = 10)
