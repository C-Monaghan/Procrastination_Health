# Assessing if there is a linear relationship between procrastination and each 
# individual health problem and health protective behavior
# ------------------------------------------------------------------------------
rm(list = ls())

# Custom functions
source(file.path("./02__Models/00__Functions.R"))

path_data <- "./01__Data/02__Processed/"

# Reading in data --------------------------------------------------------------
health_data <- readxl::read_xlsx(file.path(path_data, "Health_HRS.xlsx"))

# Defining a vector of health problems
health_problems <- c("Back_pain", "Headache", "Fatigue", 
                     "Alcohol", "Smoker_current", "Blood_pressure", 
                     "Diabetes", "Cholesterol", "Heart_condition")

# Defining a vector of health protective behaviors
health_protection <- c("Prostate_exam", "Mammogram", "Cholesterol_screening",
                       "Pap_smear", "Flu_shot", "Dental_visit_2_years")

# Plotting curves --------------------------------------------------------------
problem_curves <- lapply(health_problems, function(problem){
  linearity_plot(
    data = health_data, 
    y_variable = !!as.name(problem), 
    title = gsub("_", " ", problem))
})

protection_curves <- lapply(health_protection, function(protection){
  linearity_plot(
    data = health_data, 
    y_variable = !!as.name(protection), 
    title = gsub("_", " ", protection))
})

# Plotting curves all together
problem_curves_grouped <- cowplot::plot_grid(plotlist = problem_curves, ncol = 3)
protection_curves_grouped <- cowplot::plot_grid(plotlist = protection_curves, ncol = 2)

# Plotting abline curve --------------------------------------------------------
problem_test <- lapply(health_problems, function(problem){
  non_linear_fit(response = problem, 
                 data = health_data, 
                 title = gsub("_", " ", problem))
})

protection_test <- lapply(health_protection, function(protection){
  non_linear_fit(response = protection, 
                 data = health_data, 
                 title = gsub("_", " ", protection))
})

# Plotting curves all together
problem_abline_group <- cowplot::plot_grid(plotlist = problem_test, ncol = 3)
protection_abline_group <- cowplot::plot_grid(plotlist = protection_test, ncol = 3)

# Exporting --------------------------------------------------------------------
export_path <- "./02__Models/Results/Figures/02__GLM_Plots/03__Curve_Plots/"

cowplot::save_plot(
  filename = file.path(export_path, "01__Problem_Curves.png"),
  plot = problem_curves_grouped,
  base_height = 10)

cowplot::save_plot(
  filename = file.path(export_path, "02__Protection_Curves.png"),
  plot = protection_curves_grouped,
  base_height = 10)

cowplot::save_plot(
  filename = file.path(export_path, "03__Problem_Abline.png"), 
  plot = problem_abline_group, 
  base_height = 5)

cowplot::save_plot(
  filename = file.path(export_path, "04__Protection_Abline.png"),
  plot = protection_abline_group, 
  base_height = 5)




fit <- glm(
  formula = Headache ~ Total_procrastination + I(Total_procrastination^2), 
  data = health_data,
  family = "binomial")

anova(fit, test = "Chisq")
