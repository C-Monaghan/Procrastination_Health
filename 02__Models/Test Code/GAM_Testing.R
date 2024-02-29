rm(list = ls())

library(dplyr)
library(mgcv)
library(ggplot2)

# Function to visualize regression
plot_regression <- function(model, y_var, data, character){
  visreg::visreg(fit = model, xvar = "Age", gg = TRUE, 
                 scale = "response", rug = FALSE) +
    geom_jitter(data = data, aes(x = Age, y = y_var),
                height = 0.05, alpha = 0.5, size = 0.8) +
    scale_x_continuous(breaks = seq(50, 100, by = 10)) +
    labs(title = paste0("Relationship between ", character, " and Age (GAM)"),
         x = "Age",
         y = paste0("Prob(", character, ")")) +
    theme_bw() +
    ggeasy::easy_center_title()
}

path_data <- "./01__Data/02__Processed/"

# Reading in data
health_data <- readxl::read_xlsx(file.path(path_data, "Health_HRS.xlsx"))

# Changing education to a factor
health_data <- health_data %>%
  mutate(Education_fac = factor(case_when(
    Education == 0 ~ "No degree",
    Education == 1 ~ "GED",
    Education == 2 ~ "High School",
    Education == 3 ~ "College (2yrs)",
    Education == 4 ~ "College (4yrs)",
    Education == 5 ~ "Masters",
    Education == 6 ~ "Professional Degree"), 
    levels = c("No degree", "GED", "High School", "College (2yrs)",
               "College (4yrs)", "Masters", "Professional Degree")),
    .after = Education)

# Creating a vector of health problems
health_problems <- c("Back_pain", "Headache", "Fatigue", "Smoker_current",
                     "Blood_pressure", "Diabetes", "Cholesterol", "Heart_condition")

# Creating a vector of health protection
health_protection <- c("Prostate_exam", "Mammogram", "Cholesterol_screening",
                       "Pap_smear", "Flu_shot", "Dental_visit_2_years")

# Visualizing each variable ----------------------------------------------------
# Problems
health_data %>%
  select(ID, Age, all_of(health_problems)) %>%
  tidyr::pivot_longer(cols = !c(ID, Age),
                      names_to = "Health_Problem",
                      values_to = "Status") %>%
  filter(Age >= 50) %>%
  ggplot(aes(x = Age, y = Status)) +
  geom_jitter(height = 0.1, alpha = 0.3) +
  geom_smooth(se = FALSE, method = "gam", formula = y ~ s(x, bs = "cs")) +
  scale_x_continuous(breaks = seq(30, 100, by = 10)) +
  facet_wrap(~ Health_Problem) +
  theme_bw() +
  ggeasy::easy_remove_legend()

# Protection
health_data %>%
  select(ID, Age, all_of(health_protection)) %>%
  tidyr::pivot_longer(cols = !c(ID, Age),
                      names_to = "Health_Problem",
                      values_to = "Status") %>%
  filter(Age >= 50) %>%
  ggplot(aes(x = Age, y = Status)) +
  geom_jitter(height = 0.1, alpha = 0.3) +
  geom_smooth(se = FALSE, method = "gam", formula = y ~ s(x, bs = "cs")) +
  scale_x_continuous(breaks = seq(30, 100, by = 10)) +
  facet_wrap(~ Health_Problem) +
  theme_bw() +
  ggeasy::easy_remove_legend()

# Fitting GAMs to health problems ----------------------------------------------
# Creating a empty list to be filled
problem_fit <- list()
protection_fit <- list()

health_data_filtered <- health_data %>%
  filter(Age >= 50)

# Loop over the response variables and fit the GAM model for each one
for(i in 1:length(health_problems)) {
  
  if(i <= length(health_protection)){
    # Create a formula with the current response variable
    protection_formula <- as.formula(paste(health_protection[i], "~ s(Age)"))
    
    # Fit the GAM model and store it in the list
    protection_fit[[i]] <- gam(
      protection_formula, 
      data = health_data_filtered, 
      family = "binomial", 
      method = "REML")
  }
  
  # Create a formula with the current response variable
  problem_formula <- as.formula(paste(health_problems[i], "~ s(Age)"))
  
  # Fit the GAM model and store it in the list
  problem_fit[[i]] <- gam(
    problem_formula,
    data = health_data_filtered, 
    family = "binomial", 
    method = "REML")
}

# Visualizing
plot_regression(
  model = protection_fit[[1]], y_var = health_data_filtered$Prostate_exam, 
  data = health_data_filtered, character = "Prostate Exams")

plot_regression(
  model = protection_fit[[2]], y_var = health_data_filtered$Mammogram, 
  data = health_data_filtered, character = "Mammograms")

plot_regression(
  model = protection_fit[[3]], y_var = health_data_filtered$Cholesterol_screening, 
  data = health_data_filtered, character = "Cholesterol Screenings")

plot_regression(
  model = protection_fit[[4]], y_var = health_data_filtered$Pap_smear, 
  data = health_data_filtered, character = "Pap Smears")

plot_regression(
  model = protection_fit[[5]], y_var = health_data_filtered$Flu_shot, 
  data = health_data_filtered, character = "Flu Shots")

plot_regression(
  model = protection_fit[[6]], y_var = health_data_filtered$Dental_visit_2_years, 
  data = health_data_filtered, character = "Dental Visits")
