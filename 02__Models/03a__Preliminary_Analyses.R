rm(list = ls())

# Preliminary analyses for the assessment of best model to use for each response 
# variable 

library(dplyr)

# Data Importing ---------------------------------------------------------------
path_data <- "./01__Data/02__Processed/"

# Reading in data
health_data <- readxl::read_xlsx(file.path(path_data, "Health_HRS.xlsx"))

health_data <- health_data %>%
  filter(Age >= 50) %>%
  filter(!is.na(Total_procrastination))

# Response variables
responses <- c(
  "Prostate_exam", "Mammogram", "Cholesterol_screening", "Pap_smear", 
  "Flu_shot", "Dental_visit_2_years")

# Fitting simple model
simple_fit <- lapply(responses, function(x){
  
  formula <- as.formula(paste(x, "~ s(Total_procrastination) + s(Total_depression, k = 9) + s(Age)"))
  
  mgcv::gam(formula = formula,
            data = health_data,
            family = "binomial",
            method = "REML")
})

# Fitting model with TPS for procrastination and depression
interaction_fit_1 <- lapply(responses, function(x){
  
  formula <- as.formula(paste(x, "~ s(Age) + te(Total_procrastination, Total_depression)"))
  
  mgcv::gam(formula = formula,
            data = health_data,
            family = "binomial",
            method = "REML")
})

# Fitting model with TPS for procrastination and age
interaction_fit_2 <- lapply(responses, function(x){
  
  formula <- as.formula(paste(x, "~ s(Total_depression, k = 9) + te(Total_procrastination, Age)"))
  
  mgcv::gam(formula = formula,
            data = health_data,
            family = "binomial",
            method = "REML")
})

# Create a data frame to store and compare AIC scores
aic_scores <- data.frame(
  Response = responses,
  AIC_Simple = sapply(simple_fit, AIC),
  AIC_te_p_d = sapply(interaction_fit_1, AIC),
  AIC_te_p_a = sapply(interaction_fit_2, AIC)
)

# Rounding to 2 decimal places
aic_scores <- aic_scores %>%
  mutate(across(!c(Response), round, digits = 2))

# Exporting --------------------------------------------------------------------
export_path <- "./02__Models/Results/"

# Supplementary_Table_S1
writexl::write_xlsx(x = aic_scores, path = file.path(export_path, "00__Supplementary_Table_S1.xlsx"))
