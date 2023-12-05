rm(list = ls())

library(lavaan)
library(dplyr)

path_data <- "./01__Data/02__Processed/"

# Reading in data
health_data <- readxl::read_xlsx(file.path(path_data, "Health_HRS.xlsx"))

# Building a model
health_model <- '
  # LATENT VARIABLES
    pro =~ Procras_1 + Procras_2 + Procras_3 + Procras_4 + Procras_5 + Procras_6 + Procras_7 + Procras_8 + Procras_9 + Procras_10 + Procras_11 + Procras_12;
    # stress =~ Stress_1 + Stress_2 + Stress_3 + Stress_4 + Stress_5 + Stress_6 + Stress_7 + Stress_8 + Stress_9 + Stress_10;

    # RESIDUALS
    Procras_1 ~~ Procras_2; # delay making decisions until it\'s too late ~~ delay acting upon decisions
    Procras_7 ~~ Procras_8; # continually saying "I\'ll do it tomorrow." ~~ delay before starting on work I have to do
    Procras_10 ~~ Procras_11; # don\'t get things done on time ~~ not  good at meeting deadlines
    
    # DIRECT EFFECT
    Health_problems ~ c*pro;
    
    # A PATHS
    # pro ~ a1*stress;
    
    # INDIRECT EFFECT
    # SIDE := a1*b1;
    
    # TOTAL EFFECT
    Total_effect := c; # + (a1*b1)
'

fit <- sem(health_model, data = health_data, estimator = "ML", missing = "fiml")
summary(fit, fit.measures = TRUE, standardized = TRUE, modindices = FALSE, rsquare = TRUE)
