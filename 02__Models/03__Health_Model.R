# Does higher levels of procrastination in older adults predict 
# worse health outcomes
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

    # RESIDUALS
    Procras_1 ~~ Procras_2; # delay making decisions until it\'s too late ~~ delay acting upon decisions
    Procras_7 ~~ Procras_8; # continually saying "I\'ll do it tomorrow." ~~ delay before starting on work I have to do
    Procras_10 ~~ Procras_11; # don\'t get things done on time ~~ not  good at meeting deadlines
    
    # DIRECT EFFECT
    Health_problems ~ c*pro + b1*Health_behaviours + b2*Total_stress;
    
    # A PATHS
    Health_behaviours ~ a1*pro;
    Total_stress ~ a2*pro;

    
    # TOTAL INDIRECT
    HIDE := a1*b1;
    SIDE := a2*b2;
    
    Total_IE := (a1*b1) + (a2*b2)
    
    # TOTAL EFFECT
    Total_effect := c + (a1*b1) + (a2*b2)
'

# Fitting and reporting model --------------------------------------------------
fit <- sem(health_model, data = health_data, estimator = "ML", missing = "fiml")
summary(fit, fit.measures = TRUE, standardized = TRUE, modindices = FALSE, rsquare = TRUE)

# Exporting --------------------------------------------------------------------
export_path <- "./02__Models/Results/"

output <- capture.output(summary(fit, fit.measures = TRUE, standardized = TRUE, modindices = FALSE, rsquare = TRUE))
writeLines(output, file.path(export_path, "04__SEM_protection_stress.txt"))