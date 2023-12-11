# Does higher levels of procrastination in older adults predict worse health outcomes
rm(list = ls())

library(lavaan)
library(dplyr)

path_data <- "./01__Data/02__Processed/"

# Reading in data
health_data <- readxl::read_xlsx(file.path(path_data, "Health_HRS.xlsx"))


# Building a model -------------------------------------------------------------
# Using all 10 stress items and a 2 factor structure
health_model_1 <- '
    # LATENT VARIABLES
    pro =~ Procras_1 + Procras_2 + Procras_3 + Procras_4 + Procras_5 + Procras_6 + Procras_7 + Procras_8 + Procras_9 + Procras_10 + Procras_11 + Procras_12;
    helplessness =~ Stress_1 + Stress_2 + Stress_3 + Stress_6 + Stress_9 + Stress_10;
    efficacy =~ Stress_4 + Stress_5 + Stress_7 + Stress_8;
    
    # RESIDUALS
    Procras_1 ~~ Procras_2; # delay making decisions until it\'s too late ~~ delay acting upon decisions
    Procras_7 ~~ Procras_8; # continually saying "I\'ll do it tomorrow." ~~ delay before starting on work I have to do
    Procras_10 ~~ Procras_11; # don\'t get things done on time ~~ not  good at meeting deadlines
    
    # DIRECT EFFECT
    Health_problems ~ c*pro + b1*helplessness + b2*efficacy + b3*Health_behaviours;
    
    # A PATHS
    helplessness ~ a1*pro;
    efficacy ~ a2*pro;
    Health_behaviours ~ a3*pro;
    
    # TOTAL INDIRECT
    SIDE_1 := a1*b1;
    SIDE_2 := a2*b2;
    HIDE := a3*b3;
    
    Total_IE := (a1*b1) + (a2*b2) + (a3*b3)
    
    # TOTAL EFFECT
    Total_effect := c + (a1*b1) + (a2*b2) + (a3*b3)
'

# Using only 4 items (short scale) and a 2 factor structure
health_model_2 <- '
    # LATENT VARIABLES
    pro =~ Procras_1 + Procras_2 + Procras_3 + Procras_4 + Procras_5 + Procras_6 + Procras_7 + Procras_8 + Procras_9 + Procras_10 + Procras_11 + Procras_12;
    helplessness =~ Stress_2 + Stress_10;
    efficacy =~ Stress_4 + Stress_5;
    
    # RESIDUALS
    Procras_1 ~~ Procras_2; # delay making decisions until it\'s too late ~~ delay acting upon decisions
    Procras_7 ~~ Procras_8; # continually saying "I\'ll do it tomorrow." ~~ delay before starting on work I have to do
    Procras_10 ~~ Procras_11; # don\'t get things done on time ~~ not  good at meeting deadlines
    
    # DIRECT EFFECT
    Health_problems ~ c*pro + b1*helplessness + b2*efficacy + b3*Health_behaviours;
    
    # A PATHS
    helplessness ~ a1*pro;
    efficacy ~ a2*pro;
    Health_behaviours ~ a3*pro;
    
    # TOTAL INDIRECT
    SIDE_1 := a1*b1;
    SIDE_2 := a2*b2;
    HIDE := a3*b3;
    
    Total_IE := (a1*b1) + (a2*b2) + (a3*b3)
    
    # TOTAL EFFECT
    Total_effect := c + (a1*b1) + (a2*b2) + (a3*b3)
'

# Fitting and reporting both model
fit_1 <- sem(health_model_1, data = health_data, estimator = "ML", missing = "fiml")
fit_2 <- sem(health_model_2, data = health_data, estimator = "ML", missing = "fiml")

summary(fit_1, fit.measures = TRUE, standardized = TRUE, modindices = FALSE, rsquare = TRUE)
summary(fit_2, fit.measures = TRUE, standardized = TRUE, modindices = FALSE, rsquare = TRUE)

# Comparing model performance
comparison <- semTools::compareFit(fit_2, fit_1)
summary(comparison)

# Exporting --------------------------------------------------------------------
export_path <- "./02__Models/Results/"

output_1 <- capture.output(summary(fit_1, fit.measures = TRUE, standardized = TRUE, modindices = FALSE, rsquare = TRUE))
output_2 <- capture.output(summary(fit_2, fit.measures = TRUE, standardized = TRUE, modindices = FALSE, rsquare = TRUE))
output_3 <- capture.output(summary(semTools::compareFit(fit_2, fit_1)))

writeLines(output_1, file.path(export_path, "01__SEM_Stress_(TWO FAC).txt"))
writeLines(output_2, file.path(export_path, "02__SEM_Stress_(TWO FAC SHORT).txt"))
writeLines(output_3, file.path(export_path, "03__Model_Comparison.txt"))




