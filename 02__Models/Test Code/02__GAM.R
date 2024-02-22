rm(list = ls())

library(mgcv)
library(sjPlot)
library(patchwork)

path_data <- "./01__Data/02__Processed/"

# Reading in data
health_data <- readxl::read_xlsx(file.path(path_data, "Health_HRS.xlsx"))

# Interaction Test -------------------------------------------------------------
# fit <- glm(
#   Dental_visit_2_years ~ (Total_procrastination + Total_depression + Age)^2 + Education,
#   data = health_data, family = "binomial")

# summary(fit)
# anova(fit, test = "Chisq")
# drop1(fit, test = "Chisq")

# After fitting and analysis, the variables with an interaction effect include
# 1. Prostate Exam --> Procrastination:Depression
# 2. Pap Smear --> Depression:Age
# 3. Dentist --> Procrastination:Age
# 4. Cholesterol --> Depression:Age

# Fitting a GAM ----------------------------------------------------------------
# Prostate Exam
fit_1 <- gam(
  Prostate_exam ~ te(Total_procrastination, Total_depression) + Education + Age,
  data = health_data, family = "binomial")

summary(fit_1)

fit_1_residuals <- DHARMa::simulateResiduals(fittedModel = fit_1)
plot(fit_1_residuals)

# Pap Smear
fit_2 <- gam(
  Pap_smear ~ Total_procrastination + Education + te(Total_depression, Age),
  data = health_data, family = "binomial")

summary(fit_2)

fit_2_residuals <- DHARMa::simulateResiduals(fittedModel = fit_2)
plot(fit_2_residuals)

# Dentist
fit_3 <- gam(
  Dental_visit_2_years ~ te(Total_procrastination, Age) + Total_depression + Education,
  data = health_data, family = "binomial")

summary(fit_3)

fit_3_residuals <- DHARMa::simulateResiduals(fittedModel = fit_3)
plot(fit_3_residuals)

# Cholesterol Screening
fit_4 <- gam(
  Cholesterol_screening ~ Total_procrastination + Education + te(Total_depression, Age),
  data = health_data, family = "binomial")

summary(fit_4)

fit_4_residuals <- DHARMa::simulateResiduals(fittedModel = fit_4)
plot(fit_4_residuals)

# Visualization ----------------------------------------------------------------
# Prostate Exam
p1 <- wrap_elements(panel = ~ vis.gam(
  fit_1, view = c("Total_procrastination", "Total_depression"),
  type = "response", plot.type = 'persp', phi = 30,
  theta = 120, n.grid = 50,
  main = "Predicted probability of getting a prostate exam by procrastination and depression",
  xlab = "Total Depression (0 - 8)", ylab = "Total Procrastination (0 - 60)", 
  zlab = "Predicted Probability (0 - 1)"
  ))

# Pap Smear
p2 <- wrap_elements(panel = ~ vis.gam(
  fit_2, view = c("Total_depression", "Age"),
  type = "response", plot.type = 'persp', phi = 30, 
  theta = 120, n.grid = 50, r = 50,
  main = "Predicted probability of getting a pap smear by depression and age",
  xlab = "Total Depression (0 - 8)", ylab = "Age (Years)", 
  zlab = "Predicted Probability (0 - 1)"
  ))

# Dental Visit
p3 <- wrap_elements(panel = ~vis.gam(
  fit_3, view = c("Total_procrastination", "Age"),
  type = "response", plot.type = 'persp',
  phi = 20, theta = 120, n.grid = 50,
  main = "Predicted probability of visiting the dentist by procrastination and age",
  xlab = "Total Procrastination (0 - 60)", ylab = "Age (Years)",
  zlab = "Predicted Probability (0 - 1)"
  ))

# Cholesterol Screening
p4 <- wrap_elements(panel = ~vis.gam(
  fit_4, view = c("Total_depression", "Age"),
  type = "response", plot.type = 'persp',
  phi = 30, theta = 110, n.grid = 50,
  main = "Predicted probability of getting a cholesterol screening by depression and age",
  xlab = "Total Depression (0 - 8)", ylab = "Age (Years)",
  zlab = "Predicted Probability (0 - 1)"
  ))

# Exporting --------------------------------------------------------------------
export_path <- "./02__Models/Results/Figures/03__GAM/"

cowplot::save_plot(filename = file.path(export_path, "01__Prostate_GAM.png"), 
                   plot = p1, base_height = 10)
cowplot::save_plot(filename = file.path(export_path, "02__Pap_Smear_GAM.png"), 
                   plot = p2, base_height = 10)
cowplot::save_plot(filename = file.path(export_path, "03__Dentist_GAM.png"), 
                   plot = p3, base_height = 10)
cowplot::save_plot(filename = file.path(export_path, "04__Cholesterol_GAM.png"), 
                   plot = p4, base_height = 10)



