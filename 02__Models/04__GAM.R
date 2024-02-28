rm(list = ls())

library(dplyr)
library(mgcv)
library(sjPlot)
library(patchwork)

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

# Interaction Test -------------------------------------------------------------
# fit <- glm(
#   Dental_visit_2_years ~ (Total_procrastination + Total_depression + Age)^2 + Education_fac,
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
  Prostate_exam ~ te(Total_procrastination, Total_depression) + s(Age),
  data = health_data, family = "binomial", method = "REML")

summary(fit_1)

fit_1_residuals <- DHARMa::simulateResiduals(fittedModel = fit_1)
plot(fit_1_residuals)

# Pap Smear
fit_2 <- gam(
  Pap_smear ~ s(Total_procrastination) + te(Total_depression, Age),
  data = health_data, family = "binomial", method = "REML")

summary(fit_2)

fit_2_residuals <- DHARMa::simulateResiduals(fittedModel = fit_2)
plot(fit_2_residuals)

# Dentist
fit_3 <- gam(
  Dental_visit_2_years ~ te(Total_procrastination, Age) + s(Total_depression, k = 9),
  data = health_data, family = "binomial", method = "REML")

summary(fit_3)

fit_3_residuals <- DHARMa::simulateResiduals(fittedModel = fit_3)
plot(fit_3_residuals)

# Cholesterol Screening
fit_4 <- gam(
  Cholesterol_screening ~ s(Total_procrastination) + te(Total_depression, Age),
  data = health_data, family = "binomial", method = "REML")

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
  
plot(fit_1, select = 2, trans = plogis, shift = coef(fit_1)[1], seWithMean = TRUE,
     rug = FALSE, shade = TRUE, shade.col = "skyblue", ylim = c(0, 1), 
     ylab = "Probability", main = "Probability of getting a prostate exam throughout age")

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
# 
# 
# library(dplyr)
# library(gratia)
# library(ggplot2)
# 
# smooths(fit_1)
# 
# x <- smooth_estimates(fit_1, smooth = "s(Age)")
# 
# x %>%
#   add_confint() %>%
#   ggplot(aes(y = plogis(est), x = Age)) +
#   geom_ribbon(aes(ymin = plogis(lower_ci), ymax = plogis(upper_ci)), alpha = 0.2, fill = "skyblue") +
#   geom_line(colour = "black", linewidth = 0.74) +
#   labs(title = expression("Partial effect of" ~ f(Age)), 
#        y = "Partial effect", x = expression(Age)) +
#   ylim(0, 1) +
#   theme_bw() +
#   ggeasy::easy_center_title()
# 
# 
# test_fit <- gam(Heart_condition ~ s(Total_procrastination) + s(Total_depression, k = 9) + s(Age) + Education,
#                 data = health_data, family = "binomial", method = "REML")
# 
# summary(test_fit) 
# 
# sim_res <- DHARMa::simulateResiduals(test_fit, n = 1000, seed = 150)
# plot(sim_res)
# 
# 
# 
# 
# 
# 
# test <- gam(Cholesterol ~ Education + s(Total_depression, k = 9) + te(Total_procrastination, Age), 
#             data = health_data, family = "binomial", method = "REML")
# 
# summary(test)
# 
# 
# plot(effects::effect("Education_fac", y))
# 
# 
# 
# y <- glm(Prostate_exam ~ Education_fac, 
#          data = health_data, family = binomial(link = "logit"))
# 
# summary(y)
# 
# pred <- data.frame(
#   Education_fac = health_data$Education_fac,
#   fit = predict(y, type = "response", newdata = health_data),
#   se = predict(y, type = "response", se.fit = TRUE, newdata = health_data)$se.fit)
# 
# pred <- pred %>%
#   filter(complete.cases(fit))
# 
# pred_lower <- pred$fit - 1.96 * pred$se
# pred_upper <- pred$fit + 1.96 * pred$se
# 
# ggplot(data = pred, aes(Education_fac, y= fit, colour = Education_fac)) +
#   geom_point() +
#   geom_errorbar(aes(ymin = pred_lower, ymax = pred_upper)) +
#   labs(title = "Predicted probability of getting a prostate exam by education level", 
#        x = "", y = "Predicted probability") +
#   ylim(0, 1.1) +
#   theme_classic() +
#   ggeasy::easy_remove_legend() +
#   ggeasy::easy_remove_gridlines() +
#   ggeasy::easy_center_title()
# 
# 
# # Education
# visreg(fit = fit[[2]], xvar = "Education", 
#        gg = TRUE, scale = "response", rug = FALSE) +
#   geom_jitter(data = health_data, aes(x = Education, y = Headache),
#               height = 0.05, alpha = 0.5, size = 0.8) +
#   scale_x_continuous(breaks = seq(0, 6, by = 1), labels = c(
#     "No Degree", "GED", "High School", 
#     "College (2yrs)", "College (4yrs)", 
#     "Masters", "Professional Degree")) +
#   labs(title = "Relationship between headaches and education (GAM)",
#        subtitle = "Controlling for procrastination, depression, and age",
#        x = "Education Status", 
#        y = "Prob(Headahces)") +
#   theme_bw() +
#   theme(plot.title = element_text(hjust = 0.5),
#         plot.subtitle = element_text(hjust = 0.5)) +
#   ggeasy::easy_x_axis_labels_size(size = 7)
