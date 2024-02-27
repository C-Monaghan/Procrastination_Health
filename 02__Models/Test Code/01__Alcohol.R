rm(list = ls())

library(dplyr)
library(ggplot2)
library(visreg) # Visualizing GLM models
library(mgcv) # Working with GAMs
library(hnp) # Half normal plots

set.seed(1347)

# Custom functions -------------------------------------------------------------
# source(file.path("./02__Models/00__Functions.R"))

path_data <- "./01__Data/02__Processed/"

# Reading in data
health_data <- readxl::read_xlsx(file.path(path_data, "Health_HRS.xlsx"))

health_data <- health_data %>%
  rename(Days_drink = "Alcohol_week") %>%
  mutate(Days_no_drink = 7 - Days_drink, .after = Days_drink) %>%
  mutate(Education_fac = factor(case_when(
    Education == 0 ~ "No degree",
    Education == 1 ~ "GED",
    Education == 2 ~ "High School",
    Education == 3 ~ "College (2yrs)",
    Education == 4 ~ "College (4yrs)",
    Education == 5 ~ "Masters",
    Education == 6 ~ "Professional Degree"), 
    levels = c("No degree", "GED", "High School", 
               "College (2yrs)","College (4yrs)", 
               "Masters", "Professional Degree")), .after = Education)

# Exploring alcohol data -------------------------------------------------------
# Do you consume alcohol
health_data %>%
  select(starts_with("Alcohol")) %>%
  ggplot(aes(Alcohol)) +
  geom_bar(width = 0.5, fill = "skyblue", colour = "black") + 
  stat_count(geom = "text", aes(label = ..count..), vjust = -0.5, size = 3) +
  scale_x_continuous(breaks = c(0, 1)) +
  ylim(0, 900) +
  labs(x = "Do you consume Alcohol (0 = no; 1 = yes)") +
  theme_bw()

# How many days a week for you drink alcohol
health_data %>%
  ggplot(aes(Days_drink)) +
  geom_histogram(binwidth = 1, fill = "skyblue", colour = "black") +
  scale_x_continuous(breaks = seq(0, 7, by = 1)) +
  stat_count(geom = "text", aes(label = ..count..), vjust = -0.5, size = 3) +
  labs(title = "On average, how many days per week have you had any alcohol to drink?",
       x = "Days per week") +
  ylim(0, 900) +
  theme_bw() +
  ggeasy::easy_center_title()

# How much Alcohol per week do you drink
health_data %>%
  ggplot(aes(Alcohol_amount)) +
  geom_bar(width = 1, fill = "skyblue", colour = "black") +
  stat_count(geom = "text", aes(label = ..count..), vjust = -0.5, size = 3) +
  labs(title = " On the days you drink, about how many drinks do you have?",
       x = "# of drinks") +
  ylim(0, 900) +
  theme_bw() +
  ggeasy::easy_center_title()

# Alcohol per week and procrastination
health_data %>%
  filter(Alcohol == 1) %>%
  ggplot(aes(x = Total_procrastination, y = Days_drink / 7 )) +
  geom_jitter(width = 0.05, height = 0.05) +
  geom_smooth(se = FALSE, method = "glm") +
  labs(title = "Days Drank vs. Procrastination",
       x = "Total Procrastination", y = "Proportion of Days Drank") +
  theme_bw() +
  ggeasy::easy_center_title()

# Modelling --------------------------------------------------------------------
# Binomial GLM with different link functions (one predictor) -------------------
fit_1a <- glm(cbind(Days_drink, Days_no_drink) ~ Total_procrastination + I(Total_procrastination^2),
           data = health_data, family = binomial(link = "logit"))

fit_1b <- glm(cbind(Days_drink, Days_no_drink) ~ Total_procrastination + I(Total_procrastination^2),
              data = health_data, family = binomial(link = "probit"))

fit_1c <- glm(cbind(Days_drink, Days_no_drink) ~ Total_procrastination + I(Total_procrastination^2),
              data = health_data, family = binomial(link = "cloglog"))

# Summarizing
summary(fit_1a)
summary(fit_1b)
summary(fit_1c)

# Half normal plot
hnp(fit_1a, main = "HNP for logit model")                    # BAD 
hnp(fit_1b, main = "HNP for probit model")                   # BAD
hnp(fit_1c, main = "HNP for complementary log log model")    # BAD

# Plotting (using visreg)
visreg(fit = fit_1a, xvar = "Total_procrastination", gg = TRUE, scale = "response", rug = FALSE) +
  geom_jitter(data = health_data, aes(x = Total_procrastination, y = Days_drink / 7), 
              alpha = 0.5, size = 0.9) +
  scale_x_continuous(breaks = seq(0, 60, by = 10)) +
  labs(title = "Relationship between days drinking and procrastination (GLM)",
       x = "Total Procrasination", 
       y = "prob(Days Drinking)") +
  theme_bw()

# Plotting all together
# Creating and adding predictions
glm_data <- tibble(Total_procrastination = seq(0, 60, length = 200))

glm_data <- glm_data %>%
  modelr::add_predictions(model = fit_1a,
                          type = "response",
                          var = "logit") %>%
  modelr::add_predictions(model = fit_1b,
                          type = "response",
                          var = "probit") %>%
  modelr::add_predictions(model = fit_1c,
                          type = "response",
                          var = "cloglog")
# Plotting
glm_data %>%
  tidyr::pivot_longer(cols = !Total_procrastination,
                      names_to = "link",
                      values_to = "pred") %>%
  ggplot(aes(x = Total_procrastination, y = pred, colour = link)) +
  geom_line(linewidth = 1) +
  geom_jitter(data = health_data, aes(x = Total_procrastination, y = Days_drink / 7, colour = NA)) +
  labs(x = "Total Procrastination", y = "Prob(Day Drinking)") +
  theme_bw() +
  ggeasy::easy_add_legend_title("Link Function")

# Binomial GLM with different link functions (multiple predictors) ------------- 
fit_2a <- glm(cbind(Days_drink, Days_no_drink) ~ Total_procrastination + I(Total_procrastination^2) + Total_depression + Education + Age,
              data = health_data, family = binomial(link = "logit"))
fit_2b <- glm(cbind(Days_drink, Days_no_drink) ~ Total_procrastination + I(Total_procrastination^2) + Total_depression + Education + Age,
              data = health_data, family = binomial(link = "probit"))
fit_2c <- glm(cbind(Days_drink, Days_no_drink) ~ Total_procrastination + I(Total_procrastination^2) + Total_depression + Education + Age,
              data = health_data, family = binomial(link = "cloglog"))

# Summarizing
summary(fit_2a)
summary(fit_2b)
summary(fit_2c)

# Half normal plot
hnp(fit_2a, main = "HNP for logit model")                  # BAD
hnp(fit_2b, main = "HNP for probit model")                 # BAD
hnp(fit_2c, main = "HNP for complementary log log model")  # BAD

# Plotting
# Procrastination
visreg(fit = fit_2a, xvar = "Total_procrastination", gg = TRUE, 
       scale = "response", rug = FALSE) +
  geom_jitter(data = health_data, aes(x = Total_procrastination, y = Days_drink / 7), 
              alpha = 0.5, size = 0.9) +
  scale_x_continuous(breaks = seq(0, 60, by = 10)) +
  labs(title = "Relationship between days drinking and procrastination (GLM)",
       subtitle = "Controlling for depression, age, and education",
       x = "Total Procrasination", y = "prob(Days Drinking)") +
  theme_bw()

# Depression
visreg(fit = fit_2a, xvar = "Total_depression", gg = TRUE, 
       scale = "response", rug = FALSE) +
  geom_jitter(data = health_data, aes(x = Total_depression, y = Days_drink / 7), 
              alpha = 0.5, size = 0.9) +
  scale_x_continuous(breaks = seq(0, 8, by = 1)) +
  labs(title = "Relationship between days drinking and depression (GLM)",
       subtitle = "Controlling for procrastination, age, and education",
       x = "Total Depression", y = "prob(Days Drinking)") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

# Education
visreg(fit = fit_2a, xvar = "Education", gg = TRUE, 
       scale = "response", rug = FALSE) + 
  geom_jitter(data = health_data, aes(x = Education, y = Days_drink / 7), 
              alpha = 0.5, size = 0.9) +
  scale_x_continuous(breaks = seq(0, 6, by = 1), labels = c(
    "No Degree", "GED", "High School", 
    "College (2yrs)", "College (4yrs)", 
    "Masters", "Professional Degree")) +
  labs(title = "Relationship between days drinking and education (GLM)",
       subtitle = "Controlling for procrastination, depression, and age",
       x = "Education", y = "prob(Days Drinking)") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  ggeasy::easy_x_axis_labels_size(size = 7)

# Binomial GAM with multiple predictors ----------------------------------------
fit_3 <- gam(cbind(Days_drink, Days_no_drink) ~ s(Total_procrastination) + s(Total_depression, k = 9) + s(Age) + Education,
             data = health_data, family = "binomial", method = "REML")

# Summarizing
summary(fit_3)

# Diagnostics
fit_3_residuals <- DHARMa::simulateResiduals(fit_3)
plot(fit_3_residuals)                                   # BAD

# Plotting
# Procrastination
visreg(fit = fit_3, xvar = "Total_procrastination", gg = TRUE, 
       scale = "response", rug = FALSE) + 
  geom_jitter(data = health_data, aes(x = Total_procrastination, y = Days_drink / 7)) +
  scale_x_continuous(breaks = seq(0, 60, by = 10)) +
  labs(title = "Relationship between days drinking and procrastination (GAM)",
       subtitle = "Controlling for depression, age, and education",
       x = "Total Procrasination", y = "prob(Days Drinking)") +
  theme_bw()

# Depression
visreg(fit = fit_3, xvar = "Total_depression", gg = TRUE, 
       scale = "response", rug = FALSE) + 
  geom_jitter(data = health_data, aes(x = Total_depression, y = Days_drink / 7), 
              alpha = 0.5) +
  scale_x_continuous(breaks = seq(0, 8, by = 1)) +
  labs(title = "Relationship between days drinking and depression (GAM)",
       subtitle = "Controlling for procrastination, age, and education",
       x = "Total Depression", y = "prob(Days Drinking)") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

# Education
visreg(fit = fit_3, xvar = "Education", gg = TRUE, 
       scale = "response", rug = FALSE) +
  geom_jitter(data = health_data, aes(x = Education, y = Days_drink / 7), 
              width = 0.3, alpha = 0.5, size = 0.9) +
  scale_x_continuous(breaks = seq(0, 6, by = 1), labels = c(
    "No Degree", "GED", "High School", 
    "College (2yrs)", "College (4yrs)", 
    "Masters", "Professional Degree")) +
  labs(title = "Relationship between days drinking and education (GAM)",
       subtitle = "Controlling for procrastination, depression, and age",
       x = "Education", y = "prob(Days Drinking)") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  ggeasy::easy_x_axis_labels_size(size = 7)

# Binomial GAM with interaction for Procrastination and Age --------------------
fit_4 <- gam(cbind(Days_drink, Days_no_drink) ~ Education + s(Total_depression, k = 9) + te(Total_procrastination, Age),
             data = health_data, family = "binomial", method = "REML")

# Diagnostics
fit_4_residuals <- DHARMa::simulateResiduals(fit_4)
plot(fit_4_residuals)                                   # BAD

# Plotting
# Depression
visreg(fit = fit_3, xvar = "Total_depression", gg = TRUE, 
       scale = "response", rug = FALSE) + 
  geom_jitter(data = health_data, aes(x = Total_depression, y = Days_drink / 7), 
              alpha = 0.5) +
  scale_x_continuous(breaks = seq(0, 8, by = 1)) +
  labs(title = "Relationship between days drinking and depression (GAM)",
       subtitle = "Controlling for procrastination, age, and education",
       x = "Total Depression", y = "prob(Days Drinking)") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

# Education
visreg(fit = fit_4, xvar = "Education", gg = TRUE, 
       scale = "response", rug = FALSE) +
  geom_jitter(data = health_data, aes(x = Education, y = Days_drink / 7), 
              width = 0.3, alpha = 0.5, size = 0.9) +
  scale_x_continuous(breaks = seq(0, 6, by = 1), labels = c(
    "No Degree", "GED", "High School", 
    "College (2yrs)", "College (4yrs)", 
    "Masters", "Professional Degree")) +
  labs(title = "Relationship between days drinking and education (GAM)",
       subtitle = "Controlling for procrastination, depression, and age",
       x = "Education", y = "prob(Days Drinking)") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  ggeasy::easy_x_axis_labels_size(size = 7)

# Visualization of Interaction
vis.gam(fit_4, view = c("Total_procrastination", "Age"),
        type = "response", plot.type = 'persp', phi = 30,
        theta = 120, n.grid = 50,
        main = "Predicted probability of drinking on a given day by procrastination and age",
        xlab = "Total Procrastination (0 - 60)", ylab = "Age",
        zlab = "Predicted Probability (0 - 1)")





# Zero Inflated Model ----------------------------------------------------------
# mean(health_data$Days_drink == 0, na.rm = TRUE)
# 
# zero_fit <- pscl::zeroinfl(Days_drink ~ Total_procrastination,
#                            dist = "negbin",
#                            data = health_data)
# summary(zero_fit)
# 
# hnp::hnp(zero_fit)
# 
# test <- glm(Days_drink ~ Total_procrastination,
#             family = binomial(link = "logit"),
#             data = health_data)
# 
# summary(zero_fit)
# 
# drop1(fit_1a, test = "Chisq")
