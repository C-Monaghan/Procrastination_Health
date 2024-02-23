rm(list = ls())

library(dplyr)
library(ggplot2)
library(visreg)

# Custom functions -------------------------------------------------------------
# source(file.path("./02__Models/00__Functions.R"))

path_data <- "./01__Data/02__Processed/"

# Reading in data
health_data <- readxl::read_xlsx(file.path(path_data, "Health_HRS.xlsx"))

# Exploring alcohol data -------------------------------------------------------
# Do you consume alcohol
health_data %>%
  select(starts_with("Alcohol")) %>%
  ggplot(aes(Alcohol)) +
  geom_bar(width = 0.5, fill = "skyblue", colour = "black") + 
  scale_x_continuous(breaks = c(0, 1)) +
  ylim(0, 900) +
  labs(x = "Do you consume Alcohol (0 = no; 1 = yes)") +
  theme_bw()

# How many days a week for you drink alcohol
health_data %>%
  select(starts_with("Alcohol")) %>%
  filter(Alcohol == 1) %>%
  ggplot(aes(Alcohol_week)) +
  geom_histogram(binwidth = 1, fill = "skyblue", colour = "black") +
  scale_x_continuous(breaks = seq(0, 7, by = 1)) +
  labs(title = "In the last three months, on average, how many days per week have you had any alcohol to drink?",
       x = "Days per week") +
  theme_bw() +
  ggeasy::easy_center_title()

# How much Alcohol per week do you drink
health_data %>%
  select(starts_with("Alcohol")) %>%
  filter(Alcohol == 1) %>%
  ggplot(aes(Alcohol_amount)) +
  geom_bar(width = 1, fill = "skyblue", colour = "black") +
  labs(title = " In the last three months, on the days you drink, about how many drinks do you have?",
       x = "# of drinks") +
  ylim(0, 300) +
  theme_bw() +
  ggeasy::easy_center_title()

# Alcohol per week and procrastination
health_data %>%
  select(starts_with("Alcohol"), Total_procrastination) %>%
  filter(Alcohol == 1) %>%
  ggplot(aes(x = Alcohol_week, y = Total_procrastination)) +
  geom_jitter(width = 0.2) +
  geom_smooth(se = FALSE, method = "gam", formula = y ~ s(x, k = 7, bs = "cs")) +
  scale_x_continuous(breaks = seq(0, 7, by = 1)) +
  labs(title = "Days you drink alcohol vs. Procrastination",
       x = "Days per week", y = "Total Procrastination") +
  theme_bw() +
  ggeasy::easy_center_title()

# Amount of alcohol you drink vs. procrastination
health_data %>%
  select(starts_with("Alcohol"), Total_procrastination) %>%
  ggplot(aes(x = Total_procrastination, y = Alcohol_amount)) +
  geom_jitter(alpha = 0.5) +
  labs(title = "Amount of alcohol you drink vs. Procrastination",
       x = "Total Procrastination", y = "# of drinks per day (in last 3 months)") +
  theme_bw() +
  ggeasy::easy_center_title()

health_data %>%
  ggplot(aes(x = Total_procrastination, y = Days_drink / (Days_drink + Days_no_drink))) +
  geom_jitter(height = 0.05) +
  labs(x = "Proportion of Days Drank", y = "Total Procrastination") +
  theme_bw()

# Modelling --------------------------------------------------------------------
health_data <- health_data %>%
  rename(Days_drink = "Alcohol_week") %>%
  mutate(Days_no_drink = 7 - Days_drink, .after = Days_drink)

# Binomial GLM with only one predictor -----------------------------------------
fit_1 <- glm(cbind(Days_no_drink, Days_drink) ~ Total_procrastination,
           data = health_data, family = binomial(link = "logit"))

summary(fit_1)

hnp::hnp(fit_1) # A bad half-normal plot

# Plotting
visreg(fit = fit_1, xvar = "Total_procrastination", gg = TRUE,
       scale = "response", rug = FALSE) +
  scale_x_continuous(breaks = seq(0, 60, by = 10)) +
  labs(title = "Relationship between days drinking and procrastination (GLM)",
       x = "Total Procrasination", y = "prob(Days Drinking)") +
  theme_bw()

# Binomial GLM with multiple predictors ----------------------------------------
fit_2 <- glm(cbind(Days_no_drink, Days_drink) ~ Total_procrastination + Total_depression + Education + Age,
             data = health_data, family = binomial(link = "logit"))

summary(fit_2)

# Plotting
visreg(fit = fit_2, xvar = "Total_procrastination", gg = TRUE, 
       scale = "response", rug = FALSE) +
  scale_x_continuous(breaks = seq(0, 60, by = 10)) +
  labs(title = "Relationship between days drinking and procrastination (GLM)",
       subtitle = "Controlling for depression, age, and education",
       x = "Total Procrasination", y = "prob(Days Drinking)") +
  theme_bw()


# Binomial GAM with multiple predictors ----------------------------------------
fit_3 <- mgcv::gam(cbind(Days_no_drink, Days_drink) ~ s(Total_procrastination) + s(Total_depression, k = 9) + s(Age) + Education,
                   data = health_data, family = "binomial", method = "REML")

summary(fit_3)

# Diagnostic Plot
fit_3_residuals <- DHARMa::simulateResiduals(fit_3, n = 1000, seed = 1234)
plot(fit_3_residuals) # This looks BADDDDDDD

# Plotting (All together)
plot(fit_3, pages = 1, 
     shift = fit_3$coefficients[1], 
     trans = plogis, rug = FALSE, 
     shade = TRUE, shade.col = "skyblue")

visreg(fit = fit_3, xvar = "Total_procrastination", gg = TRUE, 
       scale = "response", rug = FALSE) +
  scale_x_continuous(breaks = seq(0, 60, by = 10)) +
  labs(title = "Relationship between days drinking and procrastination (GAM)",
       subtitle = "Controlling for depression, age, and education",
       x = "Total Procrasination", y = "prob(Days Drinking)") +
  theme_bw()

# Binomial GAM with interaction for Procrastination and Age
fit_4 <- mgcv::gam(cbind(Days_no_drink, Days_drink) ~ Education + s(Total_depression, k = 9) + te(Total_procrastination, Age),
                   data = health_data, family = "binomial", method = "REML")

fit_4_residuals <- DHARMa::simulateResiduals(fit_4, n = 1000, seed = 1234)
plot(fit_4_residuals)

# Visualization of Interaction
mgcv::vis.gam(fit_4, view = c("Total_procrastination", "Age"),
              type = "response", plot.type = 'persp', phi = 30,
              theta = 120, n.grid = 50,   
              main = "Predicted probability of drinking on a given day by procrastination and age",
              xlab = "Total Procrastination (0 - 60)", ylab = "Age", 
              zlab = "Predicted Probability (0 - 1)")

# Visualizing depression
plot(fit_4, select = 1, trans = plogis, 
     xlab = "Total Depression", ylim = c(0, 1), 
     shade = TRUE, shade.col = "skyblue")



health_data %>%
  ggplot(aes(Days_drink / (Days_drink + Days_no_drink))) +
  geom_bar(fill = "skyblue", colour = "black") +
  labs(x = "Proportion of Days Drank") +
  theme_bw()
