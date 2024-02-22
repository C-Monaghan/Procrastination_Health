rm(list = ls())

library(dplyr)
library(ggplot2)

# Custom functions -------------------------------------------------------------
# source(file.path("./02__Models/00__Functions.R"))

path_data <- "./01__Data/02__Processed/"

# Reading in data
health_data <- readxl::read_xlsx(file.path(path_data, "Health_HRS.xlsx"))

# Graphing ---------------------------------------------------------------------
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
  filter(Alcohol == 1) %>%
  ggplot(aes(x = Alcohol_amount, y = Total_procrastination)) +
  geom_jitter(height = 0, width = 0.4, alpha = 0.5) +
  geom_smooth(se = FALSE, method = "gam", formula = y ~ s(x, k = 1, bs = "cs")) +
  labs(title = "Amount of alcohol you drink vs. Procrastination",
       x = "# of drinks per day (in last 3 months)", y = "Total Procrastination") +
  theme_bw() +
  ggeasy::easy_center_title()

# Modelling --------------------------------------------------------------------
health_data <- health_data %>%
  rename(Days_drink = "Alcohol_week") %>%
  mutate(Days_no_drink = 7 - Days_drink, .after = Days_drink)

fit_1 <- glm(cbind(Days_no_drink, Days_drink) ~ Total_procrastination,
           data = health_data, family = binomial(link = "logit"))

fit_2 <- glm(cbind(Days_no_drink, Days_drink) ~ Total_procrastination + Total_depression + Education + Age,
             data = health_data, family = binomial(link = "logit"))

summary(fit_1)
summary(fit_2)

drop1(fit_1, test = "Chisq")
drop1(fit_2, test = "Chisq")

health_data %>%
  ggplot(aes(x = Total_procrastination, y = (Days_drink / (Days_drink + Days_no_drink)))) +
  geom_jitter() +
  geom_smooth(se = FALSE) +
  labs(x = "Total Procrastination", y = "Proportion of Days Drank") +
  theme_bw()

