rm(list = ls())

library(dplyr)
library(naniar)
library(ggplot2)

path_data <- "./01__Data/02__Processed/"

# Reading in data
health_data <- readxl::read_xlsx(file.path(path_data, "Health_HRS.xlsx"))

# Seeing missing data ----------------------------------------------------------
# Whole dataset
vis_miss(health_data)

# For ONLY total columns
health_data %>%
  select(c(starts_with("Total"), "Health_problems")) %>%
  rename("Stress" = Total_stress, 
         "Anxiety" = Total_anxiety,
         "Procrastination" = Total_procrastination, 
         "Depression" = Total_depression,
         "Health Problems" = Health_problems) %>%
  gg_miss_var(show_pct = TRUE) +
  labs(y = "% Missing Data", title = "So much missing data!!") +
  theme_minimal(base_size = 12) +
  ggeasy::easy_center_title()

# Visualizing relationships with procrastination -------------------------------
health_data %>%
  select(c(starts_with("Total"), "Health_problems")) %>%
  tidyr::pivot_longer(cols = !Total_procrastination,
                      names_to = "Measure",
                      values_to = "Value") %>%
  mutate(Measure = factor(Measure)) %>%
  ggplot(aes(x = Total_procrastination, y = Value)) +
  geom_point(colour = "skyblue") +
  facet_wrap(~ Measure, labeller = labeller(
    Measure = c(Total_anxiety = "Anxiety", 
                Total_depression = "Depression", 
                Total_stress = "Stress",
                Health_problems = "Health Problems"))) +
  labs(x = "Procrastination", y = "Total") +
  theme_bw(base_size = 12) +
  ggeasy::easy_remove_legend()

# Visualizing distributions ----------------------------------------------------
# Procrastination
procrastination_distribution <- health_data %>%
  ggplot(aes(Total_procrastination)) +
  geom_histogram(binwidth = 5, fill = "skyblue", 
                 color = "black", alpha = 0.8) +
  labs(x = "Total Procrastination", y = "Frequency",
       title = "") +
  theme_minimal() +
  ggeasy::easy_center_title()

# Depression
depression_distribution<- health_data %>%
  ggplot(aes(Total_depression)) +
  geom_histogram(binwidth = 1, fill = "skyblue", 
                 color = "black", alpha = 0.8) +
  labs(x = "Total Depression", y = "Frequency",
       title = "") +
  theme_minimal() +
  ggeasy::easy_center_title()

# Anxiety
anxiety_distribution <- health_data %>%
  ggplot(aes(Total_anxiety)) +
  geom_histogram(binwidth = 1, fill = "skyblue", 
                 color = "black", alpha = 0.8) +
  labs(x = "Total Anxiety", y = "Frequency",
       title = "") +
  theme_minimal() +
  ggeasy::easy_center_title()

# Stress
stress_distribution <- health_data %>%
  ggplot(aes(Total_stress)) +
  geom_histogram(binwidth = 2, fill = "skyblue", 
                 color = "black", alpha = 0.8) +
  labs(x = "Total Stress", y = "Frequency",
       title = "") +
  theme_minimal() +
  ggeasy::easy_center_title()

# Health protection
protection_distribution <- health_data %>%
  ggplot(aes(Health_behaviours)) +
  geom_histogram(binwidth = 1, fill = "skyblue", 
                 color = "black", alpha = 0.8) +
  labs(x = "Number of Health Protective Behaviours", y = "Frequency",
       title = "") +
  theme_minimal() +
  ggeasy::easy_center_title()

# Health Problems
illness_distribution <- health_data %>%
  ggplot(aes(Health_problems)) +
  geom_histogram(binwidth = 1, fill = "skyblue", 
                 color = "black", alpha = 0.8) +
  labs(x = "Total Health Problems", y = "Frequency",
       title = "") +
  theme_minimal() +
  ggeasy::easy_center_title()

distributions <- cowplot::plot_grid(
  procrastination_distribution, depression_distribution, 
  anxiety_distribution, stress_distribution, 
  illness_distribution, protection_distribution,
  nrow = 3, ncol = 2)

# Visualizing health behaviors ------------------------------------------------
# Males
male_health <- health_data %>%
  filter(Gender == 0) %>%
  select(Prostate_exam, Cholesterol_screening, 
         Flu_shot, Dental_visit_2_years, Total_procrastination) %>%
  tidyr::pivot_longer(cols = !Total_procrastination,
                      names_to = "Health_Protection",
                      values_to = "Did") %>%
  group_by(Health_Protection, Did) %>%
  summarize(mean_procrastination = round(mean(
    Total_procrastination, na.rm = TRUE), 
    digits = 2)) %>%
  filter(complete.cases(Did)) %>%
  mutate(Health_Protection = factor(case_when(
    Health_Protection == "Flu_shot" ~ "Flu Shot",
    Health_Protection == "Cholesterol_screening" ~ "Cholesterol Screening",
    Health_Protection == "Prostate_exam" ~ "Prostate Exam",
    Health_Protection == "Dental_visit_2_years" ~ "Dental Visit (2yrs)"),
    levels = c("Flu Shot", "Cholesterol Screening", "Dental Visit (2yrs)", "Prostate Exam")),
    Did = ifelse(Did == 0, "Didn\'t Get", "Got"),
    Did = factor(Did)) %>%
  ggplot(aes(x = Health_Protection, y = mean_procrastination, fill = Did)) +
  geom_bar(stat = "identity", position = "dodge", width = .75) +
  labs(x = "", y = "Mean Procrastination", 
       title = "Male Health Protective Behaviours in 2020") +
  theme_minimal(base_size = 12) +
  # Add data labels
  geom_text(aes(label = round(mean_procrastination, 2)), 
            position = position_dodge(width = 0.75), 
            vjust = -0.5, size = 4) +
  # Scaling y-axis so labels fit
  ylim(0, 30 * 1.1) +
  ggeasy::easy_center_title() +
  ggeasy::easy_add_legend_title("") +
  ggeasy::easy_move_legend(to = c("bottom"))

# Females
female_health <- health_data %>%
  filter(Gender == 1) %>%
  select(Mammogram, Pap_smear, Cholesterol_screening, 
         Flu_shot, Dental_visit_2_years, Total_procrastination) %>%
  tidyr::pivot_longer(cols = !Total_procrastination,
                      names_to = "Health_Protection",
                      values_to = "Did") %>%
  group_by(Health_Protection, Did) %>%
  summarize(mean_procrastination = round(mean(
    Total_procrastination, na.rm = TRUE), 
    digits = 2)) %>%
  filter(complete.cases(Did)) %>%
  mutate(Health_Protection = factor(case_when(
    Health_Protection == "Flu_shot" ~ "Flu Shot",
    Health_Protection == "Cholesterol_screening" ~ "Cholesterol Screening",
    Health_Protection == "Dental_visit_2_years" ~ "Dental Visit (2yrs)",
    Health_Protection == "Mammogram" ~ "Mammogram",
    Health_Protection == "Pap_smear" ~ "Pap Smear"),
    levels = c("Flu Shot", "Cholesterol Screening", "Dental Visit (2yrs)", 
               "Mammogram", "Pap Smear")),
    Did = ifelse(Did == 0, "Didn\'t Get", "Got"),
    Did = factor(Did)) %>%
  ggplot(aes(x = Health_Protection, y = mean_procrastination, fill = Did)) +
  geom_bar(stat = "identity", position = "dodge", width = .75) +
  labs(x = "", y = "", title = "Female Health Protective Behaviours in 2020") +
  theme_minimal(base_size = 12) +
  # Add data labels
  geom_text(aes(label = round(mean_procrastination, 2)), 
            position = position_dodge(width = 0.75), 
            vjust = -0.5, size = 4) +
  # Scaling y-axis so labels fit
  ylim(0, 30 * 1.1) +
  ggeasy::easy_center_title() +
  ggeasy::easy_add_legend_title("") +
  ggeasy::easy_move_legend(to = c("bottom"))

health_behaviours <- cowplot::plot_grid(
  male_health, female_health, ncol = 2, nrow = 1)

# Exporting --------------------------------------------------------------------
export_path <- "./02__Models/Results/Figures/"

cowplot::save_plot(filename = file.path(export_path, "01__Distributions.png"),
                   plot = distributions, base_height = 10)
cowplot::save_plot(filename = file.path(export_path, "02__Health_protection.png"),
                   plot = health_behaviours, base_height = 10)


