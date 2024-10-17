rm(list = ls())

set.seed(2468) # Reproducibility

# Loading libraries ------------------------------------------------------------
pacman::p_load(
  dplyr,         # Data manipulation
  purrr,         # Easily map functions onto data
  broom,         # Tidy model out
  mgcv,          # For using GAMs
  visreg,        # Visualize GAMs
  ggplot2,       # Extend GAM visualization
  cowplot,       # Saving plots
  patchwork,     # Plotting grids
  scales         # Adjusting scales
)

# Source files with all GAM functions
source(file = here::here("02__Models/00b__Functions_GAM.R"))

# Data Importing ---------------------------------------------------------------
path_data <- "./01__Data/02__Processed/"

# Reading in data
health_data <- readxl::read_xlsx(file.path(path_data, "Health_HRS.xlsx"))

# Data processing --------------------------------------------------------------
# Filtering to be people 50 or over removing those with a high level of 
# missingness in procrastination
health_data <- health_data %>%
  filter(Age >= 50) %>%
  filter(!is.na(Total_procrastination))

# Stratifying by sex
health_data_males <- health_data %>% filter(Gender == 0)

health_data_females <- health_data %>% filter(Gender == 1)

# Analyzing data using GAMs ----------------------------------------------------
# Sex specific formulas 
formulas_males <- list(
  Prostate_exam  = "~ s(Age) + te(Total_procrastination, Total_depression) + ci_binary + Retired + Medicare_coverage",
  C_screening    = "~ s(Total_procrastination) + s(Total_depression, k = 9) + s(Age) + ci_binary + Retired + Medicare_coverage",
  Flu_shot       = "~ s(Total_procrastination) + s(Total_depression, k = 9) + s(Age) + ci_binary + Retired + Medicare_coverage",
  Dental_visit   = "~ s(Age) + te(Total_procrastination, Total_depression) + ci_binary + Retired + Medicare_coverage"
)

formulas_females <- list(
  Mammogram      = "~ s(Total_procrastination) + s(Total_depression, k = 9) + s(Age) + ci_binary + Retired + Medicare_coverage",
  C_screening    = "~ s(Total_procrastination) + s(Total_depression, k = 9) + s(Age) + ci_binary + Retired + Medicare_coverage",
  Pap_smear      = "~ s(Total_procrastination) + s(Total_depression, k = 9) + s(Age) + ci_binary + Retired + Medicare_coverage",
  Flu_shot       = "~ s(Total_procrastination) + s(Total_depression, k = 9) + s(Age) + ci_binary + Retired + Medicare_coverage",
  Dental_visit   = "~ s(Age) + te(Total_procrastination, Total_depression) + ci_binary + Retired + Medicare_coverage"
)

# Fitting GAM
fit_males   <- fit_gam_models(health_data_males, formulas_males)
fit_females <- fit_gam_models(health_data_females, formulas_females)

k.check(fit_males[[1]]) %>%
  as.data.frame() %>%
  mutate(across(c(edf, "k-index"), \(x) round(x, digits = 2))) %>%
  mutate("p-value" = round(`p-value`, digits = 3))

AIC(fit_males[[1]])

sapply(fit_males, AIC)

# Summarizing and tidying output -----------------------------------------------
# Creating vectors of responses and terms
# Males
responses_males <- c(
  "Prostate Exams", "Prostate Exams", "Cholesterol Screening",
  "Cholesterol Screening", "Cholesterol Screening", "Flu Shots",
  "Flu Shots", "Flu Shots", "Dental Visit", "Dental Visit")

terms_males <- c(
  "Age", "Procrastination x Depression", "Procrastination", "Depression", "Age",
  "Procrastination", "Depression", "Age", "Age", "Procrastination x Depression")

# Females
responses_females <- c(
  "Mammograms", "Mammograms", "Mammograms",
  "Cholesterol Screening", "Cholesterol Screening", "Cholesterol Screening",
  "Pap Smears", "Pap Smears", "Pap Smears", 
  "Flu Shots", "Flu Shots", "Flu Shots", "Dental Visit", "Dental Visit")

terms_females <- c(
  "Procrastination", "Depression", "Age",
  "Procrastination", "Depression", "Age", "Procrastination", "Depression", "Age",
  "Procrastination", "Depression", "Age", "Age", "Procrastination x Depression")

# Creating dataframe of fits
tidy_males   <- tidy_and_combine(fit_males, responses_males, terms_males)
tidy_females <- tidy_and_combine(fit_females, responses_females, terms_females)

# Visualizing models -----------------------------------------------------------
# MAIN EFFECTS -----------------------------------------------------------------
# For axis labeling
prevention_males <- c(
  "Prostate_exam", "C_screening", 
  "Flu_shot", "Dental_visit")

prevention_males_tidy <- c(
  "Prostate exams", "Cholesterol screenings", 
  "Flu shots", "Dental visits")

prevention_females <- c(
  "Mammogram", "C_screening", "Pap_smear", 
  "Flu_shot", "Dental_visit")

prevention_females_tidy <- c(
  "Mammograms", "Cholesterol screenings", "Pap smears", 
  "Flu shots", "Dental visits")

# Creating plots
plots_males   <- list()
plots_females <- list()

for(m in seq_along(fit_males)) {

  plots_males[[m]] <- plot_predictions(
    model = fit_males[[m]], data = health_data_males,
    x_var = "Total_procrastination", y_var = prevention_males[m],
    x_label = "Procrastination", y_label = prevention_males_tidy[m], gender = "Men")
}

for(f in seq_along(fit_females)) {
  plots_females[[f]] <- plot_predictions(
    model = fit_females[[f]], data = health_data_females,
    x_var = "Total_procrastination", y_var = prevention_females[f],
    x_label = "Procrastination", y_label = prevention_females_tidy[f], gender = "Women")
}

# Plotting as a grid 
main_effects <- (plots_females[[1]] + plots_females[[2]]) / (plots_females[[3]] + plots_females[[4]]) / (plots_males[[2]] + plots_males[[3]]) +
  plot_annotation(
    title = "Predicted probabilties from generalised additive models (Main effects)",
    theme = theme(
      plot.title = element_text(size = 15, face = "bold", lineheight = 1.1, hjust = 0.5)))

# INTERACTION EFFECTS ----------------------------------------------------------
# Making a new dataset
new_data <- expand.grid(
  Total_procrastination = seq(0, 60, length = 200),
  Total_depression = seq(0, 8, length = 200),
  Age = median(health_data$Age),
  ci_binary = 0,
  Retired = 0,
  Medicare_coverage = 0)

# Making predictions
prostate_preds       <- predict(fit_males[[1]],   newdata = new_data, type = "response", se.fit = TRUE)
dental_preds_males   <- predict(fit_males[[4]],   newdata = new_data, type = "response", se.fit = TRUE)
dental_preds_females <- predict(fit_females[[4]], newdata = new_data, type = "response", se.fit = TRUE)

# Adding to dataset (incl std errors)
new_data$prostate_preds       <- prostate_preds$fit
new_data$prostate_se          <- prostate_preds$se.fit

new_data$dental_preds_males   <- dental_preds_males$fit
new_data$dental_se_males      <- dental_preds_males$se.fit

new_data$dental_preds_females <- dental_preds_females$fit
new_data$dental_se_females    <- dental_preds_females$se.fit

# Plotting interaction predictions ---------------------------------------------
prostate <- create_heatmap(
  data = new_data, preds = "prostate_preds", se = "prostate_se",
  title = "Predicted probability of prostate exam for men\nby procrastination and depression"
)

dental_male <- create_heatmap(
  data = new_data, preds = "dental_preds_males", se = "dental_se_males",
  title = "Predicted probability of dental visit for men\nby procrastination and depression"
)

dental_female <- create_heatmap(
  data = new_data, preds = "dental_preds_females", se = "dental_se_females",
  title = "Predicted probability of dental visit for women\nby procrastination and depression"
)

# Plotting as grid 
map_males <- prostate + dental_male + plot_layout(axis_titles = "collect", guides = "collect")
map_females <- dental_female

# Exporting --------------------------------------------------------------------
export_path_data     <- "./02__Models/Results/"
export_path_graphics <- "./02__Models/Results/Figures/02__GAM/"

# Saving results of GAMs
writexl::write_xlsx(
  path = here::here(export_path_data, "01__Results_Males.xlsx"),
  tidy_males, col_names = TRUE)

writexl::write_xlsx(
  path = here::here(export_path_data, "02__Results_Females.xlsx"),
  tidy_females, col_names = TRUE)

# Saving plots
# save_gam_plot("Main_effects_males.png", grid_males)
# save_gam_plot("Main_effects_females.png", grid_females)
save_gam_plot("Map_males.png", map_males, height = 8)
save_gam_plot("Map_females.png", map_females, height = 8)

save_gam_plot("Main_effects.png", main_effects)

