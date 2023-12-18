rm(list = ls())

library(dplyr)
library(mvtnorm)

path_data <- "./01__Data/02__Processed/"

# Reading in data
health_data <- readxl::read_xlsx(file.path(path_data, "Health_HRS.xlsx"))

# Running seperate linear regressions (find better way)
model <- lm(cbind(
  Back_pain, Headache, Fatigue, Alcohol, Blood_pressure, Diabetes, Cholesterol, Heart_condition) ~ Total_procrastination,
  data = health_data)

# summary(model)

# Exporting
export_path <- "./02__Models/Results/"

output <- capture.output(summary(model))

writeLines(output, file.path(export_path, "04__Regression_results_TEMP.txt"))
