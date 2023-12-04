rm(list = ls())

library(ggplot2)

path_data <- "./01__Data/02__Processed/"

# Reading in data
health_data <- readxl::read_xlsx(file.path(path_data, "Health_HRS.xlsx"))

ggplot(health_data, aes(x = Total_procrastination, y = HRP_count)) +
  geom_point() +
  theme_classic()
