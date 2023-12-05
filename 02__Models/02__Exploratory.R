rm(list = ls())

library(ggplot2)

path_data <- "./01__Data/02__Processed/"

# Reading in data
health_data <- readxl::read_xlsx(file.path(path_data, "Health_HRS.xlsx"))

naniar::vis_miss(health_data)