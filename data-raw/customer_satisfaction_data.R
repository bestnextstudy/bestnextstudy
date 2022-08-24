## code to prepare `customer_satisfaction_data` dataset goes here

library(readr)
library(readxl)

customer_satisfaction_data <- read_csv("data-raw/Efficiency_Best_Next_Studies_Sample_Data_081522.csv")
usethis::use_data(customer_satisfaction_data, overwrite = TRUE)
