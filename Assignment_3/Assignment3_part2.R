rm(list = ls())

library(dplyr)
library(tidyr)
library(readxl)  # To read Excel files

data <- read_excel("HH_file_with_formulae.xlsx",range = "A1:F1000")

# Filter the dataset to include only the relevant TAZ zones (522, 521, 175, 39, 72, 906)
#this is not needed actually as already the dataset is filterd out 
selected_zones <- c(522, 521, 175, 39, 72, 906)
data_filtered <- data %>% filter(HTAZ %in% selected_zones)

head(data_filtered)

# Create dummy variables for income levels 1 to 10
data_filtered <- data_filtered %>%
  mutate(
    HHFAMINC_1 = ifelse(INCOME == 1, 1, 0),
    HHFAMINC_2 = ifelse(INCOME == 2, 1, 0),
    HHFAMINC_3 = ifelse(INCOME == 3, 1, 0),
    HHFAMINC_4 = ifelse(INCOME == 4, 1, 0),
    HHFAMINC_5 = ifelse(INCOME == 5, 1, 0),
    HHFAMINC_6 = ifelse(INCOME == 6, 1, 0),
    HHFAMINC_7 = ifelse(INCOME == 7, 1, 0),
    HHFAMINC_8 = ifelse(INCOME == 8, 1, 0),
    HHFAMINC_9 = ifelse(INCOME == 9, 1, 0),
    HHFAMINC_10 = ifelse(INCOME == 10, 1, 0)
  )

# View the updated dataset with dummy variables
head(data_filtered)
