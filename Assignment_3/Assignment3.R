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


zonal_averages <- data_filtered %>%
  filter(HTAZ %in% selected_zones) %>%
  group_by(HTAZ) %>%
  summarise(
    avg_HHSIZ = mean(HHSIZ, na.rm = TRUE),
    avg_HHVEH = mean(HHVEH, na.rm = TRUE),
    avg_INCOME = mean(INCOME, na.rm = TRUE),
    avg_HHWRK = mean(HHWRK, na.rm = TRUE),
    total_households = n()
  )

print(zonal_averages)
#Linear Model (using previous parameter estimates)
# Using the parameters from  model you +++
alpha <- 0.54266
beta_HHWRK <- 0.42206
beta_HHVEH <- 0.16388
beta_INCOME <- 0.19835
beta_HHSIZ <- 2.12586


linear_model_forecast <- function(zonal_averages, alpha, beta_HHWRK, beta_HHVEH, beta_INCOME, beta_HHSIZ) {
  return(alpha + beta_HHWRK * zonal_averages$avg_HHWRK + 
           beta_HHVEH * zonal_averages$avg_HHVEH + 
           beta_INCOME * zonal_averages$avg_INCOME + 
           beta_HHSIZ * zonal_averages$avg_HHSIZ)
}


zonal_averages$averagetrip <- linear_model_forecast(zonal_averages, alpha, beta_HHWRK, beta_HHVEH, beta_INCOME, beta_HHSIZ)


zonal_averages$total_trips <- zonal_averages$averagetrip * zonal_averages$total_households


print(zonal_averages)
#sum of total trip 
print(sum(zonal_averages$total_trips))