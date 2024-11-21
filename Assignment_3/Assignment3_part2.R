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


zonal_averages <- data_filtered %>%
  filter(HTAZ %in% selected_zones) %>%
  group_by(HTAZ) %>%
  summarise(
    avg_HHSIZ = mean(HHSIZ, na.rm = TRUE),
    avg_HHVEH = mean(HHVEH, na.rm = TRUE),
    #avg_INCOME = mean(INCOME, na.rm = TRUE),
    avg_HHWRK = mean(HHWRK, na.rm = TRUE),
    total_households = n(),
    t_HHFAMINC_1 = sum(HHFAMINC_1),
    t_HHFAMINC_2 = sum(HHFAMINC_2),
    t_HHFAMINC_3 = sum(HHFAMINC_3),
    t_HHFAMINC_4 = sum(HHFAMINC_4),
    t_HHFAMINC_5 = sum(HHFAMINC_5),
    t_HHFAMINC_6 = sum(HHFAMINC_6),
    t_HHFAMINC_7 = sum(HHFAMINC_7),
    t_HHFAMINC_8 = sum(HHFAMINC_8),
    t_HHFAMINC_9 = sum(HHFAMINC_9),
    t_HHFAMINC_10 = sum(HHFAMINC_10)
  )





# Define parameter estimates from  model
params <- list(
  alpha = 2.73730,
  beta_WRKCNT = 0.42458,
  beta_HHVEHCNT = 0.16510,
  beta_HHSIZE = 2.12766,
  beta_HHFAMINC = c(
    -1.97784, -1.53642, -1.78119, -1.54876, -1.10593,
    -0.96571, -0.83258, -0.91071, -0.21164, -0.03181
  ) # Coefficients for income levels 1-10
)

# Function to calculate average trip rate using the model
calculate_trip_rate <- function(row, params) {
  # Calculate the linear part of the model
  linear_part <- params$alpha +
    params$beta_WRKCNT * row$avg_HHWRK +
    params$beta_HHVEHCNT * row$avg_HHVEH +
    params$beta_HHSIZE * row$avg_HHSIZ
  
  # Add the effect of dummy variables for income levels
  income_effect <- sum(
    c(
      row$t_HHFAMINC_1, row$t_HHFAMINC_2, row$t_HHFAMINC_3,
      row$t_HHFAMINC_4, row$t_HHFAMINC_5, row$t_HHFAMINC_6,
      row$t_HHFAMINC_7, row$t_HHFAMINC_8, row$t_HHFAMINC_9,
      row$t_HHFAMINC_10
    ) * params$beta_HHFAMINC
  )
  
  # Combine the effects for single row 
  return(linear_part * row$total_households + income_effect )
}



zonal_averages <- zonal_averages %>%
  rowwise() %>%
  mutate(
    total_trip_from_zone = calculate_trip_rate(cur_data(), params),
    
  )


print(sum(zonal_averages$total_trip_from_zone))
# Transpose the data
transposed_data <- as.data.frame(t(zonal_averages))

# Fix row and column names
colnames(transposed_data) <- rownames(zonal_averages)  # Original column names become row names


# View the transposed data
head(transposed_data)
