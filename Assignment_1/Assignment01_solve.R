library(dplyr)
library(ggplot2)

setwd("I:/Git/CE-6511-Assignments-/Assignment_1")
getwd()


# Load CSV files
hh_ga <- read.csv("hh_ga.csv", header = TRUE)
per_ga <- read.csv("per_ga.csv", header = TRUE)
trip_ga <- read.csv("trip_ga.csv", header = TRUE)
veh_ga <- read.csv("veh_ga.csv", header = TRUE)

# Function to calculate mode
calculate_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Set working directory
setwd("I:/Git/CE-6511-Assignments-/Assignment_1")
getwd()

# Load CSV files
hh_ga <- read.csv("hh_ga.csv", header = TRUE)
per_ga <- read.csv("per_ga.csv", header = TRUE)
trip_ga <- read.csv("trip_ga.csv", header = TRUE)
veh_ga <- read.csv("veh_ga.csv", header = TRUE)

# Household Data Calculations (Already provided)
# HHSIZE
mean_hhsize <- mean(hh_ga$HHSIZE, na.rm = TRUE)
median_hhsize <- median(hh_ga$HHSIZE, na.rm = TRUE)
mode_hhsize <- calculate_mode(hh_ga$HHSIZE)

# HHVEHCNT
mean_vehcount <- mean(hh_ga$HHVEHCNT, na.rm = TRUE)
median_vehcount <- median(hh_ga$HHVEHCNT, na.rm = TRUE)
mode_vehcount <- calculate_mode(hh_ga$HHVEHCNT)

# HHFAMINC
mean_income <- mean(hh_ga$HHFAMINC, na.rm = TRUE)
median_income <- median(hh_ga$HHFAMINC, na.rm = TRUE)
mode_income <- calculate_mode(hh_ga$HHFAMINC)

# Displaying Household Data Statistics
cat("Household Data Statistics:\n")
cat("----------------------------\n")
cat("HHSIZE (Household Size):\n")
cat("Mean: ", mean_hhsize, "\n")
cat("Median: ", median_hhsize, "\n")
cat("Mode: ", mode_hhsize, "\n\n")

cat("HHVEHCNT (Household Vehicle Count):\n")
cat("Mean: ", mean_vehcount, "\n")
cat("Median: ", median_vehcount, "\n")
cat("Mode: ", mode_vehcount, "\n\n")

cat("HHFAMINC (Household Income):\n")
cat("Mean: ", mean_income, "\n")
cat("Median: ", median_income, "\n")
cat("Mode: ", mode_income, "\n\n")

# Person Data Calculations
# R_AGE
mean_age <- mean(per_ga$R_AGE, na.rm = TRUE)
median_age <- median(per_ga$R_AGE, na.rm = TRUE)
mode_age <- calculate_mode(per_ga$R_AGE)

# DRIVER (as categorical distribution)
driver_dist <- table(per_ga$DRIVER) / nrow(per_ga) * 100

# Displaying Person Data Statistics
cat("Person Data Statistics:\n")
cat("----------------------------\n")
cat("R_AGE (Age):\n")
cat("Mean: ", mean_age, "\n")
cat("Median: ", median_age, "\n")
cat("Mode: ", mode_age, "\n\n")

cat("DRIVER (Driver Status) Distribution (%):\n")
print(driver_dist)
cat("\n")

# Vehicle Data Calculations
# ANNMILES
mean_annmiles <- mean(veh_ga$ANNMILES, na.rm = TRUE)
median_annmiles <- median(veh_ga$ANNMILES, na.rm = TRUE)
mode_annmiles <- calculate_mode(veh_ga$ANNMILES)

# Displaying Vehicle Data Statistics
cat("Vehicle Data Statistics:\n")
cat("----------------------------\n")
cat("ANNMILES (Annual Mileage):\n")
cat("Mean: ", mean_annmiles, "\n")
cat("Median: ", median_annmiles, "\n")
cat("Mode: ", mode_annmiles, "\n\n")

# Trip Data Calculations
# TRPMILES
mean_tripmiles <- mean(trip_ga$TRPMILES, na.rm = TRUE)
median_tripmiles <- median(trip_ga$TRPMILES, na.rm = TRUE)
mode_tripmiles <- calculate_mode(trip_ga$TRPMILES)

# TRVLCMIN
mean_trvlcmin <- mean(trip_ga$TRVLCMIN, na.rm = TRUE)
median_trvlcmin <- median(trip_ga$TRVLCMIN, na.rm = TRUE)
mode_trvlcmin <- calculate_mode(trip_ga$TRVLCMIN)

# Displaying Trip Data Statistics
cat("Trip Data Statistics:\n")
cat("----------------------------\n")
cat("TRPMILES (Trip Distance in Miles):\n")
cat("Mean: ", mean_tripmiles, "\n")
cat("Median: ", median_tripmiles, "\n")
cat("Mode: ", mode_tripmiles, "\n\n")

cat("TRVLCMIN (Trip Duration):\n")
cat("Mean: ", mean_trvlcmin, "\n")
cat("Median: ", median_trvlcmin, "\n")
cat("Mode: ", mode_trvlcmin, "\n")



# Histogram for Household Size
ggplot(hh_ga, aes(x = HHSIZE)) + 
  geom_histogram(binwidth = 1, fill = "blue", color = "black") + 
  ggtitle("Household Size Distribution") +
  xlab("Household Size") + ylab("Frequency")

# Histogram for Age
ggplot(per_ga, aes(x = R_AGE)) + 
  geom_histogram(binwidth = 5, fill = "green", color = "black") + 
  ggtitle("Age Distribution") +
  xlab("Age") + ylab("Frequency")

# Histogram for Trip Distance
ggplot(trip_ga, aes(x = TRPMILES)) + 
  geom_histogram(binwidth = 5, fill = "orange", color = "black") + 
  ggtitle("Trip Distance Distribution") +
  xlab("Trip Miles") + ylab("Frequency")


# Histogram for Household Vehicle Count
ggplot(hh_ga, aes(x = HHVEHCNT)) + 
  geom_histogram(binwidth = 1, fill = "orange", color = "black") + 
  ggtitle("Household Vehicle Count Distribution") +
  xlab("Household Vehicle Count") + ylab("Frequency")

# Histogram for Household Income
ggplot(hh_ga, aes(x = HHFAMINC)) + 
  geom_histogram(binwidth = 5000, fill = "purple", color = "black") + 
  ggtitle("Household Income Distribution") +
  xlab("Household Income") + ylab("Frequency")

# Histogram for Annual Mileage
ggplot(veh_ga, aes(x = ANNMILES)) + 
  geom_histogram(binwidth = 1000, fill = "cyan", color = "black") + 
  ggtitle("Annual Mileage Distribution") +
  xlab("Annual Mileage") + ylab("Frequency")

# Bar Plot for Driver Status
ggplot(data = per_ga, aes(x = DRIVER)) + 
  geom_bar(fill = "lightblue", color = "black") + 
  ggtitle("Driver Status Distribution") +
  xlab("Driver Status") + ylab("Count")

# Histogram for Trip Distance
ggplot(trip_ga, aes(x = TRPMILES)) + 
  geom_histogram(binwidth = 5, fill = "red", color = "black") + 
  ggtitle("Trip Distance Distribution") +
  xlab("Trip Distance (Miles)") + ylab("Frequency")

# Histogram for Trip Duration
ggplot(trip_ga, aes(x = TRVLCMIN)) + 
  geom_histogram(binwidth = 5, fill = "pink", color = "black") + 
  ggtitle("Trip Duration Distribution") +
  xlab("Trip Duration (Minutes)") + ylab("Frequency")