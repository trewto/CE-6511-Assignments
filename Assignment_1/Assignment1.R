setwd("I:/Git/CE-6511-Assignments-/Assignment_1")
getwd()


# Load CSV files
hh_ga <- read.csv("hh_ga.csv", header = TRUE)
per_ga <- read.csv("per_ga.csv", header = TRUE)
trip_ga <- read.csv("trip_ga.csv", header = TRUE)
veh_ga <- read.csv("veh_ga.csv", header = TRUE)

# Part 1: Count rows for each file
num_households <- nrow(hh_ga)
num_persons <- nrow(per_ga)
num_trips <- nrow(trip_ga)
num_vehicles <- nrow(veh_ga)


# Display row counts
cat("Number of households:", num_households, "\n")
cat("Number of persons:", num_persons, "\n")
cat("Number of trips:", num_trips, "\n")
cat("Number of vehicles:", num_vehicles, "\n")




# Calculate averages
avg_persons_per_household <- num_persons / num_households
avg_trips_per_person <- num_trips / num_persons
avg_vehicles_per_household <- num_vehicles / num_households

# Display averages
cat("Average number of persons per household:", avg_persons_per_household, "\n")
cat("Average number of trips per person:", avg_trips_per_person, "\n")
cat("Average number of vehicles per household:", avg_vehicles_per_household, "\n")
