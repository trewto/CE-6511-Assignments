rm(list = ls())
#install.packages("readxl")
library(readxl)

setwd("I:/Git/CE-6511-Assignments-/Classwork")
data <- read_excel("Mode_choice_enumeration.xlsx")

head(data)          # View the first few rows
str(data)           # Check the structure of the dataset
summary(data)       # Summary statistics for each column