# Use this command to set the correct working directory in your machine
# Working directory should preferably be the location where you have saved your data file
# bus practically you can set working directory at any folder where you have access to
#setwd("C:/Users/proti/OneDrive/00 BUET/05 Postgraduate/01 Transportation Planning CE 6511/National Household Travel Survey Data/csv")

### You can use the following command to clear memory
rm(list = ls())

# read the data file
#hh_ga = read.csv("hh_ga.csv",header=TRUE)
hh_ga <- read.csv("I:/Git/CE-6511-Assignments-/Assignment_2/hh_ga.csv")

# get rid off the rows where the income is negative
hh_ga = subset(hh_ga, (hh_ga$HHFAMINC > 0))


# run a linear regression model
model = lm(CNTTDHH ~ WRKCOUNT + HHVEHCNT, data = hh_ga)
summary(model)


# dummy variable resgression
# create hh-veh_dummies
model2 = lm(CNTTDHH~WRKCOUNT+I(HHVEHCNT==1) + I(HHVEHCNT==2) + 
              I(HHVEHCNT==3) + I(HHVEHCNT==4) + I(HHVEHCNT>=5),data = hh_ga)
summary(model2)

SSU = sum((hh_ga$CNTTDHH - predict(model2))^2)

model3 = lm(CNTTDHH~WRKCOUNT+I(HHVEHCNT==1) + I((HHVEHCNT>=2)&(HHVEHCNT<=4)) + I(HHVEHCNT>=5),data = hh_ga)
summary(model3)

SSR = sum(resid(model3)^2)

F_hat = ((SSR - SSU)*(nrow(hh_ga) - 7))/(SSU * 5)

# How to check the F-criticial value in R

qf(0.975, 4,5, lower.tail = TRUE)

# CREATE A FREQUENCY TABLE FOR INCOME



