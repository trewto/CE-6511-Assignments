# Use this command to set the correct working directory in your machine
# Working directory should preferably be the location where you have saved your data file
# bus practically you can set working directory at any folder where you have access to
setwd("C:/Users/proti/OneDrive/00 BUET/05 Postgraduate/01 Transportation Planning CE 6511/National Household Travel Survey Data/csv")

hh_ga = read.csv("hh_ga.csv",header=TRUE)

### You can use the following command to clear memory
rm(list = ls())


#if the variable is continuous use can use the summary() command - it will give you the min, max, as well as other three quartile values
summary(hh_ga$TRAVDAY)

# if you are interested in the distribution of the continuous variable you can calculate percentile at 1% interval
quantile = quantile(hh_ga$TRAVDAY, seq(0.01,1,by = 0.01))

#plot the distribution
plot(quantile,seq(0.01,1,by = 0.01) )

# if the variable is categorical you can use the table() command to get the frequency
table(hh_ga$TRAVDAY)

# you can easily convert the frequencies into percentages using the following command
(table(hh_ga$TRAVDAY)/nrow(hh_ga))*100  # Here nrow() gives the # of rows in hh_ga

hist(hh_ga$TRAVDAY, breaks = 7)
