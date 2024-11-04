rm(list = ls())
hh_ga <- read.csv("hh_ga.csv")
hh_ga <- hh_ga[hh_ga$HHFAMINC >= 0, ]

#Variable
#WRKCNT , HHVEHCNT , 
#additionally HHFAMINC, HHSIZE

model1 = lm(CNTTDHH ~ WRKCOUNT + HHVEHCNT+HHFAMINC, data = hh_ga)
print("Model 1 Summary:")
summary(model1)


model2 <- lm(CNTTDHH ~ WRKCOUNT + HHVEHCNT + HHFAMINC +HHFAMINC+ HHSIZE, data = hh_ga)
print("Model 2 Summary:")
summary(model2)

#now
#comment on sing of summmy 




