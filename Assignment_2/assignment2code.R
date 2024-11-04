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





################################


# Check the frequency distribution of HHFAMINC
income_distribution <- table(hh_ga$HHFAMINC)
print("Income Distribution:")
print(income_distribution)


# Calculate the frequency distribution of HHFAMINC
income_distribution <- table(hh_ga$HHFAMINC)

# Plot the income distribution
barplot(income_distribution,
        main = "Frequency Distribution of Household Income Categories (HHFAMINC)",
        xlab = "Income Category",
        ylab = "Number of Households",
        col = "skyblue",
        names.arg = 1:11,  # Label the x-axis with category numbers
        border = "blue")


valid_income_categories <- names(income_distribution[income_distribution >= 30])
print("Valid Income Categories with at least 30 observations:")
print(valid_income_categories)

# Create a dummy variable for each valid income category
for (cat in valid_income_categories) {
  hh_ga[[paste0("HHFAMINC_", cat)]] <- ifelse(hh_ga$HHFAMINC == as.numeric(cat), 1, 0)
}


model_formula <- as.formula(
  paste("CNTTDHH ~ WRKCOUNT + HHVEHCNT + HHSIZE +",
  paste(paste0("HHFAMINC_", valid_income_categories), collapse = " + "))
)

model_formula

model_with_dummies <- lm(model_formula, data = hh_ga)
summary(model_with_dummies)



#if we groupp 

# Create income group dummies based on the specified groups
hh_ga$HHFAMINC_A <- ifelse(hh_ga$HHFAMINC %in% 1:3, 1, 0)  # Group A: HHFAMINC 1-3
hh_ga$HHFAMINC_B <- ifelse(hh_ga$HHFAMINC %in% 4:7, 1, 0)  # Group B: HHFAMINC 4-7
hh_ga$HHFAMINC_C <- ifelse(hh_ga$HHFAMINC %in% 9:11, 1, 0) # Group C: HHFAMINC 9-11

# Model formula using the grouped income dummies (excluding category 8 as the baseline)
model_formula_grouped <- CNTTDHH ~ WRKCOUNT + HHVEHCNT + HHSIZE + HHFAMINC_A + HHFAMINC_B + HHFAMINC_C

# Fit the model with grouped income dummies
model_with_grouped_dummies <- lm(model_formula_grouped, data = hh_ga)
summary(model_with_grouped_dummies)


