setwd("seminars/11")
# ==============================================================================
# --------------------------------- SEMINAR 11 ---------------------------------
# ==============================================================================


# ----------------------------------- TASK 1 -----------------------------------


prices <- read.csv("GoldSilver.csv")

prices$time <- as.Date(prices$time)

time <- prices$time
gold <- prices$gold
silver <- prices$silver

# plot the dependence of the time on the gold price and silver price (2 figures):

plot()
plot()


# ....................................... A ....................................


# Create a linear model of the silver price dependent on the time.

# HINT: use lm() function:

?lm

model_silver_1 <- lm(silver ~ time)

(sum_silver_1 <- summary(model_silver_1))

# diagnostic graphs:

par(mfrow = c(2, 2))
plot(model_silver_1)
par(mfrow = c(1, 1))

# residuals do not look independent and normally distributed with the same variance

# now plot the results: firstly, acces the coefficients beta, then compute the
# values of price for the time vector using your model (write the prescription
# for the regression line in the form y = a*x + b) and the coefficients beta

str(model_silver_1)

# select the coefficients of the model and complete the line prescription:

coeffs <- 
silver_line <-  +  * as.numeric(time)

# plot the result line together with the original data:

plot(, , type = 'l')
lines(, , type = 'l', col = 'red')

# quality of the model (adjusted R squared, AIC):

# HINT: use the structure of sum_silver_1 and AIC() function:

str(sum_silver_1)
?AIC

sum_silver_1[]
AIC()

# prediction:

?as.Date

# 1) use the regression line prescription: pred = a + b * time:

 +  * as.numeric(as.Date("2013-04-25"))

# 2) use the built-in function predict()

# HINT: figure out what are the inputs of predict() function:

?predict

data_new <- data.frame()
predict(, data_new)


# ....................................... B ....................................


# Create a linear model of the logarithm of the silver price dependent on the time.

silver_log <- log(silver)

model_silver_2 <- lm()

sum_silver_2 <- 

# the diagnostic graphs:

par(mfrow = c(2, 2))
plot()
par(mfrow = c(1, 1))

# the pressumptions of the linear regression model seems to be more realistic then before

# now plot the results (the logarithm of silver price and the line against the time):

# HINT: choose the coefficients from the model_silver_2:

coeffs <- 

# write the regression line prescription:

silver_line_2 <-  +  * as.numeric(time)

plot(, , type = 'l')
lines(, , type = 'l', col = 'red')

# finally, plot the original silver data together with the final regression curve

# HINT: what is the inverse transformation of the logaritmic one? Apply this
# transformation to both: logarithm of silver (the result should be the original
# silver data sample) and the regression line

silver_exp <- 

plot(time, silver, type = 'l')
lines(time, silver_exp, type = 'l', col = 'red')

# quality of the model:

sum_silver_2[]
AIC()

# prediction:

# HINT: apply the inverse transformation to the reggression line prescription
# evaluated on the date 2013-04-25:




# ----------------------------------- TASK 2 -----------------------------------


comp <- read.csv("Computers.csv", sep = ",")
comp <- comp[, -1] # delete the last variable

# converting the variables to the appropriate data type:

for (i in c(4, 5, 6, 7, 8)) {
  comp[, i] <- as.factor(comp[, i])
}

for (i in c(1, 2, 3, 9)) {
  comp[, i] <- as.numeric(comp[, i])
}

# better approach (using the apply() function):

?lapply

comp[, c(4, 5, 6, 7, 8)] <- lapply(comp[, c(4, 5, 6, 7, 8)], as.factor)
comp[, c(1, 2, 3, 9)] <- lapply(comp[, c(1, 2, 3, 9)], as.numeric)


# ..................................... A ......................................


# Create a linear model using the variables SPEED and RAM, use the model without
# any interactions (1_A), only with interactions (1_B) and model with both the
# variables themselfs and the interactions (1_C). Which model seems to be the best?

# HINT: specify the "data" input of lm() function to call just the names of the,
#  variables interactions can be added by : notation, for example speed:ram,
# see "details" section in documentation:

?lm

model_1_A <- 
model_1_B <- 
model_1_C <- 

summary(model_1_A)
summary(model_1_B)
summary(model_1_C)

# the diagnostic plots of the best model:

par(mfrow = c(2, 2))
plot()
par(mfrow = c(1, 1))

# store the summary of the best model:

sum_1 <- summary()


# ..................................... B ......................................


# Create a full linear regression model considering all the variables.

model_2 <- 

(sum_2 <- summary(model_2))

# the diagnostic plots:

par(mfrow = c(2, 2))
plot(model_1_C)
par(mfrow = c(1, 1))


# ..................................... C ......................................


# Create a full model with the interactions of all pairs of the variables.
# Apply the backward step-wise procedure to this model.

# HINT: all variables can be specified by "." notation, for example price ~ .
# the "." can be also squared:

model_3 <- 

(sum_3 <- summary(model_3))

# the step-wise procedure (backward):

# HINT: use the step() built-in function:

?step

model_4 <- step()

model_4$anova
(sum_4 <- summary())

par(mfrow = c(2, 2))
plot()
par(mfrow = c(1, 1))


# ..................................... E ......................................


# Compare the quality of your models using the adjusted R-squared.

# HINT: use the adjusted R squared from the summaries, store them into a vector:

results <- c()
