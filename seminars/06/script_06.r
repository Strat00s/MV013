setwd("seminars/05")
# ==============================================================================
# --------------------------------- SEMINAR 6 ----------------------------------
# ==============================================================================


data <- c(19.92, 21.17, 24.87, 22.90, 20.88, 19.43, 19.39, 23.14, 16.99, 24.85,
          20.80, 19.93, 20.28, 27.61, 27.50, 19.63, 20.35, 22.96, 17.57, 22.91,
          21.84, 25.34, 20.13, 18.30, 15.40, 22.88, 21.90, 25.30, 21.86, 22.47)


# ----------------------------------- TASK 1 -----------------------------------


n <- length(data)
alpha <- 0.05 # confidence / significance level


# ....................................... A ....................................


# Estimate parameters mu and sigma of the normal distribution N(mu, sigma) using
# the maximum likelihood method. Then construct a two-sided confidence interval
# for parameter mu. Create a visualization of all informations you have computed (plot
# the histogram together with the density of estimated normal distribution and with CI).

# maximum likelihood estimation of parameters mu and sigma
# (from the theoretical formulas):

mu <- sum(data) / n
sigma <- sqrt(sum((data - mu)^2) / n)

# compute the quantile we need:

mu.q <- quantile(data, 1 - (alpha/2))

# compute the lower and upper limits of the CI for parameter mu
# (use formulas from the lecture):

CI_lower <- mu - mu.q * (sigma^2)/n
CI_upper <- mu + u.q * (sigma^2)/n

# plot the histogram of the data again together with the denstiy of normal distribution
# with estimated parameters and visualization of the MLE of mu with the CI for mu:

# HINT: for visualizating mu and its CI use function abline(), where "v" input 
# specifies the x-axis coordinate(s) of the vertical line(s). You can use function
# rect() plotting the rectangle as well.

?abline
?rect

xx <- 
density.n <- 

hist(, freq = F, main = "Two sided CI for mean", col = "antiquewhite")
lines(, , lty = 2, lwd = 1.5)

abline(v = , col = 'red', lwd = 2)
rect(CI_lower, 0, CI_upper, 0.15, col = rgb(red = 1, green = 0, blue = 0, alpha = 0.3))

abline(v = mu, col = 'blue', lwd = 2)
legend("topright", legend = c("mu from MLE", "CI limits"), col = c("blue", "red"), lty = c(1, 1), bty = "n")


# ....................................... B ....................................


# Test the null hypothesis H0: mu = 20, against the alternative H1: mu != 20
# at the significance level alpha = 0.05. Compute the value of the test statistic
# and find the critical region. What is your conclusion?

# fistly: does the value 20 belong to the 95% CI of mu from the previous subtask?
# What will be the result of our test based on this observation?

mu0 <- 20

# test statistic (from the lecture slides):
T <- 

# critical region (quantiles of the student t-distribution):
CR_1 <- 
CR_2 <- 

# conclusion: FILL!


# ....................................... C ....................................


# Create a density plot of a theoretical distribution of the test statistic
# (student t-distribution), visualize the critical region and the test statistic.

?dt

xx <- 
density.t <- dt(, n - 1) # (n-1) degrees of freedom we know, fill the x-axis

plot(xx, density.t, type = 'l', main = "Critical region",
     xlab = "x", ylab = "f(x)", sub = paste("mu=", round(mu, 2), "(MLE)"))
rect(-5, 0, CR_1, 1, col = rgb(red = 1, green = 0, blue = 0, alpha = 0.2), lty = 0)
rect(CR_2, 0, 5, 1, col = rgb(red = 1, green = 0, blue = 0, alpha = 0.2), lty = 0)
abline(v = T, col = 'blue', lwd = 2)


# ....................................... D ....................................


# Find the p-value of the previous test and use it for decision about the null
# hypothesis.

p.value <- 
# volume under the black curve in the blue regions in the following plot

# we DO / DO NOT reject H0: mu = 20 against H1: mu != 20


# ....................................... E ....................................


# Create a density plot of a theoretical distribution of the test statistic (student
# t-distribution), visualize the test statistic and p-value.

xx <- seq(-5, 5, 0.01)
density.t <- dt(xx, n - 1)

plot(xx, density.t, type = 'l', main = paste("P-value", round(p.value, 3)),
     xlab = "x", ylab = "f(x)", sub = paste("mu=", round(mu, 2)))
rect(, col = rgb(red = 0, green = 0, blue = 1, alpha = 0.2), lty = 0)
rect(, 1, col = rgb(red = 0, green = 0, blue = 1, alpha = 0.2), lty = 0)
abline(v = T, col = 'blue', lwd = 2)


# ----------------------------------- TASK 2 -----------------------------------


# Use the data from the previous task. Solve the same tasks as before, but now
# for the left-sided confidence interval and the right-sided alternative hypothesis.

alpha <- 0.05
n <- length(data)


# ....................................... A ....................................


# Construct a left-sided conficence interval for parameter mu. Create a
# visualization of all informations you computed (plot the histogram together with
# the density of estimated normal distribution and with CIs).

# maximum likelihood estimation of parameter mu and sigma:
mu <- 
sigma <- 

# compute the quantile we need:
u.q.L <-

# compute the lower and upper limits of the CI for parameter mu
# (use formulas from the lecture):

CI_left <- 

xx <- 
density.n <- dnorm(xx, mu, sigma)

hist(data, freq = F, main = "Left sided CI for mean", col = "antiquewhite")
lines(xx, density.n, lty = 2, lwd = 1.5)

abline(v = , col = 'red', lwd = 2)
abline(v = , col = 'blue', lwd = 2)
rect(CI_left, 0, 28, 0.15, col = rgb(red = 1, green = 0, blue = 0, alpha = 0.2), lty = 0)


# ....................................... B ....................................


# Test the null hypothesis about parameter mu H0: mu = 20, against the right-sided
# alternative H1: mu > 20 at the significance level alpha = 0.05. Compute the value
# of the test statistic and find the critical region. What is your conclusion?

mu0 <- 20
alpha <- 0.05

# test statstic:
T <- 

# critical region:
CR_1 <- 


# ....................................... C ....................................


# Create a density plot of a theoretical distribution of the test statistic
# (student t-distribution), visualize the critical region and the test statistic.

xx <- 
density.t <- 

plot(, , type = 'l', main = "Critical region",
     xlab = "x", ylab = "f(x)", sub = paste("mu=", round(mu, 2)))
rect(, , , , col = rgb(red = 1, green = 0, blue = 0, alpha = 0.2), lty = 0)
abline(v = , col = 'blue', lwd = 2)


# ....................................... D ....................................


# Find the p-value of the previous test and use it for decision about the null
# hypothesis.

p.value <- 
# volume under the black curve in the blue regions

# we DO / DO NOT reject H0: mu = 20 against H1: mu > 20


# ....................................... E ....................................


# Create a density plot of a theoretical distribution of the test statistic (student
# t-distribution), visualize the test statistic and p-value.

xx <- 
density.t <- 

plot(, , type = 'l', main = paste("P-value", round(p.value, 3)),
     xlab = "x", ylab = "f(x)", sub = paste("mu=", round(mu, 2)))
rect(, , , , col = rgb(red = 0, green = 0, blue = 1, alpha = 0.2), lty = 0)
abline(v = , col = 'blue', lwd = 2)


# ----------------------------------- TASK 3 -----------------------------------


# Use the data from the previous task. Solve the same tasks as before, but now
# for the right-sided confidence interval and the left-sided alternative hypothesis.

alpha <- 0.05
n <- length(data)


# ....................................... A ....................................


# Construct a left-sided conficence interval for parameter mu. Create a
# visualization of all informations you computed (plot the histogram together with
# the density of estimated normal distribution and with CIs).

# maximum likelihood estimation of parameter mu and sigma:
mu <- 
sigma <- 

# compute the quantile we need:
u.q.R <- 

# compute the lower and upper limits of the CI for parameter mu
# (use formulas from the lecture):

CI_right <-

xx <- 
density.n <- 

hist(, freq = F, main = "Right sided CI for mean", col = "antiquewhite")
lines(, , lty = 2, lwd = 1.5)
abline(v = , col = 'red', lwd = 2)
abline(v = , col = 'blue', lwd = 2)
rect(, , , , col = rgb(red = 1, green = 0, blue = 0, alpha = 0.2), lty = 0)


# ....................................... B ....................................


# Test the null hypothesis about parameter mu H0: mu = 20, against the left-sided
# alternative H1: mu < 20 at the significance level alpha = 0.05. Compute the value
# of the test statistic and find the critical region. What is your conclusion?

mu0 <- 20
alpha <- 0.05

# test statstic:
T <- 

# critical region:
CR_2 <- 


# ....................................... C ....................................


# Create a density plot of a theoretical distribution of the test statistic
# (student t-distribution), visualize the critical region and the test statistic.

xx <- 
density.t <- 

plot(, , type = 'l', main = "Critical region",
     xlab = "x", ylab = "f(x)", sub = paste("mu=", round(mu, 2)))
rect(, , , , col = rgb(red = 1, green = 0, blue = 0, alpha = 0.2), lty = 0)
abline(v = , col = 'blue', lwd = 2)


# ....................................... D ....................................


# Find the p-value of the previous test and use it for decision about the null
# hypothesis.

p.value <-
# volume under the black curve in the blue regions

# we DO / DO NOT reject H0: mu = 20 against H1: mu < 20


# ....................................... E ....................................


# Create a density plot of a theoretical distribution of the test statistic (student
# t-distribution), visualize the test statistic and p-value.

xx <- 
density.t <- 

plot(, , type = 'l', main = paste("P-value", round(p.value, 3)),
     xlab = "x", ylab = "f(x)", sub = paste("mu=", round(mu, 2)))
rect(, , , , col = rgb(red = 0, green = 0, blue = 1, alpha = 0.2), lty = 0)
abline(v = , col = 'blue', lwd = 2)
