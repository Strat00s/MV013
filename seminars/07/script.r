setwd("seminars/07")

# ==============================================================================
# --------------------------------- SEMINAR 7 ----------------------------------
# ==============================================================================


# ----------------------------------- TASK 1 -----------------------------------


data <- c(19.92, 21.17, 24.87, 22.90, 20.88, 19.43, 19.39, 23.14, 16.99, 24.85,
          20.80, 19.93, 20.28, 27.61, 27.50, 19.63, 20.35, 22.96, 17.57, 22.91,
          21.84, 25.34, 20.13, 18.30, 15.40, 22.88, 21.90, 25.30, 21.86, 22.47)

n <- length(data)


# ....................................... A ....................................


# Firstly estimate parameter mu and sigma of the normal distribution N(mu, sigma)
# using the maximum likelihood method. Then plot the histogram of the data together
# with the density of the normal distribution with the estimated parameters.

# theoretical MLE from the lecture:

mu <- mean(data)
sigma <- sqrt(sum((data - mu)^2) / (n - 1))

# plot the histogram together with the density of estimated N(mu, sigma):

xx <- seq(n)
density.n <- dnorm(xx, mu, sigma)

hist(data, freq=F)
lines(density.n)


# ..................................... B ......................................


# Create the Q-Q plot of the theoretical and empirical quantiles against each other.

?qqnorm
?qqline

# Q-Q plot:
qqnorm(data, pch = 19) 
qqline(data, col = "blue")

# EXTRA CODE for volunteers (Q-Q plot step by step):

sample <- sort(data)

i <- 1:n                                 # order indices, P(X = i) = 1/n
beta <- 0.5                              # adjustment
alpha <- (i - beta) / (n + 1 - 2 * beta) # probability of X <= i: P(X <= i)

theoretical.quantiles <- qnorm(alpha, mu, sigma)

plot(theoretical.quantiles, sample, main = "Q-Q plot", pch = 19)
abline(a = 0, b = 1, col = "blue")


# ..................................... C ......................................


# Plot the empirical cumulative distribution function of the data together with
# the theoretical distribution function of the normal distribution with the
# estimated parameters.

# HINT: apply function ecdf() to the data:

?ecdf

#xx <- seq(n)
xx <- seq(14, 28, by=0.1)

dist.em <- ecdf(data) # empirical distribution
dist.n <- pnorm(xx, mu, sigma) # theoretical distribution

plot(dist.em, main = "Cumulative distribution function")
lines(xx, dist.n, col = 'red', lwd = 2)

# EXTRA TASK for volunteers (do the same manually):

# firstly compute the table of the relative cumulative frequencies of the data:

(tab <- table(data))
(cum_tab <- as.table(cumsum(tab)))
(rel_cum_tab <- cum_tab / n)

# now plot it together with the theoretical distribution function dist.n:

xx <- seq(14, 28, by = 0.1)
dist.n <- pnorm(xx, mu, sigma)

plot(rel_cum_tab, type = 's', main = "Cumulative distribution function",
     ylab = "Empirical F(x)")
lines(xx, dist.n, col = 'red', lwd = 2)


# ..................................... D ......................................


# Create the P-P plot of the theoretical and empirical probability distribution
# functions against each other. Compute the difference of the theoretical
# and empirical distributions and plot it.

# HINT: apply ecdf() function to the data, the output ecdf.fun can be then used
# for calculating the value of the emp. dist. function in arbitrary data point:

ecdf.fun <- ecdf(data)
str(ecdf.fun) # it is a function

dist.em <- ecdf.fun(data)
dist.em.adj <- dist.em * n / (n - 1) # adjusted (just technical correction)

dist.n <- pnorm(data, mu, sigma)

# plot the results against each other:

plot(dist.em.adj, dist.n, main = "P-P plot", pch = 19,
     xlab = "empirical distribution", ylab = "theoretical distribution")
abline(a = 0, b = 1, col = "blue")

# compute the difference of the empirical and theoretical function for xx vector:

xx <- seq(14, 28, by = 0.1)

# HINT: apply ecdf.fun you have just created to the xx values

dist.em <- ecdf.fun(xx)
dist.n <- pnorm(xx, mu, sigma)
diff.dist <-

plot(xx, dist.em - dist.n, type = 'l',
     main = "Cumulative - Estimated distribution", ylab = "Difference")
abline(h = 0, lty = 2, col = "blue")


# ....................................... E ....................................


# Use some of the normality tests (from the lecture) to decide about the
# normality of the data at the significance level alpha = 0.05.

?ks.test

ks.test(data, "pnorm", mu, sigma)

# more tests:
install.packages("fBasics")
library(fBasics)

dagoTest(data) # at least 20 observations, based on skewness and kurtosis
jarqueberaTest(data)
shapiroTest(data)
pchiTest(data)


# ----------------------------------- TASK 2 -----------------------------------


load("toss_a_coin.RData")

n <- length(data)


# ....................................... A ....................................


# Use the test without the assumption of normality (see the lecture 6 slides).

mu0 <- 50
alpha <- 0.05

# MLE estimations:

mu <- mean(data)
sigma <- sqrt(sum((data - mu)^2) / (n - 1))


# 1) using test statistic (use the formulas from the lecture 6 slides):

T <- sqrt(n) * (sd(data) - mu) / sigma

# be careful: in the formula there is not used the MLE sigma, but the consistent
# estimation (with the multiplicative constant 1/(n - 1), not 1/n), use sd(data)

# critical region (appropriate quantiles of the standardised normal distribution):

CR_1 <- qt(alpha/2, n - 1)
CR_2 <- abs(qt(alpha/2, n - 1))

# conclusion: FILL!


# 2) using p-value of the previous test:

p.value <- 

p.value < alpha


# 3) using the confidence interval:

# compute the 1-alpha quantile of the normal distribution N(0, 1):

q.norm <- 

# use the formulas from the lecture 6 slides:

CI_lower <- 
CI_upper <- 


# ..................................... B ......................................


# Can we consider our data as normally distributed (based on the central limit
# theorem)? Check it (choose any appropriate graphical or numerical method).




# ....................................... C ....................................


# Use the test with the right-sided alternative under the assumption
# of normality (1-sample t-test).

mu0 <- 50
alpha <- 0.05

# 1) using test statistic:

T <- 

# critical region (quantile of the student t-distribution):
  
CR_1 <- 

c(CR_1, Inf)

# conclusion: FILL!

# 2) using p-value of the previous test

p.value <- 

p.value < alpha

# 3) using the confidence interval:

q.t <- 

CI_lower <- 

c(CI_lower, Inf)