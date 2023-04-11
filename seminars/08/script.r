setwd("seminars/08")
# ==============================================================================
# --------------------------------- SEMINAR 8 ----------------------------------
# ==============================================================================


# ----------------------------------- TASK 1 -----------------------------------


T2019 <- c(-1.7, 1.7, 5.6, 9.4, 10.7, 20.7, 18.8, 18.9, 13.3, 9.5, 5.6, 1.9)
Taverage <- c(-2.8, -1.1, 2.5, 7.3, 12.3, 15.5, 16.9, 16.4, 12.8, 8.0, 2.7, -1.0)

# test the following hypothesis about the temperatures (using the paired t-test)
# H0: T2019 = Taverage, H1: T2019 > Taverage (right-sided alternative)

# Under the normality assumption of T2019, Taverage (check yourself!) we can test:
# H0: T2019 - Taverage = 0, H1: T2019 - Taverage > 0

Temp <- T2019 - Taverage

mu0 <- 0
alpha <- 0.05
n <- length(Temp)

mu <- mean(Temp)
sigma <- sd(Temp)

# ....................................... A ....................................


# realization of the test statistic:

T <- sqrt(n) * (mu - mu0) /sigma

# critical region:
CR_1 <- qt(1 - alpha, n - 1)

# visualization:

xx <- seq(-5, 5, 0.1)
density.t <- dt(xx, n - 1)

c_interval = mu - CR_1 * sigma/sqrt(n)

plot(xx, density.t, type = 'l', main = "Critical region",
     xlab = "x", ylab = "f(x)", sub = paste("mu=", mu))
rect(c_interval, 0, 20, 1, col = rgb(red = 1, green = 0, blue = 0, alpha = 0.2), lty = 0)
abline(v = T, col = 'blue', lwd = 2)


# ....................................... B ....................................


# using p-value:

p.value <- 1 -pt(T, n - 1)

xx <- seq(-5, 5, 0.1)
density.t <- dt(xx, n - 1)

plot(xx, density.t, type = 'l', main = paste("P-value", round(p.value, 3)),
     xlab = "x", ylab = "f(x)", sub = paste("mu=", mu))
rect(T, 0, 5, 1, col = rgb(red = 0, green = 0, blue = 1, alpha = 0.2), lty = 0)
abline(v = T, col = 'blue', lwd = 2)

#-> reject null

# ....................................... C ....................................


# using confidence interval:

q.t <- qt(1 - alpha, n - 1)

c(mu - q.t * (sigma /sqrt(n)), Inf)


# ....................................... D ....................................


# using built-in function:

(t <- t.test(T2019, Taverage, paired = T, alternative = "greater"))

# conclusion: we REJECT H0 at the significance level 0.05 in behalf of H1


# ----------------------------------- TASK 2 -----------------------------------


mach.1 <- c(29, 27, 29, 35, 29, 32, 28, 34, 32, 33)
mach.2 <- c(31, 28, 30, 28, 37, 29, 27, 27, 39, 33,
            31, 32, 31, 29, 32, 28, 27, 28, 24, 34)

alpha <- 0.05

n1 <- length(mach.1)
mu1 <- mean(mach.1)
sigma1 <- sd(mach.1)

n2 <- length(mach.2)
mu2 <- mean(mach.2)
sigma2 <- sd(mach.2)


# ....................................... A ....................................


# Test if the variances of both measurements are the same against 2-side alternative.

# use F-test for equal variances: H0: sigma1 = sigma2, H1: sigma1 != sigma2

# 1) using test statistic (from the lecture slides):

F <- sigma1^2 / sigma2^2

# critical region

CR_1 <- qf(alpha / 2, n - 1, n - 2)
CR_2 <- qf(1 - alpha / 2, n - 1, n - 2)

xx <- seq(0, 5, 0.01)
density.f <- df(xx, n1 - 1, n2 - 1)


plot(xx, density.f, type = 'l', main = "Critical region", xlab = "x", ylab = "f(x)")
rect(0, 0, CR_1, 1, col = rgb(red = 1, green = 0, blue = 0, alpha = 0.2), lty = 0)
rect(CR_2, 0, 5, 1, col = rgb(red = 1, green = 0, blue = 0, alpha = 0.2), lty = 0)
abline(v = F, col = 'blue', lwd = 2)

# 2) using p-value:

dist.f <- pf(F, n1 - 1, n2 - 2)

p.value <- 2 * min(dist.f, 1 - dist.f)

# visualization:

xx <- seq(0, 5, 0.01)
density.f <- df(xx, n - 1, n - 2)

# the point q2 on the x-axis for which holds: F(q2) = P(X <= q2) = 1 - dist.f
# HINT: you know the value of dist. function in the unknown point q2, so use the
# quantile function (inverse map to the distribution one)
q2 <- qf(1 - dist.f, n1 - 1, n2 - 1)

plot(xx, density.f, type = 'l', main = paste("P-value", round(p.value, 3)),
     xlab = "x", ylab = "f(x)")
rect(0, 0, F, 1, col = rgb(red = 0, green = 0, blue = 1, alpha = 0.2), lty = 0)
rect(q2, 0, 5, 1, col = rgb(red = 0, green = 0, blue = 1, alpha = 0.2), lty = 0)
abline(v = F, col = 'blue', lwd = 2)

# 3) using confidence interval:

q.f.1 <- qf(1 - alpha / 2, n-1, n-2)
q.f.2 <- qf(alpha / 2, n-1, n-2)

c(sigma1^2 / (sigma2^2 * q.f.1), sigma1^2 / (sigma2^2 * q.f.2))

# 4) using built-in function:

(f <- var.test(mach.1, mach.2))

# conclusion: we do NOT REJECT H0 => we can use 2-sample T-test in (B)


# ....................................... B ....................................


# Test the hypothesis that the usual time is the same for both machines against
# 2-side alternative, assuming equal variance of the random sample distribution.

# HINT: use 2-sample T-test

# 1) using test statistic:

sigma <- sqrt(((n1 - 1) * sigma1^2 + (n2 - 1) * sigma2^2) / (n1 + n2 - 2))

T <- 

# critical region

CR_1 <- qt(alpha /2)
CR_2 <- 

# visualization:

xx <- seq(-5, 5, 0.01)
density.t <- 

plot(xx, density.t, type = 'l', main = "Critical region", xlab = "x", ylab = "f(x)")
rect(, , , , col = rgb(red = 1, green = 0, blue = 0, alpha = 0.2), lty = 0)
rect(, , , , col = rgb(red = 1, green = 0, blue = 0, alpha = 0.2), lty = 0)
abline(v = , col = 'blue', lwd = 2)

# 2) using p-value:

p.value <- 2 * min(, )

# visualization:

xx <- seq(-5, 5, 0.01)
density.t <- 

plot(xx, density.t, type = 'l', main = paste("P-value", round(p.value, 3)),
     xlab = "x", ylab = "f(x)")
rect(, , , , col = rgb(red = 0, green = 0, blue = 1, alpha = 0.2), lty = 0)
rect(, , , , col = rgb(red = 0, green = 0, blue = 1, alpha = 0.2), lty = 0)
abline(v = , col = 'blue', lwd = 2)

# 3) using confidence interval:

q.t <- 

c((mu1 - mu2)  - q.t * sigma / sqrt((n1 * n2) / (n1 + n2)),
  (mu1 - mu2)  + q.t * sigma / sqrt((n1 * n2) / (n1 + n2)))

# 4) using built-in function:

(t <- t.test(mach.1, mach.2, var.equal = T))

# conslusion: we do NOT REJECT H0


# ----------------------------------- TASK 3 -----------------------------------


# Test a hypothesis that the expected number of bombs per sector is equal to 1
# against two side alternative.

n_bombs <- c(0, 1, 2, 3, 4, 7)
n_sectors <- c(229, 211, 93, 35, 7, 1)

n <- sum(n_sectors)
lambda0 <- 1
alpha <- 0.05

# test statistic:

# compute the overall amount of all hitting bombs =
# = sum of all bombs in each segment =
# = sum of poisson distributions, which has a poisson distribution Po(lambda0 * n))

S <- 

# 1) using p-value:

(p.value <- 2 * min(, ))

# visualization:

xx <- 450:700
density.p <- 

# q2 is a point on the x-axis with the value of the dist. function: 1 - F(S)
q2 <- qpois(, lambda0 * n)

plot(xx, density.p, type = 'h', main = paste("P-value", round(p.value, 3)),
     xlab = "x", ylab = "f(x)")
rect(0, 0, S, 1, col = rgb(red = 0, green = 0, blue = 1, alpha = 0.2), lty = 0)
rect(q2, 0, 900, 1, col = rgb(red = 0, green = 0, blue = 1, alpha = 0.2), lty = 0)
abline(v = S, col = 'blue', lwd = 2)

# using the exact test:

# install.packages("exactci")
library(exactci)
poisson.exact(S, n, alternative = "two.sided")

# 2) using asymptotical confidence interval:

q.n <- 
mu <- S / n

c(mu - q.n * sqrt(mu / n), mu + q.n * sqrt(mu / n))

# conclusion: we do NOT REJECT H0
