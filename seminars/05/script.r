getwd()
setwd("seminars/05")

# ==============================================================================
# --------------------------------- SEMINAR 5 ----------------------------------
# ==============================================================================


# ----------------------------------- TASK 1 -----------------------------------


load("toss_a_coin.RData")
n <- 200


# ...................................... A .....................................


# Do You think the data sample can be from the binomial distribution?
# Explain Your answer (for example using the histogram of the data).

hist(data, freq = F, col = "red")
library()


# ...................................... B .....................................


# Use the maximum likelihood method to estimate parameter p of the binomial
# distribution Bi(n = 100, p) for this data.

# firstly, create a function for computing the negative log-likelihood of the
# binomial distribution:

negloglik.binom <- function(par, x, n) {
  # HINT: use the logarithm of the dbinom() function for each value of the input
  # vector x with parameters: par, n and then sum it:
  return(- )
}

# now find the minimum of this function (which is the maximum of the original
# log-likelihood function and our estimation of parameter p):

?optimize

# univariate optimalization ("Brent" method):
op <- optimize( , interval = , ...)
# fill the function to minimalize, range of possible values of your parameter
# ("interval") and the rest of inputs of your function ("...")

# what is in the op variable?

str(op)
op$par   # estimation of parameter p (the point of minimum)
op$value # value of the negative log-likelihood

# now compute the negative log-likelihood for each value of the vector "parameter":

parameter <- seq(0.01, 0.99, len = 200)  # x-axis of parameter values
neg_log_like <- rep(0, times = 200)      # prepare vector of the length 200

# fill the vector neg_log_like with the values of the negative log-likelihood function
# for each possible value of vector parameter:
for (i in 1:200) {
  neg_log_like[i] <- 
}

# plot the negative log likelihood function together with the numerical estimation:

plot( , , type = "l", main = "Negative log likelihood") # negative log-likelihood
points(, , col = "red", pch = 16) # point [estimated p, its neg. log. likelihood]
abline(v = op$par, col = 'red') # "v" as "vertical" line


# ...................................... C .....................................


# Plot the histogram of Your data together with the density of the theoretical
# binomial distribution with estimated parameter p.

xx <- 0:n
density.b <- 

hist(, xlim = c(0, n), freq = F, col = "red")
points(, , type = "o", pch = 19)


# ...................................... D .....................................


# Because n = 100 in the binomial distribution is large enough, we can assume
# our data distribution is normal (from central limit theorem). Use maximum
# likelihood method to estimate parameters mu and sigma of the normal
# distribution N(mu,sigma). Use both the numerical and the theoretical approach.

# create a function for computing the negative log likelihood for
# the normal distribution for both parameters:

negloglik.norm <- function(par, x) {
  mu <- par[1]
  sd <- par[2]
  return()
}

# multivariate optimalization using different numerical methods:

?optim
# 1st input = vector of initial guesses of parameters: we can use the E(X) and
# VAR(X) for the random variable X with the binomial distribution Bi(100, 0.5):
# E(X) = n * p, VAR(X) = sqrt(n * p * (1 - p)),
# because p = 0.5 is the "average" value of p (in the middle of [0; 1])

# 1) unconstrainde qusi-Newton method:
optim(, negloglik.norm, x = data, method = "BFGS")

# 2) box-constrained quasi-Newton (making sure that sd >= 0):
optim(, negloglik.norm, x = data, method = "L-BFGS-B",
      lower = c(-Inf, 0), upper = c(Inf, Inf))

# 3) Nelder-Mead method (default):
op <- optim(, negloglik.norm, x = data)

op$par

# 4) theoretical estimation from the lecture:

(mu <- mean(data))
(sigma <- sqrt(sum((data - mu)^2) / 200))

# EXTRA TASK: what does this code compute? How?
(sigma <- sqrt(var(data) * (200 - 1) / 200))


# ...................................... E .....................................


# Plot the histogram of Your data together with the density of the theoretical
# normal distribution with estimated parameters.

xx <- 0:100
density.n <-  # numerical estimation
density.n.t <-  # theoretical estimation

hist(, xlim = c(0, 100), freq = F, col = 'red')
lines(, , lwd = 3)
lines(, , lwd = 3, col = "blue", lty = 3)

# numerical and theoretical estimation is nearly the same


# ----------------------------------- TASK 2 -----------------------------------


data <- read.csv("Computers.csv")


# ...................................... A .....................................


# Assume the price of 2 MB RAM computers is following a normal distribution. Use maximum
# likelihood method to estimate parameters mu and sigma of the normal distribution
# N(mu,sigma). Plot histogram of your data together with a theoretical normal distribution.

# create a vector of price values for computers with RAM size 2:
price2 <- 
n <- length(price2)

# create a function for computing the negative log likelihood for
# the normal distribution for both parameters:

negloglik.norm <- function(par, x) {
  mu <- par[1]
  sd <- par[2]
  return()
}

# multivariate optimazition using different numerical methods:

# 1) Nelder-Mead method (default):
op <- optim(c(50, 5), negloglik.norm, x = data)

# 2) estimate the parameters mu and sigma using theoretical maximum likelihood estimation:
(mu <- mean(price2))
(sigma <- sqrt(sum((price2 - mu)^2) / n))
(sigma <- sqrt(var(price2) * (n - 1) / n))

# plot the histogram of price2 together with the density of the normal
# distribution with estimated parameters mu and sigma:
xx <- 
density.n <- 

hist(, xlim = c(700, 2500), freq = F, ylim = c(0, 0.0015), col = "green")
lines(, , lwd = 3)


# ...................................... B .....................................


# Assume the price of 4 MB RAM computers is following a normal distribution. Use
# maximum likelihood method to estimate parameters mu and sigma of the normal
# distribution N(mu, sigma). Plot histogram of your data together with a theoretical
# normal distribution. Based on the histogram, do you think the assumption was correct?

# create a vector of price values for computers with RAM size 4:
price4 <- 
n <- 

# estimate the parameters mu and sigma using theoretical maximum likelihood estimation:
mu <- 
sigma <- 

# plot the histogram of price4 together with the density of normal distribution
# with estimated parameters mu and sigma:
xx <- 
density.n <- 

hist(, xlim = c(700, 2500), freq = F, ylim = c(0, 0.0015), col = "green")
lines(, , lwd = 3)

# EXTRA CODE (for volunteers):

# QQ-plot shows that the distribution differs from normal
# especially, high (upper tail) quantiles are much larger in the sample
# than they should be if a normal distribution was adequate
# theory about QQ-plot will be part of one of the lectures ahead

qqnorm(price4, pch = 19)
qqline(price4, col = "blue")


# ...................................... C .....................................


# Assume the logarithm of the price of 4 MB RAM computers instead of the values
# themselfs and do the same as in subtask (B).

logprice4 <- 
n <- 

# estimate the parameters mu and sigma using theoretical maximum likelihood estimation

# FILL YOURSELF!
  

# plot the histogram of price4 together with the density of normal distribution
# with estimated parameters mu and sigma:

# FILL YOURSELF!


# EXTRA CODE (for volunteers):

# fits better in the right tail
qqnorm(logprice4, pch = 16)
qqline(logprice4, col = "blue")
