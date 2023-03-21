
# ==============================================================================
# --------------------------------- SEMINAR 4 ----------------------------------
# ==============================================================================


# ----------------------------------- TASK 1 -----------------------------------


# ...................................... A .....................................


# Create density plot, distribution function plot, and quantile function plot
# of random variable X with binomial distribution Bi(n = 100, p = 0.5).

# HINT: apply these functions on the values from the x-axis:
?pbinom
?dbinom
?qbinom

# Distribution function:
dist.b <- pbinom(0:100, size = 100, prob = 0.5)
# computes the values of distribution function for each value of vector 1:100
# of all possible outcomes (number of successes) of binomial distribution

plot(0:100, dist.b, type = 'l', col = "red",
     main = "Distribution", xlab = "", ylab = "")

# Density function:
density.b <- dbinom(0:100, size = 100, prob = 0.5)
plot(0:100, density.b, type = 'l', col = "red",
     main = "Probability mass function", xlab = "", ylab = "")
# computes the values of density function for each value of vector 1:100
# of all possible outcomes (number of successes) of binomial distribution

# Quantile function:
quantile.b <- qbinom(seq(0, 1, by = 0.01), size = 100, prob = 0.5)
plot(seq(0, 1, by = 0.01), quantile.b, type = 'l', col = "red",
     main = "Quantile function", xlab = "", ylab = "")
# computes the values of quantile function for each value of given vector of values
# between 0 and 1 of different probabilities


# ...................................... B .....................................


# Generate random vector of length 1000 with independent ele-
# ments, where each element X is following a binomial distribution
# Bi(100; 0.5). Produce a histogram and compare it to the
# desinty plot of the same distribution.

?rbinom

n <- rbinom(1000, size = 100, prob = 0.5) # vector of 1000 random numbers
d.b <- dbinom(0:100, size = 100, prob = 0.5) # density of binomial distribution

hist(n, xlim = c(0, 100), breaks = seq(0, 1000, by = 3), freq = F, col = "red")
points(0:100, d.b, lwd = 2)


# ----------------------------------- TASK 2 -----------------------------------


# ...................................... A .....................................


# Generate and plot a random trajectory of the Poisson process with
# lambda = 3 on the interval [0; 100]. Specifically, generate the event times
# in [0; 100] and plot a step function of time t from [0; 100] that gives the
# cumulative number of events that has occurred by time t.

# for generating the random trajectory you can use this function, explore it:

poisson_process <- function(lambda, t_end){
  all_t <- c()
  t <- 0
  while (t < t_end) {
    new_event <- rexp(1, rate = lambda) # time between current and new event
    t <- t + new_event                  # time of new event
    all_t <- c(all_t, t)                # vector of times of all events
  }
  return(all_t)
}

t_end <- 10 # final time
lambda <- 3

all_t <- poisson_process(lambda, t_end) # times of all events from 0 to t_end

plot(all_t, 1:length(all_t), typ = 's', xlab = "time",
     ylab = "number of events", main = "Poisson process")


# ................................... B, C .....................................


count_all <- c()

for (i in 1:500){
  all_t <- poisson_process(lambda, t_end) # vector of times of all events in [0; t_end]
  new_count <- length(all_t) # count of all events from all_t
  count_all <- c(count_all, new_count) # append new count
}

density.p <- dpois(200:400, t_end * lambda) # density function of POISSON distribution

hist(count_all, breaks = seq(200, 400, by = 5), freq = F, col = 'red')
points(200:400, density.p, lwd = 2, pch = 16, cex = 0.5)


# ...................................... D .....................................


# number of events in [100; 120] has Po(20 * lambda) distribution with expected value:
(expected.value <- lambda * 20)

# we want the number of events x for which holds: P(X > x) = 0.1 or P(X <= x) = 0.9,
# which is a quantile function of Po(20 * lambda):
(upper.bound <- qpois(0.9, lambda * 20))


# ----------------------------------- TASK 3 -----------------------------------


# ...................................... A .....................................


n <- 50000
p <- 0.5

X <- rbinom(1000, n, p) # 1000 random numbers from the binomial distribution

# standardization of the previous vector (from the lecture slaid):
U <- (X - n * p) / sqrt(n * p * (1 - p))

xx <- seq(-3, 3, by = 0.01)

density.n <- dnorm(xx, 0, 1) # density of normal distribution

hist(U, freq = F, col = "red")
lines(xx, density.n, lwd = 2)

# histogram of the transformed data from the binomial distribution looks quite
# similar to the density function of the normal distribution (thanks to the
# central limit theorem)


# ...................................... B .....................................


n <- 500
p <- 0.5

x <- dbinom(0:n, n, p)
# probability function of binom. distr. for all possible number of successes 0:n

# use formulas from the lecture for computing
# standart deviation and expactation value of X:
exp.b <- n * p
sd.b <- sqrt(n * p * (1 - p))

my_seq <- seq(0, n, by = 0.01)

plot(0:n, x, xlab = "number of successes", col = "red", pch = 16,
     ylab = "value in Pascal triangle", main = paste("n=", n))
lines(my_seq, dnorm(my_seq, mean = exp.b, sd = sd.b))

# the probability function of the binomial distribution for large n looks
# similar to the denstiy function of normal distribution, which is the consequence
# of the central limit theorem


# ...................................... C .....................................


# EXTRA task: compute the fifth row of the Pascal triangle using dbinom() function

n <- 5
dbinom(0:n, n, 0.5) * (2 ^ n)

# we used p = 0.5 because then the probability function of the binom. dist. looks like:
# p(x) = (n above x) * 0.5^x * (1 - 0.5)^(n - x) = (n above x) * 0.5^n,
# then we will divide this by (2 ^ n) for each x from 0:n and we get:
# (n above x) for each x from 0:n, which is the n-th row of the Pascal triangle
