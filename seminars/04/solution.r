
# ==============================================================================
# --------------------------------- SEMINAR 4 ----------------------------------
# ==============================================================================


# ----------------------------------- TASK 1 -----------------------------------


# ...................................... A .....................................


# Create density plot, distribution function plot, and quantile function plot
# of random variable X with binomial distribution Bi(n = 100, p = 0.5).

# HINT: apply these functions on the values from the x-axis. What is the domain?
?pbinom
?dbinom
?qbinom

# Distribution function:
dist.b <- 
plot(0:100, dist.b, type = 'l', col = "red",
     main = "Distribution", xlab = "", ylab = "")

# Density function:
density.b <- 
plot(0:100, density.b, type = 'l', col = "red",
     main = "Probability mass function", xlab = "", ylab = "")

# Quantile function:
quantile.b <- 
plot(seq(0, 1, by = 0.001), quantile.b, type = 'l', col = "red",
     main = "Quantile function", xlab = "", ylab = "")


# ...................................... B .....................................


# Generate random vector of length 1000 with independent ele-
# ments, where each element X is following a binomial distribution
# Bi(100; 0.5). Produce a histogram and compare it to the
# desinty plot of the same distribution.

# HINT: use functions rbinom to generate random numbers and dbinom to compute
# the density function of the binomial distribution:
?rbinom

rand.b <- 
density.b <- 

hist(rand.b, xlim = c(0, 100), breaks = seq(0, 1000, by = 3), freq = F, col = "red")
lines(0:100, density.b, lwd = 2)


# ----------------------------------- TASK 2 -----------------------------------


# ...................................... A .....................................


# Generate and plot a random trajectory of the Poisson process with
# lambda = 3 on the interval [0; 100]. Specifically, generate the event times
# in [0; 100] and plot a step function of time t from [0; 100] that gives the
# cumulative number of events that has occurred by time t.

# for generating the random trajectory you can use this function, explore it:

poisson_process <- function(lambda, t_final) {
  all_t <- c()
  t <- 0
  while (t < t_final) {
    new_event <- rexp(1, rate = lambda) # time between current and new event
    t <- t + new_event                  # time of new event
    all_t <- c(all_t, t)                # vector of times of all events
  }
  return(all_t[1:(length(all_t) - 1)])
}

t_final <- 100 # final time
lambda <- 3

all_t <- poisson_process(lambda, t_final)
count <- length(all_t)

plot(all_t, 1:count, typ = 's', xlab = "time",
     ylab = "number of events", main = "Poisson process")


# ................................... B, C .....................................


# Simulate the trajectory from (a) 500 times and record the cumulative number
# of events at t = 100 for each trajectory. Plot the histogram of these values.
# Add the Poisson density function into the figure for Po(lambda * t).

# HINT: for each run of the simulation compute the vector of times of all
# events all_t and add the number of all events into the vector count_all:

count_all <- c()

for (i in 1:500){
  all_t <- 
  count_all <- 
}

# compute the density function of the poisson distribution with parameter
# t_final * lambda lets say on the interval 200:400:
density.b <- 

# plot the histogram with the denstity (probability) function:
hist(, breaks = seq(200, 400, by = 5), freq = F, col = 'red')
points(200:400, , lwd = 2, pch = 16, cex = 0.5)


# ...................................... D .....................................


# How many events do you expect to observe during the next 20 units of time?
# Could you provide an upper bound for this number that will be exceeded
# only with probability 10% ?

# number of events in [100; 120] follows Po(lambda * ?)
# expected value of poisson distribution is the same as the parameter:
(expected.value <- )

# we want the number x of possible outcome for which holds: P(X <= x) = 90%
(upper.bound <- qpois(, ))


# ----------------------------------- TASK 3 -----------------------------------


# ...................................... A .....................................


# Generate vector of 1000 random values following the binomial distribution
# Bi (500; 0:5). Normalize the random values and plot the histogram.
# Compare it with a standardized normal distribution.

n <- 500
p <- 0.5

# generate 1000 random binomial values here:
X <- 
U <- (X - n * p) / sqrt(n * p * (1 - p))

# compute the density function for N(0; 1) for the xx values:
?dnorm

xx <- seq(-3, 3, by = 0.01)
density.norm <- 

# plot the histogram together with the density function:
hist(, freq = F, col = "red")
lines(xx, , lwd = 2)


# ...................................... B .....................................


# Plot the density (probability) function of the random variable X following the
# binomial distribution Bi(n = 20; p = 0.5) together with the density of the normal
# distribution with the same expectation value and variance like the random
# variable X. Try different values of n.


n <- 20
p <- 0.5

# compute the density (probability) function of binomial
# distribution for all possible values:
x <- 

# use formulas from the lecture for computing
# standart deviation and expactation value of X:
exp.b <- n * p
sd.b <- sqrt(n * p * (1 - p))

# compute the density function of N(exp.b, sd.b) for my_seq values:
my_seq <- seq(0, n, by = 0.01)
density.norm <- 

# plot the probability function together with the density:
plot(0:n, , xlab = "number of successes", col = "red", pch = 16,
     ylab = "value in Pascal triangle", main = paste("n=", n))
lines(my_seq, )


# ...................................... C .....................................


# EXTRA task: compute the fifth row of the Pascal triangle using dbinom() function

n <- 5
dbinom(0:n, n, 0.5) * (2 ^ n)
