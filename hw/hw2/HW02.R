# --------------- TASK 1 ---------------
uco <- 527330
set.seed(uco)
data1 <- round(rgamma(100, 1, 1/5), 2)
data1 <- data1[data1 != 0]


n <- length(data1)

# Getting a rough outlook on the data
summary(data1)
hist(data1)

# a) Code used from excercises
negloglik.exp <- function(x, lambda) {
  return(-sum(log(dexp(x, rate=lambda))))
}

op <- optimize(negloglik.exp, x = data1, interval = c(0, 1))

lambda = op$minimum

# b)
negloglik.lognorm <- function(x, meanlog, sdlog) {
  return(-sum(log(dlnorm(x, meanlog, sdlog))))
}
# Searching for meanlog
op <- optimize(negloglik.lognorm, x = data1, interval = c(0, 2), 1)
meanlog <- op$minimum

# Searching for sdlog
op <- optimize(negloglik.lognorm, x = data1, meanlog=meanlog, interval = c(1, 2))
sdlog <- op$minimum

# c) - Double check
xx = seq(0.2, 22, by=0.2)

density.exp <- dexp(xx, rate=lambda)
density.lnorm <- dlnorm(xx, meanlog=meanlog, sdlog=sdlog)

# d) Visual check
# ylim to accomodate for values of lnorm
hist(data1, ylim=c(0, 0.2),freq = F)
lines(xx, density.exp, lwd=2, col="blue")
lines(xx, density.lnorm, lwd=2, col="red")

# Numerical check
library(fitdistrplus)
result <- fitdist(data1, method = "mle", distr = "exp")
summary(result)
result <- fitdist(data1, method = "mle", distr = "lnorm")
summary(result)
# e)
(1 - pexp(5, rate=lambda))
(1 - plnorm(5, meanlog = meanlog, sdlog = sdlog))

# --------------- TASK 2 ---------------
# Students per one hour
(new_lambda <- lambda*60)
# Confidence interval, thanks to CLT
new_marg <- qt(0.975, df = n - 1) * sd(data1)/sqrt(n)
c(new_lambda - new_marg, new_lambda + new_marg)
# --------------- TASK 3 ---------------
uco <- 527330
set.seed(uco)
data2 <- sample(data1, 80)
(summary(data2))

# Visualisation
xx <- seq(0, 22, by=0.2)
density.norm <- dnorm(xx, mean(data2), sd(data2))
hist(main = "Histogram of data2 with red line showing normal DF", data2, freq=F)
lines(xx, density.norm, col="red")

# Statistical test
library(fBasics)
ksnormTest(data2)

# --------------- TASK 4 ---------------
load("customer_behaviour2.RData")
# The task doesn't say that we need to explicitly work with the 
# big variable, so I will keep them separate
big_spender <- data$age[data$money_spent > 5000]
small_spender <- data$age[data$money_spent <= 5000]
# Check normalities of both spender groups
par(mfrow = c(1, 2))
qqnorm(big_spender, pch = 19) 
qqline(big_spender, col = "blue") # Big spenders have heavy tails, will try non-norm test

qqnorm(small_spender, pch = 19) 
qqline(small_spender, col = "blue") 

# The test itself also giving the confidence interval
wilcox.test(big_spender, small_spender, alternative = "greater", conf.int = T)
# Additional check to test if it is the other way around
wilcox.test(big_spender, small_spender, alternative = "less", conf.int = T)
