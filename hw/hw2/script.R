#generate data
uco <- 492875  # insert your UCO
set.seed(uco)
data1 <- round(rgamma(100, 1, 1/5), 2)
data1 <- data1[data1 != 0]
n = length(data1)


########## 1a ##########
negloglik_exp = function(lambda, data) {
    return(-sum(dexp(data, lambda, log = TRUE)))
}

result = optimize(negloglik_exp, interval = c(0, 100), data = data1)
str(result)
lambda = result$minimum
lambda

#compare result
#probably close enough
(1/mean(data1))


########## 1b ##########
#nll for meanlog
nll_lognorm_mu = function(mu, data) {
    return(- sum(dlnorm(data, meanlog = mu, sdlog = 0.5, log = TRUE)))
}

result = optimize(nll_lognorm_mu, interval = c(0, 100), data = data1)
str(result)
mu = result$minimum

#nll for sdlog
nll_lognorm_sig = function(sigma, mu, data) {
    return(- sum(dlnorm(data, meanlog = mu, sdlog = sigma, log = TRUE)))
}

result = optimize(nll_lognorm_sig, interval = c(0, 100), mu = mu, data = data1)
str(result)
sigma = result$minimum

#results
mu
sigma

#compare results
library(MASS)
fitdistr(data1, "lognormal")


########## 1c ##########
x = seq(0, max(data1), length = 100)
hist(data1, freq = FALSE, breaks = 20)
lines(density(data1), col = "orange", lwd = 2)
lines(x, dexp(x, lambda), col = "red", lwd = 2)
lines(x, dlnorm(x, meanlog = mu, sdlog = sigma), col = "blue", lwd = 2)
legend("topright", legend = c("Kernel density", "Exponential", "Lognormal"),
       col = c("orange", "red", "blue"), lwd = 2)

#looks like exponential is indeed a better fit
par(mfrow = c(1, 3))
normalized_data <- (data1 - min(data1)) / (max(data1) - min(data1))
qqnorm(normalized_data, main = "Data")
qqline(normalized_data)
qqnorm(dexp(x, lambda), main = "Exponential distribution")
qqline(dexp(x, lambda))
qqnorm(dlnorm(x, meanlog = mu, sdlog = sigma), main = "Lognormal distribution")
qqline(dlnorm(x, meanlog = mu, sdlog = sigma))
par(mfrow = c(1, 1))

#again, exp seems to be a better fit
#akaike
aic_exp = 2 * (negloglik_exp(lambda, data1)) + 2
aic_ln = 2 * (nll_lognorm_sig(sigma, mu, data1)) + 2*2
ifelse(aic_exp < aic_ln, "Exp better", "Ln better")


########## 1d ##########
exp_wait = pexp(5, lambda)
ln_wait = plnorm(5, mu, sigma)
exp_wait
ln_wait


########## 2  ##########
#lambda - mean of events per interval (1 minute)
#number of events in an hour -> 60*lambda
student_num = lambda * 60
student_num

#we can assume our data distribution is normal (clt)
sample_mean = mean(data1)
sample_sd = sd(data1) / sqrt(n)
t_critical = qt(0.975, n - 1)
conf_int = c(sample_mean - t_critical * sample_sd, sample_mean + t_critical * sample_sd)
conf_int

########## 3  ##########
uco <- 492875  # insert your UCO
set.seed(uco)
data2 <- sample(data1, 80)

#we can see even from histogram that it probably isn't normal
x = seq(0, max(data2), length = length(data2))
hist(data2, freq = FALSE)
lines(density(data2), lwd = 2)
lines(density(rnorm(80)), lwd = 2, col = "red")

#ggplot once again nicely shows the difference
par(mfrow = c(1, 2))
qqnorm(data2)
qqline(data2)
qqnorm(rnorm(80))
qqline(rnorm(80))
par(mfrow = c(1, 1))

#shapiro-wilk test shows miniscule p-value
test = shapiro.test(data2)
str(test)
test$p.value
ifelse(test$p.value > 0.05, "Normal", "Non-normal")


########## 4a ##########
#load data
load("customer_behaviour2.RData")
data$big = as.numeric(data$money_spent > 5000)
head(data)
big_spenders = data[data$big == 1,]$age
small_spenders = data[data$big == 0,]$age

#there probably will be a difference
hist(big_spenders, freq = FALSE, col = rgb(1, 0, 0, 0.5))
hist(small_spenders, freq = FALSE, col = rgb(0, 0, 1, 0.5), add = TRUE)
lines(density(big_spenders), lwd = 2, col = "red")
lines(density(small_spenders), lwd = 2, col = "blue")
legend("topright", legend = c("Big", "Small"),
       col = c("red", "blue"), lwd = 2)

#H0 - There is not a significant difference between big and small spender
#HA - There is a significant difference
result = t.test(big_spenders, small_spenders)
ifelse(result$p.value <0.05, "H0 rejected", "Failed to reject H0")
#H0 rejected -> statistically significant evidence that
    #there is a difference in age in both groups
#failed to reject H0 -> not enough statistically significant
    #evidence to tell if there is a difference in age of both groups

#test the direction of the difference
ifelse(mean(big_spenders) < mean(small_spenders),
       "Big spenders are younger", "Big spenders are older")

#also possible to use one-tailed t-test with:
#H0 - Big spenders are significantly younger
#HA - Big spenders are significantly older
#p-value of 0.5+-0.05 tells us that there is not statistically significant 
    #difference between the ages of both groups

########## 4b ##########
t.test(big_spenders, small_spenders)

