#### Task 1 ####
uco <- 492875  # insert your UCO
set.seed(uco)

data = read.csv("women.csv")

#--------- 1a ----------
iq = data$IQ
weight = data$weight

#calculate correlation and ci
corr = cor(iq, weight, method = "pearson", use = "complete.obs")
cor_ci = cor.test(iq, weight, conf.level = 0.95)

ci = cor_ci$conf.int

#print the results
cat("Correlation: ", round(corr, 3))
cat("95% Confidence interval: ", round(ci[1], 3), ", ", round(ci[2], 3))


#--------- 1b ----------
rep = 10000
bootstrap_corr = numeric(rep)
set.seed(uco)

#bootstrap loop
for (i in 1:rep) {
    bootstrap_sample = data[sample(nrow(data), replace = TRUE), ]
    bootstrap_corr[i] = cor(bootstrap_sample$IQ, bootstrap_sample$weight)
}

bootstrap_ci = quantile(bootstrap_corr, c(0.025, 0.975))

#print the results
cat("Bootstrap 95% confidence interval: ", round(bootstrap_ci[1], 3), ", ", round(bootstrap_ci[2], 3))


#--------- 1c ----------
cor_ci = cor.test(iq, weight, conf.level = 0.95)
p_val = cor_ci$p.value

#print the results
cat("p-value: ", round(p_val, 3))
cat(ifelse(p_val < 0.05, "reject H0", "fail to reject H0"))


#--------- 1d ----------
rep = 9999
mc_corr = numeric(rep)
set.seed(uco)

#monte carlo loop
#for (i in 1:rep) {
    #n = nrow(data)
    #mu = c(mean(data$IQ), mean(data$weight))
    #sigma = matrix(c(var(data$IQ), 0, 0, var(data$weight)), nrow = 2)
    #    mc_sample = mvrnorm(n, mu, sigma)
    #    mc_corr[i] <- cor(mc_sample[,1], mc_sample[,2])
#}

for (i in 1:rep) {
    shuffled_weights = sample(data$weight)
    mc_corr[i] = cor(data$IQ, shuffled_weights)
}

sim_p_val <- mean(abs(mc_corr) >= abs(corr))

cat("p-value: ", round(p_val, 3))
cat("Simulated: ", round(sim_p_val, 3))


#### Task 2 ####
load("farm1.RData")
load("farm2.RData")

hist(farm1, freq = F)
lines(density(farm1))
hist(farm2, freq = F)
lines(density(farm2))
length(farm1)
length(farm2)

#Test normality and variance -> both are ok for t-tests
shapiro.test(farm1)
shapiro.test(farm2)
var.test(farm1, farm2)


## Farm 1 average 125kg?
result = t.test(farm1, mu = 125, alternative = "less")
cat("p-value: ", round(result$p.value, 3))
ifelse(result$p.value < 0.05, "reject H0", "fail to reject H0")


## Same average weights?
result = t.test(farm1, farm2, var.equal = T)
cat("p-value: ", round(result$p.value, 3))
ifelse(result$p.value < 0.05, "reject H0", "fail to reject H0")

# Boxplot nicely shows the overlaps of the datasets which suggests that there
    #does not seem to be a significant difference in average weights
boxplot(farm1, farm2, names = c("Farm 1", "Farm 2"), 
        ylab = "Weight", 
        main = "Comparison of production")


#### Task 3 ####
load("cholesterol.RData")
str(data)

shapiro.test(data$age)
shapiro.test(data$blood_pressure_systolic)
shapiro.test(data$blood_pressure_diastolic)
tmp <- ifelse(data$smoker == "smoker", 1, 0)
shapiro.test(tmp)
tmp <- ifelse(data$vegetarian == "vegetarian", 1, 0)
shapiro.test(tmp)
shapiro.test(data$cholesterol)

model = lm(cholesterol ~ age + blood_pressure_systolic + blood_pressure_diastolic + smoker + vegetarian, data = data)
#normality looks ok, residuals vs fitted not so ok
par(mfrow = c(2, 2))
plot(model)
par(mfrow = c(1, 1))


#try log target and
data$cholesterol_log = log(data$cholesterol)
data$blood_pressure_systolic_log = log(data$blood_pressure_systolic)
data$blood_pressure_diastolic_log = log(data$blood_pressure_diastolic)

log_model1 = lm(cholesterol_log ~ age + blood_pressure_systolic + blood_pressure_diastolic + smoker + vegetarian, data = data)
log_model2 = lm(cholesterol ~ age + smoker + vegetarian + blood_pressure_systolic_log + blood_pressure_diastolic, data = data)
log_model3 = lm(cholesterol_log ~ age + smoker + vegetarian + blood_pressure_systolic_log + blood_pressure_diastolic, data = data)
log_model4 = lm(cholesterol ~ age + smoker + vegetarian + blood_pressure_systolic + blood_pressure_diastolic_log, data = data)
log_model5 = lm(cholesterol_log ~ age + smoker + vegetarian + blood_pressure_systolic + blood_pressure_diastolic_log, data = data)
log_model6 = lm(cholesterol_log ~ age + smoker + vegetarian + blood_pressure_systolic_log + blood_pressure_diastolic_log, data = data)


# Check the residuals vs fitted values plot again
par(mfrow = c(2, 3))
plot(log_model1, which = 1)
plot(log_model2, which = 1)
plot(log_model3, which = 1)
plot(log_model4, which = 1)
plot(log_model5, which = 1)
plot(log_model6, which = 1)
par(mfrow = c(1, 1))
#model5 looks "best"

par(mfrow = c(2, 2))
plot(log_model5)
par(mfrow = c(1, 1))

#get
bws_model = step(log_model6)
bws_model$anova

par(mfrow = c(2, 2))
plot(bws_model)
par(mfrow = c(1, 1))

residuals = residuals(log_model1)
shapiro.test(residuals)
qqnorm(residuals)
qqline(residuals)

summary(bws_model)


