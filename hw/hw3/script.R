#### Task 1 ####
uco <- 492875  # insert your UCO
set.seed(uco)

data = read.csv("women.csv")

summary(data)
hist(data$weight)
#remove probably invalid data
data = data[data$weight > 10,]


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

for (i in 1:rep) {
    shuffled_weights = sample(data$weight)
    mc_corr[i] = cor(data$IQ, shuffled_weights)
}

#sim_p_val = mean(abs(mc_corr) >= abs(corr))
#from 12th seminar
sim_p_val = 2 * min((sum(mc_corr <= corr) + 1) / (rep + 1),
                    (sum(mc_corr >= corr) + 1) / (rep + 1))

cat("p-value: ", round(p_val, 3))
cat("Simulated: ", round(sim_p_val, 3))


#### Task 2 ####
load("farm1.RData")
load("farm2.RData")

summary(farm1)
summary(farm2)

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
data$id = NULL
str(data)
model = lm(cholesterol ~ age + vegetarian + smoker + blood_pressure_systolic + blood_pressure_diastolic, data = data)
#normality looks ok, linearity not so much
par(mfrow = c(2, 2))
plot(model)
par(mfrow = c(1, 1))

#this one looks pretty good
model = lm(cholesterol ~ age * vegetarian + smoker + blood_pressure_systolic + blood_pressure_diastolic, data = data)
par(mfrow = c(2, 2))
plot(model)
par(mfrow = c(1, 1))


#looks even better
data$cholesterol_log = log(data$cholesterol)
log_model = lm(cholesterol_log ~ age * vegetarian + blood_pressure_systolic + blood_pressure_diastolic + smoker, data = data)
par(mfrow = c(2, 2))
plot(log_model)
par(mfrow = c(1, 1))


## We apparently cannot use step-wise procedures, so correlation matrix instead
#make variables numeric
data$smoker_numeric <- ifelse(data$smoker == "smoker", 1, 0)
data$vegetarian_numeric <- ifelse(data$vegetarian == "vegetarian", 1, 0)

#use correlatiion matrix to check for possible predictors
library(corrplot)
cor_matrix = cor(data[, sapply(data, is.numeric)])
corrplot(cor_matrix)

#simplest and best seem to be age and if they are vegetarian or not
(cholesterol_log_cor = cor_matrix["cholesterol_log",])
print(data$vegetarian)
final_model = lm(cholesterol_log ~ age * vegetarian, data = data)
par(mfrow = c(2, 2))
plot(final_model)
par(mfrow = c(1, 1))
summary(final_model)

