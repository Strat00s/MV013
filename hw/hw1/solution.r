getwd()
setwd("hw/hw1")
options(max.print=1000000)

#TASK 1
load("customer_behaviour.RData")
str(data)
head(data)
nrow(data)

#check all columns if they are numbers
data$money_spent[is.na(as.numeric(data$money_spent))]   #ok
data$age[is.na(as.numeric(data$age))]                   #ok
data$web_visits[is.na(as.numeric(data$web_visits))]     #bad
data$mail_ads[is.na(as.numeric(data$mail_ads))]         #ok
data$shop_visits[is.na(as.numeric(data$shop_visits))]   #bad

#convert all bad items to numbers
data$web_visits = as.numeric(data$web_visits)
data$shop_visits = as.numeric(data$shop_visits)

#remove all NA rows
data = data[!is.na(data$web_visits) & !is.na(data$shop_visits),]

#also remove all rows with negative values (as they don't make sense)
data = data[data$money_spent >= 0 &
            data$age         >= 0 &
            data$web_visits  >= 0 &
            data$mail_ads    >= 0 &
            data$shop_visits >= 0,]

#check for extremes which might be a mistake
summary(data$money_spent)   #ok
hist(data$money_spent)
summary(data$age)           #ok
hist(data$age)
summary(data$web_visits)    #ok
hist(data$web_visits)
summary(data$mail_ads)      #outlier
hist(data$mail_ads)
summary(data$shop_visits)   #ok
hist(data$shop_visits)

#remove the row with way too many mail ads
data = data[data$mail_ads != max(data$mail_ads),]

#resulting number of rows
nrow(data)


###############################################################################
#TASK2
load("customer_behaviour2.RData")
str(data)
head(data)
nrow(data)

#add column big and fill it with 1 if money_spent > 5000, 0 otherwise (as.numeric convert TRUE,FALSE to 1,0)
data$big = as.numeric(data$money_spent > 5000)
head(data)

#split the plot area and draw both box plots
par(mfrow = c(1, 2))
boxplot(data$money_spent[data$big == 0], main = "Spent <= 5000", xlab = "big = 0", ylab = "money_spent")
boxplot(data$money_spent[data$big == 1], main = "Spent > 5000", xlab = "big = 1", ylab = "money_spent")

#reset area and draw histogram with kernel density
par(mfrow = c(1, 1))
hist(data$money_spent, freq = FALSE)
lines(density(data$money_spent), lwd = 2)

#mean, media, q1, q3, iqr, variance
#summary(data$age)
mean(data$age)
median(data$age)
(q1 = quantile(data$age, 0.25))
(q3 = quantile(data$age, 0.75))
(iqr = q3 - q1)
var(data$age)

#looking at the histogram, median would probably be best as it is less influenced by the extremes.
#also we can see from the histogram that the dataset is skewed to the right, so IRQ is probably the best option here, as it is also not affected by extremes
hist(data$money_spent)
mean(data$money_spent)
median(data$money_spent)


###############################################################################
#TASK3
#create subset of data without money_spent and big
short_data = data[, c("age", "web_visits", "mail_ads", "shop_visits")]
#compute correlation matrix
cor_data = cor(short_data)

#sum of the main diagonal (number of variables)
sum(diag(cor_data))
#it tells us that the every variable perfectly correlates with itself (and so it tells us the number of variables)

#correlation between vars
#the closer they are to 0, the lower their relationship, the closer to 1, the higher possitive linear relationship, the closer ot -1, the higher the negative linear relationship
cor(data$money_spent, data$age)         #-0.61 Moderately high negative correlation
cor(data$money_spent, data$web_visits)  #0.37  Moderate possitive correlation


###############################################################################
#TASK4
#Do I want to scale them?
#pretty much taken from seminar. Do PCA, get summary, calculate number of components required for at least 80%
short_data.pca <- prcomp(short_data, center = T, scale = F)
(s <- summary(short_data.pca))
plot(s$importance[3, ], main = "Cumulative Proportion of variance", ylab = "proportion",
     type = 'o', col = "red", pch = 19)
(num_components <- sum(s$importance[3, ] < 0.8) + 1)
#just 2 components are enough (can be nicely seen from the plot)

#look for values closes to 1 for each component
head(short_data.pca$rotation)
#PC1: web_visits -0.80907337
#PC2: age        -0.7944448
#PC3: mail_ads    0.9764561
#PC4: shop_visits 0.98610189

pca_scores = predict(short_data.pca, short_data)
df = data.frame(PC1 = pca_scores[, 1], PC2 = pca_scores[, 2], big = data$big)
library(ggplot2)
ggplot(df, aes(x = PC1, y = PC2, color = factor(big))) + 
    geom_point()

#library(dplyr)
#autoplot(prcomp(short_data), data = df, shape = T, colour = ifelse(data$big == 1, 'red', 'black'), loadings = TRUE, loadings.label = TRUE)
