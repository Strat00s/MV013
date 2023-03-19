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
#outlier
summary(data$mail_ads)      #outlier
hist(data$mail_ads)
summary(data$shop_visits)   #ok
hist(data$shop_visits)

#remove the row with way too many mail ads
data = data[data$mail_ads != max(data$mail_ads),]

nrow(data)  #resulting number of rows
