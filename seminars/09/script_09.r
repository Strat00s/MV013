setwd("seminars/09")
# ==============================================================================
# --------------------------------- SEMINAR 9 ----------------------------------
# ==============================================================================


# ----------------------------------- TASK 1 -----------------------------------


time <- c(43, 26, 31, 18, 31, 19, 19, 20, 22, 36, 21, 31, 27, 26, 38, 40, 49, 33,
          33, 43, 53, 28, 18, 21, 23, 21, 46, 39, 29, 52, 29, 42, 47, 26, 28, 36,
          14, 41, 36, 32)
mach <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2,
          2, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4)

mach <- factor(mach)


# ....................................... A ....................................
# Create a boxplot of the time lengths for different machines (in the 1 picture).
# Do You think that the expected values of times are different?
boxplot(time ~ mach)



# ....................................... B ....................................


# Try to decide the previous question using the pairwise t-tests for different
# machines. Why is this approach incorrect?

# firstly, select the time lengths for each machine separately:

time1 <- time[mach == 1]
time2 <- time[mach == 2]
time3 <- time[mach == 3]
time4 <- time[mach == 4]

# now check the normality assumption:
  
# HINT: you can use any method you want, but the most simple is one of the 
# statistical tests, for example shapiro.test:

shapiro.test(time1)
shapiro.test(time2)
shapiro.test(time3)
shapiro.test(time4)
# conclusion: They are normal with 95% confidence

# check the assumption of equal variances for different pairs of time lengths:

# HINT: use var.test() from the previous seminar

var.test(time1, time2)
var.test(time1, time3)
var.test(time1, time4)
var.test(time2, time3)
var.test(time2, time4)
var.test(time3, time4)


# finally, perform the 2-sample t-test for time lengths of selected pairs of machines

# HINT: use the built-in function for t-tests from the previous seminar
t.test(time1, time2, var.equal = T)
t.test(time1, time3, var.equal = T)
t.test(time1, time4, var.equal = T)
t.test(time2, time3, var.equal = T)
t.test(time2, time4, var.equal = T)
t.test(time3, time4, var.equal = T)

# conclusion: We fail to reject the H0 -> we dont have enough data
#to conclude if the times are different

# we CAN NOT claim that this conclusion holds for all 4 machines at
# the same time, even though we can claim this for each pair

# this approach causes INCORECT error of the 1st type (alpha), for all 4
# machines at once it is not 0.05 as it is for each t-test separately

# pairwise t-test is not correct = use ANOVA instead!


# ....................................... C ....................................


# Test the hypothesis that the expected time lengths for all four machines are the
# same against the alternative, that it differs for at least one pair of machines
# using the analysis of variance (ANOVA) approach. Use the diagnostic graphs to
# check presumptions of the model.

# firstly, check the assumptions of the ANOVA test:

# normality (already checked)

# equal variances (for all four groups): Levenes test for homoscedasticity

#install.packages("car")
library(car)
leveneTest(time ~ mach, center = mean)

# conclusion: FILL!

# now we can use the ANOVA test (using the built-in function):

?aov

my_aov <- aov(time ~ mach)
summary(my_aov)

# conclusion: FILL!

# EXTRA CODE (for volunteers): diagnostic graph

par(mfrow = c(2, 2))
plot(my_aov)
par(mfrow = c(1, 1))


# ....................................... D ....................................


# Use the post-hock Tukey test to compare the groups pairwise (if needed).
# Which pair of machines does probably differ in the expected values?

# ANOVA test did not rejected the null hypothesis that the expected values are
# the same, so we have no reason to perform the post-hock Tukey test now, cause
# it seems that the expected values of no pair of machines differ.



# ----------------------------------- TASK 2 -----------------------------------


time <- c(33, 30, 18, 18, 46, 32, 54, 34, 47, 43, 26, 25, 21, 27, 37, 32, 31, 22,
          45, 25, 14, 40, 53, 30, 22, 20, 19, 14, 11, 27, 22,  6, 23, 39,  3, 23,
          26, 30, 26, 32)
mach <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2,
          2, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4)
mach <- factor(mach)


# ....................................... A ....................................


# Create a boxplot of the time lengths for different machines.
# Do You think that the expected values of times are different?

boxplot(time ~ mach)


# ....................................... B ....................................


# Test the hypothesis that the expected time lengths for all four machines are the
# same against the alternative, that it differs for at least one pair of machines
# using the analysis of variance (ANOVA) approach. Use the diagnostic graphs to
# check presumptions of the model.

my_aov = aov(time ~ mach)
summary(my_aov)
par(mfrow = c(2, 2))
plot(my_aov)
par(mfrow = c(1, 1))


# conclusion: FILL!


# ....................................... C ....................................


# Use the post-hock Tukey test to compare the groups pairwise (if needed).
# Which pair of machines does probably differ in the expected values?

# HINT: apply TukeyHSD() to my_aov object you created previously, do not forget
# specify the confidence level by conf.level input of TukeyHSD()

?TukeyHSD

(TUKEY <- TukeyHSD(my_aov, conf.level = 0.95))
plot(TUKEY, las = 1 , col = "brown")

# expected values of groups 1 and 3 are on the edge, they possibly differ


# ----------------------------------- TASK 3 -----------------------------------


data <- read.csv("Computers.csv", sep = ",")
summary(data)

# ....................................... A ....................................


# Test the independence of RAM and screen. Create (and visualize) a contingency
# table and use the built-in R function to perform the test. What seems to be a problem?

# firstly, select the RAM and SCREEN columns:

ram <- data$ram
screen <- data$screen

# now create a contingency table of these two variables:

#?table

(tab.1 <- table(ram, screen))

# now visualize this contingency table:
#install.packages("vcd")
library(vcd)

# HINT: apply mosaic() to the table you created, explore the inputs

#?mosaic

mosaic(tab.1)

# finally, perform the Pearsons chi squared test of independence for this table:

# HINT: apply the chisq.test() to the table you created:

#?chisq.test

chisq.test(tab.1)

# conclusion: FILL!

# check the presumption of the test about the number of expected counts of obs.

# expected counts e_ij (manually from the lecture slaids):




# or (considering the chisq.test object structure):

str(chisq.test(tab.1))




# PROBLEM: Persons chi squared test is an asymptotical tests = we need the
# sufficient amount of observations in each cell

# the expected frequencies of each cell should be greater or equal to 5,
# which is not fullfilled now

# to correct this we need to merge some cells with too small number of observations


# ....................................... B ....................................


# Create a new categorical variable for RAM with categories 2, 4, 8, and 16+ MB.
# Test the independence of RAM and screen using this new variable.

# update the ram vector by replacing the values >= 16 by "16+":

ram[] <- 

ram <- factor(ram, levels = c(2, 4, 8, "16+")) # to ensure correct order

# create a contingency table of new RAM variable and SCREEN variable:

(tab.2 <- )
summary(tab.2)

# test the independence:



# conclusion: FILL!

# check the presumption of the test about the number of expected counts of obs.

# expected counts e_ij (manually):

rowSums(tab.2) %*% t(colSums(tab.2)) / sum(tab.2)

# or:

chisq.test(tab.2)$expected


# ....................................... C ....................................


# Compute the Cramers V coefficient both manually and using the built-in R function.

# install.packages("questionr")
library(questionr)

# 1) using the built-in function:

?cramer.v

cramer.v()

# 2) manually:

str(chisq.test(tab.2))

# test statistic (considering the chisq.test() object structure):

chi2 <- 
chi2 <- as.numeric(chi2)

# Cramers coefficient (from the slaids):

sqrt(chi2 / (sum(tab.2) * (min(nrow(tab.2), ncol(tab.2)) - 1)))

# Pearsons residua (from the mosaic plot):

chisq.test(tab.2)$residua


# ....................................... D ....................................


# Perform correspondence analysis using R.

# HINT: apply ca() function to the table tab.2

# install.packages("ca")
library(ca)

?ca

(my_ca <- ca())

plot(my_ca)
