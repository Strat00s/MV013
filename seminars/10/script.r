setwd("seminars/10")

# ==============================================================================
# --------------------------------- SEMINAR 10 ---------------------------------
# ==============================================================================


# ----------------------------------- TASK 1 -----------------------------------


income23 <- c(30, 30, 30, 30, 30, 30, 35, 35, 35, 35, 35, 35, 35, 37, 37, 37, 37,
              40, 40, 40, 40, 42, 42, 45, 45, 45, 45, 50, 50, 50, 55, 55, 55, 60,
              68, 67, 72, 78, 79, 100)

income22 <- c(15, 25, 25, 25, 25, 30, 30, 30, 30, 30, 30, 35, 35, 40, 40, 35,
              35, 40, 40, 40, 40, 45, 45, 45, 45, 45, 45, 55, 55, 55, 55, 55,
              55, 55, 55, 55, 70, 70, 70, 120)


# ....................................... A ....................................


# Firstly, check if the data sample income23 can be assumed to be normally
# distributed. Are there any outliers?

# 1)
# plot the histogram together with the density of normal distribution with
# the estimated parameters, visualize the mean and median values:
hist(income22)
hist(income23)


# 2) or you can create a QQ-plot:



# 3) or you can use some of the statistical tests, for example Shapiro-Wilk:
shapiro.test(income22)
shapiro.test(income23)

# conlusion: we REJECT the normality of income

# now check if there are any outliers:
# HINT: boxplot will help you
boxplot(income22)
boxplot(income23)

# EXTRA CODE (for volunteers): one can imagine take the logarithm of income, but
# either this will not be normal

shapiro.test(log(income23))


# ....................................... B ....................................


# Test the following hypothesis using the 1-sample sign test about median:
# H0: median = 46, H1: median < 46

# check the assumption (delete the values equal to the possible median):


# 1) manually:

n <- length(income23)

# test statistic S:

T <- sum(income23 > 46)

# critical region:

W <- c(0, qbinom(0.05, n, 0.5))

# p value:
p.value <- pbinom(T, n, 0.5)

# assymptotical p value (from the normal distribution approximation)
U <- (((T - n) / 2) / sqrt(n / 4))
p.value.approx <- pnorm(U)

# 2) using the built-in function:

#install.packages("BSDA")
library(BSDA)
SIGN.test(income23, md = 46, alternative = "less")

# conclusion: FILL!


# ....................................... C ....................................


# Test the same hypothesis for the value 45. What has changed?

# check the assumption (delete the values equal to the possible median)




# 1) manually:

n <- length(income23_new)

# test statistic S:

T <- 

# critical region:

W <- c(, )

# p value:
p.value <- 

# 2) using the built-in function:

SIGN.test(income23, md = 45, alternative = "less")

# conclusion: FILL!

# value 45 repeats 4x in the data sample, but the values equal to the median we
# test are omitted by the sign test


# ....................................... D ....................................


# Test the hypothesis that the median salary in 2023 is equal to the median
# salary in 2022 against the right-sided alternative (both manually and using
# the built-in function). Use the 1-sample sign test and 1-sample Wilcoxon test
# for the difference of incomes.


# 1) paired sign test about the median

# HINT: use 1-sample sign test for difference of incomes using the built-in function:



# conclusion: FILL!


# 2) 1-sample Wilcoxon test

# HINT: use the asymptotic Wilcoxon test (based on the approximation by N(0; 1))

diff <- income23 - income22

# assumptions: symmetrical around median, no ties

# visually check the symmetry (plot the histogram of difference together with
# the mean and median values, use abline()):




# check, if there are any ties (use table() command):
ties <- table(diff)

# yes, there are, so we have to use ties correction for 1-sample Wilcoxon test

# delete all values equal to the possible median (= 0):

diff.new <- diff[diff != 0]
n <- length(diff.new)

# test statistic:

# HINT: see slaid 40 from the lecture, for the rank computation use rank() function

r <- rank(abs(diff.new - 0))
T <- sum(r[diff.new > 0])

# adjusted (because of ties) approximate test statistic (should follow N(0; 1)):

# HINT: see the slaid 45

U.adj <- (T - n * (n + 1)/4)/sqrt(n * (n + 1) * (2 * n + 1)/24 - 1/48 * sum(ties^3 - ties))

p.value <- (1 - pnorm(U.adj))

wilcox.test(diff, alternative = "greater", correct = F)
# correct = continuity correction, ties correction is used automatically

# conclusion: FILL!


# ----------------------------------- TASK 2 -----------------------------------


library(jpeg)

img1 <- readJPEG(paste("fi.jpg", sep = ""), native = FALSE)
img2 <- readJPEG(paste("dms.jpg", sep = ""), native = FALSE)

# plot the images:

par(mfrow = c(1, 2))

plot(1:2, type = 'n', xlab = "", ylab = "", xaxt = 'n', yaxt = 'n', frame.plot = F)
rasterImage(img1, 1, 1, 2, 2)

plot(1:2, type = 'n', xlab = "", ylab = "", xaxt = 'n', yaxt = 'n', frame.plot = F)
rasterImage(img2, 1, 1, 2, 2)

par(mfrow = c(1, 1))


# ....................................... A ....................................


# Perform an exploratory data analysis of the problem. Create a histogram together
# with a kernel density estimation, boxplot and plot of the empirical distribution
# function.

g1 <- c(img1[, , 2])
g2 <- c(img2[, , 2])

# 1) histograms + kernel density estimations:

# compute the kernel density estimation of both vectors using function density()

?density

dg1 <- density(g1)
dg2 <- density(g2)

# now plot the histograms of the vecotrs together with the kernel density est.

par(mfrow = c(1, 2))

# 1st histogram
hist(g1, freq = FALSE)
lines(dg1, lwd=2)
# 2nd histogram
hist(g2, freq = FALSE)
lines(dg2, lwd=2)

par(mfrow = c(1, 1))


# 2) boxplots:

par(mfrow = c(1, 2))

# 1st boxplot
boxplot(g1)
# 2nd boxplot
boxplot(g2)

par(mfrow = c(1, 1))


# 3) empirical distribution functions (use the ecdf()):

df1 <- ecdf(g1)
df2 <- ecdf(g2)

# plot it:
plot(df1)
lines(df2)


# ....................................... B ....................................


# Are the levels of the green color the same over those two images?
# Formulate a hypothesis that could answer the posted question. Choose the
# correct test statistics and test your hypothesis.

# HINT: use 2-sample Kolmogorov-Smirnov test to compare the distributions

n1 <- length(g1)
n2 <- length(g2)


# 1) manually (using the test statistic):

# HINT: remember ecdf() returns a function, so apply the functions in df1, df2
# to xx vector and compute the absolute maximum (see slaid 15 from the lecture)

# xx is a vector of many points from the interval where you want to compare the
# empirical distribution functions (from 0 to 1):

xx <- 

Dmax <- 

# now compute the test statistic:

D <- 

# p-value (see the slaid 15):

p.value <- 

  
# 2) using a built-in function:

ks.test(g1, g2)

# right-sided:

ks.test(g1, g2, alternative = "greater")

# left-sided

ks.test(g1, g2, alternative = "less")
