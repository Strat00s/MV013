getwd()
setwd("seminars/02")


#data = read.csv("Computers.csv")
#summary(data)

#mean(data$price)
#median(data$price)

#sort(data$price)

#trimmedMean = function(data, trim) {
#    data = sort(data)
#    trim_size = length(data) * trim
#    return (mean(data[trim_size:length(data) - trim_size]))
#}


# ==============================================================================
# --------------------------------- SEMINAR 2 ----------------------------------
# ==============================================================================


# ----------------------------------- TASK 1 -----------------------------------


comp <- read.csv("Computers.csv", sep = ",")
summary(comp)
str(comp)

# load the price and ram columns into the special variables:
price <- comp$price
ram <- comp$ram

is.numeric(price)
is.numeric(ram)

# ..................................... A ......................................


# compute mean and median value of price data sample

# useful functions:
?mean
?median

(mean_price <- mean(price))
(median_price <- median(price))



# ..................................... B ......................................


# create your function to compute quantiles in R
# use it to compute quartiles of price

# HINT: explore descriptive statistics pdf-file in IS outline

# useful functions: 
?length
?sort
?ceiling

my_quantile <- function(sample, a) {
    n <- length(sample)   # number of observations
    x <- sort(sample)     # sorted data sample
    c <- ceiling(n * a) #index
    if (c == n * a)
        q <- (x[c] + x[c + 1]) / 2       # choose the right observation from x
    else
        q <- x[c]

    return(q)
}

my_quantile(sample = price, a = 0.25)
my_quantile(price, 0.5)
my_quantile(price, 0.75)

# Build-in R function:

# there are different definitions of quantiles, we use type = 2
# in lecture slides there is type = 1
(quant <- quantile(price, probs = 0.5, type = 2)) # median = 0.5-quantile
(quant <- quantile(price, probs = c(0, 0.25, 0.5, 0.75, 1), type = 2))


# ..................................... C ......................................


# create your R functions to compute trimmed, and winsorized mean
# compute 0.1-trimmed, and 0.1-winsorized mean of price

# HINT: explore descriptive statistics pdf-file in IS outline

# useful functions:
?floor

my_trimmed_mean <- function(x, k) {
    n <- length(x)    # number of observations
    x <- sort(x)      # sorted data sample 
    c <- floor(n * k) # index
    x_trimmed <- x[(c + 1):(n - c)]
    return(mean(x_trimmed))
}
my_trimmed_mean(price, 0.1)

# build-in R function:

mean(price, trim = 0.1)

my_winsorized_mean <- function(x, k) {
    n <- length(x)    # number of observations
    x <- sort(x)      # sorted data sample 
    f <- floor(n * k) # index
    x_new <- c(rep(x[f + 1], f), x[(f + 1):(n - f)], rep(x[n - f], f))  #use n - f for indexing last value; indexing from + -> +1 for first index
    return(mean(x_new))
}

my_winsorized_mean(price, 0.1)

# ..................................... D ......................................


# compute following characteristics of price variable

# use the formulas from the descriptive statistics pdf-file in IS outline

# useful functions:
?sum
?max
?min

n <- length(price)
variance <- sum((price - mean(price))^2) / n
cat("  Variance: ", variance, " ", var(price))
s <- sqrt(variance)             # standard deviation
cat("  Stdd: ", s, sqrt(var(price)))
range <- max(price) - min(price)
cat("  range: ", range)
iqr <- quantile(price, 0.75) - quantile(price, 0.25)          # interquartile range
cat("  IQR: ", iqr, IQR(price))
MAD <- median(abs(price - median(price)))   #no idea what is wrong
cat("  MAD: ", MAD, mad(price))
skew <- 
kur <- 


# ..................................... E ......................................


# create a boxplot for price variable

# HINT: apply boxplot() function to the price variable:

b <- boxplot(price)

# try it again with the specification of main input and col input:

b <- boxplot(price, main = "Boxplot of price", col = "yellow")

b$stats # are you able to interpret these values?

# HINT:
min(price)
quant[2] - 1.5 * IQR # lower than minimum: it is not the lower whisker

quant[2]
median(price)
quant[4]

quant[4] + 1.5 * IQR # lower than maximum: it is the upper whisker (nearly)
max(price)


# ..................................... F ......................................


# create a histogram for price

hist(price, col = "red")

# find out how to change the number of cells of histogram
# or how to specify the breaks of intervals:

# HINT: explore the BREAKS input parameter of hist()

?hist
hist(price, col = "red", breaks = )

# Add kernel density estimation into the histogram

?density      # kernel density estimation build-in function
?lines

d <- density(price)         # apply density() function to price

# plot the histogram together with the density line
# HINT: use lines() command for d variable after you plot the histogram

hist(price, col = "red", breaks = 50)
lines(d, col = "green")

# ..................................... G ......................................


# create a table of relative frequencies for different RAM sizes:

# useful functions:
?table
?nrow

# HINT: first apply table() to the RAM variable,
# then divide it by the number of raws:
freq <- table(ram)
relative_freq <- freq / length(ram)
print(relative_freq)

# gini index, entropy:
gini <- 1 - sum(relative_freq^2)
entropy <- - sum(relative_freq * log(relative_freq))

# create pie chart and bar chart for the RAM size:

# HINT: apply these functions to the relative_freq or freq variables:
# change the color of barplot (col input)
?pie
?barplot


# ..................................... H ......................................


# create a boxplot of price for each RAM size:

boxplot(price ~ ram, main = "Boxplot of price", col = 1:6)


# ..................................... I ......................................


# create a histogram of price for each RAM size:

par(mfrow = c(2, 3)) # divides the plot into 6 sections

# fill the 6 histograms commands, you can use for cycle if you want

par(mfrow = c(1, 1)) # returns the setting of sections back
boxplot(price ~ ram, main = "Boxplot of price", col = 1:6)


# ----------------------------------- TASK 2 -----------------------------------


# Download any colored image in jpg format. Load it into R using jpeg
# package command readJPEG. Create empty plot a insert the image into it
# using rasterImage command. From the load data matrix corresponding
# to your figure, create three histograms for red, green, and blue color. Add
# kernel density estimation to each histogram. Place all three histograms
# into one figure.

# install.packages("jpeg")
library(jpeg)
img <- readJPEG("img.jpg", native = FALSE)

?rasterImage
?par

par(mar = c(0, 0, 0, 0)) # set the plot margins to zero
plot(1:2, type = 'n', xlab = "", ylab = "", xaxt = 'n', yaxt = 'n')
# type NONE prepares the EMPTY plot
rasterImage(img, 1, 1, 2, 2) # draws the image itself

str(img)
r <- img[, , 1]
g <- img[, , 2]
b <- img[, , 3]

par(mfrow = c(1, 3), mar = c(3, 2, 3, 1))
# fill the histogram commands for each color

par(mfrow = c(1, 1), mar = c(5.1, 4.1, 4.1, 2.1)) # default setting

