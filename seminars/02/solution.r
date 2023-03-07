getwd()
setwd("seminars/02")

comp <- read.csv("Computers.csv", sep = ";")
  # load data, separate columns by a comma ","

price <- comp$price
ram <- comp$ram

#################### subtask a ##################
mean(price)
  # argument is not numeric
mean(as.numeric(price))
  # NAs introduced by coercion, let's check why
price[is.na(as.numeric(price))]
  # our problematic values
  # we need to get rid of the comma
price <- as.numeric(gsub(",", "", price))
  # we correcte the variable 
median(price)
mean(price)

##################### subtask b #######################
my_quantile <- function(data, q) {
  n <- length(data) # number of data points
  data <- sort(data) # sort in data into an increasing sequence
  c <- ceiling(n*q) # ceiling = rounding up
  if (c == n*q) { # in case when the estimate quantile lands exactly onto an observation
    alpha <- (data[c] + data[c + 1]) / 2 # move to the middle between the c-th and (c+1)th observation
  } else {
    alpha <- data[c]
  }
  return(alpha)
}

my_quantile(price, 0.25)
my_quantile(price, 0.5)
my_quantile(price, 0.75)


# there are different definitions of quantiles, we use type = 2
# in the slides you can see type = 1
quantile(price, probs = c(0, 0.25, 0.5, 0.75, 1), type = 2)
  # by specifying a vector of probs you can calculate several quantiles at once

############################ subtask c ###########################
my_trimmed_mean <- function(data, k) {
  n <- length(data) # number of data points
  data <- sort(data)  # sort data into an increasing sequence
  c <- floor(n * k / 100) # floor = rounding down
  x <- data[(c+1):(n-c)] # pick only suitable data points 
  return(mean(x))
}

my_trimmed_mean(price, 10)

# built in R function for trimmed mean
mean(price, trim = 0.1)

my_winsorized_mean <- function(data, k){
  n <- length(data)
  data <- sort(data)
  c <- floor(n * k / 100)
  x <- c(rep(data[c + 1], c), data[(c + 1):(n - c)], rep(data[n - c], c))
      # we create a vector using the c() command which contains
      # 1. c repetitions of (c + 1)th element of our data: rep(data[c + 1], c)
      # 2. the (c + 1)th to (n - c)th element of our data: data[(c + 1):(n - c)]
      # 3. c repetitions of the (n - c)th element of our data: rep(data[n - c], c)
  mean(x)
}

my_winsorized_mean(price, 10)

# a function exists in a package psych
install.packages("psych")
psych::winsor.mean(price, trim = 0.1)

########################## subtask d #############################
n <- length(price)
variance <- 1 / n * sum((price - mean_price)**2)
s <- sqrt(variance) # standard deviation
range <- max(price) - min(price)
IQR <- as.numeric(quantile(price, 0.75) - quantile(price, 0.25)) # interquartile range
MAD <- median(abs(price - median_price)) # median absolute deviation

skew <- 1 / n * sum(((price - mean_price) / s)**3) # skewness
kur <- 1 / n * sum(((price - mean_price) / s) ** 4) - 3 # kurtosis 

######################### subtask e ####################################
b <- boxplot(price, main = "Boxplot of price")
  # we can see one outlier around zero, that is suspicious, computers dont cost that little
  # the outliers in high prices are to be expected, super expensive computers exists and they are not as usual
b$stats

median(price)
  # median is the thick line in the middle of the boxplot box

quantile(price, 0.25, type = 2) - 1.5*IQR

quantile(price, 0.75, type = 2) + 1.5 * IQR


################################# subtask f ####################################
hist(price)
ceiling(log2(length(price))) + 1 # r is using this number of intervals modification of this formula
hist(price, breaks = 40) 

d <- density(price)
  # save the output of the density function for plotting later
hist(price, breaks = 40, freq = F)
lines(d, col = 'red', lwd = 2)
  # the graph input is the output of the density() function
abline(v = c(median_price, mean_price), col = c('green', 'blue'), lwd = 2)
  # v = vertical line
legend(4000, 6*10^(-4), # position of the legend
       legend = c("Kernel density", "Mean", "Median"), # names
       col = c("red", "blue", "green"), lty = 1, cex = 0.8, bty = n)
  # lty = 1 = lines
  # cex = 0.8 = 0.8 of the normal size 
  # bty = n = no line box around the legend

################################## subtask g ##################################
relative_freq <- table(ram) / nrow(comp)
gini <- 1 - sum(relative_freq^2)
entropy <- -sum(relative_freq*log(relative_freq))

pie(relative_freq, main = "RAM")
barplot(relative_freq, main = "RAM")
barplot(table(ram), main = "RAM")

################################## subtask h ##################################
b <- boxplot(price ~ ram, main = "Boxplot of price")
  # we can plot dependencies using the tilde ~ character

################################ subtask i ####################################
par(mfrow=c(2,3)) # 2 graph rows, 3 graph columns = 6 graphs in total will be plotted into one 
for (my_ram in sort(unique(ram))){ # loop going through each ram 
  hist(price[comp$ram==my_ram], # select only certain ram observations
       main=paste("RAM ",my_ram), # title of the graph
       xlab="price",
       ylim=c(0,500),
       xlim=c(900,5500))
}
par(mfrow=c(1,1)) # reset the number of plotted graphs to just one


############################### TASK 2 #######################################
library(jpeg)
img<-readJPEG(paste("img.jpg",sep=""), native = FALSE)
plot(1:2, type='n',xlab="",ylab="",xaxt='n',yaxt='n')
rasterImage(img, 1, 1, 2, 2)

par(mfrow=c(1,3))
r<-img[,,1]
dr<-density(r)
hist(r,col='red',freq=F,ylim=c(0,2),xlim=c(0,1),breaks = seq(0,1,0.05))
lines(dr,lwd=2)

g<-img[,,2]
dg<-density(g)
hist(g,col='green',freq=F,ylim=c(0,2),xlim=c(0,1),breaks = seq(0,1,0.05))
lines(dg,lwd=2)

b<-img[,,3]
db<-density(b)
hist(b,col='blue',freq=F,ylim=c(0,2),xlim=c(0,1),breaks = seq(0,1,0.05))
lines(db,lwd=2)

