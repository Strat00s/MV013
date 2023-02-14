
# ==============================================================================
# --------------------------------- SEMINAR 1 ----------------------------------
# ==============================================================================


# ----------------------------------- TASK 1 -----------------------------------


1 + 1

1 * 0
1 / 0 # R knows limits

2 ^ 10
2**10

sqrt(10)
10 ^ (1 / 2)

pi
sqrt((42 + 4.2) ^ 2 + sin(exp(1) * pi))

(3 + 2i) / (2i) # R can work with imaginary numbers

i
1i
1*i
# i can 't stand alone, always has to be accompanied by a number


# ----------------------------------- TASK 2 -----------------------------------


?sin
?load
?lm
?mean
example(mean)
example(lm) # might be useful in following lectures


# ----------------------------------- TASK 3 -----------------------------------


a <- 5
5 -> a
a = 5 # proper syntax uses <- instead of =

b <- 8
c <- a + b
c

(c <- a + b)


# ----------------------------------- TASK 4 -----------------------------------


a <- 25
A <- 4
a
A
# R is case-sensitive

A == a
a <- A
A == a


# ----------------------------------- TASK 5 -----------------------------------


1 == 1 # equal
1 == 2
1 != 1 # not equal
1 != 2
1 < 2
1 <= 2

TRUE & TRUE         # and
TRUE | FALSE        # or
(1 <= 2) & (2 >= 0) # combining
!FALSE              # negating
!(2 < 1)


# ----------------------------------- TASK 6 -----------------------------------


# numerical - use decimal point
a <- 2.2

# string - both single and double quotation marks can be used,
# as long as you use the same at both ends
a <- "something"
b <- 'something else'


# logical - TRUE / FALSE or T/F can be used
a <- TRUE
b <- T
c <- FALSE
d <- F

# can be added up - TRUE is 1, FALSE is 0
a + b

is.integer(a)
is.integer(a + b) # we changed the data type


# ----------------------------------- TASK 7 -----------------------------------


# A
c(1, 2, 3, 4)
1:4

# B
?rep

rep(1, 10)

rep(1:3, 10)
rep(1:3, times = 10)
rep(1:3, each = 10)

# C
?seq

seq(from = 1, to = 4)
seq(1, 4)
seq(to = 4, from = 1)

seq(from = 1, to = 10, length = 5)
seq(1, 10, length = 5)
seq(1, 10, by = 2)
seq(from = 1, by = 2, length = 5)

# D
runif(10)                    # uniformly distributed on [0; 1]
runif(10, min = 0, max = 10) # uniformly distributed between 0 and 10

# E
# vectors don't have to be numerical, but all elements are taken to be of the same type
(character.vector <- c('this', 'is', 'a', 'vector', 'too'))
(logical.vector <- c(TRUE, FALSE, FALSE))

(mixed.vector1 <- c(1, TRUE, 5))
# in this case the resulting vector is taken to be numerical (TRUE = 1)

(mixed.vector2 <- c(1, TRUE, 'a'))
# in this case the resulting vector is taken to be string

(mixed.vector3 <- c(1, 2, 1.5))

# F
data.vector <- c("low", "medium", "high")

sample(data.vector, replace = TRUE, size = 15)  #replace - do not remove elements from data.vectory
# chooses 15 values from the data.vecotor with replacement
# the option to replace values must be set to TRUE if we are creating
# a sample larger than our original data vector

sample(data.vector, replace = TRUE, size = 15,
       prob = c(0.1, 0.2, 0.7)) # add probabilities to sampling
sample(data.vector, replace = FALSE, size = 2)

# G
t <- 1:30

t[1]       # first element, R indexes from 1
t[1:3]     # first 3 elements
t[c(1, 7)] # 1st and 7th element
t[seq(from = 1, by = 2, to = length(t))] # elements on odd positions

head(t, 10)# first 10 elements
tail(t, 3) # last 3 elements

# H
t[rep(c(TRUE, FALSE), each = 15)]

1 > 10
2 > 10
c(1, 2) > 10        # R can compare vectors 
c(1, 2) > c(3, 1)   # element by element

t[t > 10]
t[t > 10 & t < 25]
t[t > 10 | t < 25]

for (i in t) {
    if (i > 10 && i < 25)
        print(i)
}

# ----------------------------------- TASK 8 -----------------------------------


f <- factor(1:3)    #creates categories of 1, 2, 3 -> not integers!!!
1 + 2
f[1] + f[2]
# factor understands its values as distinct categories,
# they no longer abide the rules of a string or numeric variable

x <- sample(c('child', 'teenager', 'adult'), replace = TRUE, size = 15)
(f <- factor(x)) # default order of levels is alphabetical

f <- factor(f, levels = c('child', 'teenager', 'adult')) # reordering a factor
levels(f) # can be useful in following seminars


# ----------------------------------- TASK 9 -----------------------------------


# A
matrix(c(1:20), ncol = 4)                # specify number of rows, vector/ncol -> take this number of items from vector and create a collum (so here 5 items)
matrix(c(1:20), ncol = 4, byrow = TRUE)  # default is byrow = FALSE; add items in a row, not in a collumn
matrix(1:5, ncol = 3, nrow = 5)          # specify number of rows and columns

rbind(1:3, c(4, 8, 9), c(11, 21, 23))    # bind rows
cbind(1:3, c(4, 8, 9), c(11, 21, 23))    # bind columns

# adding column or row names
G <- cbind(1:3, c(4, 8, 9) , c(11 , 21, 23))
colnames(G)
colnames(G) <- c('col1', 'col2', 'col3')
rownames(G) <- c('row1', 'row2', 'row3')
G

# B
diag(1:5) # pulling a diagonal out of an existing matrix
(A <- matrix (1:9, nrow = 3))
diag(A)

# C
# matrices don 't have to be numerical , but all elements are taken to be of the same type
(character.matrix <- matrix(c('this', 'is', 'a', 'matrix'), nrow = 2, byrow = T))
(logical.matrix <- matrix(c(T, F, F, F, T, T), nrow = 2))
(mixed.matrix1 <- matrix(c(1, T, 5, 3) , nrow = 2))
(mixed.matrix2 <- matrix(c(1, T, 'a', 5) , nrow = 2))
# in this case the resulting matrix is taken to be numerical (TRUE = 1)

# D
(A <- matrix(1:9, nrow = 3))

A[1] # element in the upper right hand corner
A[4] # 4th element of a vector which is contains values of all the matrix columns

A[1, 2]       # first row, second column
A[1, c(2, 3)] # first row , 2nd and 3th column
A[1, c(2, 4)]
A[seq(1, nrow(A), 2), seq(2, ncol(A), 2)] # odd rows, even columns
#we can use names
colnames(A) <- c('col1', 'col2', 'col3')
rownames(A) <- c('row1', 'row2', 'row3')
A['row2', 'col3']
A['row2', ] #not all dimensions need to be used
A[, 'col3'] #not all dimensions need to be used

# E
dim(A)  # dimensions, rows and columns
nrow(A)
ncol(A)
det(A)  # determinant
t(A)    # transposition


# ---------------------------------- TASK 10 -----------------------------------


# A
a <- 1:10
b <- c("just", "some", "strings")
M <- matrix(1:15 , ncol = 3)
(l <- list(a, b, M))

# B
names(l) <- c("numbers", "words", "matrix")

# C
str(l)

# D
l[[2]] # second element of the list
l$words
l[[2]][1]
l$words[1]


# ---------------------------------- TASK 11 -----------------------------------


# A
#tables
data.frame(A = 1:3,
           B = c("a", "b", "c"))

df <- data.frame(numbers = 1:10,
                 strings = c('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j'),
                 logical = rep(TRUE, 10))

df

# B
df[1, 2]          # pull values by row and column
df[[2]]           # pull the second column
df[, 2]           # pull the second column
df[2, ]           # pull the second row
df[2, 'logical']  # second element of the 'logical' column

#looks stupid tbh
df$numbers        # pull columns by names
df$logical[2]

# C
df$new.column <- 21:30  #add columns; can also use cbin()
df
df[4, ] <- list(11, 'k', TRUE, 31)
df[1, 1] <- 10
df
# we have to use a list to preserve data types


# ---------------------------------- TASK 12 -----------------------------------


getwd()                   # your current working directory
setwd("seminars/01_basics") # change working


# ---------------------------------- TASK 13 -----------------------------------


load("daisy.RData")            # load RData files
daisy <- read.csv("daisy.csv") # load.csv file into a variable
# for other data formats use specific libraries

str(daisy)
summary(daisy)


# ---------------------------------- TASK 14 -----------------------------------


is.na(daisy$flower.size)


# ---------------------------------- TASK 15 -----------------------------------


head(daisy)
tail(daisy)


# ---------------------------------- TASK 16 -----------------------------------


# numerical variables
min(daisy$flower.size)
max(daisy$flower.size)
sum(daisy$flower.size)
var(daisy$flower.size)
sd(daisy$flower.size)

# categorical variables
table(daisy$polen)
prop.table(table(daisy$name))
?prop.table


# ---------------------------------- TASK 17 -----------------------------------


barplot(table(daisy$name))
pie(table(daisy$name))
hist(daisy$flower.size)
# we will see nicer plots in task 23

# ---------------------------------- TASK 18 -----------------------------------


ls()
rm(list = ls ())


# ---------------------------------- TASK 19 -----------------------------------


a <- 1:10
for (i in 1:length(a)) {
  b <- sum(a[1:i])
  print(b)
}
i <- 0
while (i < 3) {
  i <- i + 1
  print(i)
}


# ---------------------------------- TASK 20 -----------------------------------


answer <- 42
if(answer == 42) {
  print('Correct answer')
} else if(abs(answer - 42) <= 2){
  print('Almost ...')
} else {
  print('Wrong answer')
}


# ---------------------------------- TASK 21 -----------------------------------


add <- function(a, b, c = 1){
  # function adds up given values
  # arguments: a ... number
  # b ... number
  # c ... number (default is 1)
  # returns: number (sum of a, b and c)
  return(a + b + c)
}
add(3, 4)
add(3, 4, c = 2)


# ---------------------------------- TASK 22 -----------------------------------


install.packages('circular')
library(circular) # load library into current workspace
# execute code from file ( useful when you want to load external functions )
# source ('my - functions.R')


# ---------------------------------- TASK 23 -----------------------------------


x <- 1:10 # x coordinate vector
y <- 21:30 # y coordinate vector

plot(x, y)
plot(x, y, col = "red")
plot(x, y, col = "red", type = "l")
plot(x, y, col = "red", type = "b")

# ................................... points ...................................

plot(x, y, col = "red", type = "h")
points(x, y)

# nicer points
plot(x, y, col = "grey30", type = "h", xlab = "X label", ylab = "Y label")
points(x, y, pch = 16, cex = 1.25, col = "blue")

# ................................... lines ......-.............................

x <- seq(0, 10, by = 0.1)
y1 <- sin(2 * x) / x
y2 <- sin(2 * x) * x / 10

plot(x, y1, col = "darkgreen", type = "l", lwd = 2, lty = 3,
     xlab = "X label", ylab = "Y label") # lwd changes width, lty type of line
abline(h = 0, col = "grey60")            # add horizontal line in zero value
lines(x, y2, col = "green")              # add green line
legend("topright", legend = c("Line 1", "Line 2"),
       col = c("darkgreen", "green"), lty = c(3, 1))
?legend

# we can change limits of y axes
plot(x, y1, col = "darkgreen", type = "l", lwd = 2, lty = 3,
     xlab = "X label", ylab = "Y label", ylim = c(-1, 1))
abline(h = 0, col = "grey60")
lines(x, y2, col = "green")
legend("bottom", legend = c("Line 1", "Line 2"),
       col = c("darkgreen", "green"), lty = c(3, 1), bty = "n", horiz = TRUE)

# ................................. histograms .................................

hist(y2, col = "orange", density = 30, border = "orange4", probability = TRUE,
     xlab = "Data", ylab = "Relativ frequencies", main = "Histogram")

box()
# density input specifies the density of shading lines

# .................................. barplots ..................................

x <- sample(c("A", "B", "C", "D", "E", "F"), size = 100, replace = TRUE)
x.tab <- table(x)
barplot(x.tab) # quite ugly

barplot(x.tab, col = 1:6, ylim = c(0, max(x.tab) + 1), ylab = "Frequencies")
box() # colors indexed by numbers 1:6

barplot(x.tab, col = heat.colors(6), ylim = c(0, max(x.tab) + 3), ylab = "Frequencies",
        names.arg = c("A name", "B name", "C name", "D name", "E name", "F name"))
box()
text(seq(0.75, 6.75, length = 6), x.tab + 1, labels = x.tab)
# colors are from palette heat.colors
# text has inputs: text(x.coordinates, y.coordinates, labels)
