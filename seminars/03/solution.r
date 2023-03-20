getwd()
setwd("seminars/03")


# ==============================================================================
# --------------------------------- SEMINAR 3 ----------------------------------
# ==============================================================================


install.packages("corrplot", "ggfortify", "dplyr")
library(corrplot)
library(ggfortify)
library(dplyr)


# ----------------------------------- TASK 1 -----------------------------------


data <- read.csv("market.csv")
str(data)
head(data)

unique(data$NACE_R2_LABEL) # specifies different market segments
unique(data$GEO) # specifies the country

# we are interested just in the different states of EU, not in the summaries,
# ignore the rows with the EU summaries and rename the Germany value:

data <- data[data$GEO != "European Union - 28 countries (2013-2020)" &
                 data$GEO != "Euro area - 19 countries  (from 2015)", ]
data$GEO[data$GEO == "Germany (until 1990 former territory of the FRG)"] <- "Germany"

# pick up only the variables which we are interested in:

data <- data[, c("NACE_R2_LABEL", "GEO", "Value")]


# ..................................... A ......................................


# Transform data from long format to short format. Use column GEO (indicates
# state) as key variable for each row. Use values in column NACE R2 LABEL
# (indicates market segment) to identify new columns. Use values from column
# Value (indicates percent of people working in the given segment).


?reshape
data.short <- reshape(data, idvar = "GEO", timevar = "NACE_R2_LABEL", direction = "wide")
# idvar = unique identificator of the rows
# timevar = idenrificator of columns
# direction = wide = format
# in original table each row represent one distinct combinations of
# market segments and countries, now each element represents the same

# rename the columns:
colnames(data.short)[2:12] <-
    c("Agriculture, forest", "Industry", "Manufacturing", "Construction",
      "Wholesale and retail", "Information", "Financial", "Real estate",
      "Science, Technical", "Public administration", "Arts, entertainment")
head(data.short)

# rename the rows:
rownames(data.short) <- data.short$GEO
head(data.short)

# now we can ignore the first column GEO (we have the same in rownames)
data.short <- data.short[2:12]
head(data.short)


# ..................................... B ......................................


# create a correlation matrix M using build-in R function

# HINT: use cor() and corrplot() functions

(M <- cor(data.short))
# correlation measures a linear dependence of two variables
# zero correlation does not imply independence of variables

corrplot(M, method = "circle")
corrplot.mixed(M, lower.col = "black", diag = "n", tl.pos = "lt")
corrplot(M, order = "hclust", addrect = 4)


# ..................................... C ......................................


# create a single scatter plot for Industry and Manufacturing,
# create a scatter plot matrix for all segments

x <- data.short$Industry # x axis
y <- data.short$Manufacturing # y axis

plot(x, y, col = "red", pch = 19,
     xlab = "Industry", ylab = "Manufacturing", main = "Industry vs. Manufacturing")
cor(x, y) # strong positive linear dependance
pairs(data.short, lower.panel = NULL, pch = 19, col = "red")


# ..................................... E ......................................


# Use PCA to analyze the data. Use build-in R function prcomp and
# autoplot function from package ggfortify.

# https://www.rdocumentation.org/packages/mixOmics/versions/6.3.2/topics/pca

data.pca <- prcomp(data.short)

str(data.pca)
# rotation = eigenvectors
# x = principal components
# sdev = eigenvalues

summary(data.pca)
# standard deviation = sqrt(eigenvalues)
# proportion of variance = variance explained by each principal component
# cumulative proportion = cumulative sum of eigenvalues = how much of variance
# of the original data can be explained using first ... principal components

# for example: the first 4 principal component explain 95.10% of total variance

autoplot(data.pca, data = data.short, label = T, shape = F,
         loadings = TRUE, loadings.label = TRUE)
# in the states of eastern europe there are more important the agriculture,
# manufacturing and industry
# in the western europe, there is more important science and technical
# these market segments are dividing our observations (countries) in the
# eastern and western part

head(data.pca$rotation)

# eigenvectors specify the linear combination of all variables for computing
# each principal component,
# the coordinates of the first eigenvector specify the importances of all variables
# in the first principal component etc.


# ..................................... D ......................................


# Use PCA to analyze the data. Do not use build-in R function prcomp.

# find covariance matrix of the data:
cov_matrix <- cov(data.short)

# the total variance s is the sum of the variances of all variables,
# it is also equal to the sum of the eigenvalues of cov_matrix:
(s <- sum(diag(cov_matrix)))

# find eigenvalues and eigenvectors of cov_matrix:
s.eigen <- eigen(cov_matrix)

plot(s.eigen$values, type = 'o', col = "red", pch = 19,
     xlab = 'Eigenvalue Number', ylab = 'Eigenvalue Size', main = 'Scree Graph')
# eigenvalue i = how much of the total variance can be explained by ith principal component

# the first two principal components explain 78.6% of the total variance:
cumsum(s.eigen$values / s)
plot(cumsum(s.eigen$values / s), type = 'o', col = "red", pch = 19,
     xlab = 'Component', ylab = 'acconted varriance', main = 'Total variance')

# the first twoo eigenvectors:
c1 <- s.eigen$vectors[, 1]
c2 <- s.eigen$vectors[, 2]

# prinipal component = original data transformed by the multiplication
# with eig_vector = data in new coordinates (of eig_vectors)

# the first principal component:
pc1 <- as.matrix(data.short) %*% c1
# the second:
pc2 <- as.matrix(data.short) %*% c2

plot(pc1, pc2, col = "red", pch = 19, xlim = c(-26, 16),
     ylim = c(-23, 7), xlab = "First component", ylab = "Second component")
text(pc1, pc2 + 1.5,
     labels = rownames(data.short), cex = 0.7, font = 2)
# the dependences between states and market segments
# can be seen very easily using just two first principal components

# Following code is for volunteers: what does it compute? How?

pc1 <- (pc1 - mean(pc1)) / (max(pc1) - min(pc1))
pc2 <- (pc2 - mean(pc2)) / (max(pc2) - min(pc2))

xold1 <- c(1, rep(0, 10)) %*% c1
yold1 <- c(1, rep(0, 10)) %*% c2

xold2 <- c(0, 1, rep(0, 9)) %*% c1
yold2 <- c(0, 1, rep(0, 9)) %*% c2

xold3 <- c(0, 0, 1, rep(0, 8)) %*% c1
yold3 <- c(0, 0, 1, rep(0, 8)) %*% c2

plot(pc1, pc2, col = "red", pch = 19, xlim = c(-0.7, 0.6), ylim = c(-0.6, 0.75),
     xlab = "First component", ylab = "Second component")
text(pc1, pc2 + 0.05, labels = rownames(data.short), cex = 0.7, font = 2)
arrows(x0 = c(0, 0, 0), x1 = c(xold1, xold2, xold3), y0 = c(0, 0, 0),
       y1 = c(yold1, yold2, yold3), lwd = 1.5, col = "blue", length = 0.1)
text(c(xold1, xold2, xold3), c(yold1, yold2, yold3) + 0.05,
     labels = colnames(data.short)[1:3], cex = 0.7, font = 2, col = "blue")


# ----------------------------------- TASK 2 -----------------------------------


library(jpeg)

# load the image:
img <- readJPEG("images/008.jpg", native = FALSE)

par(mar = c(0, 0, 0, 0)) # specify zero margins around the plot
plot(1:2, type = "n", xlab = "", ylab = "", xaxt = 'n', yaxt = 'n')
rasterImage(img, 1, 1, 2, 2)
# columns (186 in total) -> variables
# rows (258 in total) -> observations

# principal component analysis
data.pca <- prcomp(img)

# original data = PCs * "inverse" of eigenvector matrix
# (the same as the transpose now):
data.reconst <- data.pca$x %*% t(data.pca$rotation)

plot(1:2, type = 'n', xlab = "", ylab = "", xaxt = 'n', yaxt = 'n')
rasterImage(data.reconst, 1, 1, 2, 2)

# what is wrong? check the center and scale inputs of prcomp()
?prcomp
# the data were centered

# there are two ways how to correct that:

# 1) use default: center = T, scale = F and then add the substracted means
data.pca <- prcomp(img, center = T, scale = F)

d <- data.pca$x %*% t(data.pca$rotation) +
    matrix(rep(data.pca$center, each = 258), ncol = 186) 
# we just added the means of each variable to each column

plot(1:2, type = 'n', xlab = "", ylab = "", xaxt = 'n', yaxt = 'n')
rasterImage(d, 1, 1, 2, 2)

# 2) use: center = F, scale = F
data.pca <- prcomp(img, center = F, scale = F)
d <- data.pca$x %*% t(data.pca$rotation)

plot(1:2, type = 'n', xlab = "", ylab = "", xaxt = 'n', yaxt = 'n')
rasterImage(d, 1, 1, 2, 2)


# if the data was also scaled (center = T, scale = T):
data.pca <- prcomp(img, center = T, scale = T)

d <- (data.pca$x %*% t(data.pca$rotation)) *
    matrix(rep(data.pca$scale, each = 258), ncol = 186) +
    matrix(rep(data.pca$center, each = 258), ncol = 186)
# we added the means of each variable to each column and scale it back

plot(1:2, type = 'n', xlab = "", ylab = "", xaxt = 'n', yaxt = 'n')
rasterImage(d, 1, 1, 2, 2)

# we will work with the default: center = T, scale = F

data.pca <- prcomp(img)
# by default variables are centered to zero at first and then the PCA is done
# we will need to shift them back for reconstracting the image
s <- summary(data.pca)
par(mar = c(5.1, 4.1, 4.1, 2.1))
plot(s$importance[3, ], main = "Cumulative Proportion of variance", ylab = "proportion",
     type = 'o', col = "red", pch = 19)
# cumulative proportions are the cumulative importances of all principal components
# up to the actual index

# importances = eigenvalues
# rotation = eigenvectors = new coordinates
# x = principal components = new variables

# plot the eigenvectors (just for volunteers):
par(mfrow = c(5, 5), mar = c(1, 1, 1, 1))
for (i in 1:25){
    d <- matrix(rep(data.pca$rotation[, i], each = 200), ncol = 300, byrow = F) # = T
    d <- (d - min(d)) / (max(d) - min(d)) # scale between 0 and 1
    plot(1:2, type = 'n', xlab = "", ylab = "", xaxt = 'n', yaxt = 'n', main = i)
    rasterImage(d, 1, 1, 2, 2)
}
par(mfrow = c(1, 1), mar = c(5.1, 4.1, 4.1, 2.1))

# choose the number of PCs for explaining 90% of variance at least:
(num_components <- sum(s$importance[3, ] < 0.9) + 1)

# the reconstructed image using just few (the most important) components:
d <- data.pca$x[, 1:num_components] %*% t(data.pca$rotation[, 1:num_components])
#  original data x = PCs * (inversion of the eigen vectors matrix)

d <- d + rep(data.pca$center, each = 258) # TRY ALSO
d <- (d - min(d)) / (max(d) - min(d)) # rescale the final image

par(mar = c(0, 0, 0, 0))
plot(1:2, type = 'n', xlab = "", ylab = "", xaxt = 'n', yaxt = 'n')
rasterImage(d, 1, 1, 2, 2)

# ORIGINAL IMAGE (using all principal components):
d <- data.pca$x %*% t(data.pca$rotation) + rep(data.pca$center, each = 258)
d <- (d - min(d)) / (max(d) - min(d))
plot(1:2, type = 'n', xlab = "", ylab = "", xaxt = 'n', yaxt = 'n')
rasterImage(d, 1, 1, 2, 2)


# ----------------------------------- TASK 3 -----------------------------------


data <- c()
for (i in 1:9){
    img <- readJPEG(paste("0", "0", i, ".jpg", sep = ""), native = FALSE)
    data <- rbind(data, c(img)) # we cut the image data by columns
}
# in each iteration we loaded one image in the matrix representation and using
# c(img) we reshaped it into the vector BY COLUMNS, then we added this vector
# to the final data matrix as a new row.

# columns = pixels of each figure -> variables
# rows = figures -> observations

par(mfrow = c(1, 1), mar = c(5.1, 4.1, 4.1, 2.1))
data.pca <- prcomp(data, scale = F) # try scale=T

# plot the cumulative importances of the first few principal components
s <- summary(data.pca)
plot(s$importance[3, ], col = "red", pch = 19,
     main = "Cumulative Proportion of variance", ylab = "proportion", type = 'o')

# eigenvectors visualization (for volunteers):
par(mfrow = c(3, 3), mar = c(1, 1, 1, 1))
for (i in 1:9){
    d <- data.pca$rotation[, i]
    d <- (d - min(d)) / (max(d) - min(d))
    m <- matrix(d, nrow = dim(img)[1], byrow = FALSE) # returns new coordinates = eigenvectors
    plot(1:2, type = 'n', xlab = "", ylab = "", xaxt = 'n', yaxt = 'n')
    rasterImage(m, 1, 1, 2, 2)
}

# find the number of principal components needed for
# explaining at least 90% of variablity:
(total <- sum(s$importance[3, ] < 0.9) + 1)

# reconstruct the original data matrix using just first "total" principal components:
dd <- data.pca$x[, 1:total] %*% t(data.pca$rotation[, 1:total]) +
    rep(data.pca$center, each = 9)
# we added the means of each variable to each column again
# (because of default centering of the prcomp built-in function)

# reconstruct images using the first few principal components:
par(mfrow = c(3, 3), mar = c(1, 1, 1, 1))
for (i in 1:9){
    d <- dd[i, ]
    d <- (d - min(d)) / (max(d) - min(d))
    m <- matrix(d, nrow = dim(img)[1], byrow = FALSE) 
    plot(1:2, type = 'n', xlab = "", ylab = "", xaxt = 'n', yaxt = 'n', main = i)
    rasterImage(m, 1, 1, 2, 2)
}
par(mfrow = c(1, 1), mar = c(5.1, 4.1, 4.1, 2.1))
