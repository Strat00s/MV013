#getwd()
setwd("seminars/02")

data = read.csv("Computers.csv")
summary(data)

mean(data$price)
median(data$price)

sort(data$price)

trimmedMean = function(data, trim) {
    data = sort(data)
    trim_size = length(data) * trim
    return (mean(data[trim_size:length(data) - trim_size]))
}