getwd()
#setwd("seminars/01")

# ==============================================================================
# --------------------------------- SEMINAR 1 ----------------------------------
# ==============================================================================


comp <- read.csv("Computers.csv", sep = ";", fileEncoding = "UTF-8-BOM")

str(comp) #structure
head(comp, 10)

# ..................................... A ......................................

# investigate non numeric values of price column

# useful functions:
#?is.na
#?as.numeric

#as.numeric(comp$price)  #interpret as numbers
is.numeric(comp$price)  #is numeric?


#head(comp$price)
#is.character(comp$price)
# convert the price column into the numeric data type:
#comp$price = as.numeric(comp$price)

head(as.numeric((comp$price)))
comp$price[is.na(as.numeric(comp$price))]

# look at the values which were converted into NA:
#i = 0
#for (x in comp$price) {
#    i = i + 1
#    if (is.na(x)) {
#        cat("ID: ", i, ": ", x, "\n")
#    }
#}
#print(comp[is.na(comp$price),])


# replace "," with ""
comp$price <- gsub(",", "", comp$price)
comp$price <- as.numeric(comp$price)

is.numeric(comp$price)

max(comp$price)

min(comp$price)
#sort(comp$price)
head(sort(comp$price), 10) # first 8 values should be 1000x higher
# correct them:
comp$price[comp$price < 10] = comp$price[comp$price < 10] * 1000


# ..................................... B ......................................

table(comp$cd, useNA = "always")    #item occurancce table
# transform into the same values of "yes" and "no":
comp$cd[comp$cd == "N"]   = NA
comp$cd[comp$cd == "na"]  = NA
comp$cd[comp$cd == ""]    = NA
comp$cd[comp$cd == "Y"]   = "yes"
comp$cd[comp$cd == "Yes"] = "yes"


table(comp$cd, useNA = "always")

table(comp$multi, useNA = "always")
# transform into the same values of "yes" and "no":
comp$multi = tolower(comp$multi)
comp$multi[comp$multi == "n"]  = NA
comp$multi[comp$multi == "na"] = NA
comp$multi[comp$multi == ""]   = NA


table(comp$multi, useNA = "always")

# do the same for premium variable:
table(comp$premium, useNA = "always")
comp$premium = tolower(comp$premium)
comp$premium[comp$premium == "y"]  = "yes"
comp$premium[comp$premium == "n"]  = NA
comp$premium[comp$premium == "na"] = NA
comp$premium[comp$premium == ""]   = NA

table(comp$premium, useNA = "always")

# ..................................... B ......................................
# .......................... version 2 (using levels) ..........................

#table(comp$premium)
#comp$premium <- as.factor(comp$premium)
#levels(comp$premium)
#lev <- c(NA, NA, NA, "no", "yes", "yes", "yes", "yes")
#comp$premium <- factor(comp$premium, labels = lev)
#
#table(comp$premium)
#comp$premium <- factor(comp$premium, exclude = NA)
#table(comp$premium, useNA = "always")

# ..................................... C ......................................
comp$cd = comp$cd == "yes"
comp$multi = comp$multi == "yes"
comp$premium = comp$premium == "yes"

table(comp$cd, useNA = "always")
table(comp$multi, useNA = "always")
table(comp$premium, useNA = "always")

# ..................................... D ......................................
?as.factor
table(comp$ram, useNA = "always")
comp$ram <- as.factor(comp$ram)
table(comp$ram, useNA = "always")
str(comp)

# ..................................... E ......................................

table(comp$premium, comp$ram)
table(comp$premium, comp$ram, useNA = "always")
table(comp$premium, comp$ram, useNA = "ifany")

comp$type = comp$price >= 3000
str(comp)
table(comp$type)

table(comp$premium, comp$type)

sum(comp$type[comp$premium == TRUE], na.rm=TRUE)
# ..................................... F ......................................

# try it yourself!

