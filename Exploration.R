source("load.R")
library(ggplot2)

# Make box plot with price
plot(price ~ interest_level, data=aptmts, main="Price of Apartment by Interest Level",
     ylab="Price (US Dollars per Month)", ylim=c(400, 10000))

# Make box plots with price by number of bedrooms
par(mfrow=c(2, 2))
plotPriceInterest <- function(dataFrame) {
  plot(price ~ interest_level, data=dataFrame, ylab="Price ($/month)", ylim=c(400, 10000))
}
by(aptmts, aptmts$bedrooms, plotPriceInterest)

# How many are interested in each type of bedroom
by(apt$interest_level, apt$bedrooms, summary)

# How does hour, day, month affect interest

createPlotter <- function(time) {
  plotHist <- function(dat) {
    hist(dat, main = paste("Histogram of ", time, sep=""), xlab=time)
  }  
  return(plotHist)
}

par(mfrow=c(3, 3))
by(apt$hour, apt$interest_level, createPlotter("Hour"))
# par(mfrow=c(1, 3))
by(apt$day, apt$interest_level, createPlotter("Day"))
# par(mfrow=c(1, 3))
by(apt$month, apt$interest_level, createPlotter("Month"))
