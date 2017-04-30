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
