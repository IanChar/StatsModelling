library(ggplot2)
library(reshape2)

cleanTest <- function(apt, tooFew) {
  if (missing(tooFew)) {
    apt[which(apt$bathrooms > 4),]$bathrooms = 5
    apt[which(apt$bedrooms > 4),]$bedrooms = 5
  }
  apt$bathrooms <- ceil(apt$bathrooms)
  apt$bathrooms <- as.ordered(apt$bathrooms)
  apt$bedrooms <- as.ordered(apt$bedrooms)
  apt[which(apt$hour < 6),]$hour = 0
  apt[which(apt$hour < 12 & apt$hour >= 6),]$hour = 1
  apt[which(apt$hour < 18 & apt$hour >= 12),]$hour = 2
  apt[which(apt$hour >= 18),]$hour = 3
  apt$hour <- as.ordered(apt$hour)
  # Price center: 3829.939  Price scale: 22080.4
  apt$price <- as.numeric(scale(apt$price, center=3829.939, scale=22080.4))
  # Features center: 5.428948  Price scale: 3.922477
  apt$num_features <- as.numeric(scale(apt$num_features, center=5.428948, scale=3.922477))
  # Photos center: 5.603798  Price scale: 3.628136
  apt$num_photos <- as.numeric(scale(apt$num_photos, center=5.603798, scale=3.628136))
  # Desc center: 602.0369  Price scale: 393.2876
  apt$desc_length <- as.numeric(scale(apt$desc_length, center=602.0369, scale=393.2876))
  return(apt)
}

test.apt <- cleanTest(test.apt)
predictions <- predict(train.polr, test.apt, type="probs")
classes <- predict(train.polr, test.apt, type="class")

predictedDat <- cbind(test.apt, predictions)
head(predictedDat)
lpredictedDat <- melt(predictedDat, id.vars = c("bedrooms", "price", "hour", "num_features", "num_photos", "desc_length", "month", "day", "latitude", "longitude", "bathrooms"),
                      variable.name = "Level", value.name = "Probability")
head(lpredictedDat)

# Make some cool plots
samplingInd <- sample(1:dim(lpredictedDat)[1], dim(lpredictedDat)[1]*1/10)
sampling <- lpredictedDat[samplingInd,]
par(mfrow=c(2, 2))
coolPlot <- function(data) {
  ggplot(data, aes(x = price, y = Probability, colour = Level)) + xlim(-0.15, 0.15) + geom_line() +
    facet_grid(bedrooms ~ .)
}
coolPlot(sampling)
