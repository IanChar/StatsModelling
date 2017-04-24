### Script to generate logistic model of the data.
set.seed(100)
library(rpart)
##############################DATA LOAD IN AND CLEANING############################
# Load data in.
#source("load.R")

# Get useful predictors for this model and convert to doubles.
cleaned <- data[c("bathrooms", "bedrooms", "price", "latitude", "longitude")]
cleaned <- lapply(cleaned, function(lst) {
  as.double(lst)
})

# Append response.
interests <- as.double(lapply(data$interest_level, function(interest) {
  toReturn <- -1
  if (interest == "low") {
    toReturn <- 0
  } else if (interest == "medium") {
    toReturn <- 1
  } else if (interest == "high") {
    toReturn <- 2
  }
  toReturn
}))
cleaned <- append(cleaned, list(interests))
names(cleaned)[length(names(cleaned))] <- "interest_level"

# Trim amount of data
trimData <- function(dataset, size) {
  lapply(dataset, function(elem) {
    elem[1:size]
  })
}
aptmts <- trimData(cleaned, NUM_SAMPLES)

# Make cleaned into a Dataframe
aptmts <- as.data.frame(cleaned)

#Data points with no spatial info
aptmts <- aptmts[which(aptmts$latitude < 40.95 & aptmts$latitude > 40.55 & aptmts$longitude > -74.1 & aptmts$longitude < -73.5),]

numOfClust <- 3
clustering <- clara(aptmts[,-c(1,2,3,6)], numOfClust)$clustering

colors <- sample(color, numOfClust)
colorClust <- colors[clustering]
lon.lat <- cbind(aptmts$longitude, aptmts$latitude, colorClust)
plot(lon.lat, cex = 0.5, pch = 16, col=colorClust)


