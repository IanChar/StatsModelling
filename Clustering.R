### Script to generate logistic model of the data.
set.seed(100)
library(rpart)
library(RColorBrewer)
library(randomForest)
##############################DATA LOAD IN AND CLEANING############################
# Load data in.
source("load.R")

trainInd <- sample(1:dim(apt)[1], dim(apt)[1]*2/3)
train <- apt[trainInd,]
test <- apt[-trainInd,]


fit <- randomForest(as.factor(interest_level) ~ bathrooms + bedrooms + price + latitude + longitude
                                                + num_photos + num_features + desc_length + month
                                                + day + hour,
                    data=aptmts, 
                    importance=TRUE, 
                    ntree=10)

Prediction <- predict(fit, apt[,-12])
tab <- cbind(unname(Prediction), apt[,12])

mean(Prediction == apt[,12])


numOfClust <- 3
clustering <- clara(apt[,-c(1,2,3,6,7,8,9,10,11,12)], numOfClust)$clustering

color = grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)]
colors <- sample(color, numOfClust)
colorClust <- colors[clustering]
lon.lat <- cbind(apt$longitude, apt$latitude, colorClust)
plot(lon.lat, cex = 0.5, pch = 16, col=colorClust)


