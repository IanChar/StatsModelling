### Script to generate logistic model of the data.
set.seed(100)
library(rpart)
library(cluster)
library(RColorBrewer)
library(randomForest)
##############################DATA LOAD IN AND CLEANING############################
# Load data in.
#source("load.R")

trainInd <- sample(1:dim(apt)[1], dim(apt)[1]*2/3)
train <- apt[trainInd,]
test <- apt[-trainInd,]


fit <- randomForest(as.factor(interest_level) ~ bathrooms + bedrooms + price + latitude + longitude
                                                + num_photos + num_features + desc_length + month
                                                + day + hour,
                    data=train, 
                    importance=TRUE, 
                    ntree=5)
varImpPlot(fit)

Prediction <- predict(fit, test[,-12], type = "prob")
Prediction <- apply(Prediction, c(1,2), function(x) min(max(x, 1E-15), 1-1E-15)) 

LogLoss <- function(actual, predicted)
{
  result=-1/length(actual)*(sum((actual*log(predicted)+(1-actual)*log(1-predicted))))
  return(result)
}

LogLoss(test[,12], Prediction[,3])


numOfClust <- 3
clustering <- clara(apt[,-c(1,2,3,6,7,8,9,10,11,12)], numOfClust, metric = "manhattan")$clustering

color = grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)]
colors <- sample(color, numOfClust)
colorClust <- colors[clustering]
lon.lat <- cbind(apt$longitude, apt$latitude, colorClust)
plot(lon.lat, cex = 0.5, pch = 16, col=colorClust)


