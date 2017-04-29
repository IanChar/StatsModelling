### Script to generate logistic model of the data.
set.seed(100)
library(rpart)
library(cluster)
library(RColorBrewer)
library(randomForest)
library(foreach)
library(doSNOW)
##############################DATA LOAD IN AND CLEANING############################
# Load data in.
#source("load.R")

numOfClust <- 20
numTree <- 100
#clustering <- kmeans(apt[, c(4,5)], numOfClust, nstart = 100)$cluster
clustering <- clara(apt[,c(4,5)], numOfClust, metric = "euclidean")$clustering

color = grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)]
colors <- sample(color, numOfClust)
colorClust <- colors[clustering]
lon.lat <- cbind(apt$longitude, apt$latitude, colorClust)
plot(lon.lat, cex = 0.5, pch = 16, col=colorClust)

newApt <- apt
newApt$clustering <- clustering 

trainInd <- sample(1:dim(apt)[1], dim(apt)[1]*2/3)
train <- apt[trainInd,]
test <- apt[-trainInd,]

fit <- randomForest(as.factor(interest_level) ~ bathrooms + bedrooms + price + latitude + longitude
                    + num_photos + num_features + desc_length + month
                    + day + hour,
                    data=train, 
                    importance=TRUE, 
                    ntree=numTree)


varImpPlot(fit)

classPred <- predict(fit, test[,-12])
Prediction <- predict(fit, test[,-12], type = "prob")
Prediction <- apply(Prediction, c(1,2), function(x) min(max(x, 1E-15), 1-1E-15)) 
mean(classPred == test[,12])

# LogLoss <- function(actual, predicted)
# {
#   result=-1/length(actual)*(sum((actual*log(predicted)+(1-actual)*log(1-predicted))))
#   return(result)
# }
# 
# LogLoss(test[,12], Prediction[,3])

trainInd <- sample(1:dim(newApt)[1], dim(newApt)[1]*2/3)
train <- newApt[trainInd,]
test <- newApt[-trainInd,]

fit <- randomForest(as.factor(interest_level) ~ bathrooms + bedrooms + price + latitude + longitude
                    + num_photos + num_features + desc_length + month
                    + day + hour + clustering,
                    data=train, 
                    importance=TRUE, 
                    ntree=numTree)


varImpPlot(fit)

classPred <- predict(fit, test[,-12])
Prediction <- predict(fit, test[,-12], type = "prob")
Prediction <- apply(Prediction, c(1,2), function(x) min(max(x, 1E-15), 1-1E-15)) 
mean(classPred == test[,12])