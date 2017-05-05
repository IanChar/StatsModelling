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

trainInd <- sample(1:dim(apt)[1], dim(apt)[1]*2/3)
train <- apt[trainInd,]
test <- apt[-trainInd,]
numTree <- 100

library(doMC)
registerDoMC()

fit <- randomForest(as.factor(interest_level) ~ bathrooms + bedrooms + price + latitude + longitude
                    + num_photos + num_features + desc_length + month
                    + day + hour,
                    data=train, 
                    importance=TRUE, 
                    ntree=numTree)

varImpPlot(fit)

fit <- foreach(ntree=rep(200, 8), .combine=combine, .multicombine=TRUE,
                           .packages='randomForest') %dopar% {
                             randomForest(as.factor(interest_level) ~ bathrooms + bedrooms + price + latitude + longitude
                                          + num_photos + num_features + desc_length + month
                                          + day + hour,
                                          data=train,
                                          ntree=200)
                           }

q <- predict(fit, test[,-12])
table(q, test[,12])

Prediction <- predict(fit, test[-12], type = "prob")
Prediction <- apply(Prediction, c(1,2), function(x) min(max(x, 1E-15), 1-1E-15))

write.csv(cbind(test.apt$listing_id, Prediction[,c(3,2,1)]), 
          file = "predictions.csv", row.names = FALSE, quote = FALSE)

######TUNED MODEL######

numOfClust <- 10
clustering <- clara(apt[,-c(1,2,3,6,7,8,9,10,11,12,13)], numOfClust, metric = "manhattan")$clustering
color = grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)]
colors <- c("blue", "green", "red")
colorClust <- colors[clustering]
plot(apt[, c(5,4)], cex = 0.3, pch = 16, col=clustering)

varImpPlot(fit)


newApt <- apt
newApt$clustering <- clustering
trainInd <- sample(1:dim(newApt)[1], dim(newApt)[1]*2/3)
newApt.train <- newApt[trainInd,]
newApt.test <- newApt[-trainInd,]

# fit <- randomForest(as.factor(interest_level) ~ bathrooms + bedrooms + price + latitude + longitude
#                     + num_photos + num_features + desc_length + month
#                     + day + hour + clustering,
#                     data=newApt, 
#                     importance=TRUE, 
#                     ntree=25)
# varImpPlot(fit)

tuned.rf <- foreach(ntree=rep(200, 8), .combine=combine, .multicombine=TRUE,
        .packages='randomForest') %dopar% {
          randomForest(as.factor(interest_level) ~ bathrooms + bedrooms + price + latitude + longitude
                                           + num_photos + num_features + desc_length + month
                                           + day + hour + clustering,
                                           data=newApt.train,
                                           ntree=200)
        }

tuned.clustering <- clara(test.apt[,-c(1,2,3,6,7,8,9,10,11,12,13)], numOfClust, metric = "manhattan")$clustering
test.apt$clustering <- tuned.clustering

Prediction <- predict(tuned.rf, test.apt, type = "prob")
Prediction <- apply(Prediction, c(1,2), function(x) min(max(x, 1E-15), 1-1E-15)) 
write.csv(cbind(test.apt$listing_id, Prediction[,c(3,2,1)]), 
          file = "predictions.csv", row.names = FALSE, quote = FALSE)

q <- predict(tuned.rf, newApt.test[,-12])
mean(q == newApt.test[,12])
table(q, newApt.test[,12])

##### Increase the number of clusters and run again

numOfClust <- 50
clustering <- clara(apt[,-c(1,2,3,6,7,8,9,10,11,12,13)], numOfClust, metric = "manhattan")$clustering
color = grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)]
colors <- c("blue", "green", "red")
colorClust <- colors[clustering]
plot(apt[, c(5,4)], cex = 0.3, pch = 16, col=clustering)

varImpPlot(fit)


newApt <- apt
newApt$clustering <- clustering
trainInd <- sample(1:dim(newApt)[1], dim(newApt)[1]*2/3)
newApt.train <- newApt[trainInd,]
newApt.test <- newApt[-trainInd,]

tuned.rf <- foreach(ntree=rep(200, 8), .combine=combine, .multicombine=TRUE,
                    .packages='randomForest') %dopar% {
                      randomForest(as.factor(interest_level) ~ bathrooms + bedrooms + price + latitude + longitude
                                   + num_photos + num_features + desc_length + month
                                   + day + hour + clustering,
                                   data=newApt.train,
                                   ntree=200)
                    }

tuned.clustering <- clara(test.apt[,-c(1,2,3,6,7,8,9,10,11,12,13)], numOfClust, metric = "manhattan")$clustering
test.apt$clustering <- tuned.clustering

Prediction <- predict(tuned.rf, test.apt, type = "prob")
Prediction <- apply(Prediction, c(1,2), function(x) min(max(x, 1E-15), 1-1E-15)) 

write.csv(cbind(test.apt$listing_id, Prediction[,c(3,2,1)]), 
          file = "predictions.csv", row.names = FALSE, quote = FALSE)

q <- predict(tuned.rf, newApt.test[,-12])
mean(q == newApt.test[,12])
table(q, newApt.test[,12])

#Even more tuned model

#We find that month and day are unnecessary and cause overfitting

newApt <- apt
newApt$clustering <- clustering
trainInd <- sample(1:dim(newApt)[1], dim(newApt)[1]*2/3)
newApt.train <- newApt[trainInd,]
newApt.test <- newApt[-trainInd,]

tuned.rf <- foreach(ntree=rep(500, 8), .combine=combine, .multicombine=TRUE,
                    .packages='randomForest') %dopar% {
                      randomForest(as.factor(interest_level) ~ bathrooms + bedrooms + price + latitude + longitude
                                   + num_photos + num_features + desc_length + hour + clustering + bedrooms:price
                                   + price:bathrooms,
                                   data=newApt,
                                   ntree=500)
                    }


Prediction <- predict(tuned.rf, test.apt, type = "prob")
Prediction <- apply(Prediction, c(1,2), function(x) min(max(x, 1E-15), 1-1E-15)) 
write.csv(cbind(test.apt$listing_id, Prediction[,c(3,2,1)]), 
          file = "predictions.csv", row.names = FALSE, quote = FALSE)



set.seed(10)
newApt <- apt
trainInd <- sample(1:dim(newApt)[1], dim(newApt)[1]*2/3)
newApt.train <- newApt[trainInd,]
newApt.test <- newApt[-trainInd,]

newApt.test$clustering <- clara(newApt.test[,-c(9,10,12)], 50, metric = "euclidean")$clustering
newApt.train$clustering <- clara(newApt.train[,-c(9,10,12)], 50, metric = "euclidean")$clustering
tuned.rf <- foreach(ntree=rep(200, 8), .combine=combine, .multicombine=TRUE,
                    .packages='randomForest') %dopar% {
                      randomForest(as.factor(interest_level) ~ bathrooms + bedrooms + price + latitude + longitude
                                   + num_photos + num_features + desc_length + hour + clustering,
                                   data=newApt.train,
                                   ntree=200)
                    }

class.prediction <- predict(tuned.rf, newApt.test)
mean(class.prediction == newApt.test[,12])
table(class.prediction, newApt.test[,12])

Prediction <- predict(tuned.rf, test.apt, type = "prob")
Prediction <- apply(Prediction, c(1,2), function(x) min(max(x, 1E-15), 1-1E-15)) 
write.csv(cbind(test.apt$listing_id, Prediction[,c(3,2,1)]), 
          file = "predictions.csv", row.names = FALSE, quote = FALSE)




#### Run many many trees
newApt <- apt
trainInd <- sample(1:dim(newApt)[1], dim(newApt)[1]*2/3)
newApt.train <- newApt[trainInd,]
newApt.test <- newApt[-trainInd,]

newApt.test$clustering <- clara(newApt.test[,-c(9,10,12)], 50, metric = "euclidean")$clustering
newApt.train$clustering <- clara(newApt.train[,-c(9,10,12)], 50, metric = "euclidean")$clustering
tuned.rf <- foreach(ntree=rep(1000, 8), .combine=combine, .multicombine=TRUE,
                    .packages='randomForest') %dopar% {
                      randomForest(as.factor(interest_level) ~ bathrooms + bedrooms + price + latitude + longitude
                                   + num_photos + num_features + desc_length + hour + clustering,
                                   data=newApt.train,
                                   ntree=1000)
                    }

class.prediction <- predict(tuned.rf, newApt.test)
mean(class.prediction == newApt.test[,12])
table(class.prediction, newApt.test[,12])

Prediction <- predict(tuned.rf, test.apt, type = "prob")
Prediction <- apply(Prediction, c(1,2), function(x) min(max(x, 1E-15), 1-1E-15)) 
write.csv(cbind(test.apt$listing_id, Prediction[,c(3,2,1)]), 
          file = "predictions.csv", row.names = FALSE, quote = FALSE)
#Get data frame from each cluster and write it to a JSON file
# for(i in 1:numOfClust) {
#   clust <- newApt[which(newApt$clustering == i),]
#   exportJson <- toJSON(clust)
#   write(toJSON(clust), file = paste0(paste0("data/clust_", i), ".json"))
# }

