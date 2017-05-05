######################################################
#-----------Code to load all of the data--------------
######################################################


rm(list=ls())
TRAIN_PATH <- "data/train.json"
TEST_PATH <- "data/test.json"
packages <- c("jsonlite", "dplyr", "purrr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)

dataT <- fromJSON(TRAIN_PATH)
vars <- setdiff(names(dataT), c("photos", "features"))
dataT <- map_at(dataT, vars, unlist) %>% tibble::as_tibble(.)

# Get useful predictors for this model and convert to doubles.
cleaned <- dataT[c("bathrooms", "bedrooms", "price", "latitude", "longitude", "listing_id")]
cleaned <- lapply(cleaned, function(lst) {
  as.double(lst)
})

# Append response.
interests <- as.double(lapply(dataT$interest_level, function(interest) {
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
num.photos <- sapply(dataT$photos, FUN = function(d) {length(d)})
num.features <- sapply(dataT$features, FUN = function(d) {length(d)})
desc.length <- sapply(dataT$description, FUN = function(d) {nchar(d)})
mon <- sapply(dataT$created, FUN = function(d) {as.numeric(substr(d, 6 ,7))})
day <- sapply(dataT$created, FUN = function(d) {as.numeric(substr(d, 9 ,10))})
hour <- sapply(dataT$created, FUN = function(d) {as.numeric(substr(d, 12 ,13))})

cleaned <- append(cleaned, list(unname(num.photos)))
cleaned <- append(cleaned, list(unname(num.features)))
cleaned <- append(cleaned, list(unname(desc.length)))
cleaned <- append(cleaned, list(unname(mon)))
cleaned <- append(cleaned, list(unname(day)))
cleaned <- append(cleaned, list(unname(hour)))
cleaned <- append(cleaned, list(interests))

names(cleaned)[length(names(cleaned))-6] <- "num_photos"
names(cleaned)[length(names(cleaned))-5] <- "num_features"
names(cleaned)[length(names(cleaned))-4] <- "desc_length"
names(cleaned)[length(names(cleaned))-3] <- "month"
names(cleaned)[length(names(cleaned))-2] <- "day"
names(cleaned)[length(names(cleaned))-1] <- "hour"
names(cleaned)[length(names(cleaned))] <- "interest_level"

apt <- as.data.frame(cleaned)
#Data points with no spatial info
apt <- apt[which(apt$latitude < 40.95 & apt$latitude > 40.55 & apt$longitude > -74.1 & apt$longitude < -73.5),]
# Remove outliers in price
apt <- apt[which(apt$price > 100 & apt$price < 1000000),]
apt$bathrooms <- as.ordered(apt$bathrooms)
apt$bedrooms <- as.ordered(apt$bedrooms)
apt$interest_level <- as.ordered(apt$interest_level)




data <- fromJSON(TEST_PATH)
vars <- setdiff(names(data), c("photos", "features"))
data <- map_at(data, vars, unlist) %>% tibble::as_tibble(.)

# Get useful predictors for this model and convert to doubles.
cleaned <- data[c("bathrooms", "bedrooms", "price", "latitude", "longitude", "listing_id")]
cleaned <- lapply(cleaned, function(lst) {
  as.double(lst)
})

num.photos <- sapply(data$photos, FUN = function(d) {length(d)})
num.features <- sapply(data$features, FUN = function(d) {length(d)})
desc.length <- sapply(data$description, FUN = function(d) {nchar(d)})
mon <- sapply(data$created, FUN = function(d) {as.numeric(substr(d, 6 ,7))})
day <- sapply(data$created, FUN = function(d) {as.numeric(substr(d, 9 ,10))})
hour <- sapply(data$created, FUN = function(d) {as.numeric(substr(d, 12 ,13))})

cleaned <- append(cleaned, list(unname(num.photos)))
cleaned <- append(cleaned, list(unname(num.features)))
cleaned <- append(cleaned, list(unname(desc.length)))
cleaned <- append(cleaned, list(unname(mon)))
cleaned <- append(cleaned, list(unname(day)))
cleaned <- append(cleaned, list(unname(hour)))

names(cleaned)[length(names(cleaned))-5] <- "num_photos"
names(cleaned)[length(names(cleaned))-4] <- "num_features"
names(cleaned)[length(names(cleaned))-3] <- "desc_length"
names(cleaned)[length(names(cleaned))-2] <- "month"
names(cleaned)[length(names(cleaned))-1] <- "day"
names(cleaned)[length(names(cleaned))] <- "hour"

test.apt <- as.data.frame(cleaned)
#Data points with no spatial info
#test.apt <- test.apt[which(test.apt$latitude < 40.95 & test.apt$latitude > 40.55 & test.apt$longitude > -74.1 & test.apt$longitude < -73.5),]

rm(hour)
rm(interests)
rm(mon)
rm(num.features)
rm(num.photos)
rm(cleaned)
rm(desc.length)







######################################################
#--------------Code to make box plots-----------------
######################################################


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




######################################################
#-----------Code to generate decision Tree------------
######################################################

library(rpart)

train <- read.csv("train.csv", stringsAsFactors=FALSE)
test <- read.csv("test.csv", stringsAsFactors=FALSE)
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
             data=train,
             method="class")

Prediction <- predict(fit, test, type = "class")







######################################################
#-----------Code to perform SVM on data --------------
######################################################

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
source("load.R")

trainInd <- sample(1:dim(apt)[1], dim(apt)[1]*1/250)
train <- apt[trainInd,]
test <- apt[-trainInd,]

library(e1071)


tune.out <- tune(svm,as.factor(interest_level) ~ price + latitude + longitude + 
                   num_photos + num_features + desc_length + month + day + hour,data=train, probability=TRUE, kernel="linear",
                 ranges=list(cost=seq(0.01,50,length.out=10)))
fit.svcl <- tune.out$best.model

prediction <- predict(fit.svcl,newdata=test)
tab <- table(true=test[,13],pred=predict(fit.svcl,newdata=test))
tab

mean(as.numeric(as.character(test[,13])) == as.numeric(as.character(prediction)))

class.predl <- predict(fit.svcl, newdata = test, probability=TRUE)





tune.out <- tune(svm,as.factor(interest_level) ~ price + latitude + longitude + 
                   num_photos + num_features + desc_length + month + day + hour,data=train, probability=TRUE, kernel="polynomial",
                 ranges=list(cost=seq(0.01,100,length.out=10)))
fit.svcp <- tune.out$best.model

prediction <- predict(fit.svcp,newdata=test)
tab <- table(true=test[,13],pred=predict(fit.svcp,newdata=test))
tab

mean(as.numeric(as.character(test[,13])) == as.numeric(as.character(prediction)))

class.predp <- predict(fit.svcp, newdata = test, probability=TRUE)



tune.out <- tune(svm,as.factor(interest_level) ~ price + latitude + longitude + 
                   num_photos + num_features + desc_length + month + day + hour,data=train, probability=TRUE, kernel="radial",
                 ranges=list(cost=seq(0.01,50,length.out=10)))
fit.svcr <- tune.out$best.model

prediction <- predict(fit.svcr,newdata=test)
tab <- table(true=test[,13],pred=predict(fit.svcr,newdata=test))
tab

mean(as.numeric(as.character(test[,13])) == as.numeric(as.character(prediction)))
class.predr <- predict(fit.svcr, newdata = test, probability=TRUE)


class.predr <- predict(fit.svcr, newdata = test.apt[,-c(1,2)], probability=TRUE)
probs <- attr(class.predr,"probabilities")
write.csv(cbind(test.apt$listing_id, probs[,c(3,1,2)]), 
          file = "svm_predictions_3.csv", row.names = FALSE, quote = FALSE)








######################################################
#-----------Code to perform random forest-------------
######################################################

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







######################################################
#----------Simulation to try random guessing----------
######################################################



### Script to generate logistic model of the data.
#set.seed(100)
library(rpart)
##############################DATA LOAD IN AND CLEANING############################
# Load data in.
#source("load.R")
lon.lat <- cbind(apt$longitude, apt$latitude)

color = grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)]
colors <- c("blue", "green", "red")
colorClust <- colors[apt$interest_level + 1]
plot(lon.lat, cex = 0.3, pch = 16, col=colorClust)

quilt.plot(lon.lat, apt$interest_level, nx = 250, ny = 250)


trainInd <- sample(1:dim(apt)[1], dim(apt)[1]*2/3)
train <- apt[trainInd,]
test <- apt[-trainInd,]

fit <- rpart(interest_level ~ ., 
             data=train,
             method = "class",
             control=rpart.control(minsplit=1, minbucket=1, cp=0.001))

Prediction <- predict(fit, test[,-6], type = "class")
tab <- cbind(Prediction, test[,6])

#decision tree is only marginally better than guessing
lengthOf <- length(test[,6])
tot <- sample(1:lengthOf)
zero <- cbind(tot[1:floor(lengthOf*0.6929397)], 0)
one <- cbind(tot[ceiling(lengthOf*0.6929397) : floor(lengthOf*(0.2266586 + 0.6929397))], 1)
two <- cbind(tot[ceiling(lengthOf*(0.2266586 + 0.6929397)):lengthOf], 2)
mat <- rbind(zero, one, two)
mat <- mat[sort.list(mat[,1]), ][,2]


mean(Prediction == test[,6])
mean(mat == test[,6])
table(test[,6])/length(test[,6])








######################################################
#---------Make Pretty plot of decision tree ----------
######################################################


library(rpart)
library(rpart.plot)

train <- apt
fit <- rpart(as.factor(interest_level) ~ bathrooms + bedrooms + price + latitude + longitude
             + num_photos + num_features + desc_length + month
             + day + hour,
             data = train, 
             method = "class",
             control=rpart.control(minsplit=1, minbucket=1, cp=0.00329))

rpart.plot(fit)










######################################################
#-------Create spatial plot over google maps----------
######################################################
library(ggplot2)
library(ggmap)
lon <- as.numeric(lon.lat[,1])
lat <- as.numeric(lon.lat[,2])
# determine a reasonable center for map, 
# this could fail in some places (near poles, 180th meridian)
# also google appears to shift things slightly
center = paste(min(lat)+(max(lat)-min(lat))/2,
               min(lon)+(max(lon)-min(lon))/2, sep=" ")

# get map image from google
map <- get_map(location = center, zoom = 11, maptype = "terrain", 
               source = "google")

# start a ggplot. it won't plot til we type p
p <- ggmap(map)

# add points last so they are on top
color = grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)]
colors <- c("blue", "green", "red")
colorClust <- colors[as.numeric(as.character(apt$interest_level)) + 1]
p <- p + geom_point(data=data.frame(lon.lat),aes(x=lon, y=lat),colour=colorClust,size=0.3)

# display plot
p 








#############################################################
#Write out data so that word_cloud.py can create a word cloud
#############################################################


fileConn<-file("desc.txt")
writeLines(data$description[1:2000], fileConn)
close(fileConn)

features <- c()
for(i in 1:length(data$"features")) {
  print(i)
  features <- c(features, data$"features"[[i]])
}

fileConn<-file("features.txt")
writeLines(unlist(features), fileConn)
close(fileConn)

fileConn<-file("address.txt")
writeLines(data$street_address, fileConn)
close(fileConn)

#Now let's see what would happen if we only include the data that is high interest

newData <- data[data$interest_level == "high",]

fileConn<-file("desc_high.txt")
writeLines(newData$description, fileConn)
close(fileConn)

features <- c()
for(i in 1:length(newData$"features")) {
  print(i)
  features <- c(features, newData$"features"[[i]])
}

fileConn<-file("features_high.txt")
writeLines(unlist(features), fileConn)
close(fileConn)

fileConn<-file("address_high.txt")
writeLines(newData$street_address, fileConn)
close(fileConn)