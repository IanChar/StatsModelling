#-----------Code to load all of the data--------------
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

# # Trim amount of data
# trimData <- function(dataset, size) {
#   lapply(dataset, function(elem) {
#     elem[1:size]
#   })
# }
# apt <- trimData(cleaned, NUM_SAMPLES)
# 
# # Make cleaned into a Dataframe
# apt <- as.data.frame(cleaned)

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

# # Trim amount of data
# trimData <- function(dataset, size) {
#   lapply(dataset, function(elem) {
#     elem[1:size]
#   })
# }
# apt <- trimData(cleaned, NUM_SAMPLES)
# 
# # Make cleaned into a Dataframe
# apt <- as.data.frame(cleaned)

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








#--------------Code to make box plots-----------------
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





#-----------Code to generate decision Tree------------
library(rpart)

train <- read.csv("train.csv", stringsAsFactors=FALSE)
test <- read.csv("test.csv", stringsAsFactors=FALSE)
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
             data=train,
             method="class")

Prediction <- predict(fit, test, type = "class")








#-----------Code to perform SVM on data --------------
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









#--------------Code to perform random forest----------------