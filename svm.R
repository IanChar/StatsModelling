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