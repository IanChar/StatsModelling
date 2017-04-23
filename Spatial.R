### Script to generate logistic model of the data.
#set.seed(100)
library(rpart)
##############################DATA LOAD IN AND CLEANING############################
# Load data in.
#source("load.R")
lon.lat <- cbind(aptmts$longitude, aptmts$latitude)
plot(lon.lat, cex = 0.5, pch = 16, col=rgb(1-aptmts$interest_level/2, 0, 0, 0.25)) 

trainInd <- sample(1:dim(aptmts)[1], dim(aptmts)[1]*2/3)
train <- aptmts[trainInd,]
test <- aptmts[-trainInd,]

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


