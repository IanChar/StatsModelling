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