### Script to generate logistic model of the data.

##############################DATA LOAD IN AND CLEANING############################
# Load data in.
source("load.R")
library(MASS)
library(lmtest)
library(ggplot2)
library(Hmisc)
library(pscl)
library(fmsb)

rm(list=ls())

##############################CLEAN DATA A BIT#####################################
summary(apt)
apt[which(apt$bathrooms > 4),]$bathrooms = 5
apt$bathrooms <- ceil(apt$bathrooms)
apt$bathrooms <- as.ordered(apt$bathrooms)
apt[which(apt$bedrooms > 4),]$bedrooms = 5
apt$bedrooms <- as.ordered(apt$bedrooms)
apt[which(apt$hour < 6),]$hour = 0
apt[which(apt$hour < 12 & apt$hour >= 6),]$hour = 1
apt[which(apt$hour < 18 & apt$hour >= 12),]$hour = 2
apt[which(apt$hour >= 18),]$hour = 3
apt$hour <- as.ordered(apt$hour)
apt$interest_level <- as.ordered(apt$interest_level)
# Price center: 3829.939  Price scale: 22080.4
apt$price <- as.numeric(scale(apt$price))
# Features center: 5.428948  Price scale: 3.922477
apt$num_features <- as.numeric(scale(apt$num_features))
# Photos center: 5.603798  Price scale: 3.628136
apt$num_photos <- as.numeric(scale(apt$num_photos))
# Desc center: 602.0369  Price scale: 393.2876
apt$desc_length <- as.numeric(scale(apt$desc_length))

# Check if there is multicolinearity
X <- with(apt, as.matrix(cbind(price, bedrooms, bathrooms, num_photos, num_features, desc_length, hour)))
XtX <- t(X) %*% X
kappa(XtX)

# Find vif of the
apt.pricelm <- lm(price ~ num_features + num_photos + desc_length + bedrooms + bathrooms + hour, data=apt)
apt.num_featureslm <- lm(num_features ~ price + num_photos + desc_length + bedrooms + bathrooms + hour, data=apt)
apt.num_photoslm <- lm(num_photos ~ num_features + price + desc_length + bedrooms + bathrooms + hour, data=apt)
apt.desc_lengthpricelm <- lm(desc_length ~ num_features + num_photos + price + bedrooms + bathrooms + hour, data=apt)
vifs <- c(Price=VIF(apt.pricelm), NumFeatures=VIF(apt.num_featureslm),
          NumPhotos=VIF(apt.num_photoslm), DescLength=VIF(apt.desc_lengthpricelm))

# Separate training and testing data
trainInd <- sample(1:dim(apt)[1], dim(apt)[1]*1/4)
train <- apt[trainInd,]
test <- apt[-trainInd,]

# Try to train a model
# No price:bathrooms because std error so high it flips signs
# No price:num_features for same reason
train.polr <- polr(interest_level ~ price + num_photos + desc_length + num_features + bedrooms + bedrooms:price + hour, Hess=TRUE, data=train)
train.polr_red <- polr(interest_level ~ price + num_photos + desc_length + num_features + bedrooms + hour, Hess=TRUE, data=train)
lrtest(train.polr, train.polr_red)

### Get the p-values for this model
coefs <- coef(summary(train.polr))
p_vals <- pnorm(abs(coefs[, "t value"]), lower.tail = FALSE) * 2
# Remove t_value
coefs <- coefs[, c("Value", "Std. Error")]
coefs <- cbind(coefs, "p value" = p_vals)
coefs <- cbind(coefs, "Odds Ratio" = exp(coef(train.polr)))
coefs
# p values are all practically 0, is this true or is there some problem?
# These shows multiplicative increase in odds over having no interest.
# Odds ratios show that number of bathrooms is quite important. Since bedrooms2 is the only one with 
# OR over 1, this shows people are really looking for 2 bedrooms. price is hard to judge since not standardized.



### Conf int
confint.default(train.polr)
# bedrooms1:price has 0 in CI suggesting bathroom might change price's affect on interest level between 0 and 1

### Test the parallel slopes assumption
sf <- function(y) {
  c('Interest>=0' = qlogis(mean(y >= 0)),
    'Interest>=1' = qlogis(mean(y >= 1)),
    'Interest>=2' = qlogis(mean(y >= 2)))
}
(s <- with(train, summary(as.numeric(as.character(interest_level)) ~ price + num_photos + desc_length + num_features + bedrooms + bedrooms:price + hour, fun=sf)))
s[, 2] <- s[, 3]
s[, 3] <- s[, 4]
s[, 4] <- s[, 3] - s[, 2]
s

s[, 2] <- s[, 2] - s[, 2]
s[, 3] <- s[, 3] - s[, 3]
plot(s, which=1:6, pch=1:3, xlab='logit', main=' ', xlim=range(s[,3:4]))

# See how many fit
error <- as.numeric(predict(dat.plr)) - as.numeric(dat$interest_level)
binaryError <- lapply(error, function(val) {
  toReturn = 0
  if (val != 0) {
    toReturn = 1
  }
  toReturn
})
Reduce("+", binaryError)

# Find psuedo-R^2
pR2(train.polr)
pR2(train.polr_red)
