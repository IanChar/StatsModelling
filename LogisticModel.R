### Script to generate logistic model of the data.

##############################DATA LOAD IN AND CLEANING############################
# Load data in.
source("load.R")
library(MASS)
library(lmtest)

rm(list=ls())
# Get useful predictors for this model and convert to doubles.
cleaned <- data[c("bathrooms", "bedrooms", "price")]
cleaned <- lapply(cleaned, function(lst) {
  as.double(lst)
})
# Append response.
interests <- as.double(lapply(data$interest_level, function(interest) {
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
cleaned <- append(cleaned, list(interests))
names(cleaned)[length(names(cleaned))] <- "interest_level"

# Trim amount of data
trimData <- function(dataset, size) {
  lapply(dataset, function(elem) {
    elem[1:size]
  })
}
cleaned <- trimData(cleaned, 1000)

# Make cleaned into a Dataframe
aptmts <- as.data.frame(cleaned)
aptmts$bathrooms <- as.factor(aptmts$bathrooms)
aptmts$bedrooms <- as.factor(aptmts$bedrooms)
aptmts$interest_level <- as.factor(aptmts$interest_level)


##############################ORDINAL LOGISTIC REGRESSION############################
aptmts.plr <- polr(interest_level ~ bedrooms + bathrooms + price + bedrooms:price + bathrooms:price, data=aptmts)

# Test the significance of this model.
lrtest(aptmts.plr) # Significant so the estimators have some explaining power.

# Is bathroom:price necessary?
aptmts.plr_tmp <- polr(interest_level ~ bathrooms + bedrooms + price + bedrooms:price, data=aptmts)
lrtest(aptmts.plr, aptmts.plr_tmp) # Test says no, drop it
aptmts.plr <- polr(interest_level ~ bedrooms + bathrooms + price + bedrooms:price, data=aptmts)

# Is bedroom:price necessary?
aptmts.plr_tmp <- polr(interest_level ~ bathrooms + bedrooms + price, data=aptmts)
lrtest(aptmts.plr, aptmts.plr_tmp) # Yes

# Test to see whether each of the predictors should be included.
aptmts.plr_tmp <- polr(interest_level ~ bathrooms + bedrooms + bedroom:price, data=aptmts)
lrtest(aptmts.plr, aptmts.plr_tmp) # price is important
aptmts.plr_tmp <- polr(interest_level ~ bathrooms + price + bedroom:price, data=aptmts)
lrtest(aptmts.plr, aptmts.plr_tmp) # bedrooms is important
aptmts.plr_tmp <- polr(interest_level ~ bedrooms + price + bedroom:price, data=aptmts)
lrtest(aptmts.plr, aptmts.plr_tmp) # bathrooms is important.

### Seems that interest_level ~ bedrooms + price + bathrooms + bedrooms:price is best
# Get the p-values for this model
