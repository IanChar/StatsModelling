### Script to generate logistic model of the data.

##############################DATA LOAD IN AND CLEANING############################
# Load data in.
source("load.R")
library(MASS)
library(lmtest)
library(ggplot2)
library(Hmisc)

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

##############################SUMMARY OF THE DATA###################################
summary(aptmts)

# Although it is tough to tell because of uneven sampling, higher interest levels seem to
# to have consistently lower prices.
ggplot(aptmts, aes(x = interest_level, y = price)) +
  ylim(0, 10000) +
  geom_boxplot(size = .75) +
  geom_jitter(alpha = .5) +
  facet_grid(bedrooms ~ bathrooms, margins = TRUE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

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

#########################################ASSESSMENT OF MODEL####################################

### Get the p-values for this model
coefs <- coef(summary(aptmts.plr))
p_vals <- pnorm(abs(coefs[, "t value"]), lower.tail = FALSE) * 2
coefs <- cbind(coefs, "p value" = p_vals)
coefs
# p values are all practically 0, is this true or is there some problem?

### Calculate odds ratios
odds_ratios <- exp(coef(aptmts.plr))
odds_ratios
# These shows multiplicative increase in odds over having no interest.
# Odds ratios show that number of bathrooms is quite important. Since bedrooms2 is the only one with 
# OR over 1, this shows people are really looking for 2 bedrooms. price is hard to judge since not standardized.



### Conf int
confint.default(aptmts.plr)
# bedrooms1:price has 0 in CI suggesting bathroom might change price's affect on interest level between 0 and 1

### Test the parallel slopes assumption
sf <- function(y) {
  c('Interest>=1' = qlogis(mean(y >= 1)),
    'Interest>=2' = qlogis(mean(y >= 2)),
    'Interest>=3' = qlogis(mean(y >= 3)))
}
(s <- with(aptmts, summary(as.numeric(interest_level) ~ bedrooms + bathrooms + price, fun=sf)))

# Test coefficients across logistic regressions
glm(I(as.numeric(interest_level) >= 2) ~ bedrooms, family="binomial", data = aptmts)
glm(I(as.numeric(interest_level) >= 3) ~ bedrooms, family="binomial", data = aptmts)

glm(I(as.numeric(interest_level) >= 2) ~ bathrooms, family="binomial", data = aptmts)
glm(I(as.numeric(interest_level) >= 3) ~ bathrooms, family="binomial", data = aptmts)

glm(I(as.numeric(interest_level) >= 2) ~ price, family="binomial", data = aptmts)
glm(I(as.numeric(interest_level) >= 3) ~ price, family="binomial", data = aptmts)
