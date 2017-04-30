### Script to generate logistic model of the data.

##############################DATA LOAD IN AND CLEANING############################
# Load data in.
source("load.R")
library(MASS)
library(lmtest)
library(ggplot2)
library(Hmisc)

rm(list=ls())

##############################CLEAN DATA A BIT#####################################
dat <- head(aptmts, 1000)
dat <- filter(dat, price < 10000)
dat$price <- scale(dat$price)
dat$desc_length <- scale(dat$desc_length)

aptmts <- filter(aptmts, price < 10000)
aptmts$price <- scale(aptmts$price)


##############################SUMMARY OF THE DATA###################################
summary(dat)

# Although it is tough to tell because of uneven sampling, higher interest levels seem to
# to have consistently lower prices.
ggplot(dat, aes(x = interest_level, y = price)) +
  ylim(-5, 5) +
  geom_boxplot(size = .75) +
  geom_jitter(alpha = .5) +
  facet_grid(bedrooms ~ bathrooms, margins = TRUE) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

boxplot(price ~ interest_level, data=dat)
by(dat$price, dat$interest_level, plot)

# Plot what price looks like grouped by bedrooms
par(mfrow=c(3, 2))
by(dat$price, dat$bedrooms, plot)
by(dat$price, dat$bedrooms, summary)

plot(dat$price, dat$interest_level)

# Sooooo much MC
X <- as.matrix(cbind(dat$price, dat$bedrooms, dat$num_photos, dat$desc_length , dat$num_features))
xtx <- t(X) %*% X
kappa(xtx)

X <- as.matrix(cbind(aptmts$price, aptmts$desc_length))
xtx <- t(X) %*% X
kappa(xtx)

##############################ORDINAL LOGISTIC REGRESSION############################
dat.plr <- polr(interest_level ~ bedrooms + bathrooms + price + num_photos + desc_length + bedrooms:price + bathrooms:price, data=dat)

# Test the significance of this model.
lrtest(dat.plr) # Significant so the estimators have some explaining power.

# Is bathroom:price necessary?
dat.plr_red <- polr(interest_level ~ bathrooms + bedrooms + price + num_photos + desc_length + bedrooms:price, data=dat)
lrtest(dat.plr, dat.plr_red) # Test says no, drop it
dat.plr <- polr(interest_level ~ bedrooms + bathrooms + price + num_photos + desc_length + bedrooms:price, data=dat)

# Is bedroom:price necessary?
dat.plr_red <- polr(interest_level ~ bathrooms + bedrooms + price + num_photos + desc_length, data=dat)
lrtest(dat.plr, dat.plr_red) # Yes

# Test to see whether each of the predictors should be included.
dat.plr_red <- polr(interest_level ~ bathrooms + bedrooms + num_photos + desc_length + bedrooms:price, data=dat)
lrtest(dat.plr, dat.plr_red) # price is important
dat.plr_red <- polr(interest_level ~ bathrooms + price + num_photos + desc_length + bedrooms:price, data=dat)
lrtest(dat.plr, dat.plr_red) # bedrooms is important
dat.plr_red <- polr(interest_level ~ bedrooms + price + num_photos + desc_length + bedrooms:price, data=dat)
lrtest(dat.plr, dat.plr_red) # bathrooms is important.
dat.plr_red <- polr(interest_level ~ bedrooms + bathrooms + price + num_photos + bedrooms:price, data=dat)
lrtest(dat.plr, dat.plr_red) # desc is important.
dat.plr_red <- polr(interest_level ~ bedrooms + bathrooms + price + desc_length + bedrooms:price, data=dat)
lrtest(dat.plr, dat.plr_red) # num photos is not important

dat.plr <- polr(interest_level ~ bathrooms + bedrooms + price + desc_length + bedrooms:price, data=aptmts, Hess=TRUE)
tmp <- polr(interest_level ~ price, data=dat)
lrtest(dat.plr, tmp)
### Seems that interest_level ~ bedrooms + price + bathrooms + desc_length + bedrooms:price is best

#########################################ASSESSMENT OF MODEL####################################

### Get the p-values for this model
coefs <- coef(summary(aptmts.bedroomplr))
p_vals <- pnorm(abs(coefs[, "t value"]), lower.tail = FALSE) * 2
coefs <- cbind(coefs, "p value" = p_vals)
coefs
# p values are all practically 0, is this true or is there some problem?

### Calculate odds ratios
odds_ratios <- exp(coef(dat.plr))
odds_ratios
# These shows multiplicative increase in odds over having no interest.
# Odds ratios show that number of bathrooms is quite important. Since bedrooms2 is the only one with 
# OR over 1, this shows people are really looking for 2 bedrooms. price is hard to judge since not standardized.



### Conf int
confint.default(dat.plr)
# bedrooms1:price has 0 in CI suggesting bathroom might change price's affect on interest level between 0 and 1

### Test the parallel slopes assumption
sf <- function(y) {
  c('Interest>=1' = qlogis(mean(y >= 1)),
    'Interest>=2' = qlogis(mean(y >= 2)),
    'Interest>=3' = qlogis(mean(y >= 3)))
}
(s <- with(dat, summary(as.numeric(interest_level) ~ bedrooms + bathrooms + price, fun=sf)))

# Test coefficients across logistic regressions
glm(I(as.numeric(interest_level) >= 2) ~ bedrooms, family="binomial", data = dat)
glm(I(as.numeric(interest_level) >= 3) ~ bedrooms, family="binomial", data = dat)

glm(I(as.numeric(interest_level) >= 2) ~ bathrooms, family="binomial", data = dat)
glm(I(as.numeric(interest_level) >= 3) ~ bathrooms, family="binomial", data = dat)

glm(I(as.numeric(interest_level) >= 2) ~ price, family="binomial", data = dat)
glm(I(as.numeric(interest_level) >= 3) ~ price, family="binomial", data = dat)

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
