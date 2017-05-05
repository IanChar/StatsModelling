################################################################################################
## Support vector machines
################################################################################################

library(e1071)

##
## A poor SVC, but a good SVM
##

set.seed(10)
X <- matrix(rnorm(40*2),ncol=2)
y <- c(rep(-1,10),rep(1,20),rep(-1,10))
X[11:30,] <- X[11:30,] + 2
X[31:40,] <- X[31:40,] + 4

col <- rep(NA,40)
col[y==1] <- rainbow(2)[1]
col[y==-1] <- rainbow(2)[2]

pch <- rep(NA,40)
pch[y==1] <- 16
pch[y==-1] <- 17

plot(X[,c(2,1)],col=col,pch=pch)

dat <- data.frame(X=X,y=as.factor(y))

fit.svc <- svm(y~.,data=dat,kernel="linear",cost=1)
plot(fit.svc,data=dat)

## A support vector machine
fit.svc <- svm(y~.,data=dat,kernel="polynomial",cost=10,degree=5)
plot(fit.svc,data=dat)

## A better support vector machine
fit.svc <- svm(y~.,data=dat,kernel="radial",cost=1)
plot(fit.svc,data=dat)

################################################################################################
## Classifying precipitation
## FZRA: freezing rain
## IP: ice pellets
## RA: rain
## SN: snow
## Twb.prof: vertical wet bulb temperature profile in 100m increments 0 to 3000m
################################################################################################

rm(list=ls())

library(e1071)

load("~/Documents/Classes/Spring2017/5580Spring2017/data/ScheuererData/predictors.Rdata")

ls()

table(ptype)

dim(Twb.prof)

matplot(t(Twb.prof[1:50,]),xlab="Altitude",ylab="Wet bulb temperature")

##
## Some exploratory analysis
##

n <- 1000

set.seed(2820)

train <- sample(1:length(ptype),size=n,replace=FALSE)

twb <- Twb.prof[train,c(1,16)] # 0 and 1500m temperatures
pt <- ptype[train]

plot(twb,col="white")

points(twb[pt=="RA",],col="blue")
points(twb[pt=="FZRA",],col="green",pch=19)
points(twb[pt=="SN",],col="orange")
points(twb[pt=="IP",],col="purple",pch=19)

##
## Only deal with RA/SN
##

twb <- twb[pt == "RA" | pt == "SN",]
pt <- pt[pt == "RA" | pt == "SN"]

##
## SVM fit
##

dat <- data.frame(X=twb,y=as.factor(pt))

## support vector classifier
fit.svc <- svm(y~.,data=dat,kernel="linear",cost=1)
plot(fit.svc,data=dat)
# tuning
tune.out <- tune(svm,y~.,data=dat,kernel="linear",
                 ranges=list(cost=seq(0.01,5,length.out=100)))
fit.svc <- tune.out$best.model

fit.svc
plot(fit.svc,data=dat)

## support vector machine: polynomial
fit.svmp <- svm(y~.,data=dat,kernel="polynomial",cost=10,degree=3)
plot(fit.svmp,data=dat)
# tuning
tune.out <- tune(svm,y~.,data=dat,kernel="polynomial",degree=3,
                 ranges=list(cost=seq(0.01,5,length.out=100)))
fit.svmp <- tune.out$best.model

fit.svmp
plot(fit.svmp,data=dat)

## svm: radial
fit.svmr <- svm(y~.,data=dat,kernel="radial",cost=1)
plot(fit.svmr,data=dat)
# tuning
tune.out <- tune(svm,y~.,data=dat,kernel="radial",
                 ranges=list(cost=seq(0.01,5,length.out=100)))
fit.svmr <- tune.out$best.model

fit.svmr
plot(fit.svmr,data=dat)

##
## Testing set performance
##

dat.test <- data.frame(X=Twb.prof[-train,c(1,16)],y=as.factor(ptype[-train]))
dat.test <- dat.test[dat.test$y == "RA" | dat.test$y == "SN",]

# linear classifier
tab <- table(true=dat.test[,"y"],pred=predict(fit.svc,newdata=dat.test))
tab
(tab[3,1]+tab[4,2])/dim(dat.test)[1]

# polynomial classifier
tab <- table(true=dat.test[,"y"],pred=predict(fit.svmp,newdata=dat.test))
tab
(tab[3,1]+tab[4,2])/dim(dat.test)[1]

# radial classifier
tab <- table(true=dat.test[,"y"],pred=predict(fit.svmr,newdata=dat.test))
tab
(tab[3,1]+tab[4,2])/dim(dat.test)[1]

##
## Try with all 
##

these <- (ptype == "RA") | (ptype == "SN")
dat <- data.frame(X=Twb.prof[these,],y=as.factor(ptype[these]))

set.seed(100)
train <- sample(1:dim(dat)[1],size=10000)

## support vector machine: polynomial
fit.svm <- svm(y~.,data=dat[train,],kernel="radial",cost=3.75)
# tuning
#tune.out <- tune(svm,y~.,data=dat[train,],kernel="radial",
#  ranges=list(cost=seq(0.01,5,length.out=5)))
#fit.svm <- tune.out$best.model

fit.svm
## performance on test data
tab <- table(true=dat[-train,"y"],pred=predict(fit.svm,newdata=dat[-train,]))
tab
sum(diag(tab))/(dim(dat)[1] - length(train))