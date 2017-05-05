packages <- c("jsonlite", "dplyr", "purrr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)
library(xtable)

cleanCluster <- function(apt, tooFew) {
  if (missing(tooFew)) {
    apt[which(apt$bathrooms > 4),]$bathrooms = 5
    apt[which(apt$bedrooms > 4),]$bedrooms = 5
  }
  apt$bathrooms <- ceil(apt$bathrooms)
  apt$bathrooms <- as.ordered(apt$bathrooms)
  apt$bedrooms <- as.ordered(apt$bedrooms)
  apt[which(apt$hour < 6),]$hour = 0
  apt[which(apt$hour < 12 & apt$hour >= 6),]$hour = 1
  apt[which(apt$hour < 18 & apt$hour >= 12),]$hour = 2
  apt[which(apt$hour >= 18),]$hour = 3
  apt$hour <- as.ordered(apt$hour)
  apt$interest_level <- as.ordered(apt$interest_level)
  # Price center: 3829.939  Price scale: 22080.4
  apt$price <- as.numeric(scale(apt$price, center=3829.939, scale=22080.4))
  # Features center: 5.428948  Price scale: 3.922477
  apt$num_features <- as.numeric(scale(apt$num_features, center=5.428948, scale=3.922477))
  # Photos center: 5.603798  Price scale: 3.628136
  apt$num_photos <- as.numeric(scale(apt$num_photos, center=5.603798, scale=3.628136))
  # Desc center: 602.0369  Price scale: 393.2876
  apt$desc_length <- as.numeric(scale(apt$desc_length, center=602.0369, scale=393.2876))
  return(apt)
}

# Get all the coefficients for the models.
clust_1 <- fromJSON("data/clust_1.json")
clust_1 <- cleanCluster(clust_1)
clust_1.polr <- polr(interest_level ~ price + num_photos + desc_length + num_features + bedrooms + bedrooms:price + hour, Hess=TRUE, data=clust_1)
clust_1.info <- c(coef(clust_1.polr), "R2"=pR2(clust_1.polr)["McFadden"])

clust_2 <- fromJSON("data/clust_2.json")
clust_2 <- cleanCluster(clust_2)
clust_2.polr <- polr(interest_level ~ price + num_photos + desc_length + num_features + bedrooms + bedrooms:price + hour, Hess=TRUE, data=clust_2)
clust_2.info <- c(coef(clust_2.polr), "R2"=pR2(clust_2.polr)["McFadden"])

clust_3 <- fromJSON("data/clust_3.json")
clust_3 <- cleanCluster(clust_3)
clust_3.polr <- polr(interest_level ~ price + num_photos + desc_length + num_features + bedrooms + bedrooms:price + hour, Hess=TRUE, data=clust_3)
clust_3.info <- c(coef(clust_3.polr), "R2"=pR2(clust_3.polr)["McFadden"])

clust_4 <- fromJSON("data/clust_4.json")
clust_4 <- cleanCluster(clust_4, TRUE)
clust_4.polr <- polr(interest_level ~ price + num_photos + desc_length + num_features + bedrooms + bedrooms:price + hour, Hess=TRUE, data=clust_4)
clust_4.info <- c(coef(clust_4.polr), "bla"=1, "bla2"=1, "R2"=pR2(clust_4.polr)["McFadden"])

clust_5 <- fromJSON("data/clust_5.json")
clust_5 <- cleanCluster(clust_5)
clust_5.polr <- polr(interest_level ~ price + num_photos + desc_length + num_features + bedrooms + bedrooms:price + hour, Hess=TRUE, data=clust_5)
clust_5.info <- c(coef(clust_5.polr), "R2"=pR2(clust_5.polr)["McFadden"])

clust_6 <- fromJSON("data/clust_6.json")
clust_6 <- cleanCluster(clust_6)
clust_6.polr <- polr(interest_level ~ price + num_photos + desc_length + num_features + bedrooms + bedrooms:price + hour, Hess=TRUE, data=clust_6)
clust_6.info <- c(coef(clust_6.polr), "R2"=pR2(clust_6.polr)["McFadden"])

clust_7 <- fromJSON("data/clust_7.json")
clust_7 <- cleanCluster(clust_7)
clust_7.polr <- polr(interest_level ~ price + num_photos + desc_length + num_features + bedrooms + bedrooms:price + hour, Hess=TRUE, data=clust_7)
clust_7.info <- c(coef(clust_7.polr), "R2"=pR2(clust_7.polr)["McFadden"])

clust_8 <- fromJSON("data/clust_8.json")
clust_8 <- cleanCluster(clust_8)
clust_8.polr <- polr(interest_level ~ price + num_photos + desc_length + num_features + bedrooms + bedrooms:price + hour, Hess=TRUE, data=clust_8)
clust_8.info <- c(coef(clust_8.polr), "R2"=pR2(clust_8.polr)["McFadden"])

clust_9 <- fromJSON("data/clust_9.json")
clust_9 <- cleanCluster(clust_9)
clust_9.polr <- polr(interest_level ~ price + num_photos + desc_length + num_features + bedrooms + bedrooms:price + hour, Hess=TRUE, data=clust_9)
clust_9.info <- c(coef(clust_9.polr), "R2"=pR2(clust_9.polr)["McFadden"])

clust_10 <- fromJSON("data/clust_10.json")
clust_10 <- cleanCluster(clust_10, TRUE)
clust_10.polr <- polr(interest_level ~ price + num_photos + desc_length + num_features + bedrooms + bedrooms:price + hour, Hess=TRUE, data=clust_10)
clust_10.info <- c(coef(clust_10.polr), "bla"=1, "bla2"=1, "R2"=pR2(clust_10.polr)["McFadden"])

tableInfo1 <- cbind("Cluster 1"=clust_1.info, "Cluster 2"=clust_2.info, "Cluster 3"=clust_3.info, "Cluster 4"=clust_4.info,
                   "Cluster 5"=clust_5.info)
tableInfo2 <- cbind("Cluster 6"=clust_6.info, "Cluster 7"=clust_7.info, "Cluster 8"=clust_8.info,
                    "Cluster 9"=clust_9.info, "Cluster 10"=clust_10.info)

xtable(tableInfo2)
