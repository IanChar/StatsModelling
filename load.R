rm(list=ls())
TRAIN_PATH <- "data/train.json"
TEST_PATH <- "data/test.json"
packages <- c("jsonlite", "dplyr", "purrr")
purrr::walk(packages, library, character.only = TRUE, warn.conflicts = FALSE)

data <- fromJSON(TRAIN_PATH)
vars <- setdiff(names(data), c("photos", "features"))
data <- map_at(data, vars, unlist) %>% tibble::as_tibble(.)

# Get useful predictors for this model and convert to doubles.
cleaned <- data[c("bathrooms", "bedrooms", "price", "latitude", "longitude")]
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




data <- fromJSON(TEST_PATH)
vars <- setdiff(names(data), c("photos", "features"))
data <- map_at(data, vars, unlist) %>% tibble::as_tibble(.)

# Get useful predictors for this model and convert to doubles.
cleaned <- data[c("bathrooms", "bedrooms", "price", "latitude", "longitude")]
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
test.apt <- test.apt[which(test.apt$latitude < 40.95 & test.apt$latitude > 40.55 & test.apt$longitude > -74.1 & test.apt$longitude < -73.5),]

rm(hour)
rm(interests)
rm(mon)
rm(num.features)
rm(num.photos)
rm(cleaned)
rm(desc.length)
