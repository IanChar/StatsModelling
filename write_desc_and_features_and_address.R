fileConn<-file("desc.txt")
writeLines(data$description[1:2000], fileConn)
close(fileConn)

features <- c()
for(i in 1:length(data$"features")) {
  print(i)
  features <- c(features, data$"features"[[i]])
}

fileConn<-file("features.txt")
writeLines(unlist(features), fileConn)
close(fileConn)

fileConn<-file("address.txt")
writeLines(data$street_address, fileConn)
close(fileConn)

#Now let's see what would happen if we only include the data that is high interest

newData <- data[data$interest_level == "high",]

fileConn<-file("desc_high.txt")
writeLines(newData$description, fileConn)
close(fileConn)

features <- c()
for(i in 1:length(newData$"features")) {
  print(i)
  features <- c(features, newData$"features"[[i]])
}

fileConn<-file("features_high.txt")
writeLines(unlist(features), fileConn)
close(fileConn)

fileConn<-file("address_high.txt")
writeLines(newData$street_address, fileConn)
close(fileConn)
