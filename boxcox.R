#Price box-cox plot
newPrice <- sort(apt[which(apt$price > 100 & apt$price < 1000000),]$price)
len.price <- seq(1:length(newPrice))
new.mod <- lm(newPrice ~ len.price)

b <- boxcox(new.mod, lambda = seq(-0.7,-0.5, 1/10))
optimal <- b$x[which(max(b$y) == b$y)]

#-0.6090909