require(graphics)
library(lattice)
require(lubridate)
require(ggplot2)

setwd("/Users/seanhendryx/THE UNIVERSITY OF ARIZONA (UA)/COURSES/Fall 15/Remote Sensing for the Study of Planet Earth REM 590/Assignments/Assignment 5 - Time Series/Data/moredata")
statistics_LST_Day <- read.csv("statistics_LST_Day_1km.asc")

statistics_NDVI <-read.csv("statistics_250m_16_days_NDVI.asc")

#dates <- as.Date(statistics_LST_Day$date)
#years <- format(dates, format = "%Y")
#months <- format(dates, format = "%m")
#DOY <- format(dates, format = "%j")
#DOY <- yday (statistics_LST_Day$date)
#statistics_LST_Day$DOY <- DOY
m1 <- merge(statistics_LST_Day, statistics_NDVI, by = "date.YYYYDDD.")

x <- m1$mean
y <- m1$meanNDVI
#Here's the old linear model
lmodel <- lm(y~x)
rsquared2=summary(lm(m1$meanNDVI~m1$mean, m1))$r.squared



mylabelprestring <- c(rsquared)
mylabel <- toString(mylabelprestring)
lwps <- c("r squared =")
#plot linear model
qplot(m1$mean, m1$meanNDVI,main="Mean NDVI over Mean Day Land Surface Temperatures in Beaverton, OR", xlab="Mean LST (K)", ylab="Mean NDVI", data=m1)  + geom_point() + geom_smooth(method=lm) + geom_text(x = 270, y = .65, label = mylabel, parse = TRUE) + geom_text(x = 267, y = .65, aes(label=lwps))

#And here's the nonlinear model:
#NDVI as mapped to by the log of Land Surface Temperature
#First plot to get start values for nls:
plot(x,y)

#Start by taking logs of both sides and fit a linear model, you get estimates of log(a) and b as the slope and intercept:
#reference: http://stats.stackexchange.com/questions/183653/getting-the-right-starting-values-for-an-nls-model-in-r
learnabstarts <- lm(log(y)~log(x))
coef(learnabstarts)

#now input start values:
aStart<-exp(coef(learnabstarts)[1]) #param a is the y value when x=0
#aStart <- exp(-9.617419)
bStart <- coef(learnabstarts)[2] # #b is the decay rate
#bStart <- 1.588217 
nlmodel <- nls(formula = y ~ a*log(x), start = list(a = aStart, b = bStart))
lines(x,predict(nlmodel),lty=2,col="red",lwd=3) #issue with predict: x and y different lengths
#^ y has na's and therefore predict removes these values so yhat and x vectors are no longer same length

yHat <- coef(nlmodel)*log(x)
#let's try this again:
lines(x,yHat,lty=2,col="red",lwd=3)

cor(m1$meanNDVI, predict(nlmodel))

rsquared=summary(lm(m1$mean~m1$meanNDVI, m1))$r.squared




