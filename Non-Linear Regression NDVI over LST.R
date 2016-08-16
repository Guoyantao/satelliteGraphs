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
#fit nonlinear model by mapping x to log space:
fit <- lm(y~log(x))
#look at result and statistics
summary(fit)
#extract coefficients only
coef(fit)


#Plot with ggplot:
p <- ggplot(data=m1, aes(x=mean, y=meanNDVI)) + geom_point() + stat_smooth(method="lm",formula=y~log(x),fill="red") + theme_bw()

p




