require(graphics)
library(lattice)
require(lubridate)
require(ggplot2)

statistics_LST_Day <- read.csv(file="/Users/seanhendryx/Google Drive/THE UNIVERSITY OF ARIZONA (UA)/COURSES/Fall 15/Remote Sensing of the Environment REM 590/Assignments/Assignment 5 - Time Series/Data/moredata/statistics_LST_Day_1km.asc")

statistics_NDVI <-read.csv(file="/Users/seanhendryx/Google Drive/THE UNIVERSITY OF ARIZONA (UA)/COURSES/Fall 15/Remote Sensing of the Environment REM 590/Assignments/Assignment 5 - Time Series/Data/moredata/statistics_250m_16_days_NDVI.asc")

#dates <- as.Date(statistics_LST_Day$date)
#years <- format(dates, format = "%Y")
#months <- format(dates, format = "%m")
#DOY <- format(dates, format = "%j")
#DOY <- yday (statistics_LST_Day$date)
#statistics_LST_Day$DOY <- DOY
m1 <- merge(statistics_LST_Day, statistics_NDVI, by = "date.YYYYDDD.")


rsquared=summary(lm(m1$mean~m1$meanNDVI, m1))$r.squared
mylabelprestring <- c(rsquared)
mylabel <- toString(mylabelprestring)
lwps <- c("r squared =")
#labelwords <- toString(lwps)

qplot(m1$mean, m1$meanNDVI,main="Mean NDVI over Mean Day Land Surface Temperatures in Beaverton, OR", xlab="Mean LST (K)", ylab="Mean NDVI", data=m1)  + geom_point() + geom_smooth(method=lm) + geom_text(x = 270, y = .65, label = mylabel, parse = TRUE) + geom_text(x = 267, y = .65, aes(label=lwps))


#annotate(geom="text",x = 270, y = .65, label = mylabel, parse = TRUE)

#mylabel = bquote(italic(R)^2 == .(format(r2, digits = 3)))
#text(x = 270, y = .65, labels = mylabel)