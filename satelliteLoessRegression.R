#R SCRIPT TO VISUALIZE POINTS AND LOESS LINE ON MODIS SATELLITE LEAF AREA INDEX DATA OVER DAY OF YEAR
#AUTHORED BY SEAN M HENDRYX
require(graphics)
library(lattice)
require(lubridate)
require(ggplot2)


#READ DATA INTO R
statistics_LST_Day <- read.csv(file="/file/path/here")


#ASSIGN DATA TO VARIABLES
dates <- as.Date(statistics_LST_Day$date)
years <- format(dates, format = "%Y")
months <- format(dates, format = "%m")
DOY <- format(dates, format = "%j")


statistics_LST_Day$DOY <- DOY


#VISUALIZE
p <- ggplot(aes(statistics_LST_Day$DOY,statistics_LST_Day$LAImean, ylim(0,3.2)), data=statistics_LST_Day) 
p <- p + geom_point(alpha=2/10, shape=21, fill="blue", colour="black", size=5)

p<- p + labs(title = "Average LAI of the Santa Rita Mesquite Savannah from MODIS by DOY over 15 years") +
  theme_bw() + theme(axis.title = element_text(size = 18)) + theme(title = element_text(size = 20))

p <- p + labs(
    x = "Day Of Year",
    y = "Leaf Area Index",
    colour = "Cylinders"
  )

p <- p + stat_smooth(aes(group = 1), method = "loess") 

p
