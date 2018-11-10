air_data <- read.csv(file="Ulaanbaatar_HourlyPM25_201701.CSV")
library(ggplot2)
library(dplyr)


colnames(air_data) <- as.character(unlist(air_data[2,]))
air_data <- air_data[-(1:2), ]

air_data <- subset(air_data, !( is.na(air_data$AQI) | air_data$AQI==""))
colnames(air_data)[3] <- "Date"

air_data <- transform(air_data, AQI = as.numeric(AQI))
air_data$better_date_1 <- as.POSIXct(strptime(air_data$Date, '%m/%d/%y %H:%M'))
summary(air_data$AQI)

air_data$Week <- strftime(air_data$better_date_1, format="%V")
air_data$DayOfYear <- as.numeric(format(air_data$better_date_1, "%j"))
air_data$day <- weekdays(as.Date(air_data$better_date_1))
air_data$time <- strftime(air_data$better_date_1, format="%H:%M:%S")

library(ggpmisc)
tiff("test.tiff", units="in", width=5, height=5, res=300)
p <- ggplot(air_data, aes(better_date_1, AQI)) +  geom_line(color = "darkorchid4") +
  labs(title = "Ulaanbaatar Air Quality Index (AQI) ", subtitle = "2017 January datay (hourly)",
       y="AQI", x="Date") + theme_bw(base_size = 15)

dev.off()


tiff("test_1.tiff", units="in", width=5, height=5, res=300)

air_data %>%
  filter(Week != "05" & Week != "52") %>% 
  ggplot(mapping=aes(x=day, y=AQI, shape=Week, color=Week)) + 
  geom_point() + 
  geom_line(aes(group = 1)) %>% 
  facet_grid(facets = Week ~ ., margins = FALSE) + theme_bw() +
  labs(title = "AQI by weekly")
  
dev.off()