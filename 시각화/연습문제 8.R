
# 1번 문제 ###############################################################################
library(lattice)
data(quakes)
depthgroup <- equal.count(quakes$depth, number = 3, overlap = 0)
magnitudegroup <- equal.count(quakes$mag, number = 2, overlap = 0)

xyplot(lat ~ long | magnitudegroup*depthgroup, data=quakes, main = "Fiji Earthquakes", ylab = "latitude", xlab = "longitude",
       pch="@",col=c("orange","skyblue"))

# 2번 문제 ###############################################################################
library(lattice)
data(SeatacWeather)
xyplot(min.temp + max.temp ~ day | month, data = SeatacWeather, type = "l", layout = c(3,1))

# 3번 문제 ###############################################################################
data(diamonds)
p <- ggplot(data = diamonds, aes(x = carat, y = price, colour = clarity))
p + geom_point() + geom_smooth() 


# 4번 문제 ###############################################################################
library(ggmap)

setwd("/Users/yuhayung/Desktop")
university <- read.csv("university.csv")
university 

seoul <- c(left = 126.77, bottom = 37.40, 
           right = 127.17, top = 37.70)

map <- get_stamenmap(seoul, zoom=11, maptype='watercolor')

layer1 <- ggmap(map)

layer2 <- layer1 + geom_point(data=university,
                              aes(x=LON,y=LAT, color=학교명), size=3)

layer3 <- layer2 + geom_text(data=university,
                             aes(x=LON+0.01, y=LAT+0.01,label=학교명), size=5, family="NanumGothicBold")

ggsave("university.png",width=10.24,height=7.68)

