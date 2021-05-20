
library(ggplot2)
library(ggmap)
library(dplyr)

X <- read.csv("등록개체수동별.csv") ; X

names(X) <- c("no", "gu", "Dong", "registered", "lon", "lat"); X


# 지도 이미지에 레이어 적용 하여 동물 등록수 시도 시각화 ######################
# 인천 = 위도 37.437793 경도 126.975861

incheon <- c(left = 126.35, bottom = 37.34  , right = 126.81 , top = 37.66 )

map <- get_stamenmap(incheon, zoom = 12, maptype = 'terrain') 
ggmap(map)

layer1 <- ggmap(map)
layer2 <- layer1 + geom_point(data = X,
                              aes(x = log , y = lat,
                                  color = factor(registered),
                                  size = factor(registered)))
layer2

ggsave("Bark in Park!.png", scale=1, width = 10, height = 10)

# 군집 분석 ##################################################################

library(ggplot2) 

mydata <- X[c("registered","lon","lat")]
head(mydata)

str(mydata)

plot(mydata$lon, mydata$lat, asp = 1)

set.seed(123)

#키오스크가 2개, 3개 일 때, 각각 나누어서 k-means 모델을 학습
model2 <- kmeans(mydata, 2)
model3 <- kmeans(mydata, 3)
model10 <- kmeans(mydata, 10)









