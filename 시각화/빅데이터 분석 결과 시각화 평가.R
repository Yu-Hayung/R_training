# 1. iris 데이터를 대상으로 시각화 하시오 

data(iris)
par(mfrow = c(1,1))
x <- iris$Sepal.Length
y <- iris$Petal.Length

setwd('/Users/yuhayung/Desktop')
jpeg('iris_YHY.jpg', width = 720, height = 480)
plot(x, y, col = iris[ ,5],pch =20 , main = 'Scatter plot for iris data', family="NanumGothicBold")
dev.off()


# 2. diamonds 데이터 셋을 대상으로 시각화 하시오 

library(ggplot2)

data(diamonds)
jpeg('diamonds_YHY.jpg', width = 720, height = 480)
p <- ggplot(data = diamonds, aes(x = carat, y = price, colour = clarity))
p + geom_point() + geom_smooth() 
dev.off()

