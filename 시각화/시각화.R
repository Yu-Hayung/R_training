
# 막대 그래프 #####################################################################

chart_data <- c(305, 450, 320, 460, 330, 480, 380, 520)
names(chart_data) <- c('2018 1분기', '2019 1분기', '2018 2분', '2019 2분기', '2018 3분기', '2019 3분기', '2018 4분기', '2019 4분기')
str(chart_data)

# 세로 막대 그래프 
barplot(chart_data, ylim = c(0, 600), col = rainbow(8), main = "2018년도 vs 2019년도 매출 형황 비교" , family="NanumGothicBold")
barplot(chart_data, ylim = c(0, 600), col = rainbow(8), main = "2018년도 vs 2019년도 매출 형 " , family="NanumGothicBold",
        ylab = "매출액(단위 : 만원)", xlab = "년도별 분기 현황")

# 가로 막대 차트 
barplot(chart_data, xlim = c(0, 600), col = rainbow(8), main = "2018년도 vs 2019년도 매출 형황 비교" , family="NanumGothicBold",
        horiz = T,
        ylab = "매출액(단위 : 만원)", xlab = "년도별 분기 현황")


# 가로 차트 가격 조정 
barplot(chart_data, xlim = c(0, 600), col = rainbow(8), main = "2018년도 vs 2019년도 매출  " , family="NanumGothicBold",
        horiz = T,
        space = 1, cex.names = 0.8,
        ylab = "매출액(단위 : 만원)", xlab = "년도별 분기 현황")

# 막대ᄋ 색상 지정하기 
barplot(chart_data, xlim = c(0, 600),  main = "2018년도 vs 2019년도 매출 형황 비교" , family="NanumGothicBold",
        horiz = T,
        space = 1, cex.names = 0.8,
        ylab = "매출액(단위 : 만원)", xlab = "년도별 분기 현황",
        col = rep(c(2,4), 4))


# 누적 막대 차트 #####################################################################

data("VADeaths")
mode(VADeaths)
par(mfrow = c(1,2))

# 개별차트 
barplot(VADeaths, beside = T, col = rainbow(5), main = "미국 버지니아주 하ᄋ계측 사망 비율",  family="NanumGothicBold")
legend(19, 71, c("50-54", "55-59", "60-64", "65-59", "70-74"),  cex = 0.8, fill = rainbow(5))

# 누적차트 
barplot(VADeaths, beside = F, col = rainbow(5))
title(main = "미국 버지니아주 하위계층 사망 비율", font.main = 4,  family="NanumGothicBold")
legend(3.8, 200, c("50-54", "55-59", "60-64", "65-59", "70-74"),  cex = 0.8, fill = rainbow(5))


# 점 차트  #####################################################################

par(mfrow = c(1, 1))
dotchart(chart_data, color = c('blue', 'red'),
         lcolor = "black",
         pch = 1:2,
         labels = names(chart_data),
         xlab = "매출액",
         main = "분기별 판매현황 : 점차트 시각화",
         cex = 1.2,  family="NanumGothicBold")


# 원형 차트 ####################################################################

par(mfrow = c(1,1))
pie(chart_data, labels = names(chart_data), col = rainbow(8), cex = 1.2, 
    main = "2018 - 2019 년도 분기별 매출 현황", family="NanumGothicBold")


# 상자 그래프 시각화 ###########################################################
boxplot(VADeaths, range = 0)
boxplot(VADeaths, range = 0, notch = T)
abline(h = 37, lty = 3, col = "red")

# 히스토그램 시각화 ############################################################
data(iris)
names(iris)

hist(iris$Sepal.Length, xlab = "iris$Sepal.Length", col = "darkgreen", 
     main = "iris 꽃받치 길이 Histogram", xlim = c(4.3, 7.9), family="NanumGothicBold")

hist(iris$Sepal.Width, xlab = "iris$Sepal.Length", col = "mistyrose", 
     main = "iris 꽃받침 넓이 Histogram", xlim = c(2.0, 4.5), family="NanumGothicBold")

par(mfrow = c(1,2))
hist(iris$Sepal.Width, xlab = "iris$Sepal.Width", col = "darkgreen", main = "iris 꽃받침 너비 Histogram: 빈도수",
     xlim = c(2.0, 4.5), family="NanumGothicBold")

hist(iris$Sepal.Width, xlab = "iris$Sepal.Width", col = "darkred", main = "iris 꽃받침 너비 Histogram: 확률 밀도", freq = F,
     xlim = c(2.0, 4.5), family="NanumGothicBold")


# 밀도 기준 ᄇ  
lines(density(iris$Sepal.Width), col= "red")

# 정규분포 추정 곡선 
x <- seq(2.0, 4.5, 0.1)
curve(dnorm(x, mean = mean(iris$Sepal.Width), sd = sd(iris$Sepal.Width)), col = "blue", add = T)


# 산저도  ############################################################

par(mfrow = c(1,1))
price <- runif(10, min = 1, max =100)
plot(price, col = "red")

par(new = T)
line_chart = 1: 100

# 대각선 추가 
plot(line_chart, type = "l", col = "red", axes = F, ann = F)

# 텍스트 추가 
text(70, 80, "대각선 추가", col = "blue", family="NanumGothicBold")





# 선 그리기 type() ############################################################
par(mfrow = c(2,2))
plot(price, type = "l")
plot(price, type = "o")
plot(price, type = "h")
plot(price, type = "s")



# 산점도 그리기 pch() #########################################################
plot(price, type = "o", pch = 5)
plot(price, type = "o", pch = 15, lwd = 3)
plot(price, type = "o", pch = 20, col = "blue")
plot(price, type = "o", pch = 20, col = "orange", cex = 1.5)



# WWWusage 데이터 시각화 ######################################################
data("WWWusage")

par(mfrow = c(1,1))
plot(WWWusage) # 시계열 데이터 


# 중첩자료 시각화 ##############################################################

x <- c(1, 2, 3, 4, 2, 4)
y <- rep(2, 6)
x: y

table(x, y)
plot(x, y)

xy.df <- as.data.frame(table(x,y))
xy.df

plot(x,y,
     pch = "@", col = "blue", cex = 0.5 * xy.df$Freq,
     xlab = "x벡터 원소", ylab = "y벡터 원소", family="NanumGothicBold")

library(UsingR)
data(galton)

galtonData <- as.data.frame(table(galton$child, galton$parent))
names(galtonData) = c("child", "parent", "freq")
parent <- as.numeric(galtonData$parent)
child <- as.numeric(galtonData$child)

plot(parent, child,
     pch = 21,
     col = "blue",
     bg = "green",
     cex = 0.2 * galtonData$freq,
     xlab = "parent", 
     ylab = "child")

# 변수간의 비교 시각화 #######################################################
attributes(iris)
pairs(iris[iris$Species == "virginica", 1:4])
pairs(iris[iris$Species == "setosa", 1:4])


# 3차원 산점도 시각화  #######################################################
# install.packages("scatterplot3d")
library(scatterplot3d)

iris_setosa = iris[iris$Species == 'setosa', ]
iris_versicolor = iris[iris$Species == 'versicolor', ]
iris_virginica = iris[iris$Species == 'virginica', ]

# 틀만들기
d3 <- scatterplot3d(iris$Petal.Length, iris$Sepal.Length, iris$Sepal.Width, type = 'n')

# 산점도 시각화 
d3$points3d(iris_setosa$Petal.Length,
            iris_setosa$Sepal.Length,
            iris_setosa$Sepal.Width,
            bg = 'orange' , pch = 21)
d3$points3d(iris_versicolor$Petal.Length,
            iris_versicolor$Sepal.Length,
            iris_versicolor$Sepal.Width,
            bg = 'blue' , pch = 21)
d3$points3d(iris_virginica$Petal.Length,
            iris_virginica$Sepal.Length,
            iris_virginica$Sepal.Width,
            bg = 'pink' , pch = 21)

