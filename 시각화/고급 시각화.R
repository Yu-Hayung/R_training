
# install.packages('lattice') # 시각화 패키지
library(lattice)

# install.packages('mlmRev') # 데이터 패키지 
library(mlmRev)

data("Chem97")
str(Chem97)

# 히스토 그램 
histogram(~gcsescore, data = Chem97)

histogram(~gcsescore|score, data = Chem97)
histogram(~gcsescore|factor(score), data = Chem97)

# 밀도 그래프 
densityplot(~ gcsescore|factor(score), data = Chem97,
            groups = gender,
            plot.points = T,
            auto.key =  T)

# 막대그래프
data("VADeaths")
str(VADeaths)

dft <- as.data.frame.table(VADeaths)

barchart(Var1 ~ Freq | Var2, data = dft, layout = c(4, 1))
barchart(Var1 ~ Freq | Var2, data = dft, layout = c(4, 1), origin = 0)

# 점그래프 
dotplot(Var1 ~ Freq | Var2, dft)
dotplot(Var1 ~ Freq | Var2, dft, layout = c(4,1))

dotplot(Var1 ~ Freq, data = dft,
        groups = Var2, type = "o",
        auto.key = list(space = "right", point = T, lines = T))

# 산점도 그리기 
data("airquality")

xyplot(Ozone ~ Wind, data = airquality)
xyplot(Ozone ~ Wind|Month, data = airquality)
xyplot(Ozone ~ Wind|Month, data = airquality, layout = c(5,1))

convert <- transform(airquality, Month = factor(Month))
xyplot(Ozone ~ Wind|Month, data = convert)
 
data("quakes")
xyplot(lat ~ long, data = quakes, pch =".")
tplot <- xyplot(lat ~ long, data = quakes, pch = '.')
tplot <- update(tplot, main = "1964년 이후 태평양에서 발생한 지진 위치")
print(tplot)

range(quakes$depth)

quakes$depth2[quakes$depth >= 40 & quakes$depth <= 150] <- 1
quakes$depth2[quakes$depth >= 151 & quakes$depth <= 250] <- 2
quakes$depth2[quakes$depth >= 251 & quakes$depth <= 350] <- 3
quakes$depth2[quakes$depth >= 351 & quakes$depth <= 450] <- 4
quakes$depth2[quakes$depth >= 451 & quakes$depth <= 550] <- 5
quakes$depth2[quakes$depth >= 551 & quakes$depth <= 680] <- 6

convert <- transform(quakes, depth2 = factor(depth2))
xyplot(lat ~ long|depth2, data = convert)

xyplot(Ozone + Solar.R ~ Wind | factor(Month),
       data = airquality,
       col = c('blue', 'red'),
       layout = c(5,1))

# 데이터 범주화 

numgroup <- equal.count(1:150, number = 4, overlap = 0)
depthgroup <- equal.count(quakes$depth, number = 5, overlap = 0)

xyplot(lat ~ long | depthgroup, data = quakes,
       main = "지진의 깊이",
       ylab = "latitude", xlab = "longitude",
       pch = "@", col = "red")

magnitudegroup <- equal.count(quakes$mag, number = 2, overlap = 0)
xyplot(lat ~ long | magnitudegroup, data = quakes,
       main = "진도",
       ylab = "latitude", xlab = "longitude",
       pch = "@", col = "blue")

xyplot(lat ~ long | depthgroup * magnitudegroup, data = quakes,
       main = "수심과 진도 규모",
       ylab = "latitude", xlab = "longitude",
       pch = "@", col = c('red',"blue"))

# 조거  
coplot(lat ~ long|depth, data = quakes)
coplot(lat ~ long | depth, data = quakes,overlap = 0.1)  # overlap 겹치는 부분 

coplot(lat ~ long | depth, data = quakes, number = 5, row = 1) # 조건 5개 

coplot(lat ~ long | depth, data = quakes, number = 5, row = 1, panel = panel.smooth) # 곡선 추가 

coplot(lat ~ long | depth, data = quakes, number = 5, row = 1, panel = panel.smooth, col = 'blue', bar.bg = c(num = 'green')) # 색 


# 3ᄎ  그래프
cloud(depth ~ lat * long, data = quakes, zlim = rev(range(quakes$depth)),
      xlab = "경도", ylab = "위도", zlab = "깊이")



# qplot()  ######################################################################
# install.packages("ggplot2")
library(ggplot2)
data(mpg)
str(mpg)

#세로 막대 
qplot(hwy, data = mpg)
qplot(hwy, data = mpg, fill = drv)
qplot(hwy, data = mpg, fill = drv, binwidth = 2)
qplot(hwy, data = mpg, fill = drv, binwidth = 2, facets = .~ drv)
qplot(hwy, data = mpg, fill = drv, binwidth = 2, facets = drv ~. )

# 산점도 
qplot(displ, hwy, data = mpg)
qplot(displ, hwy, data = mpg, color = drv)
qplot(displ, hwy, data = mpg, color = drv, facets = .~ drv)

qplot(wt, mpg, data = mtcars, color = factor(carb))
qplot(wt, mpg, data = mtcars, color = factor(carb), size = qsec)
qplot(wt, mpg, data = mtcars, color = factor(carb), size = qsec, shape = factor(cyl))

data("diamonds")
qplot(clarity, data = diamonds, full = cut, geom = 'bar')
qplot(clarity, data = diamonds, colour = cut, geom = "bar")
qplot(wt, mpg, data = mtcars, size = qsec, geom = "point")
qplot(mpg, wt, data = mtcars, color = factor(cyl), geom = c("point", 'line'))


# ggplot() ######################################################################
p <- ggplot(diamonds, aes(carat, price, color = cut))
p + geom_point() # 차트 추가 

p <- ggplot(mtcars, aes(mpg, wt, color = factor(cyl)))
p + geom_point() 
p + geom_line()

p <- ggplot(diamonds, aes(price))
p + stat_bin(aes(fill = cut), geom = "bar")
p + stat_bin(aes(fill = ..density..),geom = 'bar') # 빈도를 밀도(전체합 =1)

p <- ggplot(diamonds, aes(price))
p + stat_bin(aes(fill = cut), geom = "area")
p + stat_bin(aes(color = cut, size = ..density..), geom = "point")



# 산점도와 회귀선 적용 #########################################################
library(UsingR)
data("galton")
p <- ggplot(data = galton, aes(x = parent, y = child))
p + geom_count() + geom_smooth(method = "lm")


# 테마 적용 ####################################################################
p <- ggplot(diamonds, aes(carat, price, color = cut))
p <- p + geom_point() + ggtitle("다이아몬드 무게와 가격의 상관관계")
print(p)

p + theme(
  title = element_text(color = "blue", size = 25),
  axis.title = element_text(size = 14, face = "bold"),
  axis.title.x = element_text(color = "green"),
  axis.title.y = element_text(color = "green"),
  axis.text = element_text(size = 14),
  axis.text.x = element_text(color = "purple"),
  axis.text.y = element_text(color = "red"),
  legend.title = element_text(size = 20,
                              face = "bold",
                              color = "red"),
  legend.position = "bottom",
  legend.direction = "horizontal"
)

# Stamen Maps API 이용 ##########################################################
library(ggplot2)
# install.packages("ggmap")

library(ggmap)

setwd("/Users/yuhayung/Desktop")
seoul <- c(left = 126.77, bottom = 37.40,
           right = 127.17, top = 37.70)

map <- get_stamenmap(seoul, zoom = 12, maptype = 'terrain')

layer1 <- ggmap(map) ; layer1



# 2019년도 1월 대한민국 인구수를 기준으로 지역별 인구수 표시 하기 
pop <- population201901
pop <- read.csv('population201901.csv', header = T)

library(stringr)

region <- pop$지역명
lon <- pop$LON
lat <- pop$LAT
tot_pop <- as.numeric(str_replace_all(pop$'총인구수', ',',''))

df <- data.frame(region, lon, lat, tot_pop)
df

df <- df[1:17, ]

setwd("/Users/yuhayung/Desktop")
daegu <- c(left = 123.4423013, bottom = 32.8528306,
           right = 131.601445, top = 38.8714354)
map <- get_stamenmap(daegu, zoom = 10,  maptype = 'terrain', header = T)

layer1 <- ggmap(map) ; layer1

layer2 <- layer1 + geom_point(data = df,
                              aes(x = lon, y = lat,
                                  color = factor(tot_pop),
                                  size = factor(tot_pop))) ;layer2

layer3 <- layer2 + geom_text(data = df,
                             aes(x = lon + 0.01, y = 0.08,
                                 label = region), size = 8, family="NanumGothicBold") ;layer3

# 크기지정 파일 저장 
ggsave("pop201901.png", scale = 1, width = 10.24, height = 7.68)




