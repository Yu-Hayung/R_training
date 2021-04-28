# 연습 문제 5번 

# 1번 문제 ###################################################################
data(iris3)
attributes(iris3)
str(iris3)

par(mfrow = c(2,2))
plot(iris3, main = " iris3 분포ᄃ", family="NanumGothicBold")
plot(iris3[,,1],pch =20, col = 'red', main = 'Setosa', family="NanumGothicBold")
plot(iris3[,,2],pch =20, col = 'blue', main = 'Versicolor', family="NanumGothicBold")
plot(iris3[,,3],pch =20, col = 'pink', main = 'virginica', family="NanumGothicBold")


# 2번 문제 ###################################################################

par(mfrow = c(1,1))
x <- iris$Sepal.Length
y <- iris$Petal.Length

setwd('D:/Rwork/output')
jpeg('iris.jpg', width = 720, height = 480)
plot(x, y, col = iris[ ,5],pch =20 , main = 'iris 데이터 테이블 산포도 차트', family="NanumGothicBold")
dev.off()


