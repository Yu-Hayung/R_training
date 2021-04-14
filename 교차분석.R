# 교차분석 
data1 <- read.csv("cleanDescriptive.csv", header = T)

head(data1)

x <- data1$level2
y <- data1$pass2

result1 <- data.frame(Level = x, pass = y)
dim(result1)


# 교차분석 
table(result1)

#install.packages("gmodels")
library(gmodels)

#install.packages("ggplot2")
library(ggplot2)


# 다이아몬드 교차 분석
CrossTable(x= diamonds$color, y = diamonds$cut)

# 부모 학력과 자녀 진학 교차 분석 
x2 <- data1$level2
y2 <- data1$pass2

CrossTable(x2, y2)

# 카이제곱 검정 
CrossTable(x2 = diamonds$cut,
           y2 = diamonds$color,
           chisq = T)
#  p =  1.394512e-51  유의 하다 


chisq.test(c(4, 6, 17, 16, 8, 9))


# 선호도 분석 
data2 <- textConnection(
  "스포츠음료종류 관측도수 
  1 41 
  2 30 
  3 51 
  4 71 
  5 61")
x3 <- read.table(data2, header = T)
x3

chisq.test(x3$관측도수)
#  p-value = 0.0003999 유의 수준 귀무가설 기각


# 이원 카이제곱 검정 

data3 <- read.csv("cleanDescriptive.csv", header = T)
x4 <- data$level2
y4 <- data$pass2 
CrossTable(x4,y4, chisq = T)   # p =  0.2507057 


# 동질성 검정 
data4 <- read.csv("homogenity.csv")

data4$method2[data4$method == 1] <- '방법1'
data4$method2[data4$method == 2] <- '방법2'
data4$method2[data4$method == 3] <- '방법3'

data4$survey2[data4$survey == 1] <- '매우만족'
data4$survey2[data4$survey == 2] <- '만족'
data4$survey2[data4$survey == 3] <- '보통'
data4$survey2[data4$survey == 4] <- '불만족'
data4$survey2[data4$survey == 5] <- '매우불만족'

table(data4$method2, data4$survey2)

chisq.test(data4$method2, data4$survey2)
# p-value = 0.5865 기무가설 채택





