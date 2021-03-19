# 기술통계 분석을 해본다. 
# 척도별로 의미 있는 통계량을 구분한다.

getwd()
setwd("C:/Users/tj-bu/Desktop/R/dataset2")
data <- read.csv("descriptive.csv", header = TRUE)
head(data)

dim(data)    # 차원 보기 
# [1] 300   8
length(data) # 변수(칼럼) 길이 : 열
# [1] 8
str(data)
# 'data.frame':	300 obs. of  8 variables:
#   $ resident: int  1 2 NA 4 5 3 2 5 NA 2 ...
# $ gender  : int  1 1 1 2 1 1 2 1 1 1 ...
# $ age     : int  50 54 62 50 51 55 56 49 49 49 ...
# $ level   : int  1 2 2 NA 1 2 1 2 1 2 ...
# $ cost    : num  5.1 4.2 4.7 3.5 5 5.4 4.1 675 4.4 4.9 ...
# $ type    : int  1 1 1 1 1 1 1 NA 1 1 ...
# $ survey  : int  1 2 1 4 3 3 NA NA NA 1 ...
# $ pass    : int  2 2 1 1 1 NA 2 2 2 1 ...

summary(data)


# 명목척도 기술 통계량
length(data$gender)
summary(data$gender)
table(data$gender)   # table() = 빈도, 성별 빈도수 -> 0와 5 이상치 발견 
# 0   1   2   5 
# 2 173 124   1 
data <- subset(data, gender == 1 | gender == 2)
x <- table(data$gender)
x
barplot(x) # 범주형 데이터 시각화 -> 막대 그래프
prop.table(x) # 비율 계산 : 0 < x < 1 사이값값
y <- prop.table(x)
round(y * 100, 2)
# 1     2 
# 58.25 41.75 

# 예제  matrix 함수의 행렬에 대한 기준의 비교
m <- matrix(1:4, 2)
m
prop.table(m, 1)   # 행의 모든 합 = 1 
#           [,1]      [,2]
# [1,] 0.2500000 0.7500000
# [2,] 0.3333333 0.6666667
prop.table(m, 2)   # 열의 모든 합 = 1 
#           [,1]      [,2]
# [1,] 0.3333333 0.4285714
# [2,] 0.6666667 0.5714286


# 서열척도 기술통계량
length(data$level)
summary(data$level)
table(data$level) # 빈도분석: 의미 있음
# 1   2   3 
# 115  99  70
x1 <- table(data$level)
barplot(x1) # 명목/ 서열척도 -> 막대차트 



# 등간 척도 기술 통계량
# 등간척도 = 일정한 값을 갖는 변수를 의미
survey <- data$survey
survey
summary(survey)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 1.000   2.000   3.000   2.605   3.000   5.000     112 

x2 <- table(survey)   # 빈도수: 의미 있음
x2
# 1  2  3  4  5 
# 20 72 61 25  7

hist(survey)
pie(x2)



# 비율척도 기술 통계량
# 원점이 존재 사칙연산 가능 
length(data$cost)
summary(data$cost)  #  <- 요약통계량 - 의미 있음 (mean)
#     Min.  1st Qu.   Median     Mean  3rd Qu.     Max.     NA's 
# -457.200    4.400    5.400    8.784    6.300  675.000       30 
plot(data$cost)
data <- subset(data, data$cost >=2 & data$cost <= 10) # 생활비 기준
x3 <- data$cost
mean(x3)             # 평균 계산
# [1] 5.354032
median(x3)
sort(x)
sort(x, decreasing = T)  # 내림차순
# 사분위수 구하기 
quantile(x, 1/4 )
# 25% 
# 136.25 
quantile(x, 2/4 )
# 50% 
# 148.5 
quantile(x, 3/4 )
# 75% 
# 160.75 
quantile(x, 4/4 )
# 100% 
# 173 

length(x3)
x3.t <- table(x3)
max(x3.t)
x3.m <- rbind(x3.t)
class(x3.m)
# [1] "matrix"  "array"
str(x3.m)
which(x3.m[1, ] == 18) # 해당데이터 인덱스 찾기 18 = 최빈수 
# 5
# 19
x3.df <- as.data.frame(x3.m)
which(x3.df[1, ] == 18)
# [1] 19
x3.df[1, 19]
attributes(x3.df)


# 산포도 구하기 
var(x3)       # x의 분산
# [1] 1200.5
sd(x3)        # x의 표준편차
# [1] 1.138783
sqrt(var(data$cost, na.rm = T))
# [1] 1.138783


# 빈도분석 
table(data$cost)
hist(data$cost)
plot(data$cost)

# 연속형 변수 범주화
# cost 가 연속형 변수 이다, 이것을 구분짓는 범주화가 필요하다. 
# 칼럼을 하나더 생성해서 범주화 구간을 넣어 명시한다.
data$cost2[data$cost >= 1 & data$cost <= 3]  <- 1  
data$cost2[data$cost >= 4 & data$cost <= 6]  <- 2
data$cost2[data$cost >= 7] <- 3
table(data$c)
par(mfrow = c(1, 2))
barplot(table(data$cost2))
pie(table(data$cost2))


# 비대칭도 구하기 
# 범주화가 너무 많을때 시각화가 어려울때 외도와 청도를 사용한다. 
# 평균을 중심으로 기우려진 정도를 알수 있다. 
# 외도 = 0 , 평균 중심
# 외도 > 0 , 오른쪽으로 꼬리가 길다.
# 외도 < 0 , 왼쪽으로 꼬리가 길다.
# 청도 = 0 , 중간뾰족 
# 청도 > 0 , 뾰족
# 청도 < 0 , 완만한 곡선


install.packages("moments")
library(moments)
cost3 <- data$cost
skewness(cost3)
# [1] -0.297234 
hist(cost3)

hist(cost3, freq = F) # 히스토그램의 계급을 확률밀도로 표현
lines(density(cost3), col = 'blue')  # cost3의 밀도 분포 곡선 추가
x <- seq(0, 8, 0.1)                  # 0~8 범위에서 0.1 씩증가하는 벡터 생성
# 정규분포 확률 밀도 함수를 이용하여 정규분포 곡선 추가 
curve(dnorm(x, mean(cost3), sd(cost3)), col = 'red', add = T)

attach(data)    # data$ 을 생략하기 위함
length(cost3)  # 요약 통계량 의미 있음(mean)
# [1] 248
summary(cost3 )
mean(cost3)
range(cost3)

sqrt(var(cost, na.rm = T))
sd(cost, na.rm = T)
detach(data)   # attach(data)를 해제

test <- c(1:5, NA, 10:20)
min(test, na.rm = T)
max(test, na.rm = T)
range(test, na.rm = T)
mean(test, na.rm = T)

data$resident2[data$resident == 1] <- "특별시"
data$resident2[data$resident >= 2 & data$resident <=4] <- "광역시"
data$resident2[data$resident == 5] <- "시구군"

locost <- table(data$resident2)
locost

prop.table(locost)
locostest <- prop.table(locost)
round(locostest * 100, 2) # 백분율 적용 (소수 2자리)

data$gender2[data$gender == 1] <- "남자"
data$gender2[data$gender == 2] <- "여자"
gentest <- table(data$gender2)
prop.table(gentest)

round(gentest * 100, 2)

data$age2[data$age <= 45] <- "중년층"
data$age2[data$age >= 46 & data$age <= 59] <- "장년층"
data$age2[data$age >= 60] <- "노년츨"

agetest <- table(data$age2)
agetest

prop.table(agetest)
age.p <- prop.table(agetest )
round(age.p * 100, 2)

data$level2[data$level == 1] <- "고졸"
data$level2[data$level == 2] <- "대졸"
data$level2[data$level == 3] <- "대학원졸"
lever2.t <- table(data$level2)
lever2.t

prop.table(lever2.t)
lever2.p <- prop.table(lever2.t)
round(lever2.p * 100, 2)

data$pass2[data$pass == 1] <- "합격"
data$pass2[data$pass == 2] <- "실패패"
pass.b <- table(data$pass2)

prop.table(pass.b)
pass.p <- prop.table(pass.b)
round(pass.p * 100, 2)

head(data) # 리코딩 변수 보기 


