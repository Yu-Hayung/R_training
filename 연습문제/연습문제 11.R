# 1번 문제 
# MASS 패키지에 있는 Animals 데이터 셋을 이용하여 각 단계에 맞게 기술 통계량을 구하시오 
# [1단계] MASS 패키지 설치와 메모리 로딩 
library(MASS)
data(Animals)
head(Animals)
# [2단계] R의 기본 함수를 이용하여 brain 칼럼을 대상으로 다음의 제시된 기술통계량 구하기 
# Animals 데이터 셋 차원 보기 

summary(Animals$brain)
mean(Animals$brain)
median(Animals$brain)
sd(Animals$brain)
var(Animals$brain)
min(Animals$brain)
max(Animals$brain)



# 2번 문제 
# descriptive.csv 데이터 셋을 대상으로 다음 조건에 맞게 빈도분석 및 기술통계량 분석을 수행하시오.
# 조건 1. 명목척도 변수인 학교유형(type), 합격여부(pass) 변수에 대해 빈도분석으로 수행하고 결과를 막대 그래프와 파이 차트로 시각화
# 조건 2. 비율척도 변수인 나이 변수에 대해 요약치(평균, 표준편차)와 비대칭도(왜도와 첨도)통계량을 구하시오, 히스토그램 작성하여 비대칭도 통계량 설명
# 조건 3. 나이변수에 대한 밀도분포 곡선과 정규분포 곡선으로 정규분포 검정 

setwd("C:/Users/tj-bu/Desktop/R/dataset2")
t.data <- read.csv("descriptive.csv", header = TRUE)

length(t.data$type)
summary(t.data$type)
type.t <- table(t.data$type)

barplot(type.t)
pie(type.t)

length(t.data$pass)
summary(t.data$pass)
pass.t <- table(t.data$pass)

barplot(pass.t)
pie(pass.t)

# -------------------------

length(t.data$age)
summary(t.data$age)
sd(t.data$age)

agetest <- t.data$age
agetest

range(agetest)
mean(agetest)
sd(agetest)
skewness(agetest)
kurtosis(agetest)
hist(agetest)
# 왜도가 0보다 조금 크기 때문에 오른쪽 방향으로 비대칭 꼬리가 치우치고, 첨도는 3보다 작기 때문에 표준정규분포보다 완만한 형태를 갖는다.


#-------------------------------------------------
hist(agetest, freq = F)
lines(density(agetest), col='blue')
x <- seq(35, 80, 0.1)
curve(dnorm(x, mean(agetest), sd(agetest)), col='red', add = T)

