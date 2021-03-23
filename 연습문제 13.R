# 1. 중소기업에서 생산한 HDTV 판매율을 높이기 위해서 프로모션을 진행한 결과 기존 구매비율보다 15% 향상되었는지 단계별로 분석을 수행하여 검정하시오. 
# 귀무 가설 : 기존 구매 비율보다 15% 차이 없다. 
# 연구 가설 : 기존 구매 비율보다 15% 차이 있다. 

setwd("/Users/yuhayung/Desktop/coding/학원/Rtraining/dataset2")
data <- read.csv("hdtv.csv", header = T)
summary(data)  # 이상값 없음 
length(data$buy)   
# 50 
table(data$buy) # 구매 여부 1 : 구매 안함 / 2 : 구매 함 
# 1: 40, 2: 10 
library(prettyR)
freq(data$buy)
# 비율: 구매안함 80, 구매함 20
binom.test(c(10,40), p = 0.15)
# number of successes = 10, number of trials = 50, p-value = 0.321 <= 유의 안함 

# 결론 프로모션 진행으로 인해 구매비율이 15% 차이 없다. 


binom.test(c(10,40), p=0.15, alternative="greater", conf.level=0.95) 
binom.test(c(10,40), p=0.15, alternative="less", conf.level=0.95)

# number of successes = 10, number of trials = 50, p-value = 0.2089  <= 기각
# number of successes = 10, number of trials = 50, p-value = 0.8801  <= 기각

binom.test(c(10,40), p=0.14, alternative="greater", conf.level=0.95) #  p-value = 0.1537
binom.test(c(10,40), p=0.13, alternative="greater", conf.level=0.95) #  p-value = 0.1074
binom.test(c(10,40), p=0.12, alternative="greater", conf.level=0.95) #  p-value = 0.07076
binom.test(c(10,40), p=0.11, alternative="greater", conf.level=0.95) #  p-value = 0.04345

# 결론 : 프로모션을 진행한 결과 기존 구매비율보다 11% 향상 되었다.


########################################

# 2. 우리나라 전체 중학교 2학년 여학생 평규 키가 148.5cm 로 알려진 상태에서 A중학교 2학년 전체 500명을 
# 대상으로 10%인 50명을 표본으로 선정하여 표본평균 신장을 계산하고 모집단의 평균과 차이가 있는지를 단계별로 
# 분석을 수행하여 검정하시오.
# 귀무 가설 : 표본 평균 신장 148.5cm가 차이가 없다.
# 연구 가설 : 표본 평균 신장 148.5cm가 차이가 있다.

setwd("/Users/yuhayung/Desktop/coding/학원/Rtraining/dataset2")
stheight <- read.csv("student_height.csv", header = T)
str(stheight)
summary(stheight$height)   # 이상치 없음 
length(stheight$height)    # 50개
x<-(stheight$height)
x
mean(x, na.rm = T) # (이상치가 없지만) 이상치 있을시 평균으로 취함
#  149.4

shapiro.test(x)
# 정규분포 여부를 본다 
# W = 0.88711, p-value = 0.0001853 <= 정규분포가 아니다.  

# 정규분포(모수검정) - t.test()
# 비정규분포(비모수검정) - wilcox.test()

wilcox.test(x, mu=148.5)
# V = 826, p-value = 0.067  <= 유의 하지 않다. 귀무가설 채택 



##########################################################################

# 03. 대학에 진학한 남학생과 여학생을 대상으로 진학한 대학에 대해서 만족도에 차이가 있는가를 검정하시오. 
# 귀무 가설 : 성별에 따른 만족도 차이가 없다.
# 연구 가설 : 성별에 따른 만족도 차이가 있다.


data2 <- read.csv("two_sample.csv", header=TRUE)
data2
head(data2) # 변수명 gender, survey

gender <- data2$gender
survey <- data2$survey

table(gender)   # (1 : 174, 2 : 126)
table(survey)   # (0 : 55, 1 : 245)
table(gender, survey, useNA = "ifany")   # 결측치 출력 

# prop.test(gender, survey)
prop.test(c(138,107),c(174,126)) # 만족도 차이 검정 
# X-squared = 1.1845, df = 1, p-value = 0.2765 <= 만족도 차이가 없다. 



##########################################################################

# 04. 교육방법에 따라 시험성적에 차이가 있는지 검정하시오.
# 귀무 가설 : 교육방법에 따른 성적 차이가 없다.
# 연구 가설 : 교육방법에 따른 성적 차이가 있다.

edumethod <- read.csv("twomethod.csv", header=TRUE)
head(edumethod)

result <- subset(edumethod, !is.na(score), c(method, score)) ; result
# 데이터 전처리(score의 NA 처리)

a <- subset(result,method==1)
b <- subset(result,method==2)

a1 <- a$score
b1 <- b$score

length(a1); # 22
length(b1); # 35

var.test(a1, b1) 
# F = 1.0648, num df = 21, denom df = 34, p-value = 0.8494 : 차이가 없다. 
# 동질성 분포와 차이가 없다. 
# 모수검정 방법 수행

t.test(a1, b1)
# t = -5.6056, df = 43.705, p-value = 1.303e-0 <= 귀무가설 기각  
t.test(b1, a1, alter="greater", conf.int=TRUE, conf.level=0.95) 
# t = 5.6056, df = 43.705, p-value = 6.513e-07 
t.test(b1, a1, alter="less", conf.int=TRUE, conf.level=0.95) 
# t = 5.6056, df = 43.705, p-value = 1 

# b1 교육 방법이 a1 교육방법 보다 시험성적이 더 좋다.


