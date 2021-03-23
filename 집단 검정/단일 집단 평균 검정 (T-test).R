
# 단일 집단 평균 검정(단일 표본 T 검정)
setwd("/Users/yuhayung/Desktop/coding/학원/Rtraining/dataset2")
data <- read.csv("one_sample.csv", header = T)
str(data)   # 150
head(data)
x<- data$time
head(x)
summary(x)
mean(x)
mean(x,na.rm = T)  # 데이터 정제 
x1 <- na.omit(x)   # na 데이터 (omit) 빼기
mean(x1)


# 정규분포 검정
# 귀무가설 - x의 데이터 분포는 정규분포이다.
shapiro.test(x1)    # x1 에 대한 정규분포 검정

#             Shapiro-Wilk normality test
# 
# data:  x1
# W = 0.99137, p-value = 0.7242
# p 벨류값이 유의 수준 보다 크다 즉, 정규분포를 따른다. 따라서 T 검정으로 평균 차이 검정을 수행한다.

# 정규분포 시각화
par(mfrow = c(1,2))
hist(x1) 
qqnorm(x1)
qqline(x1, lty = 1, col = "blue" )


# 평균 차이 검정 
# t- test (x, y = NULL, alternative = c("two.sided"/"less"/"greater"), mu = 0, paired = F, var.equal = F, conf.level = 0.95, ...)

t.test(x1, mu = 5.2)   # mu 모집단의 평균값

# 	One Sample t-test
# data:  x1
# t = 3.9461, df = 108, p-value = 0.0001417  <=  p-value 유의수준 0.05 보다 작기 때문에 귀무가설 채택
# alternative hypothesis: true mean is not equal to 5.2
# 95 percent confidence interval:
#   5.377613 5.736148
# sample estimates:
#   mean of x 
# 5.556881 

t.test(x1, mu = 5.2, alternative = "greater", conf.level = 0.95)

# 	One Sample t-test
# data:  x1
# t = 3.9461, df = 108, p-value = 7.083e-05 <=  p-value 유의수준 0.05 보다 매우 작기 때문에 채택 
# alternative hypothesis: true mean is greater than 5.2
# 95 percent confidence interval:
#   5.406833      Inf
# sample estimates:
#   mean of x 
# 5.556881 

qt(0.05, 108, lower.tail = F) # 귀무가설 임계값 확인 
# [1] 1.659085


# 불만족 고객 14명을 대상으로 95% 신뢰 수준에서 양측 검정을 시행한 결과 검정 통계량 p- value 값은 0.0006735로 유의 수준 0.05 보다 
# 작아 기존 불만율 20%과 차이가 있다고 볼 수 있다. 즉, 기존 2019년도 고객 불만율과 2020년도 CS 교육 후 불만율에 차이가 있다고 볼 수 있다. 
# 
# 하지만 양측 검정 결과에서는 기존 불만율보다 크다, 혹은 작다는 방향성은 제시되지 않는다.
# 따라서 방향성을 갖는 단측 가설 검정을 통해서 기존 집단과 비교하여 신규 집단의 불만율이 개선되었는지를 확인해야 한다.
