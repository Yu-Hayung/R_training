
# 대응 두 집단 평균 검정 ( 대응표본 T-test) 

setwd("/Users/yuhayung/Desktop/coding/학원/Rtraining/dataset2")
data <- read.csv("paired_sample.csv", header = T)

result <- subset(data, !is.na(after), c(before, after)) # subset 작성 
x <- result$before
y <- result$after
                
x; y

length(x) # [1] 96
mean(x)   # [1] 5.16875

length(y) # [1] 96
mean(y)   # [1] 6.220833

# 기술통계량을 계산할 때, 대응 표본이면 서로 짝을 이루고 있기 때문에 서로 표본 수가 같아야 한다.

var.test(x, y, paired =T)
# F = 1.0718, num df = 95, denom df = 95, p-value = 0.7361 기무가설 채택 
# 동질성 을 알아보는 함수 var.tet() 유의수준 0.05 보다 큰 경우 두 집단 간 분포의 모양이 동질하다고 할 수 있다. 
# 동질성 검정의 귀무가설 : 두 집단 간 분포의 모양이 동질 적이다. 
        
t.test(x, y, paired =T, alternative = "two.sided", conf.int= T, conf.level = 0.95)
# t = -13.642, df = 95, p-value < 2.2e-16
t.test(x, y, paired =T, alternative = "greater", conf.int= T, conf.level = 0.95)
# t = -13.642, df = 95, p-value = 1
t.test(x, y, paired =T, alternative = "less", conf.int= T, conf.level = 0.95)
# t = -13.642, df = 95, p-value < 2.2e-16


# 교수법 프로그램을 적용하기 전의 시험성적과 교수법 프로그램을 적용한 후 시험 성적의 평균에 차이가 있는지를 검정하기 위해서 
# 95% 신뢰수준에서 양측 검정을 시행한 결과 검정 통계량 p-value 값은 2.2e-16로 유의 수준 0.05보다 매우 작기 때문에 두집단 간의 평균에 차이가 있는 것으로 나타났다.
# 따라서 "교수법 프로그램을 적용하기 전 학생들의 학습력과 교수법 프로그램을 적용한 후 학새들의 학습력에 차이가 있다."