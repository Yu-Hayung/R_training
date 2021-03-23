
# 두집단 평균 검정 
setwd("/Users/yuhayung/Desktop/coding/학원/Rtraining/dataset2")
data <- read.csv("two_sample.csv", header = T)
summary(data)
# result <- subset(data, !is.na(score), c(method, score))
# !is.na(score)    =  na 가 아닌것만 추출 
# c(method, score) = data 전체 변수 중 두 변수만 추출    둘중 하나만 택 
result <- subset(data, !is.na(score))

a <- subset(result, method == 1)
b <- subset(result, method == 2)
# 교육 방법 별로 분리 
a1 <- a$score
b1 <- b$score
# 교육 방법에서 점수 추출

#<기술 통계량>
length(a1)    # [1] 109
length(b1)    # [1] 118
mean(a1)      # [1] 5.556881
mean(b1)      # [1] 5.80339

#<동질성 검사>
var.test(a1, b1)
# F = 1.2158, num df = 108, denom df = 117, p-value = 0.3002
# 두 집단간의 분포형태가 동질하다고 볼 수 있다. 

# 두 집단 평균 차이 검정 <양측 검정>
t.test(a1, b1, alter = "two.sided", conf.int = T, conf.level = T)
# t = -2.0547, df = 218.19, p-value = 0.0411

t.test(a1, b1, alter = "greater", conf.int = T, conf.level = T)
# t = -2.0547, df = 218.19, p-value = 0.9794
t.test(a1, b1, alter = "less", conf.int = T, conf.level = T)
# t = -2.0547, df = 218.19, p-value = 0.02055


# 프레젠테이션 교육 방법과 실시간 코딩 교육 방법 간 실기 점수의 평균에 차이가 있는지를 검정하기 
# 위해서 95%신뢰 수준에서 양측 검정을 시행한 결과 검정통계량 p - value 값은 0.0411로 유의 수준 0.05 보다 작기 때문에 
# 두 집단 간의 평균에 차이가 있는 것으로 나타났다. 
# 또한 방향성을 갖는 연구 가설을 수행한 결과 a1 집단의 편균이 b1 집단의 평균보다 더 작은 것으로 나타 났다. 
# 따라서 "교육방법에 따른 두 집단 간 실기 시험의 평균에 차이가 있다." 는 연구 가설이 채택된다.




