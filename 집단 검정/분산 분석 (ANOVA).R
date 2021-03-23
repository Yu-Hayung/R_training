# 분산 분석 (F- 검정 = ANOVA)

data <- read.csv("three_sample.csv", header = T)
head(data)
data <- subset(data, !is.na(score), c(method,score))  # NA, outlne 제거 
head(data)

par(mfrow = c(1,2))
plot(data$score)     # 산점도 - 이상치 시각화
barplot(data$score)  # 막대차트 - 이상치 시각화 
mean(data$score)     # 평균 통계량 [1] 14.44725

length(data$score)
# [1] 91  이상치 제거 전 
data2 <- subset(data, score <= 15)  # 15 이상 이상치 제거 
length(data2$score)
# [1] 88  이상치 제거 후 

#< 정제된 데이터 확인 >
x <- data2$score
par(mfrow = c(1,1))
boxplot(x)  
# 박스차트로 이상치 없음을 확인 

# < 세 집단 subset 작성과 기술 통계량 구하기 >
data2$method2[data2$method == 1]  <- "방법 1"
data2$method2[data2$method == 2]  <- "방법 2"
data2$method2[data2$method == 3]  <- "방법 3"

table(data2$method2)                  # 교육 방법별 빈도수
x <- table(data2$method2) ; x         # 교육 방법을 x 변수에 저장 
y <- tapply(data2$score, data2$method2, mean)

df <- data.frame(교육방법 = x, 성적 = y)
df 
# 교육 방법에 따른 시험 성적 평균 차표 
# 교육방법.Var1 교육방법.Freq     성적
# 방법 1        방법 1            31 4.187097
# 방법 2        방법 2            27 6.800000
# 방법 3        방법 3            30 5.610000



#< 동질성 검사 >  bartlett.test() 함수, 검정결과가 0.05보다 큰 경우 세집단 분포의 모양이 동질하다 할 수 있음.

bartlett.test(score ~ method, data = data2)
# Bartlett's K-squared = 3.3157, df = 2, p-value = 0.1905

result <- aov(score ~ method2, data = data2)
names(result)
summary(result)
#              Df Sum Sq Mean Sq F value   Pr(>F)    
# method2      2  99.37   49.68   43.58 9.39e-14 ***
# Residuals   85  96.90    1.14                     
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#  p-value 값이 9.39e-14 으로 유의 수준 0.05보다 매우 작기 때문에 세 가지 교육 방법간의 평균에 차이가 있다
# 즉, 연구가설 = "교육 방법에 따른 세 집단 간 실기 시험의 평균에 차이가 있다." 유의하다.


# < 사후 검정 - 시각화>
install.packages("extrafont")
library(extrafont)
TukeyHSD(result)
plot(TukeyHSD(result))





