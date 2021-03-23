
# 모평균간 구간 추정
표본학생수 = 10000
N <- 표본학생수
학생평균키 = 165.1
X <- 학생평균키
표준편차 = 2
S <- 표준편차

low <- X - 1.96 * S / sqrt(N) 
high <- X + 1.96 * S / sqrt(N)
low; high
# [1] 165.0608
# [1] 165.1392


# 신뢰구간으로 표본오차 구하기
high - X
# [1] 0.0392

(low-X) * 100
# [1] -3.92

(high-X) * 100
# [1] 3.92


# 단일 집단 검정 
# 귀무 가설 - 기존 2019년도 고객 불만족과 2020년도 CS 교육 후 불만율에 차이가 없다.
# 대립 가설 - 기존 2019년도 고객 불만족과 2020년도 CS 교육 후 불만율에 차이가 있다.


setwd("/Users/yuhayung/Desktop/coding/학원/Rtraining/dataset2")
data <- read.csv("one_sample.csv", header = T)
head(data)
x <- data$survey
summary(x)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.0000  1.0000  1.0000  0.9067  1.0000  1.0000 
length(x)
# [1] 150
table(x)   
#0 :불만족, 1 : 만족
# 0   1 
# 14 136 

install.packages("prettyR")
library(prettyR)
freq(x)  # 단일 집단을 대상으로 기술 통계량을 구한다. 

# Frequencies for x 
# 1    0   NA
# 136   14    0
# %    90.7  9.3    0 
# %!NA 90.7  9.3 



# 이항분포 비율 검정 
# binom.test(x,n,p=, alternative = "two.sided":양측검정/ "less" 좌측검정 / "greater" 우측검정, conf.level = 0.95 신뢰수준)
# 예 binom.test(14, 150, p =20)

binom.test(14, 150, p= 0.2) # 기존 20% 불만율을 기준으로 검정 실시 
binom.test(14, 150, p= 0.2, alternative = "two.sided", conf.level = 0.95)

binom.test(14, 150, p= 0.2, alternative = "greater", conf.level = 0.95)
binom.test(14,150, p= 0.2, alternative = "less", conf.level = 0.95)
# 전체 150명 중에 14명의 불만족 고객이 전체 비율의 20% 보다 적은 비율인가를 위한 속성

