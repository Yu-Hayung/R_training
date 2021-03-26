setwd("/Users/yuhayung/Desktop/coding/학원/Rtraining/dataset2")

data1 <- read.csv("descriptive.csv", header = TRUE)
dim(data1)   # 차원 보기 
length(data1) # 열의 개수 
length(data1$survey)
str(data1)
summary(data1)


############################# 명목척도 (성별) 빈도 구하여 백분율로 통계 하기 ###################################
data1 <- subset(data1,gender == 1 | gender == 2)  # 이상치 제거 
data1$gender
gender <- table(data1$gender)
t_gender <- prop.table(gender)   # 빈도수 
round(t_gender * 100 , 2)  # 백분율 적용 

############################## 서열척도 (학력수준) 빈도 구하여 시각화 통계 하기 ################################
length(data1$level)
summary(data1$level)
t_level <- table(data1$level) # 빈도수 
barplot(t_level)  # 막대 차트 

############################## 등간척도 (시설 만족도) 빈도 구하여 시각화 통계 하기 #############################
data1$survey
summary(data1$survey)
t_survey <- table(data1$survey)
hist(t_survey)       # 만족도가 낮았다 
pie(t_survey)

############################## 비율척도 (생활비)  통계 하기 ####################################################
length(data1$cost)
summary(data1$cost)  # 평균 
mean(na.rm=T, data1$cost)
plot(data1$cost)      # 이상치 시각화 
data1 <- subset(data1, data1$cost >= 2 & data1$cost <= 10)
m_cost <- mean(data1$cost)
mean(m_cost)
median(m_cost)
table(data1$cost)
hist(data1$cost) # 빈도에 따른 시각화 
data1$cost2[data1$cost >= 1 & data1$cost <= 3] <- 1   # 범주화 
data1$cost2[data1$cost >= 4 & data1$cost <= 6] <- 2
data1$cost2[data1$cost >= 7] <- 3
table(data1$cost2)
par(mfrow = c(1,2))
barplot(table(data1$cost2))   # 시각화   즉, 중산층 고객이 많이 탑승했다. 
pie(table(data1$cost2))
