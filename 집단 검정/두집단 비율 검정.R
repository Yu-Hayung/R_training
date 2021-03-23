
# 두집단 비율 검정 
setwd("/Users/yuhayung/Desktop/coding/학원/Rtraining/dataset2")
data <- read.csv("two_sample.csv", header = T)
head(data)
x <- data$method
y <- data$survey

table(x)  # 교육 방법 1과 교육 방법 2 모두 150명 참여
# 1   2 
# 150 150 
table(y)  # 교육방법 만족 : 1 / 불만족 :0
# 0   1 
# 55 245 

table(x, y, useNA = "ifany")  # 빈도 검정 useNA = "ifany" 결측치까지 출력
#    y
# x     0   1
# 1    40 110
# 2    15 135

# 명목척도의 비율을 바탕으로 prop.test() 함수를 이용하여 두 집단간 
# 이항분포의 양측 검정을 통해서 검정 통계량을 구한 후 이를 이용하여 가설을 검정한다. 



prop.test(c(110,135), c(150,150))
# 첫번째 벡터 = PT 교육과 코딩 교육 방법에 대해 만족 수
# 두번째 벡터 = 교육방법에 대한 변량의 길이(시행횟수)
# X-squared = 12.824, df = 1, p-value = 0.0003422  <= 유의 수준 보다 작다 즉, 두 교육 방법 간의 만족도 차이가 있다고 볼 수 있다. 


# 즉, 두 가지 교육 방법에 따라 교육생의 민족율에 차이가 있다는 연구 가설이  채택된다. 

# 방향성을 갖는 단축 가설 검정
prop.test(c(110,135), c(150,150), alternative = "greater", conf.level = 0.95)
# X-squared = 12.824, df = 1, p-value = 0.9998
prop.test(c(110,135), c(150,150), alternative = "less", conf.level = 0.95)
# X-squared = 12.824, df = 1, p-value = 0.0001711

# 즉, 코딩교육 방법이 PT 교육방법보다 만족도가 더 크다고 볼 수 있다. 



          