# 교육 수준과 흡연율 간의 관련성 분석 하기 

# 귀무 가설 : 교육 수준과 흡연율은 관련 차이가 없다 
# 연구 가설 : 교육 수준과 흡연율은 관련 차이가 있다. 

smoke <- read.csv("smoke.csv", header = T)
summary(smoke)

smoke$education2[smoke$education == 1] <- '대졸'
smoke$education2[smoke$education == 2] <- '고졸'
smoke$education2[smoke$education == 3] <- '중졸'

smoke$smoking2[smoke$smoking == 1] <- '꼴초'
smoke$smoking2[smoke$smoking == 2] <- '중꼴초'
smoke$smoking2[smoke$smoking == 3] <- '비흡연'

table(smoke$education2, smoke$smoking2)

CrossTable(smoke$education2,
           smoke$smoking2,
           chisq = T)

# p =  0.0008182573  유의함 - 귀무가설 기각 

#해설 = 교육 수준과 흡연율은 관련이 있다. 



#----------------------------------------------------------------#

data <- read.csv("cleanData.csv", header = T)

x <- data$position
y <- data$age3

plot(x, y)

CrossTable(x, y, chisq = T)
# p =  1.548058e-57           유의 하지 않다 - 귀무가설 채택 

# 해설 = 장년층 나이와 직위는 차이가 없다. 
