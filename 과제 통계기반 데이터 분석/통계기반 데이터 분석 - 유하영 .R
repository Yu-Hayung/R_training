library(gmodels)


# 평가 문항 4 ##########
cleanData <- read.csv('cleanData.csv')
data <- cleanData

x <- data$position
y <- data$age3

plot(x,y)

CrossTable(x,y, chisq= T) # p =  1.548058e-57  귀무 가설 기 

# 귀무가설 = 나이와 직위가 관련성 없다 / 대립 가설 = 나이와 직와가 관련이 있다 
# 해설 : 나이와 직위의 관련성 검정 귀무 가설 기각으로 관련성 있다. 




# 평가 문항 5 ##########

response <- read.csv('response.csv')

response$job2[response$job == 1] <- '학생'
response$job2[response$job == 2] <- '직장인'
response$job2[response$job == 3] <- '주부'
response$response2[response$response == 1] <- 'ᄆ'
response$response2[response$response == 2] <- '낮음'
response$response2[response$response == 3] <- '높음'

table(response$job2, response$response2)

chisq.test(response$job2, response$response2)
#  p-value = 6.901e-12  유ᄋ 수준 보다 작다 귀무가설 기각 

# 귀무가설 = 직업과 만족도는 관련이 없다./ 대립 가서 = 직업과 만족도는 관련이 있다.
#해설 : job 과 response 차이가 있다. 


# 평가 문항 6 ##########
cardata <- mtcars

cary <- mtcars$vs
carx1 <- mtcars$mpg
carx2 <- mtcars$am

car_df <- data.frame(carx1, carx2, cary) 

car.glm <- glm(formula = cary ~carx1 + carx2, data = car_df, family = 'binomial')
summary(car.glm) 
car.glm

# 회귀 식 :  cary = -12.7051   +   (0.6809*carx1 )   +  (-3.0073 *carx2 ) 

odds = -0.89278   +   (0.6809*30 )   +  (-3.0073*0 ) ; odds
# 19.53422

# 평가 문항 7 ##########

att <- attitude
att.lm <- lm(formula = rating ~ ., data = att )

library('car')
vif(att.lm)

# complaints  privileges   learning    raises    critical    advance 
#   2.667060   1.600891    2.271043   3.078226   1.228109   1.951591 

sqrt(vif(att.lm))>2
# complaints privileges   learning     raises   critical    advance 
#    FALSE      FALSE      FALSE      FALSE      FALSE      FALSE 
# 공선성 문제 없음 = 독립변수 없음 

summary(att.lm) # p-value: 1.24e-05 회귀선 모델 저 

# 더ᄇ 왓슨으로 재확인 

library(lmtest)
dwtest(att.lm)   #  p-value = 0.2875 독립성 없음 

#회귀식 
# rating =  10.78708 + 0.61319*complaints + -0.07305*privileges +  0.32033*learning + 0.08173*raises + 0.03838*critical + -0.21706*advance 

cor(att)
#              rating complaints privileges  learning    raises  critical   advance
# rating     1.0000000  0.8254176  0.4261169 0.6236782 0.5901390 0.1564392 0.1550863
# rating 은 complaints 와 상관 관계가 크고 advance 와 작다 


