# 상관관계 분석 

# 단순 회귀 분석 #################################################################

product <- read.csv("product.csv", header = T)
str(product)

y = product$제품_만족도
x = product$제품_적절성 

df <- data.frame(x, y)

result.lm <- lm(formula = y ~x, data = df )
result.lm


# 적합값과 잔차 보기 
# 적합값 보기 
fitted.values(result.lm)[1:2]

head(df, 1)

# 회귀방정식을 적용하여 모델의 적합값 계산

Y = 0.7789 + 0.7393 *4
Y  # [1] 3.7361


# 자차 오차 구하기 
3 - 3.735963 
# -0.735963


residuals(result.lm)[1:2]
# -0.7359630 -0.9966869


- 0.7359630 + 3.735963
# [1] 3 

# 단순 회귀 분석/ 선형회궈분석 시각화#########################################

plot(formals = y ~ x, data = product)
result.lm <- lm(formula = y ~ x, data = product)
abline(result.lm, col = 'blue')
 
# 단순 회귀 분석/  선형회귀 분석 결과보기 #####################################

summary(result.lm)


# Call:
#   lm(formula = y ~ x, data = product)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -1.99669 -0.25741  0.00331  0.26404  1.26404 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.77886    0.12416   6.273 1.45e-09 ***
#   x            0.73928    0.03823  19.340  < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.5329 on 262 degrees of freedom
# Multiple R-squared:  0.5881,	Adjusted R-squared:  0.5865 
# F-statistic:   374 on 1 and 262 DF,  p-value: < 2.2e-16


# 다중회귀분석 ########################################################

y = product$제품_만족도  # 종속 변수 
x1 = product$제품_친밀도 # 독립 변수
x2 = product$제품_적절성 # 독립 변수 

df <- data.frame(x1, x2)

result.lm <- lm(formula = y ~ x1 + x2, data =df)
result.lm


# 다공산성 문제 확인 (분산의 팽창 요인값 확인)

install.packages("car")
library(car)


vif(result.lm)
# x1       x2 
# 1.331929 1.331929    
# 공분산성 10 이상이면 의심해 볼수 있디만 2미만으로 공분산성이 아니다.

summary(result.lm)


# Call:
#   lm(formula = y ~ x1 + x2, data = df)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -2.01076 -0.22961 -0.01076  0.20809  1.20809 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.66731    0.13094   5.096 6.65e-07 ***
#   x1           0.09593    0.03871   2.478   0.0138 *  
#   x2           0.68522    0.04369  15.684  < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.5278 on 261 degrees of freedom
# Multiple R-squared:  0.5975,	Adjusted R-squared:  0.5945 
# F-statistic: 193.8 on 2 and 261 DF,  p-value: < 2.2e-16

# 다중 공분산성 문제 해결과 모델 성능 평가 ######################################

data(iris)

model <- lm(formula = Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width, data = iris)

vif(model)
# Sepal.Width Petal.Length  Petal.Width 
# 1.270815    15.097572    14.234335     <- 10 이 넘은 공분산성 값

sqrt(vif(model)) > 2 

cor(iris[ , -5])
 

# 회귀모델 생성 

x <- sample(1:nrow(iris), 0.7 * nrow(iris))
train <- iris[x, ]   # 7
test  <- iris[-x, ]  # 3


# 공분산성 값이 큰  Petal.Width  값을 삭제

model2 <- lm(formula = Sepal.Length ~ Sepal.Width + Petal.Length, data = train )
model2

# Call:
#   lm(formula = Sepal.Length ~ Sepal.Width + Petal.Length, data = train)
# 
# Coefficients:
#   (Intercept)   Sepal.Width  Petal.Length  
#        2.0465        0.6727        0.4629  



head(train, 1)
# Sepal.Length Sepal.Width Petal.Length Petal.Width   Species
# 133          6.4         2.8          5.6         2.2 virginica

Y2 = 2.0465 + 0.6762 *2.8 +  0.4629 * 5.6 
Y2

2.0465 - Y2   # [1] -4.4856


# 다중 공분산성 제거/ 회귀모델 평가 ####################################################

pred <- predict(model, test)
pred

cor(pred, test$Sepal.Length)
# 결과 > [1] 0.92662 매우 높은 상관관계를 보여준다. 


 

# 기본 가정 충족으로 회귀분석 수행 #############################################

furmula = Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width
model3 <- lm(formula =  formula, data = iris)
model3


# 오차 분석 

install.packages('lmtest')
library(lmtest)
dwtest(model)

# 등분산성 검정 - 잔차와 적합값의 분포
plot(model, which = 1)


# 잔차의 정규성 검정
attributes(model)

res <- residuals(model)
shapiro.test(res)


par(mfrow = c(1, 2))
hist(res, freq = F)
qqnorm(res)


library(car)
sqrt(vif(model)) > 2



formula = Sepal.Length ~ Sepal.Width + Petal.Length 
model4 <- lm(formula =  formula, data = iris)
summary(model)

# Call:
#   lm(formula = Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width, 
#      data = iris)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.82816 -0.21989  0.01875  0.19709  0.84570 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   1.85600    0.25078   7.401 9.85e-12 ***
#   Sepal.Width   0.65084    0.06665   9.765  < 2e-16 ***
#   Petal.Length  0.70913    0.05672  12.502  < 2e-16 ***
#   Petal.Width  -0.55648    0.12755  -4.363 2.41e-05 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.3145 on 146 degrees of freedom
# Multiple R-squared:  0.8586,	Adjusted R-squared:  0.8557 
# F-statistic: 295.5 on 3 and 146 DF,  p-value: < 2.2e-16


