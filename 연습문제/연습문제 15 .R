# 만족도 문제 ##############################################################################
product <- read.csv("product.csv", header=TRUE)

idx <- sample(1:nrow(product), 0.7*nrow(product))
train <- product[idx,] 
test <- product[-idx, ] 
dim(test) 

model <- lm(formula=제품_만족도 ~ 제품_적절성 + 제품_친밀도, data=train)
summary(model) 

pred <- predict(model, test)
cor(pred, test$제품_만족도) 



# 다이아몬드 문제 #################################################################
library(ggplot2)
data(diamonds)
formula <- price ~ carat + table + depth

result <- lm(formula, data=diamonds)
summary(result)

# carat은 price에 정(+)의 영향을 미치지만, table과 depth는 부(-)의 영향을 미친다

# 엔진 무제 ########################################################################

library(ggplot2)
data(mpg)
str(mpg)

idx <- sample(1: nrow(mpg), nrow(mpg) * 0.7)
train <- mpg[idx, ] 
test <- mpg[-idx, ] 

formula <- cty ~ displ + cyl + year

mpg_train <- ctree(formula, data=train)
mpg_test <- ctree(formula, data=test)
plot(mpg_test)

# 실린더가 5이하이면 엔진크기에 의해서 23개가 분류되고, 
# 실 5이상이고, 6이하이면 27개가 분류되고,
# 6을 초과한 경우 21개가 분류된다. 


# 날씨 데이터 문제  ###################################################################
library(rpart)
weather = read.csv("weather.csv", header=TRUE)

weather.df <- weather[, c(-1,-14)]
nrow(weather.df)
idx <- sample(1:nrow(weather.df), nrow(weather.df)*0.7)
weather_train <- weather.df[idx, ]
weather_test <- weather.df[-idx, ]

weather_model <- rpart(RainTomorrow ~ ., data = weather.df)
weater_pred <- predict(weather_model, weather_test)

weater_class <- ifelse(weater_pred[,1] >=0.5, 'No Rain', 'Rain')

table(weater_class, weather_test$RainTomorrow)
(87 + 12) / nrow(weather_test)
# [1] 0.9


