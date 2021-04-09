# 로지스틱 회귀분석 

weather = read.csv("weather.csv", stringsAsFactors = F)
dim(weather)
head(weather)


# 더미변수 생성
weather_df <- weather[ , c(-1, -6, -8, -14)]
weather_df

weather_df$RainTomorrow[weather_df$RainTomorrow == 'Yes'] <- 1
weather_df$RainTomorrow[weather_df$RainTomorrow == 'No'] <- 0

weather_df$RainTomorrow <- as.numeric(weather_df$RainTomorrow) 

head(weather_df)

idx <- sample(1:nrow(weather_df), nrow(weather_df) *0.7)
train <- weather_df[idx, ]
test  <- weather_df[-idx, ] 


# 로지스틱 회귀모델 생성 
weather_model <- glm(RainTomorrow ~ ., data = train, family = 'binomial', na.action = na.omit)
weather_model

# Call:  glm(formula = RainTomorrow ~ ., family = "binomial", data = train)
# 
# Coefficients:
#   (Intercept)        MinTemp        MaxTemp       Rainfall       Sunshine  WindGustSpeed      WindSpeed  
# 116.12197       -0.16243        0.24979        0.02373       -0.26502        0.07587       -0.06003  
# Humidity       Pressure          Cloud           Temp  
# 0.06601       -0.12324        0.11326       -0.01331  
# 
# Degrees of Freedom: 250 Total (i.e. Null);  240 Residual
# (5 observations deleted due to missingness)
# Null Deviance:	    245 
# Residual Deviance: 151.4 	AIC: 173.4

summary(weather_model)

# Call:
#   glm(formula = RainTomorrow ~ ., family = "binomial", data = train)
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -2.0388  -0.4706  -0.2791  -0.1296   2.5667  
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)   
# (Intercept)   116.12197   46.52178   2.496  0.01256 * 
#   MinTemp        -0.16243    0.07555  -2.150  0.03155 * 
#   MaxTemp         0.24979    0.22031   1.134  0.25689   
# Rainfall        0.02373    0.04615   0.514  0.60720   
# Sunshine       -0.26502    0.11420  -2.321  0.02031 * 
#   WindGustSpeed   0.07587    0.02752   2.757  0.00584 **
#   WindSpeed      -0.06003    0.03729  -1.610  0.10743   
# Humidity        0.06601    0.02778   2.376  0.01749 * 
#   Pressure       -0.12324    0.04499  -2.739  0.00616 **
#   Cloud           0.11326    0.11136   1.017  0.30913   
# Temp           -0.01331    0.22956  -0.058  0.95377   
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 244.98  on 250  degrees of freedom
# Residual deviance: 151.36  on 240  degrees of freedom
# (5 observations deleted due to missingness)
# AIC: 173.36
# 
# Number of Fisher Scoring iterations: 6


# 로지스틱 회귀 모델 예측기 생성 

pred <- predict(weather_model, newdata = test, type = 'response')
pred

# 시그모이드 함수 : 0.5 기준 -> 비 유무 판단
result_pred <- ifelse(pred >= 0.5, 1, 0)
result_pred

table(result_pred)


# 모델 평가 - 분류정확도 계산 
table(result_pred, test$RainTomorrow)



#install.packages("ROCR")
library(ROCR)

pr <- prediction(pred, test$RainTomorrow)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

