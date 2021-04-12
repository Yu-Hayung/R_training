# 시계열 분석 

# 데이터 불러오기 
data(AirPassengers)

# 차분 적용 평균 정상화 보기 
par(mfrow = c(1, 2))
ts.plot(AirPassengers)
diff<- diff(AirPassengers)
plot(diff)

# 로그 적용 평균 정상화 보기
par(mfrow = c(1, 2))
ts.plot(AirPassengers)
log<- diff(log(AirPassengers))
plot(log)



# 단일 시계여 자료 시각화 ############################################

data(WWWusage)
str(WWWusage)
WWWusage


# 시계열 자료 추세선 시각화 
X11()
ts.plot(WWWusage, type = "l", col = "blue")

# 다중 시계열 자료 시각화 #############################################

data("EuStockMarkets")
head(EuStockMarkets)

EuStock <- data.frame(EuStockMarkets)
head(EuStock)

# 단일 시계ᄋ 자료 추세선 시각화 (1,000개 데이터 대상)
X11()
plot(EuStock$DAX[1:1000], type = 'l', col = 'blue')

plot.ts(cbind(EuStock$DAX[1:1000], EuStock$SMI[1:1000]), main = "주가지수 ", family="NanumGothicBold" )


# 시계열 요소 분해 시각화 #############################################

data <- c(45, 56, 45, 43, 69, 75, 85, 59, 66, 64, 62, 65, 55, 49, 67, 55, 71, 78, 71, 65, 69, 43, 70, 75, 56, 56, 65, 55, 82, 85, 75, 77, 77, 69, 79, 89)
length(data)

tsdata <- ts(data, start = c(2016, 1), frequency = 12)
tsdata

# 추세건 확인 
ts.plot(tsdata)
plot(stl(tsdata, "periodic")) #stl() : 요소, 추세,  모두 제공 

m <- decompose(tsdata) # decompose() : 시계열 분해 

attributes(m)

plot(m)     # 추세요인. 계절요인, 불규칙 요인 그래ᄑ 

par(mfrow = c(3, 1))
plot(tsdata - m$seasonal)  # 계절 요인 제거 
plot(tsdata - m$trend)  # 추세요인 제거 그래프 
plot(tsdata - m$seasonal - m$trend) # 불규칙 요 출력 



# 자기 상관 함수 / 부분 자기 상관 함수 시각화 

input <- c(3180, 3000, 3200, 3100, 3300, 3200, 3400, 3550, 3200, 3400, 3300, 3700)
length(input)

tsdata2 <- ts(input, start = c(2015, 2), frequency = 12)   

ts.plot(tsdata2)
plot(stl(tsdata2, "periodic"))


par(mfrow = c(2, 1), family = "NanumBarunGothicBold")
acf(na.omit(tsdata2), main = "자기상관함수") # 자기 상관 함수 시각화

pacf(na.omit(tsdata2), min = "부분 자기 상관함수") # 부분 자기 상관 함수 시각화 

# ᄎ 패턴 찾기 시각화 ########################################################
# 추세패턴이란 시계열 자료가 증가 또는 감소하느 경향이 있는지 알아보고
# 증가 또는 감소하는 경향이 성현 인지 비선형 인지를  . 
# 추세 패턴의 객관적인 근거는 차분과 자기상관성을 통해서 얻을 수 있는데 여ᄀ  현재 시점에서 이전 시점의 자료를 빼는 연산을 의미 한다. 

input2 <- c(3180, 3000, 3200, 3100, 3300, 3200, 3400, 3500, 3200, 3400, 3300, 3700)
tsdata2 <- ts(input2, start = c(2015, 2), frequency = 12)



par(mfrow = c(3, 1), family = "NanumBarunGothicBold")
plot(tsdata2, type = "l",  main = "tsdata2 추세선")
acf(na.omit(tsdata2), main = "자기상관하수")
plot(diff(tsdata2, difference = 1), main = "차분 시각화 ") 

# 시계열 분석 기법 (지수평활법) ###############################################

# 지 
data3 <- c(45, 65, 45, 43, 69, 75, 58, 59, 66, 64, 62, 65, 55, 49, 67, 55, 71, 78, 71, 65, 69, 43, 70, 75, 56, 56, 65, 55, 82, 85, 75, 77, 77, 69, 79, 89)
length(data3)

tsdata3 <- ts(data3, start = c(2016, 1), frequency = 12)

#install.packages("TTR")
library(TTR)

par(mfrow = c(2,2), family = "NanumBarunGothicBold")
plot(tsdata3, main="원 시계열 자료")
plot(SMA(tsdata3, n=1), main = "1년 단위 이동평균법으로 평")
plot(SMA(tsdata3, n=2), main = "2년 단위 이동평균법으로 평활")
plot(SMA(tsdata3, n=3), main = "3년 단위 이동평균법으로 펴활")

# 회귀 분석법 
# ARIMA 모형법 
# 정상성 시계열 비 계절형 #####################################################

input3 <- c(3180, 3000, 3200, 3100, 3300, 3200, 3400, 3550, 3200, 3400, 3300, 3700)

tsdata3 <- ts(input3, start = c(2015, 2), frequency = 12)
tsdata3

plot(tsdata, type = "l") # 그래프가 일정하지 않음 = 차분을  정상성 시계열로 변경
par(mfrow = c(1, 2))
ts.plot(tsdata3)
diff <- diff(tsdata3)
plot(diff)

install.packages("forecast")
library(forecast)

arima <- auto.arima(tsdata3)
arima

# Series: tsdata3 
# ARIMA(1,1,0)   > IAR모형 즉, d번 차분라면 AR 모형 
# Coefficients:
#           ar1
#       -0.6891
# s.e.   0.2451
# sigma^2 estimated as 31644:  log likelihood=-72.4
# AIC=148.8   AICc=150.3   BIC=149.59

model <- arima(tsdata, order= c(1, 1, 0))
model
# Call:
#   arima(x = tsdata, order = c(1, 1, 0))
# Coefficients:
#           ar1
#       -0.6891
# s.e.   0.2451
# sigma^2 estimated as 28767:  log likelihood = -72.4,  aic = 148.8

#모형 ᄌ - 모형의 타당성 보기 방법 1
tsdiag(model) # 상관함수 결과가 유의미한 시차가 없는 경우/ p값 0 ᄋ , 양호한 모습 


#모형 진단 - 모형의 타당성 보기 방법 2 
Box.test(model$residuals, lag = 1, type = "Ljung") # X-squared = 0.12353, df = 1, p-value = 0.7252  > 0.5 이상이면 통계적으로 적절하다.

# 미래 예측 ###################################################################

fore <- forecast(model) # 향ᄒ 2년 예측 
fore

par(mfrow = c(1, 2))
plot(fore)
medel2<- forecast(model, h = 6) # 6개월 예측치 
plot(medel2)



# 정상성 시계열의 계절형 #####################################################

data4 <- c(55, 56, 45, 43, 69, 75, 58, 59, 66, 64, 52, 56, 55, 49, 67, 55, 71, 78, 61, 65, 59, 53, 70, 75, 56, 56, 65, 55, 68, 80, 65, 67, 77, 69, 79, 82, 57, 55, 63, 60, 68, 70, 58, 65, 70, 55, 65, 70)
length(data4)

tsdata4 <- ts(data4, start = c(2020, 1), frequency = 12)
tsdata4

ts_feature <- stl(tsdata4, s.window = "periodic")
plot(ts_feature)    # 그래프를 보니 계절성을 갖는 데이터 이다. 

par(mfrow = c(1,2))
ts.plot(tsdata4)
diff4 <- diff(tsdata4)
plot(diff4)  # 차분 시각화 

library(forecast)
ts_model2 <-auto.arima(tsdata4)
ts_model2
# Series: tsdata 
# ARIMA(1,1,0) 
# Coefficients:
#           ar1
#       -0.6891
# s.e.   0.2451
# sigma^2 estimated as 31644:  log likelihood=-72.4
# AIC=148.8   AICc=150.3   BIC=149.59

model3 <- arima(tsdata4, c(0,1,1), seasonal = list(order = c(1,1,0)))
model3
# Call:
#   arima(x = tsdata4, order = c(0, 1, 1), seasonal = list(order = c(1, 1, 0)))
# Coefficients:
#           ma1     sar1
#       -0.6752  -0.4184
# s.e.   0.1740   0.1983
# sigma^2 estimated as 64.88:  log likelihood = -124.14,  aic = 254.29


# 모형 진단 1 (모형 타ᄃ성 검정). 
tsdiag(model3)

# 모형 진단 2
Box.test(model3$residuals, lag = 1, type ="Ljung")
# X-squared = 1.381, df = 1, p-value = 0.2399

par(mfrow = c(1,2))
fore2 <- forecast(model3, h = 24) ; plot(fore2)  # 2년 예측 
fore3 <- forecast(model3, h =  6) ; plot(fore3)  # 6개월 예측 
plot(fore3)
