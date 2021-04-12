install.packages('mlbench')
library(mlbench)
data("BostonHousing")

# BostonHousing 전진선택법 #################################################
ss <- lm(medv ~., data = BostonHousing)
ss1 <- step(ss, direction = "forward")
# Start:  AIC=1589.64
# medv ~ crim + zn + indus + chas + nox + rm + age + dis + rad + tax + ptratio + b + lstat

formula(ss1)
# medv ~ crim + zn + indus + chas + nox + rm + age + dis + rad + tax + ptratio + b + lstat





# BostonHousing 후지   #################################################

ss <- lm(medv ~ .,data=BostonHousing)

ss2 <- step(ss, direction = "backward")
formula(ss2)

# Start:  AIC=1589.64
# medv ~ crim + zn + indus + chas + nox + rm + age + dis + rad + tax + ptratio + b + lstat

# Step:  AIC=1587.65
# medv ~ crim + zn + indus + chas + nox + rm + dis + rad + tax +  ptratio + b + lstat

# Step:  AIC=1585.76
# medv ~ crim + zn + chas + nox + rm + dis + rad + tax + ptratio + b + lstat


# attitude 데이터 이용 ########################################################
data(attitude)
head(attitude)

model <- lm(rating~., data = attitude)
summary(model)

# Call:
#   lm(formula = rating ~ ., data = attitude)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -10.9418  -4.3555   0.3158   5.5425  11.5990 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 10.78708   11.58926   0.931 0.361634    
# complaints   0.61319    0.16098   3.809 0.000903 ***
#   privileges  -0.07305    0.13572  -0.538 0.595594    
# learning     0.32033    0.16852   1.901 0.069925 .  
# raises       0.08173    0.22148   0.369 0.715480    
# critical     0.03838    0.14700   0.261 0.796334    
# advance     -0.21706    0.17821  -1.218 0.235577    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 7.068 on 23 degrees of freedom
# Multiple R-squared:  0.7326,	Adjusted R-squared:  0.6628 
# F-statistic:  10.5 on 6 and 23 DF,  p-value: 1.24e-05


# attitude 후진제거법 ########################################################

reduced <- step(model, direction = "backward")
summary(reduced)

# Call:
#   lm(formula = rating ~ complaints + learning, data = attitude)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -11.5568  -5.7331   0.6701   6.5341  10.3610 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   9.8709     7.0612   1.398    0.174    
# complaints    0.6435     0.1185   5.432 9.57e-06 ***
#   learning      0.2112     0.1344   1.571    0.128    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 6.817 on 27 degrees of freedom
# Multiple R-squared:  0.708,	Adjusted R-squared:  0.6864 
# F-statistic: 32.74 on 2 and 27 DF,  p-value: 6.058e-08



# attitude 단계 선택법 ####################################################

ss <- lm(medv ~ .,data=BostonHousing)
ss3 <- step(ss, direction = "both")
# Start:  AIC=1589.64 //medv ~ crim + zn + indus + chas + nox + rm + age + dis + rad +  tax + ptratio + b + lstat
# Step:  AIC=1587.65 // medv ~ crim + zn + indus + chas + nox + rm + dis + rad + tax + ptratio + b + lstat
# Step:  AIC=1585.76 // medv ~ crim + zn + chas + nox + rm + dis + rad + tax + ptratio + b + lstat

formula(ss3)
# medv ~ crim + zn + chas + nox + rm + dis + rad + tax + ptratio + b + lstat

summary(ss3)
# Residual standard error: 4.736 on 494 degrees of freedom
# Multiple R-squared:  0.7406,	Adjusted R-squared:  0.7348 
# F-statistic: 128.2 on 11 and 494 DF,  p-value: < 2.2e-16

