# 요인 분석  

# 데이터 만들기 
s1 <- c(1,2,1,2,3,4,2,3,4,5)
s2 <- c(1,3,1,2,3,4,2,4,3,4)
s3 <- c(2,3,2,3,2,3,5,3,4,2)
s4 <- c(2,4,2,3,2,3,5,3,4,2)
s5 <- c(4,5,4,5,2,1,5,2,4,3)
s6 <- c(4,3,4,4,2,1,5,2,4,2)
name <- 1:10

# 데이터 프레임화 
subject <- data.frame(s1, s2, s3, s4, s5, s6)
str(subject)

# 주성분 분석으로 요인 수 알아보기
pc <- prcomp(subject)
summary(pc)

plot(pc) # 시각화

# 고유값으로 요인 수 분석
en <- eigen(cor(subject))
names(en)

en$values

# 고유값을 이용한 시각화 
plot(en$values, type = "o")


#변수 간의 상관 분석과 요인 
cor(subject)


#베리믹스 회전법
result <- factanal(subject, factors = 2, rotation = "varimax")
result
# The p-value is 0.0134  요인수가 부족하다 . 


# 요인분석 - 요인 회전법 적용 
result <- factanal(subject,
                   factors = 3,
                   rotation = "varimax",
                   scores = "regression")
result
# Uniquenesses:
#   s1    s2    s3    s4    s5    s6 
# 0.254 0.005 0.005 0.053 0.005 0.051  <- 변수 모두 유효함 0.5 이하 


attributes(result)

result$loadings   # 기본 요인적재량 보기

# 요인부하량 0.5 이상, 소수점 2자리 표기 
print(result,digits = 2, couoff = 0.5) 
# Uniquenesses:
#   s1   s2   s3   s4   s5   s6 
# 0.25 0.00 0.00 0.05 0.00 0.05 

# 모든 요인적재량 보기 : 감추어진 요인 적재량 보기 
print(result$loadings, cutoff = 0)


#Factor1 과 Factor2시각화 
par(family="NanumGothicBold")
plot(result$scores[ ,c(1:2)], main ="Factor1 과 Factor2 요인 점수 행렬")
text(result$scores[,1], result$scores[,2], labels = name, cex = 0.7, pos = 3, col ="blue")


#요인 적재량 추가 
points(result$loadings[ ,c(1:2)], pch = 19, col = "red")
text(result$loadings[,1], result$loadings[ ,2],labels = rownames(result$loadings), cex = 0.8, pos = 3, col ="red" )


#Factor1 과 Factor3 요인적재량 시각화 
plot(result$scores[,c(1,3)], main = "Factor1 과 Factor3 요인 점수 행렬")
text(result$scores[,1], result$scores[ ,3],labels = rownames(result$loadings), cex = 0.7, pos = 3, col ="blue" )

#요인 적재량 추가 
points(result$loadings[,c(1,3)], pch = 19, col = "red")
text(result$loadings[,1], result$loadings[ ,3],labels = rownames(result$loadings), cex = 0.8, pos = 3, col ="red" )


# 3차원 산점도 
# install.packages("scatterplot3d")
library(scatterplot3d)

# 요인 점수별 분류 및 3차원 프레임 생성 
Factor1 <- result$scores[ ,1]
Factor2 <- result$scores[ ,2]
Factor3 <- result$scores[ ,3]


d3 <- scatterplot3d(Factor1, Factor2, Factor3, type ='p')


# 요인적재량 표시 
loadings1 <- result$loadings[, 1]
loadings2 <- result$loadings[, 2]
loadings3 <- result$loadings[, 3]
d3$points3d(loadings1, loadings2, loadings3, bg ="red", pch= 21, cex =2, type ="h")

#단계1 : 요인별 과목 변수 ᄋ 데이터프레임 생성 
app <- data.frame(subject$s5, subject$s6)
soc <- data.frame(subject$s3, subject$s4)
nat <- data.frame(subject$s1, subject$s2)

# 요인별 산술 평균 ᄀ 
app_science <- round((app$subject.s5 + app$subject.s6)/ ncol(app), 2)
soc_science <- round((soc$subject.s3 + soc$subject.s4)/ ncol(soc), 2)
nat_science <- round((nat$subject.s1 + nat$subject.s2)/ ncol(nat), 2)

# 상관분석 
subject_factor_df <- data.frame(app_science, soc_science, nat_science)
cor(subject_factor_df)


# 잘못 분류된 요인 제거로 변수 정제 ########


