getwd()   # 저장 위치 확인 

# 산술 연산자 
num1 <- 100
num2 <- 20
result <- num1 + num2
result
result2 <- num1 - num2
result3 <- num1 * num2
result4 <- num1 / num2
result5 <- num1 %% num2  #  나머지 계산
result6 <- num1 ^2    # 제곱 계산 
result7 <- num1 ^ num2


# 관계 연산자
boolean <- num1 == num2
boolean2 <- num1 != num2
boolean3 <- num1 > num2
boolean4 <- num1 < num2
boolean5 <- num1 >= num2
boolean6 <- num1 <= num2


# 논리 연산자 
logical <- num1 >= 50 & num2 <= 10
logical
logical2 <- num1 >=50 | num2 <= 10
logical3 <- num1 >=50     # 관계식 판단
logical4 <- !(num1 >= 50) # 괄호 안의 관계식 판단 결과에 대한 부정 
logical4

x<- TRUE ; y<- FALSE
xor(x,y)  
# 배타적 논리합을 연산하는 xor() 함수는 두 논리적인 값이 상반되는 경우  TRUE 반환한다.

# ---------------------------------------------------------------
# 조건문 

x <- 50 ; y <- 4 ; z <- x * y
if (x * y >= 40) {
  cat("x * y 의결과는 40 이상 입니다. \n")
  cat("x * y =" , z)
} else {
  cat("x * y의 결과는 40 미만입니다. x* y = ",z , "\n")
}
# x * y 의결과는 40 이상 입니다. 
# x * y = 200


score <- scan()  # 입력값 받기 
result <- "노력"
if(score >= 80){
  result8 <- "우수"
}
cat("당신의 학점은", result8, score )
# 당신의 학점은 우수 85

score2 <- scan()
# 85, 75, 70, 90, 95, 82
ifelse(score2 >= 80, "우수", "노력")   # 참 = 선자 , 불 = 후자 
# [1] "우수" "노력" "노력" "우수" "우수" "우수"
ifelse(score2 <= 80, "노력", "우수")
# [1] "우수" "노력" "노력" "우수" "우수" "우수"


excel <- read.csv("/Users/yuhayung/Desktop/coding/학원/Rtraining/dataset1/excel.csv", header = T)
q1 <- excel$q1
q1
ifelse(q1 >= 3, sqrt(q1),q1) # q1 값이 3 보다 큰 경우 'sqrt()= 루트' 함수 적용 
ifelse(q1 >=2 & q1 <= 4, q1 ^2, q1)  # 1과 5만 출력, 나머지(2~4)는 제곱승 적용

switch("name", id="hong", pwd ="1234", age = 105, name = "홍길동")
empname <- scan(what="")   # "hong" 입력 
empname
switch(empname,
        hong = 250,
        lee = 350,
        kim = 200,
        kang = 400
)


name <- c("kim","lee", "choi", "park")
which(name == "choi")  # 인덱스 값 반환 
# [1] 3

no3 <- c(1:5)
name3 <- c("홍길동","이순신","강감찬","유관순","김유신")
score3 <- c(85, 78, 89, 90, 74)
exam <- data.frame(학번 = no3, 이름 = name3, 성적 = score3)
exam
#    학번   이름 성적
# 1    1 홍길동   85
# 2    2 이순신   78
# 3    3 강감찬   89
# 4    4 유관순   90
# 5    5 김유신   74

which(exam$이름 == "유관순")
# [1] 4
exam[4, ]
# 학번   이름 성적
# 4    4 유관순   90


# ---------------------------------------------------------------
#반복문 
i <- c(1:10)
for(n in i) {
  print(n * 10)
  print(n)
}

HJ<- c(1:10)    # HJ = 홀짝 이니셜 
for(n in HJ) {
  if(n%%2 == 0) print(n)  # 짝수만 출력 / 홀수는 해당 안됨 
}

HJ2 <- c(1:10)
for(n in HJ2){
  if(n %% 2 == 0 ) {
    next # 다음문장으로 skip, 반복문 계속 continue 키워드와 동일
  } else {
  print(n) #홀수 일 때만 출력 
  }
}


score4 <- c(85, 95, 98)
name4 <- c ("홍길동", "이순신", "강감찬")
t <- 1 
for(s in score4) {
  cat(name4[t],"->", s, "\n")
  t <- t + 1
}

i2 = 0
while(i2 < 10) {
  i2 <- i2 + 1
  print(i2)
}

# ---------------------------------------------------------------

# 사용자 정의 함수
f1 <- function(){
  cat("매개변수가 없는 함수")
}
f1
f3 <- function(x,y) {
  add <- x + y
  return(add)
}
add <- f3(10,20)
add    # [1] 30
f3(10,40)  # [1] 50


# 기술 통계량을 계산하는 함수 정의
setwd("/Users/yuhayung/Desktop/coding/학원/Rtraining/dataset1")
test1 <- read.csv("test.csv", header = T)
head(test1)
summary(test1)    # 변수 (A, B, C, D, E) 별 요약 통계량 
table(test1$A)    # 변수 A를 대상으로 빈도수 구하기 : 5점 척도 ( 만족도 조사)
data_pro <- function(x) {
  for (idx in 1:length(x)) {
    cat(idx, "번째 칼럼의 빈도 분석 결과")
    print(table(x[idx]))
    cat("\n")
  }
  for(idx in 1:length(x)) {
    f <- table(x[idx])
    cat(idx, "번째 칼럼의 최댓값/ 최소값 \n")
    cat("max=", max(f), "min=", min(f), "\n")
  }
}

data_pro(test1)

# 분산과 표준편차 식
x <- c(7, 5, 12, 9, 15, 6)
var_sd <- function(x){
  var <- sum((x - mean(x))^2) / (length(x)-1)
  sd <- sqrt(var)
  cat("표본분산:", var, "\n")
  cat("표본표준편차:",sd, "\n")
}
var_sd(x)

# 피타고라스와 구구단 함수 
pytha <- function(s,t){
  a <- s^2 - t^2
  b <- 2 * s* t
  c <- s^2 + t^2
  cat("피타고라스 정리 : 3개의 변수 ", a, b, c)
}
pytha(2,1)
pytha(45,33)


# 구구단 출력함수 
gugu <- function(i, j){
  for(x in i) {
    cat("**", x, "단 **\n")
  for(y in j) {
    cat(x, "*", y, "=", x*y, "\n")
  }
    cat("\n")
  }
}
i <-c(2:9)
j <-c(1:9)
gugu(i,j)


# 결측치 포함 자료의 평균 계산 함수 정의
data <- c(10, 20, 5, 4, 40, 7, NA, 6, 3, NA, 2, NA)
na <-function(x){
  print(x)
  print(mean(x, na.rm = T))  # NA 제거 
  data1 = ifelse(!is.na(x), x, 0)   # NA, 0으로 대체
  print(data1)
  print(mean(data1))
  data2 = ifelse(!is.na(x), x, round(mean(x, na.rm = T), 2))  # NA , 평균으로 대체  
  print(data2)
  print(mean(data2))
}
na(data) #함수 호출 

# 동전 앞면 뒷면 난수 확률분포 
coin <- function(n){
  r <- runif(n, min = 0, max =1)
  result <- numeric()
  for(i in 1:n){
    if(r[i] <= 0.5)
      result[i] <- 0 # 앞면 
    else
      result[i] <- 1 # 뒷면 
  }
  return(result)
}
coin(10)
# [1] 0 0 1 0 0 1 0 0 0 0

#-------------------------------------------------------------------------


#주요 내장 함수 
testv <- c(54, 60, 8, 540, 54.2, 57, 17, 39, 28, 41, 2.7)
testm <- matrix(c(1:32), nrow= 8 )
#       [,1] [,2] [,3] [,4]
# [1,]    1    9   17   25
# [2,]    2   10   18   26
# [3,]    3   11   19   27
# [4,]    4   12   20   28
# [5,]    5   13   21   29
# [6,]    6   14   22   30
# [7,]    7   15   23   31
# [8,]    8   16   24   32
min(testv) 
min(testm)
max(testv)
max(testm)
range(testv)  #range(최소값, 최댓값)
range(testm)
mean(testv)   # 평균값 
mean(testm) 
median(testv) # 중위수
median(testm)
sum(testv)    # 합계
sum(testm)
sort(testv)  # 정렬 
sort(testm)
order(testv) # 색인 [인덱스]
order(testm)
rank(testv)  # 벡터의 원소의 순위
rank(testm)
sd(testv)    # 표준편차
sd(testm)
summary(testv)  # 통계량
summary(testm)
table(testv)    # 빈도수 
table(testm)
sample()

install.packages("RSADBE")
library(RSADBE)
data("Bug_Metrice_Software")
Bug_Metrice_Software[, , 1]
rowSums(Bug_Metrice_Software[, , 1])
rowMeans(Bug_Metrice_Software[, , 1])
colSums(Bug_Metrice_Software[, , 1])
colMeans(Bug_Metrice_Software[, , 1])

seq(-2, 2, by=.2)
vec <- 1:10
min(vec)
max(vec)
range(vec)
mean(vec)
median(vec)
sum(vec)
sd(rnorm(10))  #무작위 대상 표준편차 (난수)
table(vec)

n <- 100
rnorm(n, mean = 0, sd = 1)
hist(rnorm(n,mean = 0, sd = 1))

n2 <- 1000
runif(n2, min=0, max=10)
hist(runif(n, min=0, max= 10)) #표준정규분포 - 히스토그램 

n3 <- 20
rbinom(n, 1, prob= 1/2) # 0 또는 1의 이산변량 대상 0.5 확률로 n개 선정
rbinom(n, 2, 0.5)
rbinom(n, 10, 0.5)

n4 <- 1000
rbinom(n, 5, prob = 1 / 6) # 0~5 사이의 이산변량 대산 1/6의 확률오 n개 선정 

rnorm(5, mean = 0, sd = 1) # 매번 실행하는 경우 임의의 난수 생성 
set.seed(123)   # 종자값을 123 으로 설정 
rnorm(5, mean = 0, sd = 1)
set.seed(123) 
rnorm(5, mean = 0, sd = 1)
set.seed(345)
rnorm(5, mean = 0, sd = 1)



# -----------------------------------------------------------------


#수학 관련 내장함수 

vec <- 1:10 
prod(vec) # 벡터의 곱 (1*2*3*4*5*6*7*8*9*10)
factanal(5)
abs(-5)   # 절대값
sqrt(16)  # 제곱근 
vec
#  [1]  1  2  3  4  5  6  7  8  9 10
cumsum(vec)
# 벡터 값에 대한 누적합
# [1]  1  3  6 10 15 21 28 36 45 55
log(10)  #  10의 자연로그 
log10(10)  # 10의 일반로스 
exp(vec)  # 자연상수 

#행렬관련 내장함수 

x1 <- matrix(1:9, nrow= 3, ncol = 3, byrow = T)  # 정방행렬
y1 <- matrix(1:3, nrow= 3)             # 3 X 1 행렬
ncol(x1)
nrow(x1)
t(x1)
cbind(x1, 1:3)     # 열추가
rbind(x1, 10:12)   # 행추가 
diag(x1)           # 정방행렬 x1에 서 대각선 값 반환 
det(x1)            
apply(x1, 1, sum)  # x의 행 단위 합계
apply(x1, 2, mean) # x의 열 단위 평균 
svd(x1)            # 차원 축소 특이값 (이미지에 많이 사용 )
eigen(x1)          # 정방 행렬 고유값 분해 
x1 %*% y1          # 행렬 곱 



# 집합연산 관련 내장 함수 

x3 <- c(1,3,5,7,9)
y3 <- c(3,7)
union(x3, y3)     # 벡터에 관한 합집합 
setequal(x3, y3)  # 동일성 검사 
intersect(x3, y3) # 교집합
setdiff(x3, y3)   # 차집합
setdiff(y3, x3)   # 차집합 
5 %in% y3        # 5가 y의 원소 인가?  [1] FALSE

