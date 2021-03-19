# 01. 다음 조건에 맞게 client 데이터프레임을 생성하고, 조건에 맞게 처리하시오. 
name <-c("유관순","홍길동","이순신","신사임당")
gender <- c("F","M","M","F")
price <-c(50,65,45,75)
# 조건1) 다음 3개 벡터 객체를 이용하여 client 데이터프레임을 생성하시오.
# 조건2) price 변수의 값이 65만원 이상이면 문자열 "Best", 65만원 미만이면 문자열
# "Normal"을 변수 result에 추가하시오. 힌트) ifelse() 함수 이용
# 조건3) result 변수를 대상으로 빈도수를 구하시오.

client <- data.frame(name = name, gender = gender, price = price)
client$ rescult <- ifelse(price >= 65, "Best", "Normal")
table(client$result)


# 02. 다음 벡터 EMP는 '입사년도이름급여'순으로 사원의 정보가 기록된 데이터 있다. 
# 이벡터 데이터를 이용하여 다음과 같은 출력결과가 나타나도록 함수를 정의하시오. 
EMP <- c("2014홍길동220", "2002이순신300", "2010유관순260")

install.packages("stringr")

emp_pay <- function(x) {
  library(stringr) 
  pay <- numeric() # 급여 
  name <- character() # 이름 
  idx <- 1 # index 변수
  
  for(n in x){ 
    name[idx] <- str_extract(n, '[가-히]{3}') # 이름
    spay <- str_extract(n, '[가-히]{3}[0-9]{3}') 
    spay <- str_replace(spay, '[가-히]{3}', '') # 급여만 남은 벡터
    npay <- as.numeric(spay) 
    pay[idx] <- npay 
    idx <- idx + 1 # index 카운터
  }
  avg <- mean(pay)
  cat('전체 급여 평균 :', avg, '\n')
  cat('평균 이상 급여 수령자\n')

  n <- 1: length(x)
  for(idx in n){
    if(pay[idx] >= avg){
      cat(name[idx], '=>', pay[idx],'\n')
    }
  }
}
emp_pay(EMP)  # 반환 



# 03. 함수 y = f(x)에서 x의 값이 a에서 b까지 변할 때 △x = b - a를 x의 증분이라고 하며, 
# △y = f(b) - f(a)를 y의 증분이라고 표시합니다.
# 평균변화율 = △y/△x = f(b) - f(a)/ b-a 이다.
# 조건) 함수 f(x) = x3 + 4에서 x의 값이 1에서 3까지 변할 때 평균변화율(mean ratio of change)을 구하는 함수 만들어라 

mrc <- function(x){
  x <- x^3 + 4
}
a <- 1
b <- 3
mrc_result <- (mrc(b) - mrc(a)) / (b - a)
mrc_result


# 4번 문제는 맥(MAC) os 문제로 풀이 불가능 

# 04. RSADBE 패키지에서 제공되는 Bug_Metrics_Software 데이터 셋을 대상으로 소프트웨
# 어 발표 후 행 단위 합계와 열 단위 평균을 구하고, 칼럼 단위로 요약통계량을 구하시오.
library('RSADBE')
data('Bug_Metrics_Software')
rowSums(Bug_Metrics_Software[,,2])
colMeans(Bug_Metrics_Software[,,2])
summary(Bug_Metrics_Software[,,2]) 

# 책보고 일단 진행 


