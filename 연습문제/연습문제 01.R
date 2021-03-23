# 1) 현재 작업공간을 확인하고 [/Users/yuhayoung/Desktop/coding/학원파일/Rwork/R_Training] 경로 변경 하시오. 
getwd() # 현 작업 위치 알려주는 함수
# setwd("/Users/yuhayoung/Desktop/coding/학원파일/Rwork/R_Training") 
# 원하는 저장위치가 있어서 가상위치임 각주 표시 


# 2) 조건에 맞게 name, age, address 변수를 생성처리
# 조건1. 각 변수의 특성에 맞게 값을 초기화 하고 결과를 확인 
# 조건2. 다음 함수를 이용하여 각 변수의 자료형(data type)을 확인 
#         mode(), is.character(), is.numeric()

name <- 'yuhayung'
age <- 27
address <- 'Incheon, South Korea'
mode(name)           # 결과 = [1] "character"
mode(age)            # 결과 = [1] "numeric"
mode(address)        # 결과 = [1] "character"
is.character(name)   # 결과 = [1] TRUE
is.character(age)    # 결과 = [1] FALSE
is.character(address)# 결과 = [1] TRUE
is.numeric(name)     # 결과 = [1] FALSE
is.numeric(age)      # 결과 = [1] TRUE
is.numeric(address)  # 결과 = [1] FALSE


# 3) R에서 제공하는 women 데이터 셋을 다음과 같이 처리하시오
# 조건1. women 데이터 셋은 어떤 데이터의 모음인가? 
# 조건2. women 데이터 셋은 어떤 자료형 자료구조인가? 
# 조건3. plot() 함수를 이요하여 디본 차트 그리기

mode(women)
is.list(women)
class(women)
plot(women)

#4) R에서 제공하는 c()함수를 이용하여 벡터를 생성하고 데이터를 처리하시오
# 조건1. 1~100까지 벡터를 생성한다.
# 조건2. 생성된 벡터를 대상으로 평균을 구한다.

c(1:100)            # 1부터 100까지의 벡터 생성 
number <- c(1:100)  # 백터의 변수화 
mean(number)        # mean 평균을 구하는 함수 
                    # 결과 = [1] 50.5

