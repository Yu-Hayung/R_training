3+4
# 주석 
9*9
#install.packages("패키지명") 
libarary(stringr)
remove.packages("stringr")
plot(Ngender)
as.data.frame()
install.packages("stringr")
library(stringr)
search()
remove.packages("stringr ")

hist(Nile) 
hist(Nile, freq = F) # 밀도 기준으로 히스토그램 그리기
lines(density(Nile)) # 결과에 분포 곡선 추가 
par(mfrow = c(1,1))
pdf("/Users/yuhayoung/Desktop/coding/학원 파일/Rwork/R_Training/Nile.pdf")
hist(rnorm(20))
dev.off()             # 출력파일 닫기 

varl<-0  # <- 로 변수 지정 동일한 변수 사용시 덮여쓰기 됨
varl
var1<-1
var1
var2 <- 2
var2
var3 <-3
var3
goods.corde<-'a001'
goods.name <- '냉장고'
goods.price <- 850000
goods.des <- '최고사양, 동급 최고 품질'

age <- 35
name <- '홍길동'
age
name
age<-35
names <- c("홍길동","이순신","유관순")
age
names
int
int<-20
int
string <- "홍길동"
string
boolean<-TRUE
boolean
sum(10,20,30)
sum(10,20,30,NA)   # NA - 결측치 자료형
sum(10,20,30,NA,na.rm=TRUE) # NA 결측치 제거후 합계 여산
ls()

is.character(string)
x <- is.numeric(int)
x
is.logical(boolean)
is.na(x)

a <- c(1,2,'3')
a
result <-a * 3  # 이항연산자에 수치가 아닌 인수 입니다.
result <- as.numeric(a) *3 # 벡터 a를 숫자형으로 변환
result

z<- 5.3 - 3i     # 복수소 자료 생성 
Re(z)            # 실수 (real number):현실의 수 
Im(z)            # 허수 (imaginary number): 상상의 수
is.complex(x)    # 복소수 여부 확인
as.complex(5.3)  # 복소수로 형 변환

# 자료형과 자료구조 
# mode(변수): 자료의 성격(type)을 알려준다.
mode(int)  # numeric
mode(string)
mode(boolean)

# class(변수): 자료구조의 성격(type)을 알려준다.
class(int)
class(string)
class(boolean)


#문자 벡터와 그래프 생성 
gender <- c("man",'woman','woman','man','man')
plot(gender)  # error 발생 : 차트는 수치 데이터만 가능하다.

Ngender <- as.factor(gender) # 변수를 정하고 벡터로 만들어 준다.
table(Ngender)

plot(Ngender)
mode(Ngender)
class(Ngender)
is.factor(Ngender)

Ngender
# factor(x, levels, ordered) = factor 함수의 형식 
# factor 함수로 벡터 형 변환
args(factor)  # 벡터의 매개 변수 보기 
Ogender <- factor(gender, levels = c("woman",'man'), ordered = T)
Ogender

par(mfrow = c(1,2)) #par 함수로 두개의 그래프를 그릴 수 있다.
plot(Ngender)
plot(Ogender)

# 날짜형 변환
as.Date("21/03/13","%y/%m/%d")
class(as.Date("21/03/13","%y/%m/%d"))
dates <-c("02/28/20","02/30/20","03/01/20")
as.Date(dates,"%m/%d/%y") #해당날짜가 없는 경우 NA 출력 

#로케일 정보 
Sys.setlocale(category = "LC_ALL",locale = "ko_KR") # 대한민국 
Sys.getlocale(category = "LC_ALL")
Sys.getlocale(category = "LC_COLLATE")
Sys.setlocale('LC_ALL','ko_KR.UTF-8')
Sys.time() #현재 시간 

# strptime() 함수
sdate <-"2021*03*13 06:30:05"
class(sdate)
today <- strptime(sdate,format = "%Y-%m-%d %H:%M:%S")
class(today)
strptime("30-11-2019",format = ("%d-%m-%Y"))
strptime("30-11-2019",format = ("%d-%m-%y"))


# example() 내장 함수에 대한 예제를 보여줌
example(seq)   # 시퀀스 함수 
seq(1,9,by=3)  # 1부터 9까지 3의 패턴으로 보여줘 

mean(10:20)   # 평균 구하기 

#작업 공간 변경 
# setwd("위치경로") 
# data <- read.csv("지정된 경로에서 파일 가져오기")

getwd() # 현프로그램 저장관리에 대한 위치확인 


# 함수의 모움말 : googls 사이트에 '함수명()in r' 검색하면 도움이 된다. 
# 함수 파라미터 보기
args('함수명')
args(max)
max(10,20,NA,30)  # 결과 = [1] NA / 파라미터에서 NA를 제거하지 않음을 의미

example(seq)
# example 은 기본 함수들의 예제를 제공해 준다. 
# seq함수는 시작숫자와 끝 숫자를 정해주면 숫자를 생성합니다. 
seq(1,100, by=5)
x <- seq(1,100, by=10)

mean(x) # 평균을 구해주는 함수 
mean(10:20)  # 10~20 범위 평균 구하기 결과 = [1] 15

w <- c(0:10,50)
mean(w)     #  [1] 8.75
