# vector() R에서 가장 기본이 되는 자료구조이다. 벡터 자료구조는 연속된 선형구조의 형태로만 들어지고, 첨자에 의해서 접근할 수 있다.
# 벡터 생성 함수 : c(), seq(), rep()
# 벡터 자료 처리 함수 : union(), setdiff(), intersect()

c(1:20) # :로 범위를 지정
1:20    # 같은 의미 
c(1,2,3,4,5) # c(1:5) 와 같은의미 

seq(1, 10, 2) # sequence value 함수 -> 1부터 10까지 2씩 증가 
# [1] 1 3 5 7 9
rep(1:3,3)  # [1] 1 2 3 1 2 3 1 2 3

rep(1:3,3) # (시작,끝,전체 반복횟수) 
# [1] 1 2 3 1 2 3 1 2 3
rep(1:3,each = 3) # (each는 각 자료가 반복할 횟수지정) 
# [1] 1 1 1 2 2 2 3 3 3

x<-c(1,3,5,7)
y<-c(3,5)
union(x,y)       # 합집합 [1] 1 3 5 7
setdiff(x,y)     # 차집합 [1] 1 7
intersect(x,y)   # 교집합[1] 3 5

v1 <- c(33,-5,20:23, 12,-2:3)
v2<-c('홍길동','이순신','유관순')
v3<-c(T,TRUE,FALSE,T,TRUE,F,T)
v1;v2;v3
# [1] 33 -5 20 21 22 23 12 -2 -1  0  1  2  3
# [1] "홍길동" "이순신" "유관순"
# [1]  TRUE  TRUE FALSE  TRUE  TRUE FALSE  TRUE


v4 <-c(33, 05, 20:23, 12, "4") # 자료형이 혼합된 경우 
v4    
# [1] "33" "5"  "20" "21" "22" "23" "12" "4"  (모두 문자형으로 변환)

v1;mode(v1);class(v1) 
#[1] 33 -5 20 21 22 23 12 -2 -1  0  1  2  3
# mode:numerical class:numerical
v2;mode(v2);class(v2)   
#"홍길동" "이순신" "유관순"
# mode:character class:character
v3;mode(v3);class(v3)   
#TRUE  TRUE FALSE  TRUE  TRUE FALSE  TRUE
# mode:logical class:logical 

a1<- c(1:50)   # 벡터 객체 생성 
a1[10:40]       # 10에서 45 사이의 벡터 원소 출력
# [1] NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA
# [28] NA NA NA NA
a1[19:(length(a) - 5)]  #10부터의 벡터전체 길이에서 5를 뺀길이(45)만큼출력 
# [1] 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36
# [28] 37 38 39 40

a2[1,2] 
# 잘못된 첨자르 지정 오류 발생 (,) 는 다차원 배열을 의미 

# c() 함수에서 콤마 사용은 벡터 객체 생성에 사용 된다.
v1<- c(13, -5, 20:23, 12, -2:3)  #벡터 객체 생성
v1[1]    # 1번째 원소출력
# [1] 13
v1[c(4,5:8,7)]  # 콤마와 세미콜론의으로 벡터객체의 원소출력 
# [1] 21 22 23 12 -2 12

# 첨자를 음수 값으로 지정할 경우 여지합 개념으로 해당 첨자가 제외된다. 
v1[-1];v1[-c(2,4)];v1[-c(2:5)]
# v1 벡터에서 1번째 원소 제거 등등.. 이렇게 순차적으로 (,) 구분하여 차례로 출력 된다.  
# [1] -5 20 21 22 23 12 -2 -1  0  1  2  3
# [1] 13 20 22 23 12 -2 -1  0  1  2  3
# [1] 13 23 12 -2 -1  0  1  2  3

# R의 패캐지에서는 알고리즘이 적용된 유용한 함수를 제공
# 벡터 객체로 제공되는 데이터셋 불러오기

install.packages('RSADBE')
library(RSADBE)
data(Severity_Counts)
str(Severity_Counts)
Severity_Counts


# Matrix 자료구조 
# 행렬 생성 함수 : matrix(),rbind(),cbind()
# 행렬 자료 처리 함수 : apply()
m<- matrix(c(1:5))
m
#       [,1]
# [1,]    1
# [2,]    2
# [3,]    3
# [4,]    4
# [5,]    5
m2 <- matrix(c(1:10),nrow = 2) # 열 우선으로 2행 2열을 객체 생성
m2
#       [,1] [,2] [,3] [,4] [,5]
# [1,]    1    3    5    7    9
# [2,]    2    4    6    8   10
m3 <- matrix(c(1:30),nrow = 4)
# 경고/ 데이터의 길이[30]가 행의 개수[4]의 배수가 되지 않습니다./ 허나 가능
m3
#       [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8]
# [1,]    1    5    9   13   17   21   25   29
# [2,]    2    6   10   14   18   22   26   30
# [3,]    3    7   11   15   19   23   27    1  앞 숫자가 와서 이어줌
# [4,]    4    8   12   16   20   24   28    2

m4 <- matrix(c(1:23), nrow = 2, byrow = T) #  byrow = T 행우선 
m4
#       [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11] [,12]
# [1,]    1    2    3    4    5    6    7    8    9    10    11    12
# [2,]   13   14   15   16   17   18   19   20   21    22    23     1


x1 <-c(5,40,50:52)   # 5개
x2 <- c(30,5,6:8)    # 5개
mr <-rbind(x1,x2)    # 행 묶음으로 매트리스 객체 생성 
mr
#     [,1] [,2] [,3] [,4] [,5]
# x1    5   40   50   51   52
# x2   30    5    6    7    8   # 2행 5열 구조의 행렬 객체

mc <- cbind(x1,x2)
mc                 # 열 묶음으로 객체 생성 
      #x1 x2
# [1,]  5 30
# [2,] 40  5
# [3,] 50  6
# [4,] 51  7
# [5,] 52  8

# matrix(data = NA, nrow =1, ncol=1, byrow= FALSE, dimnames = NULL)
m3 <- matrix(10:19, 2)
m4 <- matrix(10:20, 2)  # 에러 발생 = 데이터 개수가 홀수 
m3 
mode(m3);class(m3)
# [1] "numeric"
# [1] "matrix" "array" 

m3[1,]   # [1] 10 12 14 16 18
m3[,5]   # [1] 18 19
m3[1,c(2:5)]  # [1] 12 14 16 18

x3 <- matrix(c(1:30),nrow = 3, ncol = 3)
x3
#       [,1] [,2] [,3]
# [1,]    1    4    7
# [2,]    2    5    8
# [3,]    3    6    9

length(x3)  # [1] 9  = 행렬 객체의 전체 원소 개수를 반환
ncol(x3)    # [1] 3  = 열수와 행 수를 반환 

# apply(X,MARGIN,FUN....)
apply(x3,1,max)  #   [1] 7 8 9  행단위로 각 행의 최대값 구하기
apply(x3,1,min)  #   [1] 1 2 3  행단위로 각 행의 최소값 구하기
apply(x3,2,mean) #   [1] 2 5 8  열단위로 각 행의 평균값 구하기

fnu <- function(x3){     # 사용자 정의 함수
  x3 * c(4,5,6)
}
result1 <- apply(x3,1,fnu)   # 행우선순위로 f 함수 적용 
result1                   # 벡터 데티어 연산할 때 열 단위로 결과 출력
#       [,1] [,2] [,3]
# [1,]    4    8   12
# [2,]   20   25   30
# [3,]   42   48   54

result2<- apply(x3,2,fnu)  # 열 우선순서로 사용자 정의 함수 f를 적용
result2
#       [,1] [,2] [,3]
# [1,]    4   16   28
# [2,]   10   25   40
# [3,]   18   36   54


# Array 자료 구조 
# 배열 자료구조는 같은 자료형을 갖는 다차원 배열구조를 갖는다.
vec <- c(1:12)
arr <- array(vec,c(3,2,2))    # array(vec,c(행,열,면))
arr
# , , 1
# 
#       [,1] [,2]
# [1,]    1    4
# [2,]    2    5
# [3,]    3    6
# 
# , , 2
# 
#       [,1] [,2]
# [1,]    7   10
# [2,]    8   11
# [3,]    9   12


arr[,,1]    # [행, 열, 면]
#       [,1] [,2]   인덱스로 원하는 부분만 출력가능 
# [1,]    1    4
# [2,]    2    5
# [3,]    3    6
mode(arr);class(arr)    # 배열의 자료형과 자료구조 
# [1] "numeric"         # 배열은 숫자 
# [1] "array"           # 자료형은 행렬 

install.packages("RSADBE")
library(RSADBE)
data("Bug_Metrics_Software")
str(Bug_Metrics_Software)

# DataFrame 자료구조
# 칼럼 단위로 서로 다른 데이터형의 저장이 가능하다 
# 데이터프레임 생성 함수 : data.frame(), read.table(), read.csv()
# 데이터프레임 자료 처리 함수 : str(), nocol(), nrow(), apply(),summary(),subset() 등.. 
# 형식 data.frame (칼럼1= 자료, 칼럼2 = 자료,...칼럼n =자료)

# 벡터를 이용한 데이터프레임 객체 생성하기
nia <- c(1,2,3,4)
name <- c("루피","나미","조로","상디")
pay <- c(150,200,250,300)
vemp <- data.frame(No = nia, Name = name, Pay = pay) # 칼럼명 지정
vemp
#    No Name Pay
# 1  1 루피 150
# 2  2 나미 200
# 3  3 조로 250
# 4  4 상디 300

# 매트릭스를 이용한 데이터프레임 객체 생성하기
m <- matrix(
  c( 1, "루피", 150,
     2, "나미", 200,
     3, "조로", 250),3, by = T)
memp <- data.frame(m)
memp
#    X1   X2  X3
# 1  1 루피 150
# 2  2 나미 200
# 3  3 조로 250

# 텍스트 파일을 이용한 데이터 프레임 객체 생성하기
getwd()
txtemp <-read.table('dataframe_test_txtemp.txt',header=1, sep= "") # 제목 있음, 칼럼 공백 구분
txtemp
#    사번 이름 급여   내가 직접 txt 작성한 것을 불러왔다.
# 1  101 루피  300
# 2  201 나미  340
# 3  301 조로  250

csvtemp <-read.csv('emp_update.csv',header = T) #제목있음, 칼럼 콤마 구분
csvtemp
help(read.csv)   # read.csv()함수의 파라미터 보기
name <-c
read.csv('emp')

df <- data.frame(x = c(1:5),y = seq(2,10,2),z=c('a','b','c','d','e'))
df
#   x  y z
# 1 1  2 a
# 2 2  4 b
# 3 3  6 c
# 4 4  8 d
# 5 5 10 e

df$x    # 리스트 $ = '키' / 데이터프레임 $ = '칼럼'
# [1] 1 2 3 4 5
df$y
# [1]  2  4  6  8 10

#data.frame 객체 자료 처리 함수 = str(), summary(), apply()
str() # 데이터프레임의 구조를 보여주는 함수 

str(df)
# 'data.frame':	5 obs. of  3 variables:
# $ x: int  1 2 3 4 5
# $ y: num  2 4 6 8 10
# $ z: chr  "a" "b" "c" "d"

ncol(df)   # 데이터프레임 객체의 행 수 : 3
# [1] 3
nrow(df)   # 데이터프레임 객체의 열 수 : 5
# [1] 5
names(df)  # 데이터프레임 객체의 칼럼명 출력
# [1] "x" "y" "z"
df[c(2:3),1]  # 데이터프레임 객체의 특정 행 출력 
# [1] 2 3

summary(df)  # 객체에 대해 요약해서 보여주는 함수 
# x           y           z            
# Min.   :1   Min.   : 2   Length:5          
# 1st Qu.:2   1st Qu.: 4   Class :character  
# Median :3   Median : 6   Mode  :character  
# Mean   :3   Mean   : 6                     
# 3rd Qu.:4   3rd Qu.: 8                     
# Max.   :5   Max.   :10  

subset()
x1 <- subset(df,x >= 3)  # x가 3이상인 행을 대상으로 서브셋 생성 
x1
#   x  y z
# 3 3  6 c
# 4 4  8 d
# 5 5 10 e

y1 <- subset(df, y<=8)
xyand <- subset(df,x >= 2 & y <= 6)
xyor <- subset(df, x>=2 | y <= 6)
y1
xyand
xyor

# 벡터객체 생성 
sid = c("A","B","C","D")
score = c(90,80,70,60)
subject = c("컴퓨터","국어국문","소프트웨어","유아교육")
#데이터프레임 생성
student <- data.frame(sid,score,subject)
student

mode(student); class(student) 
# [1] "list"                   # 자료형 
# [1] "data.frame"             # 자료구조 
str(sid); str(score); str(subject)   # 백터 자료구조
# chr [1:4] "A" "B" "C" "D"
# num [1:4] 90 80 70 60
# chr [1:4] "컴퓨터" "국어국문" "소프트웨어" "유아교육"
str(student)                 # 데이터프레임 자료구조

# 두개 이상의 데이터프레임 병합하기
height <- data.frame(id = c(1,2),h = c(180,175))   # 키를 저장한 데이터프레임
weight <- data.frame(id = c(1,2),w = c(80,75))     # 몸무게를 저장한 데이터프레임
# 데이터 프레임 병합하기 
user <- merge(height,weight,by.x = "id", by.y = "id")
user
#    id h  w
# 1  1 180 80
# 2  2 175 75

install.packages("UsingR")
library(UsingR)
data("Galton" )
str(galton)
dim(galton)   # 차원 보기 
# [1] 928   2
head(galton,15)   # 앞부분 15개 관측치 출력 


# list 자료구조 
# 리스트 생성 함수 : list()
# 리스트 자료 처리 함수 : unlist(), lapply(), sapply()

list <- list("lee","이순신", 95) #리스트 객체 생성
list
# [[1]]         # 대괄호 중첩된 모양의 [[n]] n 은 원소의 위치를 나타내는 색인
# [1] "lee"
# 
# [[2]]
# [1] "이순신"
# 
# [[3]]
# [1] 95

unlist <- unlist(list)  # unlist()함수를 이용하여 리스트를 벡터 구조로 변경
unlist                  # vector 형식으로 출력 

#list(key1 = value, key2 = value2 ....)

member <- list(name =c("홍길동","유관순"),age = c(35, 25),
               address =c ("한양","충남"),gender =c("남자","여자"),
               htype = c("아파트","오피스텔"))
member
member$name       # [1] "홍길동" "유관순"
member$name[1]    # [1] "홍길동"
member$name[2]    # [1] "유관순"

member$age[1] <- 45
member$id <- "뽀로로"   # id key 추가
member$pwd <- "1234"    # pwd key 추가 
member
member$ age <- NULL  # age 원소 제거 
member
length(member) # 리스트 객체 개수 확인 
mode(member); class(member)
# [1] "list"   # 리스트 객체 member 의 자료구조 확인 
# [1] "list"

test_a <- list(c(1:5))
test_b <- list(6:10)
lapply(c(test_a,test_b),max)

multi_list <- list(c1 = list(1,2,3),
                   c2 = list(10,20,30),
                   c3 = list(100,200,300))
#다차원 리스트 보기 
multi_list$c1;multi_list$c2;multi_list$c3
# 다차원 리스트를 열 단위로 바인딩 하기 
do.call(cbind,multi_list)
#      c1 c2 c3 
# [1,] 1  10 100
# [2,] 2  20 200
# [3,] 3  30 300


# 문자열 처리 
install.packages("stringr")
library(stringr)
str_extract_all("홍길동35이순신45유관순25","[1-9]{2}")
# [[1]]
# [1] "35" "45" "25"

string1 <- "tamy27yuong5leess1002yummi201감강찬2005유하영25교사7778코키리88"
str_extract_all(string1,"[a-z]{3}")  #영문 소문자 3자 연속 하는 경우 추출
# [[1]]
# [1] "tam" "yuo" "lee" "yum"
str_extract_all(string1,"[a-z]{3,5}") #영문 소문자 3~5자 연속하는 경우 추출
# [[1]]
# [1] "tamy"  "yuong" "leess" "yummi"
str_extract_all(string1,"yu")        # 해당 문자열 추출
# [[1]]
# [1] "yu" "yu"
str_extract_all(string1,"25")        # 해당 숫자 추출
# [[1]]
# [1] "25"
str_extract_all(string1,"[가-핳]{3}")     # 3개 연속된 한글 문자열  추출
# [[1]]
# [1] "감강찬" "유하영" "코키리"
str_extract_all(string1,"[^a-z]{3}")  # 영문자를 제외한 연속된 3글자 추출 
# [[1]]
# [1] "100"    "201"    "감강찬" "200"    "5유하"  "영25"   "교사7"  "778"    "코키리"
str_extract_all(string1,"[^0-9]{4}")  # 숫자를 제외한 연속된 4글자 추출
# [1] "tamy" "yuon" "lees" "yumm"


jumin <- "123456-1234567"
str_extract(jumin,"[0-9]{6}-[1234][0-9]{6}")
str_extract_all(jumin,"\\d{6}-[1234]\\d{6}")  # d{6}: 숫자 6개
# \\d{6}  = "[0-9]{6}" 같은 결과 

string2 <- "hongkd105leess1002you25강감찬2005"
len <- str_length(string2)    # 문자열의 길이 구하기 
len   
# [1] 30
str_locate(string2 ,"강감찬")  # 문자열의 위치 구하기 
#       start end
# [1,]    24  26
ustr <-str_to_lower(string2)  # 모두 소문자로 바꾸기 
ustr
ustr <-str_to_upper(string2)  # 모두 대문자로 바꾸기 
ustr

str_extract_all(string1,"[^a-z]") # 영문자 제외한 나머지 추출
str_extract_all(string1,"[^a-z]{4}")  # 영문자 제외한 연속된 4글자 추출
str_extract_all(string1,"[^가-힣]{5}")

str_extract_all(name,"\\w{3,}") # 3글자 이상의 단어(숫자포함)만 추출
unlist(str_extract_all(name,"\\w{3,}"))
string_join <- paste(TrainingJob,collapse = ",")

