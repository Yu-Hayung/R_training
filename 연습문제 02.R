# 1. 다음과 같은 벡터 객체를 생성하시오
#   조건1) 벡터 변수 Vec1을 만들고 . R 문자가 5회 반복되도록 하시오.
#   조건2) 벡터 변수 Vec2에 1~10까지 3을 가격으로 연속된 정수를 만드시오.
#   조건3) 벡터 변수 Vec3에 1~10까지 3을 가격으로 연속된 정수가 3회 반복되도록 만드시오.
#   조건4) 벡터 변수 Vec4에는 Vec2~Vec3이 모두 포함되는 벡터를 만드시오.
#   조건5) 25~-15까지 5를 간격으로 seq()함수를 이용하여 벡터를 생성하시오.
#   조건6) 벡터 변수 Vec4에서 홀수 번째 값들만 선택하여 벡터 변수 Vec5에 첨자를 이용하여 할당하시오.

Vec1 <- rep('R',each = 5)
Vec2 <- rep(1:10)
Vec3 <- rep(1:10, each = 3)
Vec4 <- c(Vec2,Vec3)
seq(25,-15,-5)
seq(15,25,5)
Vec5 <- Vec4[seq(1, 41, by=2)]





# 2. 다음과 같은 벡터 칼럼으로 갖는 데이터프레임을 생성하시오.
TrainingName <- c("최민수","유관순","이순신","김유신","홍길동")
TrainingAge <- c(55,45,45,53,15)
TrainingGender <- c(1,2,1,1,1)
TrainingJob <- c("연예인","독립운동가","군인","군인","학생")
TrainingSat <- c(3,4,2,5,5)
TrainingTotal <-c(44.4,28.5,43.4,NA,27.1) #총구매금액(NA:결측치)
#   조건1) 위 7개의 벡터를 칼럼으로 갖는 TrainingUser 데이터프레임을 생성하시오.
#   조건2) gender 변수를 이용하여 히스토그램을 그리시오.
#   조건3) 데이터프레임 TrainingUser에서 짝수 행만 선택해서 TrainingUser2에 넣으시오.

TrainingUser <- data.frame(name=TrainingName, age=TrainingAge, gender=TrainingGender, job=TrainingJob,
                           sat=TrainingSat, total=TrainingTotal)
hist(TrainingUser$gender) # 문자열이 포함되어 있어 히스토그램 안그려짐
TrainingUser2 <- TrainingUser[seq(2,nrow(TrainingUser), by=2),]  # 2~끝 까지 짝수 행 추출



# 3. Data를 대상으로 apply()를 적용하여 행/열 방향으로 조건에 맞게 통계량을 구하시오.
kor <- c(90,85,90)
eng <- c(70,85,75)
mat <- c(86,92,88)
#   조건1) 3개의 과목점수를 이용하여 데이터프레임(Data)을 생성하시오.
#   조건2) 행/열 방향으로 max()함수를 적용하여 최댓값을 구하시오.
#   조건3) 행/열 방향으로 mean()함수를 적용하여 평균을 구하여 소숫점2자리까지 표현하시오.
#   조건4) 행단위의 분산과 표준편차를 구하시오.

subjectData <- data.frame(kor=kor,eng=eng,mat=mat)
apply(subjectData,1,max)
apply(subjectData,2,max)
round(apply(subjectData,1,mean),2)
round(apply(subjectData,2,mean),2)
apply(subjectData,1,var)
apply(subjectData,1,sd)




# 4. 다음의 Data2 객체를 대상으로 정규표현식을 적용하여 문자열을 처리하시오.
Data2 <- c("2017-02-05 수입3000원",
           "2017-02-06 수입4500원",
           "2017-02-07 수입2500원")
library(stringr)
#   조건1) 일자별 수입을 다음과 같이 출력하시오.
#   조건2) 위 벡터에서 연속하여 2개 이상 나오는 모든 숫자를 제거하시오
#   조건3) 위 벡터에서 -를 /로 치환하시오.
#   조건4) 모든 원소를 쉽표(,)에 의해서 하나의 문자열로 합치시오.

won <- str_extract_all(Data2,"[0-9]{4}[가-힣]")
unlist(won)
str_replace_all(Data2, '[0-9]{2}', '') 
str_replace_all(Data2, '-', '/')
paste(Data2, collapse = ',')

