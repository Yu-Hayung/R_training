# 분석용 데이터 구축 평가 문제 답안 제출 - 유하영 HAYUNG YOU


##################################  평가 문항 2 번   ##############################

# (1) Animals 데이터 셋 구조 보기
library(MASS)
data(Animals)
head(Animals)

# (2) 요약통계량
summary(Animals$body)

# (3) 평균
mean(Animals$body)

# (4) 표준편차
sd(Animals$body)

# (5) Animals 데이터 셋의 빈도수 구하기
table(Animals$body)
plot(Animals$body)


##################################  평가 문항 3 번   ##############################

# 우리나라 전체 중학교 2학년 여학생 평균 키가 150cm로 알려진 상태에서 
# A 중학교 2학년 전체 500명을 대상으로 20%인 100명을 표본으로 선정하여 표본평균 
# 신장을 계산하고, 95% 신뢰수준에서 모집단의 평균과 차이가 있는지를 단계별로 분석을 수행하여 검정하시오.

# 두집단 평균 차이 검정 

data1 <- read.csv("/Users/yuhayung/Desktop/student_height2.csv", header = T)
head(data1)

# (1) 데이터의 요약기술통계량 계산
summary(data1)


# (2) 데이터 전처리: 결측치 제거하여 데이터 구성
student <- subset(data1, !is.na(height), c(sudent.id, height))
student_h <- student$height

length(student_h)  # [1] 95
mean(student_h)    # [1] 149.4737

# (3) 정규성 검정
shapiro.test(student_h) # p-value = 0.2794  = 정규분포함 

# (5) 가설 검정
t.test(student_h,  alternative = c("two.sided"), mu = 149.47 , conf.level = 0.95)
# p-value = 0.1866 <- 귀무가설 채택 

# (4) 가설 설정: 귀무가설과 대립가설 설정 (6) 검정결과 및 해석

# 귀무가설 :모집단의 평균과 차이가 없다 
# 연구가설 :모집단의 평균과 차이가 있다. 
# 결론 - 귀무가설 채택으로 평균 차이가 없다. 


##################################  평가 문항 4 번   ##############################
exam_data = data.frame(
  name = c('Anastasia', 'Dima', 'Katherine', 'James', 'Emily', 'Michael', 'Matthew', 'Laura', 'Kevin', 'Jonas'),
  score = c(12.5, 9, 16.5, 12, 9, 20, 14.5, 13.5, 8, 19),
  attempts = c(1, 3, 2, 3, 2, 3, 1, 1, 2, 1),
  qualify = c('yes', 'no', 'yes', 'no', 'no', 'yes', 'yes', 'no', 'no', 'yes')
)

# (1) 각 이름의 국적은 다음과 같다. 각 개인의 국적을 데이터프레임에 추가하고 데이터프레임을 화면 출력하시오.
from <- c("RUS", "CHN", "USA", "USA", "USA", "USA", "USA", "USA", "USA", "USA") 
exam_data[5] <- from
names(exam_data) <- c("name","score","attempts","qualify","from")
exam_data

# (2) 기존의 데이터프레임에 다음의 두 사람을 추가하고 업데이트 된 데이터프레임을 화면 출력하시오
df1 <- data.frame( name = c("kim", "lee"),
                   score = c(15, 10), 
                   attempts = c(1, 3), 
                   qualify = c('yes', 'no') ,
                   from = c("KOR", "KOR") 
                   )

exam_data <- rbind(exam_data,df1)
exam_data

# (3) Qualify 항목을 제외한 데이터프레임을 화면 출력하시오
exam_data1 <- exam_data[ ,-4]
exam_data1 

# (4) Dima와 Jona를 제외한 데이터프레임을 화면 출력하시오
df2 <- exam_data[-2, ]
exam_data2 <- df2[-9, ]
exam_data2

# (5) 이름과 그들의 국적만 화면 출력하시오
exam_data3 <- data.frame(exam_data$name,exam_data$from)
exam_data3

