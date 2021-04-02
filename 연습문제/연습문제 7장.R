# 연습문제 7장 

# 1. 본문에서 생성된 dataset2의 직급(position) 칼럼을 대상으로 1급 > 5급, 5급 >1ᄀ  역코딩하여 position2 칼럼에 추가하시오 
직급 <- dataset2$position
역직급 <- 6 - 직급
dataset2$position <- 역직급
dataset2$position2[dataset2$position == 1] <-'1급'
dataset2$position2[dataset2$position == 2] <-'2급'
dataset2$position2[dataset2$position == 3] <-'3급'
dataset2$position2[dataset2$position == 4] <-'4급'
dataset2$position2[dataset2$position == 5] <-'5급'


# 2. 본문에서 생성된 dataset2의 resident 칼럼을 대상으로 NA 값을 제거한후 resident2 변수에 ᄌ장하시오. 
resident2 <- na.omit( dataset2$resident )


# 03. dataset2의 gender 칼럼을 대상으로 1->"남자", 2->"여자" 형태로 코딩 변경 gender2 칼럼에 추가하고, 파이 차트로 결과를 확인하시오.
dataset2$gender2[dataset2$gender == 1] <- '남자'
dataset2$gender2[dataset2$gender == 2] <- '여자'
gender2 <- table(dataset2$gender2)
pie(gender2) 

# 04. 나이를 30세 이하 -> 1, 30~55 -> 2, 56이상 -> 3 으로 리코딩하여 age3 칼럼에 추가한  age, age2, age3 칼럼만 확인하시오.
dataset2$age3[dataset2$age <= 30] <- 1
dataset2$age3[dataset2$age > 30 & dataset2$age <= 55] <- 2
dataset2$age3[dataset2$age >= 56] <- 3

dataset2$age
dataset2$age2
dataset2$age3


# 05. 정제된 data를 대상으로 작업 디렉터리 cleanData.csv 파일명으로 따옴표와 행 이름을 제거ᄒ 저장하고, new_data변수로 읽어오시오. 
setwd("/Users/yuhayung/Desktop/coding/학원/Rtraining/dataset3")
write.csv(dataset2,"cleanData.csv ", quote=F, row.names=F)

new_data <- read.csv("cleanData.csv", header=TRUE)
new_data
dim(new_data)
str(new_data)


# 06. user_data.csv와 return_data.csv 파일을 이용하여 각 고객별 반품사유코드 (return_code)ᄅ 대상으로 다음과 같이 파생변수를 추가하시오.
data <- read.csv('user_data.csv', header = T)
head(data)

redata <- read.csv('return_data.csv',  header = T)
head(redata)

library(reshape2)
u_redata <- dcast(redata, user_id ~ return_code, length)
head(u_redata, 3)
names(u_redata)<-c('user_id','개인사유','오배송','하자품','변심')
head(u_redata, 3)


library(plyr) 
u_re_data <- join(data, redata, by='user_id')
head(u_re_data)

