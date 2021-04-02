library("dplyr")
tbl_df(iris)

# 1번 문제 / iris의 꽃잎의 길이(Petal.Length) 칼럼을 대상으로 1.5 이상의 값만 필터링하시오. 
data1 <-  iris %>% filter(iris$Petal.Length >= 1.5 )
data1

# 2번 문제 / 문제1번 결과에서 1,3,5번 칼럼을 선택하시오.
data2 <-  data1 %>% select(Sepal.Length, Petal.Length,Species) 
data2

# 3번 문제 / 문제2번 결과에서 1번 칼럼에서 3번 칼럼의 값을 뺀 diff 파생변수를 만들고, 앞부분 6개만 출력하시오.(diff = 1번 칼럼 – 3번 칼럼)
data3 <- data2 %>% mutate(diff = Sepal.Length - Petal.Length) %>% head() 
data3

# 4번 문제 / 문제3번 결과에서 꽃의 종(Species)별로 그룹화하여 Sepal.Length와 Petal.Length 변수의 평균을 계산하시오.
data4 <- data3 %>% group_by(Species) %>% summarise(Sepal_mean = mean(Sepal.Length), Petal_mean = mean(Petal.Length))
data4

# 5번문제 / reshape2 패키지를 이용하여 단계별로 iris 데이터 셋을 처리하시오
install.packages("reshape2")
library("reshape2")

# [단계1] 꽃의 종류(Species)를 기준으로 ‘넓은 형식’을 ‘긴 형식’으로 변경하기
melt <- melt(iris, id=c("Species"), na.rm=TRUE)
head(melt)

# [단계2] 꽃의 종별로 나머지 4가지 변수의 합계 구하기
dcast <- dcast(melt, Species ~ variable, sum)
dcast
