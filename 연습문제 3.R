# 1. 본문에서 작성한 Titanic 변수를 다음과 같은 단계를 통해서
# "titanic.csv" 파일로 저장한 후 파일을 불러 오시오

# [단계 1] "C:/Rwork/output" 폴더에 "titanic.csv"로 저장한다. 
# [단계 2] "tanic.csv" 파일을 titanicData 변수로 가져와서 결과를 확인하고,
# titanicData의관측치와 칼럼수를 확인한다.
# [단계 3] 1번, 3번 칼럼을 제외한 나머지 칼럼을 대상으로 상위 6개의 관측치를 확인한다. 

write.csv(Titanic, "/Users/yuhayung/Desktop/coding/학원/R_Training/dataset1/titanic.csv")
titanicData = read.csv("/Users/yuhayung/Desktop/coding/학원/R_Training/dataset1/titanic.csv")
str(titanicData)
head(titanicData[, -c(1,3)])


# 2. R에서 제공하는 CO2 데이터셋을 대상으로 다음과 같이 파일로 저장하시오. data(CO2)
# [단계1] Treatment 칼럼 값이 'nonchilled'인 경우 'CO2_df1.csv' 파일로 저장 
# [단계2] Treatment 칼럼 값이 'chilled'인 경우 'CO2_df2.csv' 파일로 저장 

data1 <- subset(CO2, Treatment=='nonchilled')
write.csv(data1, "CO2_df1.csv", row.names = F)
data2 <- subset(CO2, Treatment=='chilled')
write.csv(data2, "CO2_df1.csv", row.names = F)

