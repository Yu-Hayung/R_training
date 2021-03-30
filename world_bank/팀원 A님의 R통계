# 제공된 데이터셋은 세계은행(World Bank)에서 집계한 세계 264개국 인구와 관련된 다양한 통계량이다. 
# 제공된 데이터셋을 이용하여 R code로 다음을 수행하고 결과를 팀 별로 리포트 하시오.
rm(Series)

data <- read.csv("world_bank_Data.csv", header = T)
Metadata <- read.csv("world_bank_Metadata.csv", header = T)

names(data)[1] <- c("Country.Name") # 이름 재정의
unique(data$Country.Name) 

Year2019 <- subset(data, select = c(Country.Name, Country.Code, Series.Name, Series.Code, X2019..YR2019.)) 
Year2019# subset 작성(모든 문제는 2019 기준이로 작성한다)

install.packages('dplyr')
library(dplyr)

# (1) 2019년 기준 나라별 총 인구수, 그 나라에서 가장 큰 도시에 사는 인구수, 나라별 총 인구수 대비 그 나라에서 가장 큰 도시에 사는 인구의 비율을 구하시오.
# (Population in largest city 항목 데이터 사용)

# 2019년 기준 나라별 총 인구수
totalPopulation <- subset(Year2019 %>% filter(Series.Code == 'SP.POP.TOTL'), select = c(Country.Name, X2019..YR2019.))
totalPopulation
# 그 나라에서 가장 큰 도시에 사는 인구수
populationInLargestCity <- subset(Year2019 %>% filter(Series.Name == 'Population in largest city'), select = c(Country.Name, X2019..YR2019.))
populationInLargestCity

# 위의 두 데이터를 나라 이름기준으로 병합
popuSummary <- merge(x=totalPopulation, y=populationInLargestCity, by="Country.Name", all = TRUE)
names(popuSummary)[2] <- c("Total")  # 이름 재정의
names(popuSummary)[3] <- c("populationInLargestCity") # 이름 재정의
popuSummary

# 나라별 총 인구수 대비 그 나라에서 가장 큰 도시에 사는 인구의 비율
popuSummary$populationInLargestCity<-as.numeric(popuSummary$populationInLargestCity)# 형변환
popuSummary$Total<-as.numeric(popuSummary$Total)
popuSummary$per <- popuSummary$populationInLargestCity / popuSummary$Total * 100
popuSummary # 완성!

# (2) 총 인구수 대비 그 나라에서 가장 큰 도시에 사는 인구의 비율이 가장 높은 나라 10개국 리스트를 작성하시오 (비율 항목 포함)
Top10Country <- popuSummary %>% arrange(desc(per))%>% slice(1:10)

Top10CountryList <- paste(Top10Country$Country.Name, Top10Country$per, seq = ' ')
Top10CountryList
# (3) 총 인구수 대비 그 나라에서 가장 큰 도시에 사는 인구의 비율이 가장 낮은 나라 10개국 리스트를 작성하시오. (비율 항목 포함)
Bottom10Country <- popuSummary %>% arrange(per)%>% slice(1:10)
Bottom10Country

Bottom10CountryList <- paste(Bottom10Country$Country.Name, Bottom10Country$per, seq = ' ')
Bottom10CountryList

# (4) 해당 항목에 데이터가 없는 나라의 리스트를 작성하시오
naCountryList <- subset(popuSummary %>% filter(is.na(per)),select = c(Country.Name))['Country.Name']
naCountryList
