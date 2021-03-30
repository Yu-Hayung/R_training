#  데이터 불러오기
setwd("c:/rwork")
data <- read.csv("world_bank_data.csv", header = T)
meta <- read.csv("world_bank_metadata.csv", header = T)
library(dplyr)
library(stringr)

#  데이터 전처리
data1 <- data[-c(2:3, 5:22, 24)]
str(data1)
data1 <- data1[-c(13672:16637), ]
pop.total <- subset(data1, Series.Code == "SP.POP.TOTL")
pop.city <- subset(data1, Series.Code == "EN.URB.LCTY")

names(pop.total) <- c("Country", "Code", "Data")
names(pop.city) <- c("Country", "Code", "Data")
#########################################################
# (1) 2019년 기준 나라별 총 인구수, 그 나라에서 가장 큰 도시에 사는 인구수, 나라별 총 인구수
# 대비 그 나라에서 가장 큰 도시에 사는 인구의 비율을 구하시오.
# (Population in largest city 항목 데이터 사용)
#########################################################

pop.total  # 2019년 기준 나라별 총 인구 수
pop.city   # 가장 큰 도시에 사는 인구 수

col.data <- bind_cols(pop.total, pop.city$Data)
col.data <- col.data[-2]
names(col.data) <- c("Country", "Total", "City")
ratio.pop <- round((as.numeric(col.data$City) / as.numeric(col.data$Total)), 4) * 100
col.data2 <- bind_cols(col.data, ratio.pop)
names(col.data2) <- c("Country", "Total", "City", "Ratio")  # 도시 인구 / 총 인구 비율


#########################################################
# (2) 총 인구수 대비 그 나라에서 가장 큰 도시에 사는 인구의 비율이 가장 높은 나라 10개국 리
# 스트를 작성하시오 (비율 항목 포함)
# (3) 총 인구수 대비 그 나라에서 가장 큰 도시에 사는 인구의 비율이 가장 낮은 나라 10개국 리
# 스트를 작성하시오. (비율 항목 포함)
#########################################################

col.data3 <- na.omit(col.data2)

higher.data <- col.data3 %>% arrange(desc(Ratio))
head(higher.data, 10)  # (2) 비율 높은 나라 10개국
lower.data <- col.data3 %>% arrange(Ratio)
head(lower.data, 10)  # (3) 비율 낮은 나라 10개국


#########################################################
# (4) 해당 항목에 데이터가 없는 나라의 리스트를 작성하시오
#########################################################

non.data <- subset(col.data2, is.na(col.data2$Ratio) == T)
non.data  # 데이터가 없는 나라 리스트
