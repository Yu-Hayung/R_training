#연습 문제 16 

# 1번 문제 ########################################################################
idist<- dist(iris[1:4]) # or dist(iris[, -5])
head(idist)

hc <- hclust(idist)

plot(hc, hang=-1) 

rect.hclust(hc, k=4, border="red")



# 2번 문제 ########################################################################
sales <- read.csv("product_sales.csv", header=TRUE)
model <- kmeans(sales, 3) 
model

model$cluster

sales$group <- model$cluster
head(sales)

cor(sales[,-5], method="pearson")

# 결과: 가장 큰 영향을 미치는 변수: avg_price

# 시각화 
plot(sales[c("tot_price", "avg_price")], col=sales$group)
# 군집의 중심점 시각화
points(result2$centers[,c("tot_price", "avg_price")], col=1:3, pch=8, cex=2)


# 3번 문제 ########################################################################
library(arules)
tranExam <- read.transactions("tranExam.csv", format="single",
                              sep=",", cols=c(1,2), rm.duplicates=T)

ruleExam<- apriori(tranExam)
ruleExam <- apriori(tranExam, parameter = list(supp= 0.3, conf= 0.1)) 

inspect(ruleExam) 


# 4번 문제 ########################################################################
library(arules)
data(Adult)
adult <- apriori(Adult, parameter = list(supp= 0.5, conf= 0.9))
adult # set of 52 rules

rules<- inspect(head (sort(adult, by="lift"), 10))

# 시각화 
library(arulesViz)
plot(adult) # 지지도, 신뢰도 향상도 산점도 


plot(adult, method="grouped") 
plot(adult, method="graph", control=list(type="items"))

# 결과: 자본이익과 자본손실의 단어를 중심으로 연관어가 형성되ᄋ 있다.