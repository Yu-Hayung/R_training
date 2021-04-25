# 연관성 분석 

# install.packages("arules")
# install.packages('arulesViz')
library(arules)
library(arulesViz)
data("Groceries")

inspect(Groceries[1:10])

sort(itemFrequency(Groceries, type = "absolute"), decreasing = T)

itemFrequencyPlot(Groceries, topN = 10, type = "absolute" , main = "아이템 (절대)빈도" , family="NanumGothicBold")
itemFrequencyPlot(Groceries, topN = 10, type = "relative" , main = "아이템 (상)빈도" , family="NanumGothicBold")

apriori(Groceries)
# set of 0 rules 

result_rules <- apriori(Groceries, parameter = list(support = 0.005, confidence = 0.5, minlen = 2))
# 120 rule(s)

summary(result_rules)
inspect(result_rules[1:5])

rules_lift <- sort(result_rules, by = "lift", decreasing = T) # 향상도 기준 정렬 
inspect(rules_lift[1:5])
rules_conf <- sort(result_rules, by = "confidence", decreasing = T) # 신뢰도 기준 ᄌ 
inspect(rules_conf[1:5])

# 한가지 제품으로 연관성 찾기 
rhs.milk_rule <- subset(rules_lift, rhs %in% "whole milk")
rhs.milk_rule
inspect(rhs.milk_rule[1:5])


# 한가지 제품으로 규칙과 세분화된 연관성 규칙을 만들기 
wholemik_rule <- apriori(Groceries, parameter = list(support = 0.005, confidence = 0.5, minlen = 2), 
                         appearance = list(default = 'lhs', rhs = "whole milk"))
wholemik_rule <- sort(wholemik_rule, by = 'lift', decreasing = T)
inspect(wholemik_rule[1:5])


# 시각화 

library(arulesViz)
plot(wholemik_rule[1:10], method = "graph", measure = "lift", shading = 'confidence')

