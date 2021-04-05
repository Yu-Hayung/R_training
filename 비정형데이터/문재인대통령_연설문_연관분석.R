# 문재인 대통령 연설문 _ 연관 분석 

# 연관어 분석 
library(igraph)
library(arules)
library(KoNLP)



# 텍스트 파이 가져오기와 단어  
moon <- file('speech_moon.txt', encoding = "UTF-8" )
moon2 <- readLines(moon)  # 줄 단위 데이터 생성 
close(moon2)   # marketing   제거 
head(moon2)

lword <- Map(extractNoun, moon2)
length(lword)

lword <- unique(lword)
length(lword)

lword <- sapply(lword, unique)
length(lword)
lword

filter1 <- function(x){nchar(x) <= 4 && nchar(x) >= 2 && is.hangul(x)}
filter2 <- function(x){Filter(filter1, x)}

lword <- sapply(lword, filter2)
lword

wordtran <- as(lword, "transactions")
wordtran



tranrules <- apriori(wordtran, parameter = list(supp = 0.06 , conf = 0.1))
tranrules

inspect(tranrules)


rules <- labels(tranrules, ruleSep = " ")
rules

rules <- sapply(rules, strsplit, " ", USE.NAMES = F)

rulemat <- do.call("rbind", rules)
class(rulemat)


ruleg <- graph.edgelist(rulemat[c(17:60), ], directed = F)
ruleg

# 시각화
par(family="NanumGothicBold")
plot.igraph(ruleg, vertex.label = V(ruleg)$name,
            vertex.label.cex = 1.2,
            vertex.label.color = 'black',
            vertex.size = 20, 
            vertex.color = 'pink',
            vertex.frame.color = 'red')







