#####

# 준비단계1 : 패키지와 라이브러리를 설치 한다.

install.packages("KoNLP")                            # 한글 명사를 추출하기 위한 패키지  
install.packages("tm" )                              # 글자 전처리 패키지 
install.packages('devtools')                         # 필요한 패키 다른 경로로 추출 패키지 
install.packages("arules")                           # 연관 분석 패키지
install.packages("igraph")                           # 연과 분석 시각화 패키지
install.packages("httr")                             # httr : 원격 서버의 URL 요청
install.packages("XML")                              # XML: HTML 문 파싱
library(igraph)
library(arules)                    
library(devtools)                
devtools::install_github("lchiffon/wordcloud2")  
library('KoNLP')             
library('tm')                 
library('wordcloud2')
library(httr)
library(XML)


#####

# 준비단계2 : 필요한 데이터를 불러 온다. 

# 1. 닥터 루터킹 연설문

# txt 파일 불러오 
Drking <- file('/Users/yuhayung/Desktop/coding/학원/texttest/DrKing.txt', encoding = "UTF-8")
Drking_data <- readLines(Drking)
head(Drking_data)

# 2. 뉴스 페이지 불러오기 ( html 부분의   추출하여 news에 데이터 저장 )

# url 요청
url <- "https://news.daum.net/"
web <- GET(url)
web


# HTML 파싱
html <- htmlTreeParse(web, useInternal=TRUE, trim = T, encoding = "utf-8")
rootNode <- xmlRoot(html)

# 태그
news <- xpathSApply(rootNode, "//a[@class = 'link_txt']", xmlValue) 
head(news, 20)

#####

# 준비 단계 3 : 한글 단어 사전에 명사 추가 ᄃ록

user_dic <- data.frame(term = c("펜데믹","오세훈", "코로나19", "코스닥","타다", "생생정","방역당국",
                                "집단감염", "신규", "확진자 ", "확진", "안전보장이사회", "국가안전",
                                'R프로ᄀ래밍','긴박','마틴루터','마틴','콜로라도','펜실베이니아', '펜더닉',
                                '고달픈', 'ᄏ19'), tag ='ncn')

buildDictionary(ext_dic = 'sejong', user_dic = user_dic)


######

# 준비 단계 4 :  필요한 사용자 하 정의  = 좀더 간결한 코드 ᄌ성을 위한 함수 

exNouns <- function(x){paste(extractNoun(as.character(x)), collapse = " ")}

filter1 <- function(x){nchar(x) <= 4 && nchar(x) >= 2 && is.hangul(x)}
filter2 <- function(x){Filter(filter1, x)}


############
 # 1. 마틴 루터킹 연설문 토픽 분석 
############
# 단어집에서 단어 추출 
Drking_nouns <- sapply(Drking_data, exNouns)
Drking_nouns[1]

# 추출 단어 전처리 

myCorpus <- Corpus(VectorSource(Drking_nouns))
myCorpus


myCorpusPrepro <- tm_map(myCorpus, removePunctuation)   # 문장부호 제거 
myCorpusPrepro <- tm_map(myCorpusPrepro, tolower)       # 소문자 변경  
myCorpusPrepro <- tm_map(myCorpusPrepro, removeNumbers) # 수 제거 
myStopwords = c(stopwords('english'),'하기','하게','날들','자리','야', '예전','이곳', '들이');   # 제거할 단어 지정
myCorpusPrepro <- tm_map(myCorpusPrepro, removeWords, myStopwords) 

# 결과 확인 

inspect(myCorpusPrepro[1:5])

# 단어 선별하기 

myCorpusPrepro_term <- TermDocumentMatrix(myCorpusPrepro, control = list(wordLengths = c(4, 16)))
myCorpusPrepro_term

# matrix 자료 구조를 data.frame 자료구조로 

myTerm_df <- as.data.frame(as.matrix(myCorpusPrepro_term))
dim(myTerm_df)

# 단어 추현 빈도수 구하기

wordResult <- sort(rowSums(myTerm_df), decreasing = T)
wordResult[1:10]  # 빈도수가   10개 단어 보기 

# 단어 선별

myCorpusPrepro_term <- TermDocumentMatrix(myCorpusPrepro, control = list(wordLengths = c(4,16)))

# 단어 출현 빈도

wordResult <- sort(rowSums(myTerm_df), decreasing = T)
wordResult[1:20]

wordResult_m <- as.matrix(wordResult)
wordResult_m

# 단어구름 시각화

myName <- names(wordResult)        # 단어 이름 생성 
df <- data.frame(word = myName, freq = wordResult)
head(df, 50)


wordcloud2(data = df , fontFamily = '나눔바른고딕')

############
# 2. 마틴 루터킹 연설문 연관 분ᄉ
############

# 연관 분석을 위한 전처리 

lword <- Map(extractNoun, Drking_data)

lword <- unique(lword)

# 중복 단어 제거

lword <- sapply(lword, unique)


lword <- sapply(lword, filter2)
head(lword)

wordtran <- as(lword, "transactions")
wordtran

# 연관 규칙 

tranrules <- apriori(wordtran, parameter = list(supp = 0.25, conf = 0.05, target="rules"))

# 행렬구조 

rules <- labels(tranrules, ruleSep = " ")
rules
rules <- sapply(rules, strsplit, " ", USE.NAMES = F)
rulemat <- do.call("rbind", rules)

ruleg <- graph.edgelist(rulemat[c(1:28), ], directed = F)
ruleg

# 시각화 

par(family="NanumGothicBold")
plot.igraph(ruleg, vertex.label = V(ruleg)$name,
            vertex.label.cex = 1.2,
            vertex.label.color = 'black',
            vertex.size = 20, 
            vertex.color = 'pink',
            vertex.frame.color = 'red')

############
# 2.    분석  
############

# 수집한 자료  
news_pre <- gsub("[\r\n\t]",' ', news)
news_pre <- gsub("[[:punct:]]",' ', news_pre)
news_pre <- gsub("[[:cntrl:]]",' ', news_pre)
news_pre <- gsub("\\d+",' ', news_pre)
news_pre <- gsub("[a-z]+",' ', news_pre)
news_pre <- gsub("[A-Z]+",' ', news_pre)
news_pre <- gsub("\\s+",' ', news_pre)

news_data2 <- news_pre[1:59]
head(news_data2, 20 )


# 파일 저장과 이동 

setwd("/Users/yuhayung/Desktop/coding/학원/texttest")
write.csv(news_data2, "news_data2.csv", quote = F )

news_text <- read.csv("news_data2.csv", header = T , stringsAsFactors = F)
str(news_data2)

# 칼럼명 지정

names(news_data2)<- c("no", "news_text")
head(news_data2)

# 단어 추출

news_nouns <- sapply(news_data2, exNouns)  # exNouns - 사용자 함수 
newsCorpus <- Corpus(VectorSource(news_nouns))

# 단어 vs ᄆ 집계 해 

TDM <- TermDocumentMatrix(newsCorpus, control = list(wordLengths = c(4,16)))
TDM


tdm.df <- as.data.frame(as.matrix(TDM))
dim(tdm.df)

wordResult2 <- sort(rowSums(tdm.df), decreasing = TRUE)
wordResult2[1:20]


myNames2 <- names(wordResult2)
myNames2
str(myNames2)


myNames2_m <- as.matrix(myNames2)
myNames2_m

# 뉴스

df2 <- data.frame(word = myNames2, freq = wordResult2)
head(df2)
wordcloud2(data = df2 , fontFamily = '나눔바른')


