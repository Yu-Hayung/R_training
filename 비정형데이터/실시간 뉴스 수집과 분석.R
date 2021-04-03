# 실시간 뉴스 수집과 분석 

# 준비
install.packages("httr")
library(httr)

install.packages("XML")
library(XML)

# url 요청 
url <- "http://www.incheonilbo.com/news/articleList.html?view_type=sm"
web <- GET(url)
web

# HTML 파싱 
html <- htmlTreeParse(web,useInternal=TRUE, trim = T, encoding = "utf-8")
rootNode <- xmlRoot(html)


# 태그 자료 수집하기 
# <section class="user-snb">
news <- xpathSApply(rootNode, "//section[@class = 'user-snb']", xmlValue)



# 수집한 자료 전처리 
news_pre <- gsub("[\r\n\t]",' ', news)
news_pre <- gsub("[[:punct:]]",' ', news_pre)
news_pre <- gsub("[[:cntrl:]]",' ', news_pre)
news_pre <- gsub("\\d+",' ', news_pre)
news_pre <- gsub("[a-z]+",' ', news_pre)
news_pre <- gsub("[A-Z]+",' ', news_pre)
news_pre <- gsub("\\s+",' ', news_pre)


news_data <- news_pre
news_data 
# 파일 저장과 읽ᄀ 
setwd("/Users/yuhayung/Desktop/coding/학원/texttest")
write.csv(news_data, "news_data.csv", quote = F )

news_text <- read.csv("news_data.csv", header = T)
news_text

install.packages("remotes")
remotes::install_github("haven-jeon/KoNLP", upgrade = "never", INSTALL_opts = c("--no-multiarch"))
library(KoNLP)
# 세종 사전에 단어 추가 
user_dic <- data.frame(term = c ("펜데믹", "코로나19", "타다", "에듀ᄋ", "시밀러룩", 
                                 "생생정보","방역당국", "집단감염", "신규", "확진자 ",
                                 "확진", "안전보장이사회", "국가안보국장"))
buildDictionary(ext_dic = 'sejong', user_dic = user_dic)

# 사용자 정의 함수 작성 
exNouns <- function(x){paste(extractNoun(as.character(x)), collapse = " ")}


# 단어 추출 
news_nouns <- sapply(news_text, exNouns)
news_nouns
str(news_nouns)

library(tm)
newsCorpus <- Corpus(VectorSource(news_nouns))
newsCorpus

# 단어 vs 문서 집계 행렬 만들기 
TDM <- TermDocumentMatrix(newsCorpus, control = list(wordLengths = c(4,16)))
TDM


tdm.df <- as.data.frame(as.matrix(TDM))
dim(tdm.df)

wordResult <- sort(rowSums(tdm.df), decreasing = TRUE)
wordResult[1:10]

library(devtools)
devtools::install_github("lchiffon/wordcloud2")
library(wordcloud2)

myNames <- names(wordResult)
myNames
str(myNames)

df <- data.frame(word = myNames, freq = wordResult)
head(df)
wordcloud2(data = df , fontFamily = '나눔바른고딕')
