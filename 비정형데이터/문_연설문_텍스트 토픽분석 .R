# 문재인 대통령 취임식 연설문 텍스트 분석 


# 토픽분석 
# 한글 사전과 텍스트 ᄆ  패키지 설치 

library('KoNLP')
library('RSQLite')
library('tm')
library('wordcloud2')

#   가져오기 
moon <- file('speech_moon.txt', encoding = "UTF-8")
moon_data <- readLines(moon )
head(moon_data)

buildDictionary(ext_dic = 'sejong', user_dic = user_dic)

exNouns <- function(x){paste(extractNoun(as.character(x)), collapse = " ")}

moon_nouns <- sapply(moon_data, exNouns)
moon_nouns[1:20]


myCorpus <- Corpus(VectorSource(moon_nouns))
myCorpus


myCorpusPrepro <- tm_map(myCorpus,removePunctuation)
myCorpusPrepro <- tm_map(myCorpusPrepro, removeNumbers)
myCorpusPrepro <- tm_map(myCorpusPrepro, tolower)
myCorpusPrepro <- tm_map(myCorpusPrepro, removeWords, stopwords('english'))
inspect(myCorpusPrepro[1:5])

myCorpusPrepro_term <- TermDocumentMatrix(myCorpusPrepro, control = list(wordLengths = c(4, 16)))
myCorpusPrepro_term

myTerm_df <- as.data.frame(as.matrix(myCorpusPrepro_term))
dim(myTerm_df)


wordResult <- sort(rowSums(myTerm_df), decreasing = T)
wordResult_s <- wordResult[1:100]

#################################################################################################3
# 단어구름 시각화 

myName <- names(wordResult_s)    
word.df <- data.frame(word = myName, freq = wordResult_s) 
str(word.df)

wordcloud2(word.df, fontWeight = 'bold', backgroundColor = 'gray', color = 'white', shape = 'diamond', ellipticity = 0.65 )



