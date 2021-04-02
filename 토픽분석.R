# 토픽분석 
# 한글 사전과 텍스트 마이닝 관련 패키지 설치 

library('KoNLP')
library('RSQLite')
library('tm')
library('wordcloud')

#   가져오기 
facebook <- file('/Users/yuhayung/Desktop/coding/학원/texttest/facebook_bigdata.txt', encoding = "UTF-8")
facebook_data <- readLines(facebook)
head(facebook_data)

user_dic <- data.frame(term = c('R프로그래밍', '페이스북', '김진성', '소셜네트워크'), tag ='ncn')
# term : 추가 단어 / tag : ncn(명사 지시코드)

buildDictionary(ext_dic = 'sejong', user_dic = user_dic)
# buildDictionary KoNLP 제공함수 : 신규단어를 사전에 추가 하는 역활을 한다. 
# ext_dic = 'sejong' : 속성은 구축할 사전을 지정
# user_dic : 사전에 추가될 단어의 모음이다. 
# tag = 'ncn' 속성은 추가하  품사가 명사라는 의미이다. 



###################################################################################################

# 단어를 추출을 위한 ᄉ 함수 정의 
paste(extractNoun('유하영은 많은 사람과 소통을 위해서 소셜네트워크에 가입하였습ᄂ다.'), collapse =" ")
# collapse : 추출된 명사들을 공백으로 연결하는 역활을 한다. 

exNouns <- function(x){paste(extractNoun(as.character(x)), collapse = " ")}
# 사용자 정의 함수는 [문자변환] > [단어 추출] > [공백으로 합침] 순서로 실ᄒ 예에서 처럼 정의

facebook_nouns <- sapply(facebook_data, exNouns)
facebook_nouns[1]


###################################################################################################
# 추출 단어  
myCorpus <- Corpus(VectorSource(facebook_nouns))
myCorpus

# 데이터 전처리 
# 단계2-1 : 문장부호 제거
myCorpusPrepro <- tm_map(myCorpus,removePunctuation)

# 단계2-2 : 수
myCorpusPrepro <- tm_map(myCorpusPrepro, removeNumbers)

# 단계2-3 : 소문자 변경
myCorpusPrepro <- tm_map(myCorpusPrepro, tolower)

# 단계2-4 : 불용어 제거 
myCorpusPrepro <- tm_map(myCorpusPrepro, removeWords, stopwords('english'))

# 단계2-1 : 절처리 결과 확인 
inspect(myCorpusPrepro[1:5])


###################################################################################################

# 단어 선별하기 / 한글 1 음절은 2byte에 저장(2 음절 =4byte)
myCorpusPrepro_term <- TermDocumentMatrix(myCorpusPrepro, control = list(wordLengths = c(4, 16)))
myCorpusPrepro_term

# matrix 자료 구조를 data.frame 자료구조로 변셩 
myTerm_df <- as.data.frame(as.matrix(myCorpusPrepro_term))
dim(myTerm_df)


###################################################################################################
# 단어 출현 빈도수 구하기
wordResult <- sort(rowSums(myTerm_df), decreasing = T)
wordResult[1:10]  # 빈도수가 높은 상위 10개 단어 보기 

# 불용어 제거 
myCorpusPrepro <- tm_map(myCorpus, removePunctuation)   # 문장부호 제거 
myCorpusPrepro <- tm_map(myCorpusPrepro, tolower)       # 소문자 변경  
myCorpusPrepro <- tm_map(myCorpusPrepro, removeNumbers) # 숫자 제거 
myStopwords = c(stopwords('english'), '사용','하기');   # 제거할 단어 지
myCorpusPrepro <- tm_map(myCorpusPrepro, removeWords, myStopwords) 

# 단어 선별과 평서문 변환 
myCorpusPrepro_term <- TermDocumentMatrix(myCorpusPrepro, control = list(wordLengths = c(4,16)))

# 말뭉치 객체를 평서문으로 변환 
myTerm_df <- as.data.frame(as.matrix(myCorpusPrepro_term))

# 단어 출현 빈도수 구하기 
wordResult <- sort(rowSums(myTerm_df), decreasing = T)
wordResult[1:10]


#################################################################################################3
# 단어구름 시각화 
# 단어구름ᄋ 디자인(빈도수 , 생상, 위치, 회전 등) 적용하기

# 단어 이름과 빈도수로 data.frame 생성 
myName <- names(wordResult)        # 단어 이름 생성 
word.df <- data.frame(word = myName, freq = wordResult) # 데이터프레임 생성 
str(word.df)

# 단어색상과 글  
pal <- brewer.pal(12, "Paired")   # 12가지 생상 적용 

# 단어 구름 시각화 
wordcloud(word.df$word, word.df$freq, scale = c(5, 1), min.freq = 3, 
          random.color = F, rot.per = .1, colors = pal, family = 'NanumGothic')


