# 비교 분석 

# 라이브러리 ############################################################



library(tidyr)
library(extrafont)
library(dplyr)
library(stringr)
library(tidytext)
library(KoNLP)
library(ggplot2)


# ᄃ어 빈도 비교하기 ####################################################

# 불러오기 
# 문재인 대통령 연설문 불러오기
raw_moon <- readLines("speech_moon.txt", encoding = "UTF-8")
moon <- raw_moon %>%
  as_tibble() %>%
  mutate(president = "moon")

# as_tibble 강제 자료구조화 
# mutate 는 강제 파생 column을 만ᄃ 

# 박근혜 대통령 연설문 불러오기
raw_park <- readLines("speech_park.txt", encoding = "UTF-8")
park <- raw_park %>%
  as_tibble() %>%
  mutate(president = "park")

bind_speeches <- bind_rows(moon, park) %>%
  select(president, value)

head(bind_speeches)
tail(bind_speeches)

#간단한 전처리 
speeches <- bind_speeches %>%
  mutate(value = str_replace_all(value, "[^가-힣]", " "),
         value = str_squish(value))
speeches


# 토큰화 = 형태소 분석기를 이용해 명사 기준 토큰화
speeches <- speeches %>%
  unnest_tokens(input = value,
                output = word,
                token = extractNoun)
speeches

# 하위 집단별 단어 빈도 구하기

df <- tibble(class = c("a", "a", "a", "b", "b", "b"),
             sex = c("female", "male", "female", "male", "male", "female"))

# 두 연설문의 단어 빈도 구하기
frequency <- speeches %>%
  count(president, word) %>% # 연설문 및 단어별 빈도
  filter(str_count(word) > 1) # 두 글자 이상 추출
head(frequency)


# 빈도 동점 단어 제외하고 추출하기
# slice_max(with_ties = F) : 원본 데이터의 정렬 순서에 따라 행 추출 

top10 <- frequency %>%
  group_by(president) %>%
  slice_max(n, n = 10, with_ties = F)
top10


# 단어 빈도 비교 시각화 ##################################################



ypar(family = 'AppleGothic')
ggplot(top10, aes(x = reorder(word, n),
                  y = n,
                  fill = president)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~ president) +
  theme(text = element_text(size = 12, family = "NanumBarunGothicBold"))


# 그래프별 y축 설정하기

ggplot(top10, aes(x = reorder(word, n),
                  y = n,
                  fill = president)) +
  geom_col() +
  coord_flip() +
  theme(text = element_text(size = 12, family = "NanumBarunGothicBold")) +
  facet_wrap(~ president, # president별 그래프 생성
             scales = "free_y") # y추 통일하지 않음



# 특정 단어 제외하고 막대 그래프 만들기 
# 국민이라는 단어가 너무 ᄆ이 제외 하기 

top10 <- frequency %>%
  filter(word != "국민") %>%
  group_by(president) %>%
  slice_max(n, n = 10, with_ties = F)

ggplot(top10, aes(x = reorder(word, n),
                  y = n,
                  fill = president)) +
  geom_col() +
  coord_flip() +
  theme(text = element_text(size = 12, family = "NanumBarunGothicBold")) +
  facet_wrap(~ president, scales = "free_y")



# (오즈비) 상대적으로 중요한   ###############################################

# 여러 텍스트 비교하기 편하게 데이터 구조 바꾸기
df_long <- frequency %>%
  group_by(president) %>%
  slice_max(n, n = 10) %>%
  filter(word %in% c("국민", "우리", "정치", "행복"))
df_long


# wide form: 가로로 넓은 형태의 데이터
# 범주별로 단ᄋ  비교하기 편함
# 변수간 연산하기 편함
# tidyr::pivot_wider() : long form을 wide form으로 변환
# names_from : 변수명으ᄅ  값이 들어 있는 변수
# values_from : 변수에 채워넣을 값이 들어 있는 변수


df_wide <- df_long %>%
  pivot_wider(names_from = president,
              values_from = n)
df_wide


#NA를 0으로 바꾸기

df_wide <- df_long %>%
  pivot_wider(names_from = president,
              values_from = n,
              values_fill = list(n = 0))
df_wide


# 연ᄉ 단어 빈도를 Wide form으로 변환하기

frequency_wide <- frequency %>%
  pivot_wider(names_from = president,
              values_from = n,
              values_fill = list(n = 0))
frequency_wide

#  비중을 나타낸 변수 추가하기
# 어떤 단어가 한 연설문에 전혀 사용되지 않으면 빈도 0, 오즈비 0, 단어 비중 비교 불가
# 빈도가 0보다 큰 값이 되도록 모든 값에 +1
frequency_wide <- frequency_wide %>%
  mutate(ratio_moon = ((moon + 1)/(sum(moon + 1))), # moon에서 단어의 비중
         ratio_park = ((park + 1)/(sum(park + 1)))) # park에서 단어의 비중
frequency_wide

# 오즈비 변ᄉ 추가하기
frequency_wide <- frequency_wide %>%
  mutate(odds_ratio = ratio_moon/ratio_park)
frequency_wide

frequency_wide %>%
  arrange(-odds_ratio)

# 상대적으로 중요한 단어 추ᄎ하기
# 오즈비가 가장 높거나 가장 낮은 단어 추출하기
top10 <- frequency_wide %>%
  filter(rank(odds_ratio) <= 10 | rank(-odds_ratio) <= 10)
top10 %>%
  arrange(-odds_ratio)

# (오즈비) 상대적으로 중요한 단어 시각화 ################################################
# 막대 그래프 만들기
# 비중이 큰 연설문을 나타낸 변수 추가하기
top10 <- top10 %>%
  mutate(president = ifelse(odds_ratio > 1, "moon", "park"),
         n = ifelse(odds_ratio > 1, moon, park))
top10

ggplot(top10, aes(x = reorder_within(word, n, president),
                  y = n,
                  fill = president)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~ president, scales = "free") +
  theme(text = element_text(size = 12, family = "NanumBarunGothicBold")) +
  scale_x_reordered()



# 주요 단어가 사용된 문장 살펴보기 
# 원문을 문장 기준으로 토크화하기
speeches_sentence <- bind_speeches %>%
  as_tibble() %>%
  unnest_tokens(input = value,
                output = sentence,
                token = "sentences")
speeches_sentence


# 주요 단어가 사용ᄃ  추출하기 - str_detect()
speeches_sentence %>%
  filter(president == "moon" & str_detect(sentence, "복지국가"))


# 중요도가 비슷한 단어 살펴보기
frequency_wide %>%
  arrange(abs(1 - odds_ratio)) %>%
  head(10)


# 중요도가 비슷하면서 빈도가 높은 단어: 두 텍스트에서 모두 강ᄌ 단어
frequency_wide %>%
  filter(moon >= 5 & park >= 5) %>%
  arrange(abs(1 - odds_ratio)) %>%
  head(10)


# 로그 오즈비 구하기
frequency_wide <- frequency_wide %>%
  mutate(log_odds_ratio = log(odds_ratio))
frequency_wide

# moon에서 비중이 큰 단어
frequency_wide %>%
  arrange(-log_odds_ratio)

# park에서 비중이 큰 단어
frequency_wide %>%
  arrange(log_odds_ratio)

# 비중이 비슷한 단어
frequency_wide %>%
  arrange(abs(log_odds_ratio))

# 두 연설문 각각 log_odds_ratio Top 10 추출
top10 <- frequency_wide %>%
  group_by(president = ifelse(log_odds_ratio > 0, "moon", "park")) %>%
  slice_max(abs(log_odds_ratio), n = 10, with_ties = F)
top10


# 막대 그래프 만들기
# 단어가 어느 연설문에서 중요  서로 다른 축 방향으로 표현됨
ggplot(top10, aes(x = reorder(word, log_odds_ratio),
                  y = log_odds_ratio,
                  fill = president)) +
  geom_col() +
  coord_flip() +
  labs(x = NULL) +
  theme(text = element_text(family = "NanumBarunGothicBold"))


# TF-IDF########################################################################

# 오즈비의 한계 - 두 조건의 확류 이용해 계산 여러 텍스트 비교하기 불편
# 두 개 이상의 텍스트 비교할 때는 TF-IDF 활용
# TF(단 빈도)와 IDF(역 문서 빈도)를 곱한 값
# TF: 단어가 분석 대상이 되는 텍스트 내에서 많이 사용될수록 커짐
# IDF: 단어가 사용된 텍스트가 드물수록 커짐



# 불러오기 

# speeches_presidents.csv : 역ᄃ  대선 출마 선언문
# readr::read_csv() 데이터를 다루기 편한 tibble 구조로 만들어 줌, read.csv()보다 빠름

install.packages("readr")
library(readr)
raw_speeches <- read_csv("speeches_presidents.csv")
raw_speeches

# 기본적인 전처리

speeches <- raw_speeches %>%
  mutate(value = str_replace_all(value, "[^가-힣]", " "),
         value = str_squish(value))
# 토큰화

speeches <- speeches %>%
  unnest_tokens(input = value,
                output = word,
                token = extractNoun)
# 단어 빈도 구하기

frequecy <- speeches %>%
  count(president, word) %>%
  filter(str_count(word) > 1)
frequecy


# # 2 TF-IDF 구하기
# tidytext::bind_tf_idf()
# term : 단어
# document : 텍스트 구분 기준
# n : 단어 비도

frequecy <- frequecy %>%
  bind_tf_idf(term = word, # 단어
              document = president, # 텍스트 구분 기준
              n = n) %>% # 단어 빈도
  arrange(-tf_idf)
frequecy


frequecy %>% filter(president == "문재인")
frequecy %>% filter(president == "박근혜")
frequecy %>% filter(president == "이명박")
frequecy %>% filter(president == "노무현")



# TF-IDF 낮은 단어 살펴보기
frequecy %>%
  filter(president == "문재인") %>%
  arrange(tf_idf)

frequecy %>%
  filter(president == "박근혜") %>%
  arrange(tf_idf)

frequecy %>%
  filter(president == "이명박") %>%
  arrange(tf_idf)

frequecy %>%
  filter(president == "노무현") %>%
  arrange(tf_idf)

#TF-IDF  시각화 #################################################################
# 막대 그래프 만들기 
# 주요 단어 추출
top10 <- frequecy %>%
  group_by(president) %>%
  slice_max(tf_idf, n = 10, with_ties = F)

# 그래프 순서 정하기
top10$president <- factor(top10$president,
                          levels = c("문재인", "박근혜", "이명박", "노무현"))

# 막대 그래프 만들기
ggplot(top10, aes(x = reorder_within(word, tf_idf, president),
                  y = tf_idf,
                  fill = president)) +
  geom_col(show.legend = F) +
  coord_flip() +
  facet_wrap(~ president, scales = "free", ncol = 2) +
  scale_x_reordered() +
  labs(x = NULL) +
  theme(text = element_text(family = "NanumBarunGothicBold"))


#모두 정리하기#################################################################

# 1. 단어 빈도 비교하기####################################
# 토큰화
speeches <- speeches %>%
  unnest_tokens(input = value,
                output = word,
                token = extractNoun)



# 하위 집단별 단어 빈도 ᄀ
frequency <- speeches %>%
  count(president, word) %>%
  filter(str_count(word) > 1)




# 가장 많이 사용된 단어 추출
top10 <- frequency %>%
  group_by(president) %>%
  slice_max(n, n = 10, with_ties = F)





# 2. 로그 오즈비로 단어 비교하기####################################
# long form을 wide form으로 변환

frequency_wide <- frequency %>%
  pivot_wider(names_from = president,
              values_from = n,
              values_fill = list(n = 0))



# 로그 오즈비 구하기

frequency_wide <- frequency_wide %>%
  mutate(log_odds_ratio = log(((moon + 1) / (sum(moon + 1))) /
                                ((park + 1) / (sum(park + 1)))))



# 상대적으로 중요한 단어 추출

top10 <- frequency_wide %>%
  group_by(president = ifelse(log_odds_ratio > 0, "moon", "park")) %>%
  slice_max(abs(log_odds_ratio), n = 10, with_ties = F)





# 3. TF-IDF로 단어 비교하기####################################
# TF-IDF 구하기
frequecy <- frequecy %>%
  bind_tf_idf(term = word,
              document = president,
              n = n) %>%
  arrange(-tf_idf)



# 상대적으로 중요한 단어 추출

top10 <- frequecy %>%
  arrange(tf_idf) %>%
  group_by(president) %>%
  slice_max(tf_idf, n = 10, with_ties = F)


