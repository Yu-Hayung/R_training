# 의미성 초점 분석 R 

# 기본적인 전처리
# 기생충 기사 댓글 불러오기
library(readr)
raw_news_comment <- read_csv("news_comment_parasite.csv")

# 전처리
library(dplyr)
library(stringr)
library(textclean)
news_comment <- raw_news_comment %>%
  select(reply) %>%
  mutate(reply = str_replace_all(reply, "[^가-힣]", " "),
         reply = str_squish(reply),
         id = row_number())

# 동시 풀현 단어 분석 : Co-occurrence analysis  ################

# 토큰화하기
# 단어가 사용된 맥락 살펴봐야 하므로 명사, 형용사, 동사 함께 추출
# 1. 형태소 분석기를 이용해 품사 기준ᄋ 토큰화하기
# SimplePos22() : 문장의 단어를 22개의 품사로 구분

library(tidytext)
library(KoNLP)
comment_pos <- news_comment %>%
  unnest_tokens(input = reply,
                output = word,
                token = SimplePos22,
                drop = F)
comment_pos %>%
  select(reply, word)


# 품사 분리하여 행 구성하기


# 품사별로 해 분리
library(tidyr)
comment_pos <- comment_pos %>%  separate_rows(word, sep = "[+]")

comment_pos %>%   select(word, reply)

# 3. 품사 추출하기 

# 명사 추출하기
noun <- comment_pos %>%
  filter(str_detect(word, "/n")) %>%
  mutate(word = str_remove(word, "/.*$"))
noun %>%  select(word, reply)

# 명사 빈도 구하기
noun %>%   count(word, sort = T)

# (2) 동사, 형용사 추ᄎ

# 동사, 형용사 추출하기
pvpa <- comment_pos %>%
  filter(str_detect(word, "/pv|/pa")) %>% # "/pv", "/pa" 추출
  mutate(word = str_replace(word, "/.*$", "다")) # "/"로 시작 ᄆ "다"로 바꾸기
pvpa %>%   select(word, reply)


# (3) 추출한 데이터 결합하기
# 추출한 단어 결합하기 이해할 수 이는 두 글자 이상 단어만 남기기

# 품사 결합
comment <- bind_rows(noun, pvpa) %>%
  filter(str_count(word) >= 2) %>%
  arrange(id)
comment %>%  select(word, reply)

# 명사, 동사, 형용사를 한 번에 추출하기
# 명사, 동사, 형용사를 추출해 결합한 후 두 글자 이상만 남기기
comment_new <- comment_pos %>%
  separate_rows(word, sep = "[+]") %>%
  filter(str_detect(word, "/n|/pv|/pa")) %>%
  mutate(word = ifelse(str_detect(word, "/pv|/pa"),
                       str_replace(word, "/.*$", "다"),
                       str_remove(word, "/.*$"))) %>%
  filter(str_count(word) >= 2) %>%
  arrange(id)

# 단어 동시 출현 빈도 구하기
렬


install.packages("widyr")
library(widyr)
pair <- comment %>%
  pairwise_count(item = word,
                 feature = id,
                 sort = T)
pair

# 특정 단어와 자주 함께 사용된 단어 사펴보기
pair %>% filter(item1 == "특정단어 이곳에 입력")


# 동시 출현 네트워크: Co-occurrence network     ################

install.packages("tidygraph")
library(tidygraph)
graph_comment <- pair %>%
  filter(n >= 25) %>%
  as_tbl_graph()
graph_comment

# 네트워크 그래프 만들기

install.packages("ggraph")
library(ggraph)

# 한글 폰트 설정
install.packages('showtext')
library(showtext)
font_add_google(name = "Nanum Gothic", family = "nanumgothic")
showtext_auto()

word_network <- function(x) {
ggraph(graph_comment, layout = "fr") +
  geom_edge_link(color = "gray50", alpha = 0.5) + # 엣지
  geom_node_point(color = "lightcoral", size = 5 ) + # 노드
  ggtitle("동시 출현 빈도 ")+
  geom_node_text(aes(label = name), family="NanumBarunGothicBold", repel = TRUE) # 텍
}

set.seed(1234)
word_network(graph_comment)

# 유의어 처리하기
comment <- comment %>%
  mutate(word = ifelse(str_detect(word, "감독") &
                         !str_detect(word, "감독상"), "", word),
         word = ifelse(word == "오르다", "올리다", word),
         word = ifelse(str_detect(word, "축하"), "축하", word))

# 단어 동시 출현 빈도 구하기
pair <- comment %>%
  pairwise_count(item = word,
                 feature = id,
                 sort = T)

# 네트워크 그래프 데이터 만들기
graph_comment <- pair %>%
  filter(n >= 25) %>%
  as_tbl_graph()

# 네트워크 그래프 만들기
set.seed(1234)
word_network(graph_comment)

# 연결 중심성과 커뮤니티 표현하기 ##############################


set.seed(1234)
graph_comment <- pair %>%
  filter(n >= 25) %>%
  as_tbl_graph(directed = F) %>%
  mutate(centrality = centrality_degree(), # 연결 중심성
         group = as.factor(group_infomap())) # 커뮤니티
graph_comment 


set.seed(1234)

ggraph(graph_comment, layout = "fr") + # 레이아웃
  geom_edge_link(color = "gray50", # 엣지 색깔
                 alpha = 0.5) + # 엣지 며암
  geom_node_point(aes(size = centrality, # 노드 크기
                      color = group), # 노드 색깔
                  show.legend = F) + # 범례 삭제
  scale_size(range = c(5, 15)) + # 노드 크기 범위
  geom_node_text(aes(label = name), # 텍스트 표시
                 repel = T, # 노드밖 표시
                 size = 5, # 텍스트 크기
                 family = "nanumgothic") + # 폰트
  theme_graph() # 배경 삭제


# 주요 단어의 커뮤니티 살펴보기
graph_comment %>%
  filter(name == "봉준호")

# 같은 커뮤니티로 분류된 단어 살펴보기
graph_comment %>%
  filter(group == 4) %>%
  arrange(-centrality) %>%
  data.frame()

# 연결 중심성 높은 주요 단어 살펴보기
graph_comment %>%
  arrange(-centrality)

# 연결 중심성이 높은 주요 단어 살펴보기
# 2ᄇ 커뮤니티로 분류된 단어
graph_comment %>%
  filter(group == 2) %>%
  arrange(-centrality) %>%
  data.frame()

# 주요 단어가 사용된 원문 살펴
news_comment %>%
  filter(str_detect(reply, "봉준호") & str_detect(reply, "대박")) %>%
  select(reply)

# 단어 간 상관 분석: Phi coefficient ###########################################


# 파이 계수 구하기
word_cors <- comment %>%
  add_count(word) %>%
  filter(n >= 20) %>%
  pairwise_cor(item = word,
               feature = id,
               sort = T)
word_cors

# 관심 단어벼로 파이 계수가 큰 단어 추출하기
# 관심 단어 목록 생성
target <- c("대한민국", "역사", "수상ᄉ", "조국", "박근혜", "블랙리스트")
top_cors <- word_cors %>%
  filter(item1 %in% target) %>%
  group_by(item1) %>%
  slice_max(correlation, n = 8)

# 그래프 순서 정하
top_cors$item1 <- factor(top_cors$item1, levels = target)
library(ggplot2)
ggplot(top_cors, aes(x = reorder_within(item2, correlation, item1),
                     y = correlation,
                     fill = item1)) +
  geom_col(show.legend = F) +
  facet_wrap(~ item1, scales = "free") +
  coord_flip() +
  scale_x_reordered() +
  labs(x = NULL) +
  theme(text = element_text(family = "nanumgothic"))

# 네트워크 그래프 데이터 만들기. 연결 중심성과 커뮤니티 추가하기
set.seed(1234)
graph_cors <- word_cors %>%
  filter(correlation >= 0.15) %>%
  as_tbl_graph(directed = F) %>%
  mutate(centrality = centrality_degree(),
         group = as.factor(group_infomap()))

# 네트워크 그래프 만들기
set.seed(1234)
ggraph(graph_cors, layout = "fr") +
  geom_edge_link(color = "gray50",
                 aes(edge_alpha = correlation, # 엣지 며암
                     edge_width = correlation), # 엣지 두께
                 show.legend = F) + # 범례 삭제
  scale_edge_width(range = c(1, 4)) + # 엣지 두께 범위
  geom_node_point(aes(size = centrality,
                      color = group),
                  show.legend = F) +
  scale_size(range = c(5, 10)) +
  geom_node_text(aes(label = name),
                 repel = T,
                 size = 5,
                 family = "nanumgothic") +
  theme_graph()

# 연이어 사용된 단어쌍 분석: n-gram             ################

# 엔그램으로 토큰화하기 
text <- tibble(value = "대한민국은 민주공화국이다. 대한민국의 주권은 국민에게 있고, 모든권력은 국민으로부터 나온다.")
text

# 바이그램 토큰화
text %>%
  unnest_tokens(input = value,
                output = word,
                token = "ngrams",
                n = 2)

# 트라이그램 토큰화
text %>%
  unnest_tokens(input = value,
                output = word,
                token = "ngrams",
                n = 3)

# 단어 기준 토큰화
text %>%
  unnest_tokens(input = value,
                output = word,
                token = "words")

# 유니그램 토큰화
text %>%
  unnest_tokens(input = value,
                output = word,
                token = "ngrams",
                n = 1)

# 기사 댓글로 바이그램 만들기
comment_new <- comment_pos %>%
  separate_rows(word, sep = "[+]") %>%
  filter(str_detect(word, "/n|/pv|/pa")) %>%
  mutate(word = ifelse(str_detect(word, "/pv|/pa"),
                       str_replace(word, "/.*$", "다"),
                       str_remove(word, "/.*$"))) %>%
  filter(str_count(word) >= 2) %>%
  arrange(id)

 # 유의어 처리하기
comment_new <- comment_new %>%
  mutate(word = ifelse(str_detect(word, "감독") &
                         !str_detect(word, "감독상"), "봉준호", word),
         word = ifelse(word == "오르다", "올리다", word),
         word = ifelse(str_detect(word, "축하"), "축하", word))


# 한 댓글이 하나의 행이 되도록 결합하기
comment_new %>%
  select(word)

# 한 댓글이 하나의 행이 되도록 결합하기
line_comment <- comment_new %>%
  group_by(id) %>%
  summarise(sentence = paste(word, collapse = " "))
line_comment

# 바이 토큰화하기
bigram_comment <- line_comment %>%
  unnest_tokens(input = sentence,
                output = bigram,
                token = "ngrams",
                n = 2)
bigram_comment

# 바이그램 분리
bigram_seprated <- bigram_comment %>%
  separate(bigram, c("word1", "word2"), sep = " ")
bigram_seprated

# 단어쌍 빈도 구하기
pair_bigram <- bigram_seprated %>%
  count(word1, word2, sort = T) %>%
  na.omit()
pair_bigram


# 동시 출현 단어쌍
pair %>%
  filter(item1 == "대한민국")


# 바이그램 단어쌍
pair_bigram %>%
  filter(word1 == "대한민구")


# 엔그램으로 네트워크 그래프 만들기
# 네트워크 그래프 데이터 만들기
graph_bigram <- pair_bigram %>%
  filter(n >= 8) %>%
  as_tbl_graph()
# 네트워크 그래프 만들기
set.seed(1234)
word_network(graph_bigram)

# 유의어 처리
bigram_seprated <- bigram_seprated %>%
  mutate(word1 = ifelse(str_detect(word1, "대ᄃ"), "대단", word1),
         word2 = ifelse(str_detect(word2, "대단"), "대단", word2),
         word1 = ifelse(str_detect(word1, "자랑"), "자랑", word1),
         word2 = ifelse(str_detect(word2, "ᄌ"), "자랑", word2),
         word1 = ifelse(str_detect(word1, "짝짝짝"), "짝짝짝", word1),
         word2 = ifelse(str_detect(word2, "짝짝짝"), "짝짝짝", word2)) %>%
    # 같은 단어 연속 제거
  filter(word1 != word2)

# 단어쌍 빈도 구하기
pair_bigram <- bigram_seprated %>%
  count(word1, word2, sort = T) %>%
  na.omit() 

# ᄂ 그래프 데이터 만들기
set.seed(1234)
graph_bigram <- pair_bigram %>%
  filter(n >= 8) %>%
  as_tbl_graph(directed = F) %>%
  mutate(centrality = centrality_degree(), # 중심성
         group = as.factor(group_infomap())) # 커뮤니티


# 네트워크 그래프 만들기
set.seed(1234)
ggraph(graph_bigram, layout = "fr") + # 레이아웃
  geom_edge_link(color = "gray50", # 엣 색깔
                 alpha = 0.5) + # 엣지 명암
  geom_node_point(aes(size = centrality, # 노드 크기
                      color = group), # 노드 색깔
                  show.legend = F) + # 범례 삭제
  scale_size(range = c(4, 8)) + # 노드 크기 범위
  geom_node_text(aes(label = name), # 텍스트 표시
                 repel = T, # 노드밖 표ᄉ
                 size = 5, # 텍스트 크기
                 family = "nanumgothic") + # 폰트
  theme_graph()  # 배경 삭제






