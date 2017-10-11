############################ 비정형 데이터 ####################################


### 1. 비정형 데이터 가공 - 표준화

# 파일 위치 확인
library(tm)
(folder <- system.file("texts", "txt", package = "tm"))

## -1. 조건 부여하여 표준화하기

# Corpus형식으로 만들기 (Corpus는 데이터 마이닝의 절차 중 데이터의 정제, 통합, 선택, 변환의 과정)
(doc <- Corpus(DirSource(folder), 
               readerControl = list(language = "lat")))
doc[[1]] # 단어 개수 확인
inspect(doc[1:2]) # text파일 불러오기


doc <- tm_map(doc, stripWhitespace) #공백제거
doc[[1]]          # 개수 확인 화면서 표준화 현황 체크
inspect(doc[1:2]) # 공백 제거확인


(doc <- tm_map(doc, content_transformer(tolower))) #소문자로 변환
inspect(doc[1:2]) # 소문자 변환 확인


(doc <- tm_map(doc, content_transformer(gsub), 
               pattern='@\\s*', replacement='')) # @로 시작하는 단어를 공백으로 대체
(doc <- tm_map(doc, content_transformer(gsub), 
               pattern='http\\s*', replacement='')) # http로 시작하는 단어 공백대체
doc[[1]]          # 개수 확인 화면서 표준화 현황 체크
inspect(doc[1:2]) # 변환 확인


(doc <- tm_map(doc, removePunctuation)) # 구두점 삭제
doc[[1]]          # 개수 확인 화면서 표준화 현황 체크
inspect(doc[1:2]) # 변환 확인

## -2 stopword 사용(조사, 띄어쓰기, 시제 등) 제거 하고 표준화

mystopwords <- c(stopwords('en'),'rt','via') #stopword지정
doc <- tm_map(doc, removeWords, mystopwords)
doc[[1]]          # 개수 확인 화면서 표준화 현황 체크!!!
inspect(doc[1:2]) # 변환 확인


### 2. 자연어 처리 - 형태(소) - 의미를 가지는 요소 추출하여 연관성과 빈도 알아보기 

## -1 stemming - 형태소를 묶는 방법

install.packages("SnowballC")
library(SnowballC)

# stemDocument - 공통적인 부분만 추출
test <- stemDocument(c('updated', 'update', 'updating'))
test  # [1] "updat" "updat" "updat"

#stemCompletion - 공통적인 부분을 기반으로 가장 기본적인 단어로 추출
test <- stemCompletion(test, dictionary = c('updated', 'update', 'updating'))
test  #   updat    updat    updat 


# 어간만 추출
(doc_steD <- tm_map(doc, stemDocument))
inspect(doc[1:2]) # 변환 확인

#dictionary인 dic에 있는 단어만을, doc에서 추출하고자할 때
#예들 들면 dic = c("시간","공부","부족") 라고 할 때,doc 문서들에서 dic에 있는 단어들만 추출한다. 
(doc_0 <- tm_map(doc, stemCompletion, dictionary = dic))
inspect(doc_0[1])


# rJava, KoNLP 설치
install.packages("rJava")
library(rJava)
install.packages("KoNLP")
library(KoNLP)




### 특정단어의 연관성 계산
# 데이터 읽기

library(tm)
data("crude")
crude               # 문서가 20개가 있다.
crude[1]            # crude의 1번째 문서
crude[[1]]          # 527 단어로 되어 있다.
inspect(crude[1:3]) # crude의 1~3번째 문서
edit(crude)         # 본문 자세히 보기  
       
#본문에 단어와 Oil의 연관성을 보자.
tdm_all <- TermDocumentMatrix(crude)
tdm_all
inspect(tdm_all)

# oil과 각 단어와의 연광성을 계수로 나타냄.
# tdm_all에서 'oil'과 연관성이 0.65 이상인 단어만 추출
findAssocs(tdm_all, "oil", 0.65)  
tdm_all <- TermDocumentMatrix(crude)


### 3. Word cloud
# 문서에 포함되는 단어의 사용 빈도를 효과적으로 보여주기 위해 워드 클라우드 이용
install.packages("wordcloud")
library(wordcloud)

tdm_all <- TermDocumentMatrix(crude)
tdm_all_m <- as.matrix(tdm_all)

#빈도수기준 정렬
tdm_freq <- sort(rowSums(tdm_all_m), decreasing = TRUE)

wordcloud(names(tdm_freq), freq=tdm_freq,
          min.freq = 15, random.order = F, colors = brewer.pal(8,'Dark2'))





