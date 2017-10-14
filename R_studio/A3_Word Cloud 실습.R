############################# 웹크롤링 실습 ################################
## 1. 트위터 크롤링 - 중고차 판매 가격과 연관된 트윗 크롤링
## 2. 가격 이라는 단어와 연관성이 높은 단어 추출
## 3. 워드 클라우드 그려보기


##### 1. 트위터 크롤링
# 순서 : 크롤링(list) -> dataframe -> corpus -> TDM 

# package 실행
library(twitteR)
library(ROAuth)
library(plyr)       
library(stringr)    
library(RJSONIO)    
library(RCurl)      
library(ggplot2)
library(openssl)
library(SnowballC)
library(tm)
library(wordcloud)

#기본 정보 셋팅
api_key <- 'api_key'
api_secret <- 'api_secret'
access_token <- 'access_token'
access_token_secret <- 'access_token_secret'

setup_twitter_oauth(api_key, api_secret, access_token,access_token_secret)

#크롤링 하기
keyword <- '#CarForSale'
crw.tw <- searchTwitter(keyword,n=1000)

# 크롤링 결과 확인
crw.tw[[1]]

# list -> dataframe       
crw.df <- twListToDF(crw.tw)

#text 추출 (vector)
crw.text <- crw.df$text


##### 2. 전처리
# crw.text (vector상태)를 corpus로 변경한 다음 
# tm.map() 함수를 이용하여 전처리를 한다. 

## 2.1 vector -> corpus
my.corpus <- Corpus(VectorSource(crw.text))
my.corpus[[1]]
inspect(my.corpus[1:2])

## 2.2 전처리

# 구두점 삭제
my.corpus <- tm_map(my.corpus, removePunctuation) # 구두점 삭제
my.corpus[[1]]         
inspect(my.corpus[1:2]) # 변환 확인

# white space 제거
my.corpus <- tm_map(my.corpus , stripWhitespace)
my.corpus[[1]]          
inspect(my.corpus[1:2]) # 공백 제거확인

# 대문자를 소문자로 변환
my.corpus <- tm_map(my.corpus, content_transformer(tolower)) 
inspect(my.corpus[1:2]) # 소문자 변환 확인

# 특수 문자 제거
(my.corpus <- tm_map(my.corpus, content_transformer(gsub), 
               pattern='@\\s*', replacement='')) # @로 시작하는 단어를 공백으로 대체
(my.corpus<- tm_map(my.corpus, content_transformer(gsub), 
               pattern='http\\s*', replacement='')) # http로 시작하는 단어 공백대체
my.corpus[[1]]          
inspect(my.corpus[1:2]) # 변환 확인

# stopword 사용(조사, 띄어쓰기, 시제 등) 제거 하고 표준화
mystopwords <- c(stopwords('en'),'rt','via','even') #stopword지정
my.corpus <- tm_map(my.corpus, removeWords, mystopwords)
my.corpus[[1]]          
inspect(my.corpus[1:2]) # 변환 확인



##### 3. Stemming
### stemDocument, stemCompletion 수행


## 1. stemDocument
test <- my.corpus
test <- tm_map(test,stemDocument)
inspect(test[1:2])

## 2. stemCompletion_mod 
dict.corpus <- test
test <- stemCompletion(test, dictionary = dict.corpus)
test <- Corpus(VectorSource(test))

inspect(test[[1]])


##### 4. Association
### 결과로 나온 단어중 아래 4개 단어가 유의하다고 판단.
### '45000', '5yr', 'kms', 'diesel'

my.TDM <- TermDocumentMatrix(my.corpus)
dim(my.TDM)

findAssocs(my.TDM, 'price', 0.5)



##### 4. Word cloud
my.TDM.m <- as.matrix(my.TDM)
term.freq <- sort(rowSums(my.TDM.m), decreasing=T)
head(term.freq, 30)

wordcloud(words=names(term.freq), freq=term.freq, min.freq=3, random.order=F,
          colors=brewer.pal(8,'Dark2'))







