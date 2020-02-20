install.packages("twitteR")
install.packages("ROAuth")
install.packages("base64enc")
install.packages("KoNLP")
install.packages("wordcloud")
install.packages("plyr")
install.packages("tm")

library(twitteR)
library(ROAuth)
library(base64enc)
library(KoNLP)
library(wordcloud)
library(plyr)
library(RColorBrewer)
library(tm)


# 작업폴더 경로 확인
getwd()

# 저장할 폴더 지정
setwd("./")
getwd()

# 트위터 계정에서 발급받은 키 값을 입력
# https://apps.twitter.com
# 디벨로버 계성 활성화
# App name : R_test
# Application description :
# Website URL : www.pusan.ac.kr
# Callback URLs : http://127.0.0.1:1410
# 
#consumerKey <- "Consumer Key (API Key)"
#consumerSecret <- "Consumer Secret (API Key)"
#AccessToken <- "Access Token"
#accessTokenSecret <- "Access Token Secret"
consumerKey <- "oXjexxPMIXcjPZJ2vTyidCwhZ"
consumerSecret <- "v2yomrbwQf6niIczFW9wuZi8ly8bBK8JNXMMdGlPljEis05bij"
accessToken <- "44918953-wlHHPJcBgbP6VTQ3I2iHjPYetew5FrdGqo0ObVR10"
accessTokenSecret <- "cB8MKN4WQDYWGqObYa5vpO7yv8LOG5ZC498PqbRVfAmL2"

# setup_twitter_oauth 함수를 사용해서 oauth 인증 파일 저장
setup_twitter_oauth(consumerKey, consumerSecret, accessToken, accessTokenSecret)

# enc2utf8 함수를 사용하여 키워드 저장
keyword <-enc2utf8("빅데이터")

# 크롤링할 트위터 수(n=500)와 언어(lang="ko") 설정
bigdata <- searchTwitter(keyword, n=500, lang="ko")

# 트위터 데이터를 형태별로 분류하고 멘션 부분만 추출
bigdata.df <- twListToDF(bigdata)
bigdata.text <- bigdata.df$text
head(bigdata.text)

keyword <-enc2utf8("#여행")
# 트위터에서 키워드로 검색, 시작날짜, 지역코드(우리나라만 적용), 가져올 개수를 옵션에 대입
h3_twitter <- searchTwitter(keyword,since='2012-10-29',geocode='35.874,128.246,400km',n=1000000)

# 트위터 데이터를 형태별로 분류하고 멘션 부분만 추출
h3_twitter.df <- twListToDF(h3_twitter)
h3_twitter.text <- h3_twitter.df$text

# 불필요한 문자를 필터링
h3_twitter.text <- gsub("\n", "", h3_twitter.text)
h3_twitter.text <- gsub("\r", "", h3_twitter.text)
h3_twitter.text <- gsub("RT", "", h3_twitter.text)
h3_twitter.text <- gsub("H3", "", h3_twitter.text)
h3_twitter.text <-gsub("개발자","",h3_twitter.text)
h3_twitter.text <-gsub("컨퍼런스","",h3_twitter.text)
h3_twitter.text <-gsub("co","",h3_twitter.text)
h3_twitter.text <-gsub("세션","",h3_twitter.text)
h3_twitter.text <-gsub("h3","",h3_twitter.text)
h3_twitter.text <-gsub("2012","",h3_twitter.text)
h3_twitter.text <-gsub("http","",h3_twitter.text)

# 문자 분리
h3_twitter_nouns <- Map(extractNoun, h3_twitter.text)

# 쓸모없는 문자들을 제거한다. 특히 영문자의 경우 tm의 stopwords를 활용한다. 
h3_twitter_word <- unlist(h3_twitter_nouns, use.name=F)
h3_twitter_word <- h3_twitter_word[-which(h3_twitter_word %in% stopwords("english"))]
h3_twitter_word <- gsub("[[:punct:]]","", h3_twitter_word)
h3_twitter_word <- Filter(function(x){nchar(x)>=2}, h3_twitter_word)

# 단어별 카운팅
h3_twitter_count <- table(h3_twitter_word)

# 컬러 세팅
pal <- brewer.pal(12,"Paired")

# 폰트 세팅
windowsFonts(malgun=windowsFont("Arial"))

# 그리기
wordcloud(names(h3_twitter_count),freq=h3_twitter_count,scale=c(4,0.5),min.freq=1,
          random.order=F,rot.per=.1,colors=pal,family="malgun")
