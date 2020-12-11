# 패키지 설치 
install.packages("Sejong")
install.packages("wordcloud")
install.packages("tm")
install.packages("RSQLite")
install.packages("hash")
install.packages("tau")
install.packages('rJava')
install.packages('DBI')
install.packages('RJDBC')
install.packages('devtools')


library(devtools)
library(DBI)
library(RJDBC)
library(rJava)
library(RSQLite)
library(KoNLP)
library(tm)
library(wordcloud)

install.packages("httr")
library(httr)
install.packages("XML")
library(XML)

# url 요청
url <- "https://news.daum.net/"
web <- GET(url)
web

# HTML파싱 
html <- htmlTreeParse(web, useInternalNodes=T, trim=T, encoding='utf-8')
rootNode <- xmlRoot(html)

# 태그자료수집 
daum[[1]] <- xpathSApply(rootNode,"//div[@class='feature_home']", xmlValue)
daum[[2]] <- xpathSApply(rootNode,"//div[@id='mArticle']", xmlValue)

daum 

# 수집된 자료 전처리
daum_news <- gsub("[\r\n\t]","",daum)
daum_news <- gsub("[[:punct:]]","",daum_news)
daum_news <- gsub("[[:cntrl:]]","",daum_news)
daum_news <- gsub("\\d+","",daum_news)
daum_news <- gsub("[A-Z]+","",daum_news)
daum_news <- gsub("[a-z]+","",daum_news)
daum_news <- gsub("\\s+"," ", daum_news)
daum_news <- gsub("[위]","",daum_news)
daum_news <- gsub("[포토,뉴스,기사,헤드라인]","",daum_news)
daum_news

daum_data <- daum_news
daum_data

# 파일 저장과 읽기
setwd("C:/Users/USER/Desktop/수열/수열_학과/전산통계1/과제")
write.csv(daum_data, "daum_data.csv", quote=F)
as.vector(daum_data)

# 토픽 분석
exNouns <- function(x){paste(extractNoun(x),collapse = " ")}
news_nouns <- sapply(daum_data, exNouns)
news_nouns
str(news_nouns)

newsCorpus <- Corpus(VectorSource(news_nouns))
newsCorpus

TDM <- TermDocumentMatrix(newsCorpus, control = list(wordLengths=c(4,16)))
TDM

tdm.df <- as.data.frame(as.matrix(TDM))
dim(tdm.df)

wordResult <- sort(rowSums(tdm.df), decreasing = TRUE)
wordResult[1:15]

pie(wordResult[1:10], clockwise = T)

library(wordcloud)
myNames <- names(wordResult)
myNames

df <- data.frame(word=myNames, freq=wordResult)
head(df)

pal <- brewer.pal(12,"Paired")
wordcloud(df$word, df$freq, min.freq = 2, random.order = F, scale = c(4, 0.7),
          rot.per = .1, colors = pal, family="malgun")
install.packages("wordcloud2")
library(wordcloud2)
wordcloud2(df, word=daum)

############################################################################(naver)

# url 요청
url1 <- "https://news.naver.com/"
web1 <- GET(url1)
web1

# HTML파싱 
html1 <- htmlTreeParse(web1, useInternalNodes=T, trim=T, encoding='utf-8')
rootNode1 <- xmlRoot(html1)

# 태그자료수집 
naver <- xpathSApply(rootNode1,"//div[@class='main']", xmlValue)  
naver

# 수집된 자료 전처리
naver_news <- gsub("[\r\n\t]","",naver)
naver_news <- gsub("[[:punct:]]","",naver_news)
naver_news <- gsub("[[:cntrl:]]","",naver_news)
naver_news <- gsub("\\d+","",naver_news)
naver_news <- gsub("[A-Z]+","",naver_news)
naver_news <- gsub("[a-z]+","",naver_news)
naver_news <- gsub("\\s+"," ", naver_news)
naver_news

naver_data <- naver_news
naver_data

# 파일 저장과 읽기
setwd("C:/Users/USER/Desktop/수열/수열_학과/전산통계1/과제")
write.csv(naver_data, "naver_data.csv", quote=F)
as.vector(naver_data)

# 토픽 분석
exNouns <- function(x){paste(extractNoun(x),collapse = " ")}
news_nouns <- sapply(naver_data, exNouns)
news_nouns
str(news_nouns)

newsCorpus <- Corpus(VectorSource(news_nouns))
newsCorpus

TDM <- TermDocumentMatrix(newsCorpus, control = list(wordLengths=c(7,12)))
TDM

tdm.df <- as.data.frame(as.matrix(TDM))
dim(tdm.df)

wordResult <- sort(rowSums(tdm.df), decreasing = TRUE)
wordResult[1:20]
pie(wordResult[1:10], clockwise = T)

library(wordcloud)
myNames <- names(wordResult)
myNames

df <- data.frame(word=myNames, freq=wordResult)
head(df)

pal <- brewer.pal(12,"Paired")
wordcloud(df$word, df$freq, min.freq = 2, random.order = F, scale = c(4, 0.7),
          rot.per = .1, colors = pal, family="malgun")
install.packages("wordcloud2")
library(wordcloud2)
wordcloud2(df, word=daum)

#########################################################################(한국일보)
# url 요청
url2 <- "https://www.hankookilbo.com/"
web2 <- GET(url2)
web2

# HTML파싱 
html2 <- htmlTreeParse(web2, useInternalNodes=T, trim=T, encoding='utf-8')
rootNode2 <- xmlRoot(html2)

# 태그자료수집 
hankookilbo <- xpathSApply(rootNode2,"//div[@class='container main']", xmlValue)  
hankookilbo

# 수집된 자료 전처리
hankookilbo_news <- gsub("[\r\n\t]","",hankookilbo)
hankookilbo_news <- gsub("[[:punct:]]","",hankookilbo_news)
hankookilbo_news <- gsub("[[:cntrl:]]","",hankookilbo_news)
hankookilbo_news <- gsub("\\d+","",hankookilbo_news)
hankookilbo_news <- gsub("[A-Z]+","",hankookilbo_news)
hankookilbo_news <- gsub("[a-z]+","",hankookilbo_news)
hankookilbo_news <- gsub("\\s+"," ", hankookilbo_news)
hankookilbo_news

hankookilbo_data <- hankookilbo_news
hankookilbo_data

# 파일 저장과 읽기
setwd("C:/Users/USER/Desktop/수열/수열_학과/전산통계1/과제")
write.csv(hankookilbo_data, "hankookilbo_data.csv", quote=F)
as.vector(hankookilbo_data)

# 토픽 분석
exNouns <- function(x){paste(extractNoun(x),collapse = " ")}
news_nouns <- sapply(hankookilbo_data, exNouns)
news_nouns
str(news_nouns)

newsCorpus <- Corpus(VectorSource(news_nouns))
newsCorpus

TDM <- TermDocumentMatrix(newsCorpus, control = list(wordLengths=c(7,12)))
TDM

tdm.df <- as.data.frame(as.matrix(TDM))
dim(tdm.df)

wordResult <- sort(rowSums(tdm.df), decreasing = TRUE)
wordResult[1:15]
pie(wordResult[1:10],clockwise = T)

library(wordcloud)
myNames <- names(wordResult)
myNames

df <- data.frame(word=myNames, freq=wordResult)
head(df)

pal <- brewer.pal(12,"Paired")
wordcloud(df$word, df$freq, min.freq = 2, random.order = F, scale = c(4, 0.7),
          rot.per = .1, colors = pal, family="malgun")
install.packages("wordcloud2")
library(wordcloud2)
wordcloud2(df, word=daum)

#########################################################################(중앙일보)
# url 요청
url3 <- "https://joongang.joins.com/"
web3 <- GET(url3)
web3

# HTML파싱 
html3 <- htmlTreeParse(web3, useInternalNodes=T, trim=T, encoding='utf-8')
rootNode3 <- xmlRoot(html3)

# 태그자료수집 
joongangilbo <- xpathSApply(rootNode3,"//main[@class='main']", xmlValue)  
joongangilbo

# 수집된 자료 전처리
joongangilbo_news <- gsub("[\r\n\t]","",joongangilbo)
joongangilbo_news <- gsub("[[:punct:]]","",joongangilbo_news)
joongangilbo_news <- gsub("[[:cntrl:]]","",joongangilbo_news)
joongangilbo_news <- gsub("\\d+","",joongangilbo_news)
joongangilbo_news <- gsub("[A-Z]+","",joongangilbo_news)
joongangilbo_news <- gsub("[a-z]+","",joongangilbo_news)
joongangilbo_news <- gsub("\\s+"," ", joongangilbo_news)
joongangilbo_news

joongangilbo_data <- joongangilbo_news
joongangilbo_data

# 파일 저장과 읽기
setwd("C:/Users/USER/Desktop/수열/수열_학과/전산통계1/과제")
write.csv(joongangilbo_data, "joongangilbo_data.csv", quote=F)
as.vector(joongangilbo_data)

# 토픽 분석
exNouns <- function(x){paste(extractNoun(x),collapse = " ")}
news_nouns <- sapply(joongangilbo_data, exNouns)
news_nouns
str(news_nouns)

newsCorpus <- Corpus(VectorSource(news_nouns))
newsCorpus

TDM <- TermDocumentMatrix(newsCorpus, control = list(wordLengths=c(7,12)))
TDM

tdm.df <- as.data.frame(as.matrix(TDM))
dim(tdm.df)

wordResult <- sort(rowSums(tdm.df), decreasing = TRUE)
wordResult[1:15]
pie(wordResult[1:15], clockwise = T)

library(wordcloud)
myNames <- names(wordResult)
myNames

df <- data.frame(word=myNames, freq=wordResult)
head(df)

pal <- brewer.pal(12,"Paired")
wordcloud(df$word, df$freq, min.freq = 2, random.order = F, scale = c(4, 0.7),
          rot.per = .1, colors = pal, family="malgun")
install.packages("wordcloud2")
library(wordcloud2)
wordcloud2(df, word=daum)

#########################################################################(조선일보)
# url 요청
url4 <- "https://www.chosun.com/"
web4 <- GET(url4)
web4

# HTML파싱 
html4 <- htmlTreeParse(web4, useInternalNodes=T, trim=T, encoding='utf-8')
rootNode4 <- xmlRoot(html4)

# 태그자료수집 
chosunilbo <- xpathSApply(rootNode4,"//div[@class='layout--bg box-lg--pad-left-xs box-lg--pad-right-xs']", xmlValue)  
chosunilbo

# 수집된 자료 전처리
chosunilbo_news <- gsub("[\r\n\t]","",chosunilbo)
chosunilbo_news <- gsub("[[:punct:]]","",chosunilbo_news)
chosunilbo_news <- gsub("[[:cntrl:]]","",chosunilbo_news)
chosunilbo_news <- gsub("\\d+","",chosunilbo_news)
chosunilbo_news <- gsub("[A-Z]+","",chosunilbo_news)
chosunilbo_news <- gsub("[a-z]+","",chosunilbo_news)
chosunilbo_news <- gsub("\\s+"," ", chosunilbo_news)
chosunilbo_news

chosunilbo_data <- chosunilbo_news
chosunilbo_data

# 파일 저장과 읽기
setwd("C:/Users/USER/Desktop/수열/수열_학과/전산통계1/과제")
write.csv(chosunilbo_data, "chosunilbo_data.csv", quote=F)
as.vector(chosunilbo_data)

# 토픽 분석
exNouns <- function(x){paste(extractNoun(x),collapse = " ")}
news_nouns <- sapply(chosunilbo_data, exNouns)
news_nouns
str(news_nouns)

newsCorpus <- Corpus(VectorSource(news_nouns))
newsCorpus

TDM <- TermDocumentMatrix(newsCorpus, control = list(wordLengths=c(7,12)))
TDM

tdm.df <- as.data.frame(as.matrix(TDM))
dim(tdm.df)

wordResult <- sort(rowSums(tdm.df), decreasing = TRUE)
wordResult[1:15]
pie(wordResult[1:15],clockwise = T)

library(wordcloud)
myNames <- names(wordResult)
myNames

df <- data.frame(word=myNames, freq=wordResult)
head(df)

pal <- brewer.pal(12,"Paired")
wordcloud(df$word, df$freq, min.freq = 2, random.order = F, scale = c(4, 0.7),
          rot.per = .1, colors = pal, family="malgun")
install.packages("wordcloud2")
library(wordcloud2)
wordcloud2(df, word=daum)


################################################################################(sbs 조유진)

# url 요청
url5 <- "https://www.chosun.com/"
web5 <- GET(url5)
web5

# HTML파싱 
html4 <- htmlTreeParse(web4, useInternalNodes=T, trim=T, encoding='utf-8')
rootNode4 <- xmlRoot(html4)

# 태그자료수집 
chosunilbo <- xpathSApply(rootNode4,"//div[@class='layout--bg box-lg--pad-left-xs box-lg--pad-right-xs']", xmlValue)  
chosunilbo

# 수집된 자료 전처리
chosunilbo_news <- gsub("[\r\n\t]","",chosunilbo)
chosunilbo_news <- gsub("[[:punct:]]","",chosunilbo_news)
chosunilbo_news <- gsub("[[:cntrl:]]","",chosunilbo_news)
chosunilbo_news <- gsub("\\d+","",chosunilbo_news)
chosunilbo_news <- gsub("[A-Z]+","",chosunilbo_news)
chosunilbo_news <- gsub("[a-z]+","",chosunilbo_news)
chosunilbo_news <- gsub("\\s+"," ", chosunilbo_news)
chosunilbo_news

chosunilbo_data <- chosunilbo_news
chosunilbo_data

# 파일 저장과 읽기
setwd("C:/Users/USER/Desktop/수열/수열_학과/전산통계1/과제")
write.csv(chosunilbo_data, "chosunilbo_data.csv", quote=F)
as.vector(chosunilbo_data)

# 토픽 분석
exNouns <- function(x){paste(extractNoun(x),collapse = " ")}
news_nouns <- sapply(chosunilbo_data, exNouns)
news_nouns
str(news_nouns)

newsCorpus <- Corpus(VectorSource(news_nouns))
newsCorpus

TDM <- TermDocumentMatrix(newsCorpus, control = list(wordLengths=c(6,12)))
TDM

tdm.df <- as.data.frame(as.matrix(TDM))
dim(tdm.df)

wordResult <- sort(rowSums(tdm.df), decreasing = TRUE)
wordResult[1:15]
pie(wordResult[1:15],clockwise = T)

library(wordcloud)
myNames <- names(wordResult)
myNames

df <- data.frame(word=myNames, freq=wordResult)
head(df)

pal <- brewer.pal(12,"Paired")
wordcloud(df$word, df$freq, min.freq = 2, random.order = F, scale = c(4, 0.7),
          rot.per = .1, colors = pal, family="malgun")
install.packages("wordcloud2")
library(wordcloud2)
wordcloud2(df, word=daum)






###############################################################################
## Naver와 Daum간의 비교 
daum$choice <- c(1,2,3,4,5,6,7)
daum$density <- c(30.61,30.61,8.16,14.28,0,8.16,8.16)
daum
plot(daum$choice,daum$density, type='o', ylim=c(0,50), col='blue', xlab = "Topic", ylab='Percentile', 
     main = 'Daum vs Naver의 2020/12/04 Hot Topic')

naver$choice <- c(1,2,3,4,5,6,7)
naver$density <- c(9.89,29.67,7.69,25.27,0,18.68,8.79)
naver
points(naver$choice, naver$density, type='o', col='green')

ex$choice  <- c(1,2,3,4,5,6,7)
ex$density <- c(14.28,14.28,14.28,14.28,14.28,14.28,14.28)
ex
points(ex$choice, ex$density, type='o', col='brown')
legend(x=4.5, y=50 , legend=c('Daum','Naver','14.28% (Topic별로 동일한 확률)'), col=c('blue','green','brown'),
       lty=1, bg="white", cex=1)


















## 신문사별 비교 
hangookilbo <- c()
hangookilbo$choice <- c(1,2,3,4,5,6,7)
hangookilbo$density <- c(9.89,23.08,14.28,25.27,0,18.68,8.79)
hangookilbo
plot(hangookilbo$choice,hangookilbo$density, type='o', ylim=c(0,55), col='green', xlab = "Topic", ylab='Percentile', 
     main = '신문사별 2020/12/04 Hot Topic 비교')

joongangilbo <- c()
joongangilbo$choice <- c(1,2,3,4,5,6,7)
joongangilbo$density <- c(32.35,0,0,23.53,17.65,17.64,8.82)
joongangilbo
points(joongangilbo$choice, joongangilbo$density, type='o', col='orange')

chosunilbo <- c()
chosunilbo$choice <- c(1,2,3,4,5,6,7)
chosunilbo$density <- c(8.42,18.25,0,50.52,7.02,6.32,9.47)
chosunilbo
points(chosunilbo$choice, chosunilbo$density, type='o', col='red')

hanilbo <- c()
hanilbo$choice <- c(1,2,3,4,5,6,7)
hanilbo$density <- c(18.18,9.09,0,49.96,9.09,12.12,4.55)
hanilbo
points(hanilbo$choice, hanilbo$density, type='o', col='brown')

khanilbo <- c()
khanilbo$choice <- c(1,2,3,4,5,6,7)
khanilbo$density <- c(19.23,34.62,0,19.23,0,0,26.92)
khanilbo
points(khanilbo$choice, khanilbo$density, type='o', col='blue')

ex$choice  <- c(1,2,3,4,5,6,7)
ex$density <- c(14.28,14.28,14.28,14.28,14.28,14.28,14.28)
ex
points(ex$choice, ex$density, type='o', col='black', lwd=3)
legend(x=4.5, y=55 , legend=c('한국일보','중앙일보','조선일보' ,'한겨례일보','경향일보','14.28% (Topic별로 동일한 확률)'), 
       col=c('green','orange','red','brown','blue','black'),
       lty=1, bg="white", cex=1)






## 웹 vs 신문사 vs 방송사 
web$choice <- c(1,2,3,4,5,6,7)
web$density <- c(20.25, 30.14, 7.925, 19.775, 0, 13.42, 8.475)
web
plot(web$choice,web$density, type='o', ylim=c(0,50), col='green', xlab = "Topic", ylab='Percentile', 
     main = '웹 vs 신문사 vs 방송사의 2020/12/04 Hot Topic', lwd=1.5)

ilbo <- c()
ilbo$choice <- c(1,2,3,4,5,6,7)
ilbo$density <- c(17.614,17.008,2.856,33.702,6.752,10.952,11.71)
ilbo
points(ilbo$choice, ilbo$density, type='o', col='red', lwd=1.5)

broadcast <- c()
broadcast$choice <- c(1,2,3,4,5,6,7)
broadcast$density <- c()
broadcast
points(broad$choice, broad$density, type='o', col='blue', lwd=1.5)


ex$choice  <- c(1,2,3,4,5,6,7)
ex$density <- c(14.28,14.28,14.28,14.28,14.28,14.28,14.28)
ex
points(ex$choice, ex$density, type='o', col='black', lwd=3)
legend(x=4.5, y=50 , legend=c('웹','신문사', '방송사','14.28% (Topic별로 동일한 확률)'), col=c('green','red','blue','black'),
       lty=1, bg="white", cex=1)











































