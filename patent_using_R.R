install.packages('cluster')
library(cluster)
head(iris)
summary(iris)
hist(iris$Sepal.Length)
boxplot(iris$Sepal.Length)
plot(iris)

### 정형 데이터분석(상관분석, 회귀분석, 군집화) ### 
## cars 예제 
cars
dim(cars)
max(cars$speed)
summary(cars)
boxplot(cars)
plot(cars)
cor(cars)
lm(dist~speed, data=cars) # -17.579 + 3.932X = y

mode10 = lm(dist~0 + speed, data=cars)
plot(cars, xlim=c(0,30))
abline(mode10, col='blue')

## nike 본사 온도& 특허 수 
y = c(1.5, 0.9, 1.4, 0.5, -0.1, 2.4, 0.7, 1.7, 0.5, -0.7, -0.4, -1, -1.5, 0.7)
x = c(48,80,100,110,63,24,41,52,95,150,116,121,106,112)
data.set = data.frame(y,x)
data.set
boxplot(data.set)
summary(data.set)
cor(data.set) 
mode1 = lm(x~y, data=data.set) # lm(x~y 순서대로)
plot(data.set)
abline(mode1, col='red')

## iris 군집화
head(iris)
sepal = iris[,1:2]
sepal_kmeans = kmeans(sepal, centers=3)
sepal_col = sepal_kmeans$cluster
iris_pch = as.numeric(iris$Species)
plot(sepal, col=sepal_col, pch=iris_pch)

petal = iris[,3:4]
petal_kmeans = kmeans(petal, centers=3)
petal_col = petal_kmeans$cluster
iris_pch = as.numeric(iris$Species)
plot(petal, col=petal_col, pch=iris_pch)

## chickwts 군집화 
chickwts
head(chickwts)
dim(chickwts)
weight = chickwts[,1]
weight_kmeans = kmeans(weight, centers = 5)
chickwts_pch = as.numeric(chickwts$feed)
chickwts_col = weight_kmeans$cluster
plot(chickwts, col=chickwts_col, pch=chickwts_pch)

? chickwts[,1] != chickwts$weight


### 비정형 데이터 ###
## 워드클라우드 실습 
install.packages('NLP')
install.packages('tm')
install.packages('SnowballC')
install.packages('RColorBrewer')
install.packages('wordcloud')
library(NLP)
library(tm)
library(SnowballC)
library(RColorBrewer)
library(wordcloud)

patentDocu = read.csv('C:/Temp/data-master/01_WordCloud.csv', header=T, sep=',')
head(patentDocu)
patentDocu = as.matrix(patentDocu[2])
head(patentDocu)
corpus = Corpus(VectorSource(patentDocu))
tdm = TermDocumentMatrix(corpus, control = list(stopwords=T, 
                                                minWordLenght=3, # 최소 3건 이상 
                                                removeNumbers=T,
                                                removePunctuation=T)) # text만 남게되 
termMatrix = as.matrix(tdm)
term_list = sort(rowSums(termMatrix), decreasing = T)
term_list
word_list = data.frame(word=names(term_list), freq=term_list)
head(word_list)
pal = brewer.pal(8,'Dark2') # 5, Set1 이랑은 다른 파레토 
pal
wordcloud(word_list$word, word_list$freq, random.order = F, colors=pal, scale=c(3,0.5)) # 가장 큰 것 = 3, 가장 작은 것 = 0.5 


## 히트맵 실습 
library(stringr)
library(dplyr)
install.packages('pheatmap')
library(pheatmap)

patentDataset <- read.csv('C:/Temp/data-master/02_HeatMap.csv', header = T, sep=',')
head(patentDataset)
colnames(patentDataset) = c('Patent_No', 'IPC_all', "Inventor", 'Fm_patent', 'Forward', 'Fm_Country')

count = data.frame(matrix(nrow=0,ncol=3))
for(i in (1:nrow(patentDataset))){
  count_inventor = str_count(patentDataset$Inventor[i], '[::|::]')
  count_IPC = str_count(patentDataset$IPC_all[i], '[::|::]')
  count_FmPatent = str_count(patentDataset$Fm_patent[i], '[::|::]')
  
  df = data.frame(count_inventor, count_IPC, count_FmPatent)
  count = rbind(count,df)
}
count = count +1

row_name = patentDataset[,1]
newDataset = cbind(patentDataset, count)[,-(1:4)]
row.names(newDataset) = (row_name)

newDataset[is.na(newDataset)] = 0 
newDataset$Fm_Country <- as.numeric(newDataset$Fm_Country)

newDataset = as.matrix(newDataset)
newDataset
pheatmap(newDataset[1:10,], scale='none', legend = T)
































## 사회네트워크분석 실습 
library(stringr)
library(dplyr)
install.packages('tidyverse')
library(tidyverse)
install.packages('igraph')
library(igraph)

ipcData <- read.csv('C:/Temp/data-master/03_IPC_Network.csv', header =T, sep=',')
colnames(ipcData) = c('Patent_No','IPC')
head(ipcData)

split_into_multiple = function(column, pattern = ',', into_prefix){
  cols = str_split_fixed(column, pattern, n = Inf)
  cols[which(cols == '')] = NA
  cols = as_tibble(cols)
  m = dim(cols)[2]
  
  names(cols) = paste(into_prefix, 1:m, sep='_')
  return(cols)
}

ipc_matrix = ipcData %>%
  bind_cols(split_into_multiple(.$IPC,'[::|::]', 'IPC')) %>%
  select(Patent_No, starts_with('IPC_'))

ipc_matrix = ipc_matrix[,-1]
for (i in 1:nrow(ipc_matrix)){
  for(j in 1:ncol(ipc_matrix)){
    if (!is.na(ipc_matrix[i,j])){
      ipc_matrix[i,j] = substr(str_trim(ipc_matrix[i,j], side = 'left'), 1, 4)
    }
  }
}

df = data.frame(matrix(nrow=0, ncol=2))
for( i in 1:nrow(ipc_matrix)){
  for(j in 2:ncol(ipc_matrix)){
    if (!is.na(ipc_matrix[i,j])){
      df = rbind(df, data.frame(ipc_matrix[i,1], ipc_matrix[i,j]))
    }
  }
}

colnames(df) = c('from', 'to')
gdf = graph.data.frame(df, directed = F)
plot(gdf)




library(stringr)
library(dplyr)
install.packages('tidyverse')
library(tidyverse)
install.packages('igraph')
library(igraph)

ipcData <- read.csv('C:/Temp/data-master/03_IPC_Network.csv', header =T, sep=',')
colnames(ipcData) = c('Patent_No','IPC')

split_into_multiple = function(column, pattern = ',', into_prefix){
  cols = str_split_fixed(column, pattern, n = Inf)
  cols[which(cols == '')] = NA
  cols = as_tibble(cols)
  m = dim(cols)[2]
  
  names(cols) = paste(into_prefix, 1:m, sep='_')
  return(cols)
}

ipc_matrix = ipcData %>%
  bind_cols(split_into_multiple(.$IPC,'[::|::]', 'IPC')) %>%
  select(Patent_No, starts_with('IPC_'))

ipc_matrix = ipc_matrix[,-1]
for (i in 1:nrow(ipc_matrix)){
  for(j in 1:ncol(ipc_matrix)){
    if (!is.na(ipc_matrix[i,j])){
      ipc_matrix[i,j] = substr(str_trim(ipc_matrix[i,j], side = 'left'), 1, 4)
    }
  }
}

df = data.frame(matrix(nrow=0, ncol=2))
for( i in 1:nrow(ipc_matrix)){
  for(j in 2:ncol(ipc_matrix)){
    if (!is.na(ipc_matrix[i,j])){
      df = rbind(df, data.frame(ipc_matrix[i,1], ipc_matrix[i,j]))
    }
  }
}

colnames(df) = c('from', 'to')
gdf = graph.data.frame(df, directed = F)
plot(gdf)






































