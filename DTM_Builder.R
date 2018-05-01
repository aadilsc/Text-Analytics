if (!require(tm)) {install.packages("tm")} 
if (!require(RWeka)) {install.packages("RWeka")}
if (!require(SnowballC)) {install.packages("SnowballC")}
if (!require(magrittr)) {install.packages("magrittr")}
if (!require(wordcloud)) {install.packages("wordcloud")}
if (!require(stringr)) {install.packages("stringr")}

library(tm)
library(RWeka)
library(SnowballC)
library(magrittr)
library(stringr)
library(wordcloud)

text_Cleaner <- function(c,remove_stopwords= T,remove_numbers = T){
  
  ct <- c %>% tm_map(content_transformer(function(x) x<- gsub("<.*?>", " ", x))) %>%
    tm_map(content_transformer(function(x) x<- iconv(x, "latin1", "ASCII", sub=""))) %>%
    tm_map(content_transformer(function(x) x<- gsub("[^[:alnum:]]", " ", x))) %>%
    tm_map(content_transformer(tolower)) %>%
    tm_map(content_transformer(stripWhitespace))
  
  if(remove_numbers){ct<-tm_map(ct,content_transformer(removeNumbers))}
  
  if(remove_stopwords){ct<- tm_map(ct,removeWords, stopwords("english"))}
  
  return(ct)
}

dtm_Builer <- function(x,min_tok=1,max_tok=2,TFIDF = T,min_word_len=3,min_bound = 1){
  Tokenizer <- function(x) NGramTokenizer(x, Weka_control(min = min_tok, max = max_tok))
  dtm_ngram <- DocumentTermMatrix(x, control = list(tokenize = Tokenizer,
                                                    removePunctuation = TRUE,
                                                    weighting =if(TFIDF){function(x)weightTfIdf(x, normalize =FALSE)} else {function(x)weightTf(x)}, 
                                                    stemming = F,
                                                    wordLengths = c(min_word_len, Inf),
                                                    bounds=list(local=c(min_bound,Inf)),
                                                    stopwords = T
  ))
  return(dtm_ngram)
}

wordCloudBuilder<- function(dtm,mf =1,mw =100,ro = F,plot.title = "wordCloud"){
  dtm.matrix <- as.matrix(dtm_ngram)
  wordcount <- colSums(dtm.matrix)
  wordcloud(words = names(wordcount),wordcount, min.freq = 1,
            max.words=100, random.order=FALSE, rot.per=0.35, 
            colors=brewer.pal(8, "Dark2"))
  title(sub = plot.title)
}
