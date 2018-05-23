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
  dtm.matrix <- as.matrix(dtm)
  wordcount <- colSums(dtm.matrix)
  wordcloud(words = names(wordcount),wordcount, min.freq = 1,
            max.words=100, random.order=FALSE, rot.per=0.35, 
            colors=brewer.pal(8, "Dark2"))
  title(sub = plot.title)
}

COG = function(dtm, title="COG",central.nodes=4,max.connexns = 5){
  
  library(igraph)
  
  dtm_1 = as.matrix(dtm)   
  adj_mat = t(dtm_1) %*% dtm_1
  diag(adj_mat) = 0   
  mat_2 = order(apply(adj_mat, 2, sum), decreasing = T)   
  mat_final = as.matrix(adj_mat[mat_2[1:50], mat_2[1:50]])
  
  cs= colSums(mat_final) 
  csr = order(-cs)   
  
  mat_ordered = mat_final[csr, csr]   
  diag(mat_ordered) =  0
  
  wc = NULL
  
  for (i in 1:central.nodes){ 
    thresh = mat_ordered[i,][order(-mat_ordered[i, ])[max.connexns]]
    mat_ordered[i, mat_ordered[i,] < thresh] = 0   
    mat_ordered[i, mat_ordered[i,] > 0 ] = 1
    word = names(mat_ordered[i, mat_ordered[i,] > 0])
    mat_ordered[(i+1):nrow(mat_ordered), match(word,colnames(mat_ordered))] = 0
    wc = c(wc, word)
  }
  
  mat_3 = mat_ordered[match(wc, colnames(mat_ordered)), match(wc, colnames(mat_ordered))]
  ord = colnames(mat_ordered)[which(!is.na(match(colnames(mat_ordered), colnames(mat_3))))]  
  mat_4 = mat_3[match(ord, colnames(mat_3)), match(ord, colnames(mat_3))]
  
  graph <- graph.adjacency(mat_4, mode = "undirected", weighted=T)    
  graph = simplify(graph) 
  V(graph)$color[1:central.nodes] = "steelblue1"
  V(graph)$color[(central.nodes+1):length(V(graph))] = "springgreen1"
  
  graph = delete.vertices(graph, V(graph)[ degree(graph) == 0 ]) 
  
  plot(graph, layout = layout.davidson.harel, main = title)
} 
