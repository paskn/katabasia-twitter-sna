library(tm)
library(FactoMineR)
library(ggplot2)
library(Cairo)
library(wordcloud)
library(topicmodels)

load("./data/txt_data.rda")
stop_words <- readLines("./data/stop.txt")
set.seed(23)

for (i in 1:length(unique(txt_data$Cluster))) {
  cluster <- unique(txt_data$Cluster)[i]
  comm1 <- subset(txt_data, Cluster == cluster)
  tweet_corpus = Corpus(VectorSource(na.omit(comm1$Lem)))
  tweet_corpus = tm_map(tweet_corpus, removeWords, c(stopwords("russian"), stop_words))
  tweet_corpus = tm_map(tweet_corpus, removeWords, c(stopwords("english")))
  tweet_corpus = tm_map(tweet_corpus, stripWhitespace)
  tdm = DocumentTermMatrix(tweet_corpus)
  m = as.matrix(tdm)
  
  word_freqs = sort(rowSums(t(m)), decreasing=TRUE) 
  dm = data.frame(word=names(word_freqs), freq=word_freqs)
  if (cluster %in% c("comm3", "comm11") == TRUE) {
    CairoPDF(file=paste("./plots/comm-wordclouds/", cluster, ".pdf", sep=""))
    wordcloud(dm$word, dm$freq, random.order=FALSE, min.freq=1, colors=brewer.pal(8, "Dark2"))
    dev.off()
  }
  CairoPDF(file=paste("./plots/comm-wordclouds/", cluster, ".pdf", sep=""))
  wordcloud(dm$word, dm$freq, random.order=FALSE, colors=brewer.pal(8, "Dark2"))
  dev.off()
}

for (i in 1:length(unique(txt_data$Cluster))) {
  cluster <- unique(txt_data$Cluster)[i]
  comm1 <- subset(txt_data, Cluster == cluster)
  tweet_corpus = Corpus(VectorSource(na.omit(comm1$Lem)))
  tweet_corpus = tm_map(tweet_corpus, removeWords, c(stopwords("russian"), stop_words))
  tweet_corpus = tm_map(tweet_corpus, removeWords, c(stopwords("english")))
  tweet_corpus = tm_map(tweet_corpus, stripWhitespace)
  dtm = DocumentTermMatrix(tweet_corpus)
  rowTotals <- apply(dtm , 1, sum)
  DTM_new   <- dtm[rowTotals> 0, ]
  topics <- LDA(DTM_new, 5)
  write.csv(terms(topics, 100), file=paste("./data/comm-topics/", cluster, ".csv", sep=""), 
            fileEncoding = "UTF-8")
}
