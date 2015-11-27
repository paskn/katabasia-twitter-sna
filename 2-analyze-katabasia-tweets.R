source("./funs/get_tweets.R")
source("./funs/lemmatize.R")
library(wordcloud)
library(tm)
library(FactoMineR)
library(cluster)
library(topicmodels)

stop_words <- readLines("./data/stop.txt")

load("./data/katab_lem.rda")
#katab_tweets <- get_tweets('katab_asia')  # grab the tweets
#katab_lem <- lemmatize(as.character(katab_tweets$Tweet))  # lemmatize them
#save(katab_lem, file="./data/katab_lem.rda")

katab_corpus = Corpus(VectorSource(katab_lem))

# convert to lower case
katab_corpus = tm_map(katab_corpus, content_transformer(tolower))
# remove stoprwords
katab_corpus = tm_map(katab_corpus, removeWords, c(stopwords("russian"), stop_words))
katab_corpus = tm_map(katab_corpus, removeWords, c(stopwords("english")))
# remove extra white-spaces
katab_corpus = tm_map(katab_corpus, stripWhitespace)

# term-document matrix
tdm = TermDocumentMatrix(katab_corpus)

# convert as matrix
m = as.matrix(tdm)

# remove sparse terms (word frequency > 90% percentile)
wf = rowSums(m)
m1 = m[wf>quantile(wf,probs=0.9), ]

# remove columns with all zeros
m1 = m1[,colSums(m1)!=0]

# for convenience, every matrix entry must be binary (0 or 1)
m1[m1 > 1] = 1
katab_ca = CA(m1, graph=FALSE)

# partitioning around medoids iwth 6 clusters
k = 6

# pam clustering
katab_pam = pam(katab_ca$row$coord[,1:2], k)

# get clusters
clusters = katab_pam$clustering

# create data frame
katab_words_df = data.frame(
  words = rownames(m1),
  dim1 = katab_ca$row$coord[,1],
  dim2 = katab_ca$row$coord[,2],
  freq = rowSums(m1),
  cluster = as.factor(clusters))

CairoPDF("./plots/katab_wordcloud.pdf")
par(mar=c(1,1,1,1))
wordcloud(katab_words_df$words, katab_words_df$freq, 
          random.order=FALSE, colors=brewer.pal(8, "Dark2"))
dev.off()
par(opar)

# topic modeling
DTM <- DocumentTermMatrix(katab_corpus)
rowTotals <- apply(DTM , 1, sum)
DTM_new   <- DTM[rowTotals> 0, ]
topics <- LDA(DTM_new, 5)
terms(topics, 10)
