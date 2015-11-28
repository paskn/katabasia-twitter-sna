# takes a list of users and download 1000 tweets for each
# returns a data frame with tweets and user vars
get_tweets <- function(users, n_tweets, clust) {
  label <- NULL
  tweets <- NULL
  comm <- NULL
  label_loop <- NULL
  clust_loop <- NULL
  for (i in 1:length(users)) {
    label_loop <- NULL
    clust_loop <- NULL
    user_tweets = userTimeline(users[i], n=n_tweets)
    
    user_df = twListToDF(user_tweets)
    
    user_txt = user_df$text
    
    user_clean = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", user_txt)
    user_clean = gsub("@\\w+", "", user_clean)
    user_clean = gsub("[[:punct:]]", "", user_clean)
    user_clean = gsub("[[:digit:]]", "", user_clean)
    user_clean = gsub("http\\w+", "", user_clean)
    
    label_loop[1:length(user_clean)] <- users[i]
    clust_loop[1:length(user_clean)] <- clust[i]
    label <- c(label, label_loop)
    tweets <- c(tweets, user_clean)
    comm <- c(comm, clust_loop)
  }
  network_tweets <- data.frame(User = label, Tweet = tweets, Cluster = comm)
  return(network_tweets)
}
