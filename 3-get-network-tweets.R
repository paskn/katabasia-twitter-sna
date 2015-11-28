# 1. subset names of user with degree > 3
# 2. take their community var
# 3. download their tweets (?)
# 4. lemmatize them
# 5. do correspondence analysis with the txts and community var
library(twitteR)
library(igraph)

source("./funs/get_tweets.R")
source("./funs/lemmatize.R")
set.seed(23)
load("./data/katab_ego.rda")

network <- graph.edgelist(katab_ego, directed = FALSE)
high_degree_nodes <- names(igraph::degree(network, mode="all")[igraph::degree(network, mode="all")>3])

imc <- infomap.community(network)

high_degree_subnet <- subgraph.edges(network, V(network)[name %in% high_degree_nodes])
hd_df <- as_data_frame(high_degree_subnet)

hd_df$community <- imc$membership[imc$name %in% high_degree_nodes]

twitter_sub <- data.frame(name = unique(c(hd_df$from,hd_df$to)),
                          community = imc$membership[imc$name %in% unique(c(hd_df$from,hd_df$to))])

