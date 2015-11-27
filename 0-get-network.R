library(twitteR)
library(igraph)
library(stringr)
library(sna)
library(Cairo)

#setwd("~/Documents/socnet/katabasia-twitter-sna/")
set.seed(23)
# настраиваем доступ к апи твитера

# берем юзера и его список его друзей и делаем edge list
katabasia <- getUser("@katab_asia")
katab_friends <- katabasia$getFriends()

y <- NULL
x <- sapply(katab_friends, function(x) x$screenName)
names(x) <- NULL
y[1:13] <- "katab_asia"

ed1 <- c(x, y)
ed2 <- c(y, x)
katab_ego <- cbind(ed1, ed2)  # наш edge list

#katab_ego <- graph.edgelist(katab_ego)

# теперь берем друзей друзей нашего эго и делаем edge list
ego_node <- NULL
for (i in 1:length(x)) {
  ego <- getUser(x[i])
  ego$getScreenName()
  actors <- ego$getFriends()
  actors <- sapply(actors, function(x) x$screenName)
  names(actors) <- NULL
  ego_node[1:length(actors)] <- ego$getScreenName()
  part1 <- c(actors, ego_node)
  part2 <- c(ego_node, actors)
  ego_net <- cbind(part1, part2)
  
  katab_ego <- rbind(katab_ego, ego_net)  # большой edge list с друзьями друзей
}

#katab_ego_sub <- subset(x=data.frame(katab_ego),
#                        subset = ed1 %in% cc & ed2 %in% cc)
#network <- graph_from_data_frame(katab_ego_sub)
##############################################
save(katab_ego, file="./data/katab_ego.rda")
# сохраняем для gephi
g1.gexf <- igraph.to.gexf(network)
f <- file("./data/campnet.gexf")
writeLines(g1.gexf$graph, con = f)
close(f)
###############################################
