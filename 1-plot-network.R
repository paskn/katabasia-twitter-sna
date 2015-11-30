library(twitteR)
library(igraph)
library(stringr)
library(sna)
library(Cairo)
library(RColorBrewer)

load("./data/katab_ego.rda")
network <- graph.edgelist(katab_ego, directed = FALSE)
g <- get.adjacency(network,sparse=FALSE)  # adjacency matrix нашего листа
opar <- par(no.readonly = T)

# строим нашу сеть

CairoPNG("./plots/katab_ego.png", width = 1600, height = 1600)
par(mar=c(.01,.01,.01,.01))
gplot(g, jitter=TRUE,
      mode = "kamadakawai", gmode = "graph", vertex.col = "#8DD3C7",
      #label = ifelse(betweenness(g) > 11000, 
      #               unique(katab_ego[,1]), NA),  #лейблы нодам с высокой betweenness
      label="katab_asia", edge.col = "gray", label.cex = 2.2)
dev.off()
par(opar)

# ищем сообщества в нашей сети
imc <- infomap.community(network)

#membership(imc)
#communities(imc)

#plot(imc, network)
gg <- network
comm_colors <- factor(imc$membership)
levels(comm_colors) <- brewer.pal(11, "Set3")
V(gg)$color <- as.character(comm_colors)

ad_gg <- get.adjacency(gg,sparse=FALSE)
CairoPNG("./plots/katab_ego_comm.png", width = 1600, height = 1600)
par(mar=c(.01,.01,.01,.01))
gplot(ad_gg, mode="kamadakawai", gmode = "graph",
      vertex.col = V(gg)$color,
      edge.col = "gray", label="katab_asia", label.cex = 2.2)
dev.off()
par(opar)

# plot high degree nodes network
high_degree_nodes <- names(igraph::degree(gg, mode="all")[igraph::degree(network, mode="all")>3])
ad_gg_sub <- subgraph.edges(network, V(network)[name %in% high_degree_nodes])
ad_gg_sub <- get.adjacency(ad_gg_sub,sparse=FALSE)
CairoPNG("./plots/katab_ego_comm_sub.png", width = 1900, height = 1700)
par(mar=c(.01,.01,.01,.01))
gplot(ad_gg_sub, mode="kamadakawai", gmode = "graph",
      vertex.col = V(gg)$color,
      edge.col = "gray", displaylabels=T,
      #label="katab_asia",
      label.cex = 2.2)
dev.off()
par(opar)
