load("./data/katab_ego.rda")
network <- graph.edgelist(katab_ego)
g <- get.adjacency(network,sparse=FALSE)  # adjacency matrix нашего листа
opar <- par(no.readonly = T)

# строим нашу сеть

CairoPNG("./plots/katab_ego.png", width = 1600, height = 1600)
par(mar=c(.01,.01,.01,.01))
gplot(g, jitter=TRUE, usecurv=TRUE,
      mode = "kamadakawai", gmode = "graph",
      #label = ifelse(betweenness(g) > 11000, 
      #               unique(katab_ego[,1]), NA),  #лейблы нодам с высокой betweenness
      label="katab_asia", edge.col = "gray", label.cex = 2)
dev.off()
par(opar)

# ищем сообщества в нашей сети
imc <- infomap.community(network)

#membership(imc)
#communities(imc)

#plot(imc, network)
gg <- network
V(gg)$color <- imc$membership + 1

ad_gg <- get.adjacency(gg,sparse=FALSE)
CairoPNG("./plots/katab_ego_comm.png", width = 1600, height = 1600)
par(mar=c(.01,.01,.01,.01))
gplot(ad_gg, mode="kamadakawai", gmode = "graph",
      vertex.col = V(gg)$color,
      edge.col = "gray", label="katab_asia", label.cex = 2)
dev.off()
par(opar)
