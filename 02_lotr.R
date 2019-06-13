# Guia http://pablobarbera.com/big-data-upf/html/02a-networks-intro-visualization.html
# Data https://github.com/morethanbooks/projects/tree/master/LotR
# http://www.morethanbooks.eu/graph-network-of-the-lord-of-the-rings/
library(tidyverse)
library(igraph)



lotr <- read.csv("./data/Book1_lotr.csv") %>%
  rename(weight = Weight)
# Generating Graph from csv
g <- graph_from_data_frame(lotr,directed = FALSE)


# Basic Inspection
is.weighted(g)
V(g)$name

vertex_attr(g)

E(g)

E(g)$weight

# First plot
par(mar=c(0,0,0,0))
plot(g)

par(mar=c(0,0,0,0))
plot(g,
     vertex.color = "grey", # change color of nodes
     vertex.label.color = "black", # change color of labels
     vertex.label.cex = .75, # change size of labels to 75% of original size
     edge.curved=.25, # add a 25% curve to the edges
     edge.color="grey20") # change edge color to grey

# Trials following analysis

V(g)$size <-  strength(g) # buscar sobre esto
# Summing up the edge weights of the adjacent edges for each vertex.
par(mar=c(0,0,0,0)); plot(g)

V(g)$size <- log(strength(g)) * 4 + 3
par(mar=c(0,0,0,0)); plot(g)

V(g)$label <- ifelse( strength(g)>=100, V(g)$name, NA )
par(mar=c(0,0,0,0)); plot(g)

par(mfrow=c(2, 3), mar=c(0,0,1,0))
plot(g, layout=layout_randomly, main="Random")
plot(g, layout=layout_in_circle, main="Circle")
plot(g, layout=layout_as_star, main="Star")
plot(g, layout=layout_as_tree, main="Tree")
plot(g, layout=layout_on_grid, main="Grid")
plot(g, layout=layout_with_fr, main="Force-directed")  

V(g)$label <- V(g)$name
par(mfrow=c(1,1),mar=c(0,0,0,0)); plot(g, layout=layout_in_circle, main="tree")


# Try removing less connected
V(g)$size <- log(strength(g)) * 4 + 3
g_less <- delete.vertices(g, which(V(g)$size <22)) # A ojo

plot(g_less, layout=layout_randomly, main="Random")                                     

par(mfrow=c(1,1),mar=c(0,0,0,0)); plot(g_less, layout=layout_with_fr, main="Force-directed")


frodo_path <- shortest.paths(g_less, v = 1,weights = NULL)
igraph::get.shortest.paths(g_less, from = 20, weights = g_less$weight) # ??

# Pagerank

pg <- page_rank(g, directed = FALSE, weights = NULL)

pg1 <- pg[[1]]
rownames
stre <- strength(g)

tac <- cbind.data.frame(name = names(pg[[1]]), pg = pg[[1]])

stre2 <- cbind.data.frame(name = names(stre),strength = stre)

pg_stre <- left_join(tac, stre2, by = "name") %>%
  arrange(desc(pg))

# Shortest Path inverting wieght

g_inv <- g
E(g_inv)$weight <- 1/E(g_inv)$weight            

sh_pat <- shortest_paths(g_inv, from = "sfax")

plot(g_inv)

betweenness(g_inv, v = V(g_inv), directed = FALSE)

# lo meesmo pero con un grafo reducido
g_inv_less <- g_less
E(g_inv_less)$weight <- 1/E(g_inv_less)$weight            

sh_pat <- shortest_paths(g_inv_less, from = "sfax")

plot(g_inv_less)

betweenness(g_inv_less, v = V(g_inv_less), directed = FALSE)
