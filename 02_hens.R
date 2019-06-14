library(tidyverse)
library(igraph)
rm(list = ls())
hens <- read.csv("./data/moreno_hens/out.moreno_hens_hens", sep = "", header = FALSE) %>%
  rename(source = V1, target = V2)

g <- graph_from_data_frame(hens,directed = TRUE)


# Basic Inspection
V(g)$name
vertex_attr(g)
E(g)

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

# Force Directed con Size de Salida
V(g)$size <-  strength(g, mode = "out") # buscar sobre esto
# Summing up the edge weights of the adjacent edges for each vertex.
par(mar=c(0,0,0,0)); plot(g)
plot(g, layout=layout_with_fr, main="Force-directed")  


# Force Directed con Size de Entrada
V(g)$size <-  strength(g, mode = "in") # buscar sobre esto
# Summing up the edge weights of the adjacent edges for each vertex.
par(mar=c(0,0,0,0)); plot(g)
plot(g, layout=layout_with_fr, main="Force-directed")  



# Pagerank

pg <- page_rank(g, directed = TRUE, weights = NULL)


# Comparativo entre Pangerank y cantidad de nodos de entrada.
stre <- strength(g, mode = "in")

tac <- cbind.data.frame(name = names(pg[[1]]), pg = pg[[1]])

stre2 <- cbind.data.frame(name = names(stre),strength = stre)

pg_stre <- left_join(tac, stre2, by = "name") %>%
  arrange(desc(pg))

# Shortest Path
sh_pat <- shortest_paths(g, from = 3,to = 19,mode = "out")
