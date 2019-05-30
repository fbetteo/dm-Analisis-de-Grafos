library(igraph)
library(tidyverse)


# Genero Grafo
raw <- sample_k_regular(10, 4, directed = FALSE, multiple = TRUE) 
raw <-  igraph::simplify(raw, remove.multiple = FALSE, remove.loops = TRUE)


igraph::plot.igraph(raw)

# Lo convierto en lista de vecinos , todo formato numerico
raw2 <- map(.x = raw, .f = function(x) pluck(x,1, as.vector) )

####

simple_graph <- list( c(2,3), c(1,3), c(1,2))

# Funcion que determina si un grafo tiene todos sus nodos de grado par
is.pair <- function(adjlist){
pair <- map_dbl(adjlist, .f = function(x) length(x) %% 2) %>%
  sum()
out <-  pair == 0
print(out)
}

is.pair(raw2)

###
nodo_inicial <- 1
adjlist <- simple_graph
# Determina si un grafo es conexo. El input es el grafo y el nodo desde el que empiezo
is.conex <- function(adjlist, nodo_inicial){
  alcanzados <-  nodo_inicial
  alcanzados_acumulado <-  alcanzados
  alcanzados <- c(alcanzados,adjlist[[nodo_inicial]])[-1]
  alcanzados_acumulado <-  c(alcanzados_acumulado,alcanzados)
  
  pisados <-  vector()
  pisados <- c(pisados, nodo_inicial)
  while (length(alcanzados) >=1) {
    alcanzados <-  c(alcanzados, adjlist[[alcanzados[[1]]]])
    alcanzados_acumulado <-  c(alcanzados_acumulado, adjlist[[alcanzados[[1]]]] )
    pisados <- c(pisados,alcanzados[1])
    alcanzados <- alcanzados[-1]
    alcanzados <- alcanzados[!alcanzados %in% pisados]
  }
  out <- (nrow(available.nodes(adjlist)) == length(pisados))
  return(out)
}

is.conex(raw2,1)
aa <- available.nodes(raw2) %>% select(nodeid) %>%
  nrow()
###

# Lista de nodos con conexiones
available.nodes <- function(adjlist) {
  avai <- map_df(adjlist, .f = function(x) data.frame(connections = length(x) ), .id= "nodeid") %>%
    dplyr::filter(., connections > 0) %>%
    select(nodeid)

}

available.nodes(adjlist)

###




camino_euleriano <- function(adjlist){
  
  # Chequeo (esta bien implementado?)
  if (is.pair(adjlist) == TRUE & is.conex(adjlist,1) == TRUE){
  
for (i in 1:1000){
  adjlist_temp <- adjlist
  posible <-  TRUE
  print(i)
  # el inicial  
pisados <- sample(length(adjlist_temp),1)
  while (posible == TRUE){
      origen <- pisados[length(pisados)]
      vecinos_origen <- adjlist_temp[[origen]]
      # vecino al azar
      if (length(vecinos_origen) > 1){
      next_step <- sample(vecinos_origen,1)
      } else {
        next_step <- vecinos_origen
      }
      # borro la conexion entre ambos
      # borro solo la primera conexion con match, por si hay mas de una
      adjlist_temp[[origen]] <-  vecinos_origen[-match(next_step, vecinos_origen)]
      adjlist_temp[[next_step]] <- adjlist_temp[[next_step]][-match(origen,adjlist_temp[[next_step]])]
      pisados <- c(pisados, next_step)
      end <- nrow(available.nodes(adjlist_temp)) # si llega a 0 terminamos
      posible <- is.conex(adjlist_temp,next_step)
      print(end)
      print(posible)
      print(pisados)
   }

  if (end == 0) {
    break
  }
next
 }
 return(pisados)
  }    
}
is.conex(simple_graph,1)

gg <- camino_euleriano(simple_graph)
gg <- camino_euleriano(raw2)

print(available.nodes(raw2))
raw2[[origen]]

aa <- c(1,1,2,3,4,5,5)
b <- 5
aa <- aa[-match(b,aa)]


origen <- raw2[[pisados[length(pisados)]]]
# PRUEBAS
# 
# print_rec(raw2[[1]])
# rm(pisados)
# alcanzados = 0
# alcanzados <- c(alcanzados,raw2[[1]])[-1]
# alcanzados_acumulado = alcanzados
# 
# pisados = vector()
# while (length(alcanzados) >=1) {
#   alcanzados <-  c(alcanzados, raw2[[alcanzados[[1]]]])
#   alcanzados_acumulado <-  c(alcanzados_acumulado, raw2[[alcanzados[[1]]]] )
#   pisados <- c(pisados,alcanzados[1])
#   alcanzados <- alcanzados[-1]
#   alcanzados <- alcanzados[!alcanzados %in% pisados]
# }


sample(c(2,10),1)
sample(as.vector(10),1)
