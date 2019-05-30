library(igraph)
library(tidyverse)

# Genero Grafo
raw <- sample_k_regular(10, 4, directed = FALSE, multiple = TRUE) %>% 
  igraph::simplify( remove.multiple = FALSE, remove.loops = TRUE)


  igraph::plot.igraph(raw)

# Lo convierto en lista de vecinos , todo formato numerico
raw2 <- map(.x = raw, .f = function(x) pluck(x,1, as.vector) )

####

# Genero Grafo super sencillo para probar
simple_graph <- list( c(2,3), c(1,3), c(1))

# Funcion que determina si un grafo tiene todos sus nodos de grado par
is.pair <- function(adjlist){
  pair <- map_dbl(adjlist, .f = function(x) length(x) %% 2) %>%
    sum()
    
  out <-  pair == 0
  print(out)
}

is.pair(raw2)

###
# Lista de nodos con conexiones
available.nodes <- function(adjlist) {
  avai <- map_df(adjlist, .f = function(x) data.frame(connections = length(x) ), .id= "nodeid") %>%
    dplyr::filter(., connections > 0) %>%
    select(nodeid)
  
}


# Determina si un grafo es conexo. El input es el grafo y el nodo desde el que empiezo
# Creo que alcanzados acumulado no aporta nada
is.conex <- function(adjlist, nodo_inicial){
  alcanzados <-  nodo_inicial
  alcanzados_acumulado <-  alcanzados
  alcanzados <- c(alcanzados,adjlist[[nodo_inicial]])[-1] # vecinos
  alcanzados_acumulado <-  c(alcanzados_acumulado,alcanzados) # inicial + sus vecinos
    pisados <-  vector()
  pisados <- c(pisados, nodo_inicial)
  
  
  while (length(alcanzados) >=1) { # Mientras haya algun vecino al que no fui aun continua
  
    alcanzados <-  c(alcanzados, adjlist[[alcanzados[[1]]]]) # vecinos del primer vecino
    alcanzados_acumulado <-  c(alcanzados_acumulado, adjlist[[alcanzados[[1]]]] ) # acumulo
    pisados <- c(pisados,alcanzados[1])
    alcanzados <- alcanzados[-1] # borro el vecino al que ya fui. Pero quedan listados sus vecinos
                                 # es busqueda a lo ancho
    alcanzados <- alcanzados[!alcanzados %in% pisados] # Desestimo los vecinos por los que ya pase para evitar loop infinito
  }
  # Si ya no quedan mas vecinos a los que ir y pase por todo -> Conexo
  out <- (nrow(available.nodes(adjlist)) == length(pisados))
  return(out)
}

is.conex(raw2,1)

###

# Devuelve Camino Euleriano si existe
camino_euleriano <- function(adjlist){
  
  # Chequeo Paridad de los nodos y Conexo
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
  } else {
      print("No cumple con paridad o ser conexo")
  }
}


## Prueba del camino Euleriano
nocumple <- camino_euleriano(simple_graph)
encuentra <- camino_euleriano(raw2)

