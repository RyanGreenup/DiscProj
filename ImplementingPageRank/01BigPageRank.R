library(igraph)

g <- sample_gnp(10^3, 2/10)
## plot(g)
as_adjacency_matrix(g)
V(g)$name <- letters[1:vcount(g)]
as_adjacency_matrix(g)
E(g)$weight <- runif(ecount(g))
as_adjacency_matrix(g, attr="weight")
