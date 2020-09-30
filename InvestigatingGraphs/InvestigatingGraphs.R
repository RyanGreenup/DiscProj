  if (require("pacman")) {
    library(pacman)
  } else {
    install.packages("pacman")
    library(pacman)
  }

  pacman::p_load(PageRank, devtools, Matrix)

g1 <- igraph::erdos.renyi.game(n = 7, 0.2)
A <- igraph::get.adjacency(g1) # Row to column
plot(g1)

  DA <- PageRank::create_sparse_diag_scaling_mat(A)




 THETA <-

THETA
