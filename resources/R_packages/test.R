## * Load Packages
if (require("pacman")) {
    library(pacman)
  } else {
    install.packages("pacman")
    library(pacman)
  }
  pacman::p_load(PageRank, devtools, Matrix, igraph)

## * Generate a graph
n  <- 30
g1 <- igraph::erdos.renyi.game(n = n, 0.2)
  # Every edge is created with the same sontant probability
A <- igraph::get.adjacency(g1) # Row to column
A <- t(A)
beta <- 10
B <- beta^A

## * Find the Transition Matrix
Tpw <- PageRank::power_walk_prob_trans(A, beta = beta)
Trs <- PageRank::adj_to_probTrans(A)

## * Print the Eigen Values
# eigen(Tpw, only.values = TRUE)$values
# eigen(Trs, only.values = TRUE)$values

(eig_sol_pw          <- eigen(Tpw, symmetric = FALSE)$vectors[,1]/sum(eigen(Tpw, symmetric = FALSE)$vectors[,1]))
(power_method_sol_pw <- PageRank::power_walk_stationary_point(A, eta = 10^-6, beta = beta))
