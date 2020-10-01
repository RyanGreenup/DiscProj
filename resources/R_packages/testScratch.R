
## * Load Packages
if (require("pacman")) {
    library(pacman)
  } else {
    install.packages("pacman")
    library(pacman)
  }
  pacman::p_load(PageRank, devtools, Matrix, igraph, mise)
  mise()


## * Generate a graph
n  <- 10
g1 <- igraph::erdos.renyi.game(n = n, 0.2)
  # Every edge is created with the same sontant probability
A <- igraph::get.adjacency(g1) # Row to column
A <- t(A)
β <- beta <- 10
B <- beta^A

## * Find the Transition Matrix
Tpw <- PageRank::power_walk_prob_trans(A, beta = beta)
Trs <- PageRank::adj_to_probTrans(A)


## ** Implement the Power Walk
## *** Set Initial Values
  p_new  <- rep(1/n, n)  # Uniform
  p      <- rep(0, n)    # Zero
  η      <- 10^(-6)
## *** Implement the Loop

T <- B %*% diag(colSums(B)^(-1))

 while (sum((p_new - p)^2) > η^2) {
    (p <- as.vector(p_new)) # P should remain a vector
#     p_new  <- T %*% p + rep(t(δB) %*% p, n)
     p_new  <- T %*% p
  }
## ** Report the Values
print("The stationary point is:")
print(p)
print("\n The eigenvector for λ₁ is:")
print(eigen(T)$vectors[,1]/ sum(eigen(T)$vectors[,1]))
