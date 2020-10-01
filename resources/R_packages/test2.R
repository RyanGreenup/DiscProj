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
β <- beta <- 10
B <- beta^A

## * Find the Transition Matrix
Tpw <- PageRank::power_walk_prob_trans(A, beta = beta)
Trs <- PageRank::adj_to_probTrans(A)

## * Print the Eigen Values
eigen(Tpw, only.values = TRUE)$values
eigen(Trs, only.values = TRUE)$values

eig_sol_pw          <- eigen(Tpw, symmetric = FALSE)$vectors[,1]
power_method_sol_pw <- PageRank::power_walk_stationary_point(A, eta = 10^-6, beta = beta)

## These should really be equal huh...
sum(eig_sol_pw - power_method_sol_pw)


## * Faster Sparse Approach from 3.2 of paper
## ** Define Constants
  n <- nrow(A)
## ** Find the B Matrix
## Define B, depending on input
    B      <- A
    B      <- β^A   # Element Wise exponentiation

    Bo     <- A
    # These two approaches are equivalent
    Bo@x   <- β^(A@x) -1   # This in theory would be faster
    # Bo     <- β^(A) -1
    # Bo     <- drop0(Bo)

## ** Create the Scaling Matrix
## Create the Scaling Matrix to make row sums 1
##
## TODO So the issue is that 1/(colSums(Bo)+n) != 1/(colSums(B))
##      Investigate why this is occuring, what's wrong with the math
##      Write up notes on the mistake, make a PDF, submit the code
##
  δB   <- 1/(colSums(Bo)+n) # = 1/(colSums(B))
  δBt  <- t(δB)
  DB   <- diag(δB)
    (T = B %*% DB)


## ** Create the Transition Probability Matrix
## Create the Trans Prob Mat using Power Walk
  T <- Bo %*% DB

## ** Implement the Power Walk
## *** Set Initial Values
  p_new  <- rep(1/n, n)  # Uniform
  p      <- rep(0, n)    # Zero
  η      <- 10^(-6)
## *** Implement the Loop

 while (sum(abs(p_new - p)) > η) {
    (p <- as.vector(p_new)) # P should remain a vector
    sum(p <- as.vector(p_new)) # P should remain a vector
     p_new  <- T %*% p + rep(t(δB) %*% p, n)
  }
## ** Report the Values
print(paste("The stationary point is"))

print(p)
eig_sol_pw
power_method_sol_pw
