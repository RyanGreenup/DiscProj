  if (require("pacman")) {
    library(pacman)
  } else {
    install.packages("pacman")
    library(pacman)
  }

  pacman::p_load(PageRank, devtools, Matrix, igraph)

g1 <- igraph::erdos.renyi.game(n = 9, 0.2)
A <- igraph::get.adjacency(g1) # Row to column
A <- t(A)
# plot(g1)

## * Finding beta values to behave like Random Surfer
  beta <- 10
  B <- beta^A

  DA     <- PageRank::create_sparse_diag_sc_inv_mat(A)
  DB_inv <- PageRank::create_sparse_diag_scaling_mat(B)

 THETA <- DA %*% DB_inv

THETA <- function(A, beta) {
  B  <- beta^A
  DA     <- PageRank::create_sparse_diag_sc_inv_mat(A)
  DB_inv <- PageRank::create_sparse_diag_scaling_mat(B)
  return(DA %*% DB_inv)
}

THETA_inv <- function(A, beta) {
  B  <- beta^A
  DB     <- PageRank::create_sparse_diag_sc_inv_mat(B)
  DA_inv <- PageRank::create_sparse_diag_scaling_mat(A)
  return(DA %*% DB_inv)
}

beta_func <- function(A, beta) {
    return(1-THETA(A, beta^A) %*% THETA_inv(A, beta^A))
}

THETA(A, 10) %*% THETA_inv(A, 10)


eta <- 10^-6
beta <- 1.01
while (mean(beta*matrix(1, nrow(A), ncol(A)) - beta_func(A, beta)) > eta) {
    beta <- beta + 0.01
    print(beta)
    print(diag(beta_func(A, beta)))
    print(beta*matrix(1, nrow(A), ncol(A)))
    print(beta_func(A, beta))
#    Sys.sleep(0.1)
}

beta


diag(beta_func(A, beta))
beta


## * Will E_2 always be in gamma? (only about 3% of the time)
##
 n <- 20
E2_in_gamma <- function() {

    n <- sample.int(100, 1)
    p <- runif(1)

    g1 <- igraph::make_full_citation_graph(n = n, directed = TRUE)
    A <- igraph::get.adjacency(g1) # Row to column
    A <- t(A)
  plot(g1)

    I <- matrix(1, n, n)
    beta <- sample.int(n = 100, size = 1)
    DB_inv <- PageRank::create_sparse_diag_scaling_mat(beta^A)
    gamma <- I - n*DB_inv

    eigen(A)$values[2] %in% as.vector(gamma)

}


x <- replicate(10^3, E2_in_gamma())

mean(x) ## 96%


E2_in_gamma <- function() {

    n <- sample.int(100, 1)
    p <- runif(1)

    g1 <- igraph::erdos.renyi.game(n = n, p = p)
    A <- igraph::get.adjacency(g1) # Row to column
    A <- t(A)
  plot(g1)

    I <- matrix(1, n, n)
    beta <- 10
    DB_inv <- PageRank::create_sparse_diag_scaling_mat(beta^A)
    gamma <- I - n*DB_inv

    eigen(A)$values[2] %in% as.vector(gamma)

}


x <- replicate(10^3, E2_in_gamma())

mean(x) ## 96%


    g1 <- igraph::make_tree(30)
    A <- igraph::get.adjacency(g1) # Row to column
    A <- t(A)
  plot(g1)

## * Looking at Density and Beta

PageRank::power_walk_stationary_point()

n <- 30
g1 <- igraph::erdos.renyi.game(n = n, 0.2)
  # Every edge is created with the same sontant probability
A <- igraph::get.adjacency(g1) # Row to column
A <- t(A)
#plot(g1, vertex.size = 1)
df <- data.frame()
#df <- data.frame("e2"=c(), "A_dens" = c(), "A_det" = c(), "beta" = c())
df

n <- 100
p <- 1:n/n
beta <- 1:n/n
sz <- 1:n/n+10
vals <- matrix(nrow(n^3, ncol = 3+1)

for (p in p) {
  for (beta in beta) {
    for (sz in sz) {
      g1 <- igraph::erdos.renyi.game(n = sz, p)
       A <- igraph::get.adjacency(g1) # Row to column
       A <- t(A)

       A_dens <- mean(A)
       T      <- PageRank::power_walk_prob_trans(A)
       eigen(T, only.values = TRUE)




    }
  }
}

eigen(T, values.only = TRUE)
eigen(T, only.values = TRUE)$values[1]
