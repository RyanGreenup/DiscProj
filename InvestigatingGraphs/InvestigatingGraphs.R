  if (require("pacman")) {
    library(pacman)
  } else {
    install.packages("pacman")
    library(pacman)
  }

  pacman::p_load(PageRank, devtools, Matrix, igraph)
  mise()

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


n <- 30
g1 <- igraph::erdos.renyi.game(n = n, 0.2)
  # Every edge is created with the same sontant probability
A <- igraph::get.adjacency(g1) # Row to column
A <- t(A)
#plot(g1, vertex.size = 1)
df <- data.frame()
#df <- data.frame("e2"=c(), "A_dens" = c(), "A_det" = c(), "beta" = c())
df

n <- 20
p <- 1:n/n
beta <- 1:n/n
beta <- runif(n)*100
sz <- 1:n/n+10
input_var <- expand.grid("n" = n, "p" = p, "beta" = beta, "size" = sz)
input_var


random_graph <- function(n, p, beta, size) {
      g1 <- igraph::erdos.renyi.game(n = sz, p)
      A <- igraph::get.adjacency(g1) # Row to column
      A <- Matrix::t(A)

      A_dens <- mean(A)
      T      <- PageRank::power_walk_prob_trans(A)
      e2     <- eigen(T, only.values = TRUE)$values[2] # R orders by descending magnitude
      A_det  <- det(A)
      return(c(abs(e2), A_dens))
}

X <- as.vector(input_var[i,])
random_graph(X$n, X$p, X$beta, X$size)

## TODO this should use pmap.
Y <- matrix(ncol = 2, nrow = nrow(input_var))
for (i in 1:nrow(input_var)) {
  X <- as.vector(input_var[i,])
  Y[i,] <-  random_graph(X$n, X$p, X$beta, X$size)
}
if (sum(abs(Y) != abs(Re(Y))) == 0) {
  Y <- Re(Y)
}
nrow(input_var)
nrow(Y)
Y <- as.data.frame(Y); colnames(Y) <- c("eigenvalue2", "determinant")
head(Y)

data <- cbind(input_var, Y)
data$beta

ggplot(data, aes(x = determinant, y = eigenvalue2, size = beta, color = size, shape = factor(n))) +
  geom_point()

curve(dchisq(x, df = 10), from = 0, to = 40)



ggplot(data, aes(y = chi, x = index)) +
  geom_point(data)


names(data)
ggplot(data, aes(x = index, y = chi)) +
  geom_line()


chival <- dchisq(seq(from = 0, to = 40, length.out = 100), df = 10)*6
index  <- seq(from = 0, to = 2, length.out = 100)
chidata  <- data.frame(index = index, chi = chival)
ggplot(data) +
  geom_point(mapping = aes(x = determinant, y = eigenvalue2, size = beta, color = size, shape = factor(n))) +
  geom_line(data = chidata, mapping = aes(x = index, y = chi))
