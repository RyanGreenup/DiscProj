## * Preamble

if (require("pacman")) {
    library(pacman)
  }else{
    install.packages("pacman")
    library(pacman)
  }
  pacman::p_load(Matrix, igraph, plotly, mise, docstring)
  mise()

library(Matrix)

## Create Example Matrix
n <- 20
m <- 10^6
i <- sample(1:m, size = n); j <- sample(1:m, size = n); x <- rpois(n, lambda = 90)
(A <- sparseMatrix(i, j, x = x, dims = c(m, m)))                    

summary(A)
str(A) # note that *internally* 0-based row indices are used

## Create a way to Diagonalise a sparse matrix
sparse_diag <- function(mat) {
  #' Diagonal Factors of Sparse Matrix
  #' 
  #' Return a Diagonal Matrix of the 1 / colsum() such that
  #' matrix multiplication with this matrix would have all column sums
  #' sum to 1
  #' 
  #' This should take the transpose of an adjacency matrix in and the output
  #' can be multiplied by the original matrix to scale it to 1.
  #' i
  
  ## Get the Dimensions 
  n <- nrow(mat)
  
  ## Make a Diagonal Matrix of Column Sums
  D <- sparseMatrix(i = 1:n, j = 1:n, x = colSums(mat), dims = c(n,n))
  
  ## Throw away explicit Zeroes
  D <- drop0(D)
  
  ## Inverse the Values
  D@x <- 1/D@x
  
  ## Return the Diagonal Matrix
  return(D)
}
D <- sparse_diag(t(A))
summary(D)
summary(t(A) %*% D)

## Make the prob trans

adj_to_probTrans <- function(adjMat) {
  mat <- t(adjMat)
  if (class(adjMat) == "dgCMatrix") {
    T <- mat %*% sparse_diag(mat)
    return(T)
  } else if (class(adjMat) == "matrix") {
    print("WARNING: expected dgCMatrix but matrix detected")
    print("Attemptying to proceed anyway")
    for (i in ncol(mat)) {
      mat[, i] <- mat[, i] / sum(mat[, i])
    }
    return(mat)
  } else {
    print("ERROR: Require sparse matrix of class dgCMatrix to")
  }
}

(T <- adj_to_probTrans(A))
summary(T)

## Calculate the Random Surfer
N <- nrow(A)
B <- T  # Use T as a template to save effort
B@x <- B@x/B@x/N  # Change nonZero terms to 1/N
alpha <- 0.8

S <- alpha*T+(1-alpha)*B


summary(T)
summary(S)


## Solve using the power method
p     <- rep(0, length.out = ncol(S)); p[1] <- 1
p_new <- S %*% p

p_new[p_new != 0]
p[p != 0]

i <- 0
while (sum(round(p, 9) != round(p_new, 9))) {
    p     <- p_new
    p_new <- S %*% p
p_new[p_new != 0]
p[p != 0]
    i <- i+1
    print(i)
}

p_new[p_new != 0]
p[p != 0]



Summary(t(A))

matrix(1:9, nrow = 3)**(-1)



  