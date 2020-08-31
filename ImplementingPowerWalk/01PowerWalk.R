## * Preamble

if (require("pacman")) {
    library(pacman)
  }else{
    install.packages("pacman")
    library(pacman)
  }
  pacman::p_load(Matrix, igraph, plotly, mise, docstring, expm)
  mise()

## * Create Example Matrix
n <- 20
m <- 10^2
i <- sample(1:m, size = n); j <- sample(1:m, size = n); x <- rpois(n, lambda = 90)
A <- sparseMatrix(i, j, x = x, dims = c(m, m))
A <- sparseMatrix(i, j, x = x, dims = c(m, m))

g1 <- igraph::erdos.renyi.game(n = 50, 0.2)
A <- igraph::get.adjacency(g1) # Row to column
plot(g1)
A <- as.matrix(A)

## * Inspect the Newly Created Matrix
summary(A)
str(A) # note that *internally* 0-based row indices are used

## * Naive Approach to Stationary Point
## ** Create a Diagonalised Scaling Matrix
## Create a way to Diagonalise a sparse matrix
sparse_diag <- function(mat) {
  #' Diagonal Factors of Sparse Matrix
  #' 
  #' Return a Diagonal Matrix containing either 1 / colsum() or 0 such that
  #' matrix multiplication with this matrix would have all columns 
  #' sum to 1
  #' 
  #' This should take the transpose of an adjacency matrix in and the output
  #' can be multiplied by the original matrix to scale it to 1.
  #' i
  # mat  <- A
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




## ** Weight the Adjacency Matrix

weight_adjMat <- function(adjMat) {
  #' Weight Adjacency Matrix
  #'
  #' Randomly weights an adjacency matrix so that terms
  #' are Real (as opposed to natural) values.
  A@x*rnorm(length(A@x), 0, 0.1)
}



## ** Function to create Prob Trans Mat
adj_to_probTrans <- function(wadjmat, beta) {
  #' Adjacency to Probability Transition Matrix
  #' 
  #' Returns a probability transition matrix from an input adjacency matrix
  #' 
  #' Transposes an input matrix and then scales each column to sum to 1. 
  #' Implemented with the Matrix dgCMatrix class in mind however also
  #' has logic to deal with a base matrix.
  #
  #' @param wadjmat A weighted adjacency matrix, ideally of the class dgCMatrix
  #' or atleast of the class matrix.
  #' @param beta The probability of following an edge

  wadjmat <- t(wadjmat)    # transpose Assuming row->column (like igraph)
#  wadjmat  <- A; beta  <- 0.8

  if ("dgCMatrix" %in% class(wadjmat)) {

#    B     <- sparseMatrix(i = summary(wadjmat)$i, j = summary(wadjmat)$j, x = beta^wadjmat@x) # element wise exponentiation
#    Don't do this ^^ because it comes out with clipped off dimensions
    B     <- wadjmat
    B@x   <- beta^wadjmat@x    # Element Wise exponentiation
    D_in  <- sparse_diag(B)
    T = B %*% D_in
    return(T)

  } else if ("matrix" %in% class(wadjmat)) {
    print("WARNING: expected dgCMatrix but matrix detected")
    print("Attemptying to proceed anyway")
    for (i in ncol(wadjmat)) {
      #  wadjmat[, i] <- wadjmat[, i] / sum(wadjmat[, i])
    B     <- wadjmat
    B   <- beta^wadjmat    # Element Wise exponentiation
    D_in  <- sparse_diag(B)
    T = B %*% D_in
    return(as.matrix(T))
    }
    return(wadjmat)
  } else {
    print("ERROR: Require sparse wadjmatrix of class dgCWadjmatrix to")
  }
}

class(A)
(T <- adj_to_probTrans(A, beta = 0.843234))  %>% summary()
T
## ** Power Method
p    <- rep(0, nrow(T))
p[1] <- 1
p_new    <- rep(0, nrow(T))
p_new[2]    <- 1

while (sum(round(p, 9) != round(p_new, 9))) {
    (p     <- p_new)
    (p_new <- T %*% p)
}



print(paste("The stationary point is"))
print(p)

## ** Faster Approach from 3.2 of paper

## * Sparse Matrix Approach
