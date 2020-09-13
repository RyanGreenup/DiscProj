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
n <- 7
m <- 10^2
i <- sample(1:m, size = n); j <- sample(1:m, size = n); x <- rpois(n, lambda = 90)
A <- sparseMatrix(i, j, x = x, dims = c(m, m))
A <- sparseMatrix(i, j, x = x, dims = c(m, m))

beta = 0.843234
β = beta

g1 <- igraph::erdos.renyi.game(n = 7, 0.2)
A <- igraph::get.adjacency(g1) # Row to column
plot(g1)

## * Inspect the Newly Created Matrix
summary(A)
str(A) # note that *internally* 0-based row indices are used

## * Naive Approach to Stationary Point
## ** Create a Diagonalised Scaling Matrix
## Create a way to Diagonalise a sparse matrix
## mat <- B
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
      ## If a column sums to zero the diag can be zero iff the adjacency_matrix>=0
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
    B     <- beta^wadjmat    # Element Wise exponentiation
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
(T <- adj_to_probTrans(A, beta = β))  %>% summary()
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



print(paste("The stationary point is, using the naive method"))
print(p)

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
