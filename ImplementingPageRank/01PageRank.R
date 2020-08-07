## * Preamble

if (require("pacman")) {
    library(pacman)
  }else{
    install.packages("pacman")
    library(pacman)
  }
  pacman::p_load(Matrix, igraph, plotly, mise, docstring)
  mise()

## * Create Example Matrix
n <- 20
m <- 10^6
i <- sample(1:m, size = n); j <- sample(1:m, size = n); x <- rpois(n, lambda = 90)
(A <- sparseMatrix(i, j, x = x, dims = c(m, m)))                    

## * Inspect the Newly Created Matrix
summary(A)
str(A) # note that *internally* 0-based row indices are used

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

## * Function to create Prob Trans Mat
adj_to_probTrans <- function(adjMat) {
  #' Adjacency to Probability Transition Matrix
  #' 
  #' Returns a probability transition matrix from an input adjacency matrix
  #' 
  #' Transposes an input matrix and then scales each column to sum to 1. 
  #' Implemented with the Matrix dgCMatrix class in mind however also
  #' has logic to deal with a base matrix.
  #' @param adjMat An adjacency matrix, ideally of the class dgCMatrix or 
  #' atleast of the class matrix.

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

## * State the Random Surfer (Can't be Implemented)
N <- nrow(A)
alpha <- 0.8
  ## We can't though because this is too slow
    # rep(1/N, nrow(A)^2) %>% matrix(nrow = nrow(A)) -> B
    # S <- alpha*T+(1-alpha)*B

## Instead, with a bit of algebra we can jump right into the power method 
## by modifying the random surfer.

## * Implement the Power Method to Solve Random Surfer (Modified Strategy)
## ** Variables for Random Surfer (Alternative Algebra)
## Find Stationary point of random surfer
N     <- nrow(A)
alpha <- 0.8
F     <- rep((1-alpha)/N, nrow(A))  ## A nx1 vector of (1-alpha)/N

## ** Implement the Power Method Loop
## Solve using the power method
p     <- rep(0, length.out = ncol(T)); p[1] <- 1
p_new <- alpha*T %*% p + F

## use a Counter to debug
i <- 0
while (sum(round(p, 9) != round(p_new, 9))) {
    p     <- p_new
    p_new <- alpha*T %*% p + F
    (i <- i+1) %>% print()
}

p %>% head() %>% print()
