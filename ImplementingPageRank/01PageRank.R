## * Preamble

if (require("pacman")) {
    library(pacman)
  }else{
    install.packages("pacman")
    library(pacman)
  }
  pacman::p_load(Matrix, igraph, plotly, mise, docstring)
  mise()
## * Code
##
g1 <- igraph::graph.formula(1 -2, 1 - 3, 1 - 4, 2 - 3)
plot(g1)


library(Matrix)
## simple example
i <- c(1,3:8); j <- c(2,9,6:10); x <- 7 * (1:7)
(A <- sparseMatrix(i, j, x = x))                    ##  8 x 10 "dgCMatrix"
summary(A)
str(A) # note that *internally* 0-based row indices are used


## simple example
n <- 20
m <- 10^6
i <- sample(1:m, size = n); j <- sample(1:m, size = n); x <- rpois(n, lambda = 90)
(A <- sparseMatrix(i, j, x = x, dims = c(m, m)))                    ##  8 x 10 "dgCMatrix"

summary(A)
str(A) # note that *internally* 0-based row indices are used


## Create a way to Diagonalise a sparse matrix
sparse_diag <- function(mat) {
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

working_sparse_transpose <- function(sparseMat) {
  #' Working Sparse Matrix Transpose
  #' 
  #' This transposes a sparse matrix, useful when working with 
  #' adjacency matrices and needing to get a probability transition matrix.
  #' 
  #' The t() function seems to ocassionaly break matrix multiplication
  #' so I needed to do it this way.
  #' @param sparseMat A sparse Matrix created with the Matrix library
  transmat <- sparseMatrix(p = sparseMat@p,
                          j = sparseMat@i,
                          x = sparseMat@x,
                          dims = c(nrow(sparseMat),
                                   ncol(sparseMat)))
  return(transmat)
}
D <- sparse_diag(A)
summary(D)
summary(A %*% D)
At <- working_sparse_transpose(A)
summary(At)
summary(At %*% D)


str(A)

B <- sparseMatrix(i = 1:5, j = 1:5, x = (1:5)^2)
t(B)
B@p 
t(B) %*% B




## Make the prob trans


adj_to_probTrans <- function(adjMat) {
##  if (class(A) == "dgCMatrix") print("ERROR: Require class dgCMatrix"); return
  t(adjMat) %*% sparse_diag(adjMat)
}

(T <- adj_to_probTrans(A))
summary(T)
summary(A)
summary(t(A))

summary(A %*% D)
summary(t(A) %*% D)

summary(A)
A@j
B <- sparseMatrix(i = 1:5, j = 1:5, x = 1:5, dims = c(5,5))
summary(B)$i <- 77:81

summ























## Calculate the Random Surfer
N <- nrow(A)
alpha <- 0.8
summary(T)

S <- alpha*T+(1-alpha)*B
S

T@x
B <- T
summary(B)
Matrix::i


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


