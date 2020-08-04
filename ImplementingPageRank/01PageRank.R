## * Preamble

if (require("pacman")) {
    library(pacman)
  }else{
    install.packages("pacman")
    library(pacman)
  }
  pacman::p_load(Matrix, igraph, plotly)
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
sparse_diag <- function(vec) {
  
  ## Work out which values are non-zero
#  i <- which(vec != 0)
  
  ## What are those values
#  x <- vec[i]
  
  ## Return a sparse diagonal matrix
#  Matrix::sparseMatrix(i = i, j = j, x = x, dims = c(length(vec), length(vec)))
  
  ## Better Approach
  Diagonal(n = length(colSums(A)), colSums(A))
}

## Make the prob trans

adj_to_probTrans <- function(adjMat) {
  adjMat_colSums     <- colSums(t(adjMat))
  adjMat_colSums_inv <- adjMat_colSums@x**(-1)
  t(adjMat) %*% sparse_diag(1/colSums(t(adjMat)))
}

(T <- adj_to_probTrans(A))
summary(T)
summary(t(A))
summary(sparse_diag(colSums(A)))
sparse_diag(colSums(A))@x


## Calculate the Random Surfer
N <- nrow(A)
B <- (T-T)/N
Matrix::sparseMatrix(dd)
l <- 0.8
S <- l*T+(1-l)*B
S

T@x


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


