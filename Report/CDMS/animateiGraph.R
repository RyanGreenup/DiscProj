library(igraph)
library(tidyverse)



for (i in 1:10) {
  g <- igraph::sample_pa(n = i, power = 2.5, m = 2)
  Sys.sleep(1)
  plot(g)
}






l <- layout.reingold.tilford(g,params=list(root=2))


gridLayout <- function(x)
{
  LmatX <- seq(-1,1,length=ncol(x))
  LmatY <- seq(1,-1,length=nrow(x))
  
  loc <- t(sapply(1:max(x),function(y)which(x==y,arr.ind=T)))
  layout <- cbind(LmatX[loc[,2]],LmatY[loc[,1]])
  return(layout)
}



grid <- matrix(c(
  0,0,1,0,0,
  2,0,3,0,4),nrow=2,byrow=TRUE)

library("igraph")

g <- graph.adjacency(grid)

plot(g)
plot(g,layout=gridLayout(grid))


g   <- igraph::sample_pa(n = 30, power = 2.5, m = 2)
L   <- layout.reingold.tilford(g)
L
i <- 100
for (i in 1:10) {
  g   <- igraph::sample_pa(n = i, power = 2.5, m = 2)
  plot(g,layout=L)
  Sys.sleep(1)
}
