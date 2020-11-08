setwd("/tmp/tmp.PcR5Lhk1be")
#!/usr/bin/R
## For these examples, first generate a scale free network using preferential attachment:
library(qgraph)
library(animation)
library(mise)
mise()

############################################################################
## This script will implement a scale free preferential attachment graph. ##
## Each time step a node will be added and connected to a previous node.  ##
## The probability of connecting to a previous node will be given by:     ##
##                                                                        ##
##   pᵢ ∝  kᵢᵞ + a                                                        ##
##                                                                        ##
## Where ɣ is a constant value (usually betwee 2-3),                      ##
## k is the degree of the ith node                                        ##
## a is the probability of connecting to an orphan node                   ##
############################################################################

## In this script I used a for loop to generate the graph, I didn't need to
## to do this, I could have just created a sample_pa with igraph, which
## I'll implement in another script



## * Create Variables
# Number of nodes
n <- 500
# Empty vector for degrees:
degree_vec <- rep(0, n)

# Edges in the form of two-col matrix
edges_mat <- matrix(NA, n - 1, 2)
colnames(edges_mat)  <- c("From", "To")

## * Connect the nodes
## ** Intitial Node
edges_mat[1, ] <- 1:2
degree_vec[1:2] <- 1

## ** Loop over List
# For each node, add it with probability proportional to degree:

for (i in 2:(n - 1)) {
  degrees <- degree_vec[1:i]

  edges_mat[i, 1] <- i + 1
  con <- sample(1:i, 1, prob = degree_vec[1:i]^-2.5)
  degree_vec[c(con,i+1)] <- degree_vec[c(con,i+1)] + 1

  edges_mat[i, 2] <- con
}

# Convert the list of edges to an adjacnecy matrix
edges_to_adjacency <- function(edges,n) {
  adj <- matrix(0,n,n)
  for (i in 1:nrow(edges)) {
    # From Column to Row (opposite of igraph)
      adj[edges[i,2],edges[i,1]]  <- 1  + adj[edges[i,2], edges[i,1]]
  }
#  adj <- adj + t(adj)   # Presume it's bi directional
  return(adj)
}


## Create Adjacency Matrix
adj_mat <- edges_to_adjacency(edges_mat, n)

## Generate a series of plots
## The qgraph object is a function that generates many plots
# animation::saveGIF(
#   print(
#     qgraph.animate(adj_mat, filetype = 'png',
#                 color = "blue", labels = FALSE,
#                 constraint = 30, sleep = 0.01,
#                 title = "Growth of Scale Free Network", smooth = TRUE)
#     ),
# movie.name = "animation.gif", clean = FALSE)


Edges <- as_tibble(edges_mat)
colnames(Edges) <- c("Source", "Target")
# GGPlot (# Because of the way that I made the graph, the nodes come from the edges, hence, every node must be connected.)
G <- igraph::graph_from_data_frame(d = Edges, directed = FALSE)
laytg <- igraph::layout_as_tree(G, root = 1)
laytg <- igraph::layout.auto(G)
colnames(laytg) <- c("xval", "yval")
laytg <- as_tibble(laytg)
laytg$node <- vertex_attr(G)[[1]]


ne <- nrow(Edges)
ys <- xe <- ye <- xs <- vector(length = ne)
for (i in seq_len(length(xs))) {
  xs[i] <- laytg$xval[laytg$node==Edges$Source[i]]
  ys[i] <- laytg$yval[laytg$node==Edges$Source[i]]
  xe[i] <- laytg$xval[laytg$node==Edges$Target[i]]
  ye[i] <- laytg$yval[laytg$node==Edges$Target[i]]
}

starts    <- data.frame("xval" = xs, "yval" = ys, edgenum = 1:ne) # TODO Make a factor
ends      <- data.frame("xval" = xe, "yval" = ye, edgenum = 1:ne)
Edges_val <- as_tibble(rbind(starts,ends))



#library(ggrepel)

edgesdf <- rbind(starts, ends)
edgesdf <- edgesdf[edgesdf$edgenum<=4, ]
laytgdf <- laytg[1:4, ]

p <-  ggplot(edgesdf, aes(x = xval, y = yval)) +
  geom_line(aes(group = edgenum), lty = 3, col = "darkgrey", size = 0.3) +
  geom_point(data = laytgdf, aes(x = xval, y = yval, col = node), size = 4) +
  labs(x = "", y = "") +
#  geom_label_repel(data = laytgdf, aes(x = xval, y = yval, label = node, col = node), size = 1.5, nudge_x = 0, nudge_y = 0) +
  guides(col = FALSE) +
  theme_classic() +
  theme(axis.line = element_blank(),  # https://stackoverflow.com/a/6542792/12843551
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.text.x=element_blank())
p



theplot <- function(i) {
  
  
edgesdf <- rbind(starts, ends)
edgesdf <- edgesdf[edgesdf$edgenum<=i, ]
laytgdf <- laytg[1:i, ]

p <-  ggplot(edgesdf, aes(x = xval, y = yval)) +
  geom_line(aes(group = edgenum), lty = 3, col = "darkgrey", size = 0.3) +
  geom_point(data = laytgdf, aes(x = xval, y = yval, col = node), size = 2) +
  labs(x = "", y = "") +
#  geom_label_repel(data = laytgdf, aes(x = xval, y = yval, label = node, col = node), size = 1.5, nudge_x = 0, nudge_y = 0) +
  guides(col = FALSE) +
  theme_classic() +
  theme(axis.line = element_blank(),  # https://stackoverflow.com/a/6542792/12843551
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.text.x=element_blank())
filename = paste0("/home/ryan/Downloads/graphs/00000", sprintf("%03d", i), ".png")

 ggsave(filename, plot = p, width = 10, height = 10, units = 'cm')
 
p



  
}

theplot(3)

for(i in 1:n) {
  theplot(i)
}


