#!/usr/bin/R
#
## * Load Packages ------------------------------------------------------------
## For these examples, first generate a scale free network using preferential attachment:
library(qgraph)
library(animation)
library(mise)
mise()

## * Create Temp Directory ----------------------------------------------------
TEMP="/tmp/tmp.ncR5Lhklaskdfj1be"
dir.create(TEMP)
setwd(TEMP)

## * Set Variables ------------------------------------------------------------
n          <- 20
G          <- igraph::sample_pa(n, power = 1.1, m = 2)
Edges      <- data.frame(igraph::get.edgelist(G))
laytg      <- igraph::layout.auto(G)
laytg      <- as_tibble(laytg)
laytg$node <- 1:n

## ** Set Column Names ========================================================
colnames(Edges) <- c("Source", "Target")
colnames(laytg) <- c("xval", "yval", "node")

## * Create Edge Data Frame ---------------------------------------------------
## ** Create x and y co-ordinates =============================================
ne <- nrow(Edges)
ys <- xe <- ye <- xs <- vector(length = ne)
for (i in seq_len(length(xs))) {
  xs[i] <- laytg$xval[laytg$node==Edges$Source[i]]
  ys[i] <- laytg$yval[laytg$node==Edges$Source[i]]
  xe[i] <- laytg$xval[laytg$node==Edges$Target[i]]
  ye[i] <- laytg$yval[laytg$node==Edges$Target[i]]
}
## ** Merge Start and End DF ==================================================
starts    <- data.frame("xval" = xs, "yval" = ys, edgenum = 1:ne) # TODO Make a factor
ends      <- data.frame("xval" = xe, "yval" = ye, edgenum = 1:ne)
Edges_val <- as_tibble(rbind(starts,ends))
edgesdf   <- rbind(starts, ends) # TODO change everything to only be either Edges_val or edgesdf

## * make a Plot function -----------------------------------------------------
laytgdf <- laytg
laytgdf$node <- c(1, rep(0, (nrow(laytgdf)-1)))
# library(ggrepel)

make_plot <- function() {
  p <-  ggplot(edgesdf, aes(x = xval, y = yval)) +
    geom_line(aes(group = edgenum), lty = 3, col = "purple", size = 0.5) +
    geom_point(data = laytgdf, aes(x = xval, y = yval, col = node, size = node)) +
    labs(x = "", y = "") +
#    geom_label_repel(data = laytgdf, aes(x = xval, y = yval, label = round(node*100, 2), col = node), size = 1.5, nudge_x = 0, nudge_y = 0) +
    scale_color_gradient(high = "red", low = "blue")  +
    guides(col = FALSE) +
    theme_classic() +
    guides(size = FALSE) +
    theme(axis.line = element_blank(),  # https://stackoverflow.com/a/6542792/12843551
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.text.x=element_blank()) +
    scale_size_continuous(range = c(10, 18))
  return(p)
}

## * Make a function to move the node and make the plot -----------------------

 for (i in 1:50) {
    # Which nodes may our point travel to
      # This gets the rows of the edgelist with those sources
    rows_of_possible_destinations <- which(Edges$Source==which(laytgdf$node==1))
    length(rows_of_possible_destinations)
  
      # this is the corresponding targets, if there are no targets
      # Just teleport, this isn't quite the random surfer but it will
      # suffice for an animation for the moment
  
  if (length(rows_of_possible_destinations) !=0) { # Only when stuck
        possible_destinations <- Edges$Target[rows_of_possible_destinations]
    } else {
        possible_destinations <- 1:n
    }
  
    # Sample one of those points randomly (random walk)
    new_dest <- sample(possible_destinations, 1)
  
    # Set the location accordingly
    laytgdf$node <- rep(0, (nrow(laytgdf)))
    laytgdf$node[new_dest] <- 1
    
    p <- make_plot()
    # Save the plot
    filename = paste0("/home/ryan/Downloads/graphs/00000", sprintf("%03d", i), ".png")
    ggsave(filename, plot = p, width = 10, height = 10, units = 'cm')
 }
  

  

