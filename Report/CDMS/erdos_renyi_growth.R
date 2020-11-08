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
G          <- igraph::erdos.renyi.game(n, p.or.m = 0.4)
laytg      <- igraph::layout.auto(G)
laytg      <- as_tibble(laytg)
laytg$node <- 1:n

make_plot <- function(j) {
Edges      <- data.frame(igraph::get.edgelist(G))
Edges <- Edges[1:j,]

## ** Set Column Names ========================================================
colnames(Edges) <- c("Source", "Target")
colnames(laytg) <- c("xval", "yval", "node")

## * Create Edge Data Frame ---------------------------------------------------
## ** Create x and y co-ordinates =============================================
# ne <- nrow(Edges)
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
edgesdf$current <- rep(0, (nrow(edgesdf)))
edgesdf$current[j] <- 1
# library(ggrepel)

  p <-  ggplot(edgesdf, aes(x = xval, y = yval)) +
    geom_line(aes(group = edgenum, col = current, size = current), size = 0.5) +
    geom_point(data = laytgdf, aes(x = xval, y = yval ), size = 8, col = "royalblue") +
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

 for (i in 1:(2*300)) {
    p <- make_plot(i)
    # Save the plot
    filename = paste0("/home/ryan/Downloads/graphs/00000", sprintf("%03d", i), ".png")
    ggsave(filename, plot = p, width = 10, height = 10, units = 'cm')
 }
