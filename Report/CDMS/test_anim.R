# and now we create the (i)graph - undirected and using Fruchterman-
# Reingold for positioning. Looks like no big deal but took about 8 hours
# on my notebook for the big animation.
g <- igraph::sample_pa(n = 100, power = 2.5, m = 2)
layout_g <- layout.fruchterman.reingold(g)

# this function creates a PNG with the graph for every day
create_graph_png <- function(i) {
  
  V(gx)$color="black"
  V(gx)$frame.color="black"
  
  col_driver <- hsv(0,1,1,alpha=.5)
  V(gx)[which(nodes$type == "d")]$color=col_driver
  V(gx)[which(nodes$type == "d")]$frame.color=col_driver
  
  col_passenger <- hsv(0.66,1,1,alpha=.5)
  V(gx)[which(nodes$type == "p")]$color=col_passenger
  V(gx)[which(nodes$type == "p")]$frame.color=col_passenger
  
  col_hybrid <- hsv(.7,1,1,alpha=.5)
  V(gx)[which(nodes$type == "pd" | nodes$type == "dp")]$color=col_hybrid
  V(gx)[which(nodes$type == "pd" | nodes$type == "dp")]
  $frame.color=col_hybrid
  
  # the size of a node grows with the number of attached edges
  V(gx)$size <- degree(gx)^(1/3)
  
  # takes care of the new ones being emphasized by bright color
  # and a changing size
  newones <- hsv(.36,1,1,alpha=.5)
  
  V(gx)[which(abs(as.numeric(nodes$date - max_date)) < 5)]$color=newones
  V(gx)[which(abs(as.numeric(nodes$date - max_date)) < 5)]
  $frame.color=newones
  V(gx)[which(abs(as.numeric(nodes$date - max_date)) < 3)]$size=1.6
  V(gx)[which(abs(as.numeric(nodes$date - max_date)) < 1)]$size=2.5
  #V(gx)[which(abs(as.numeric(nodes$date - max_date)) < 3)]$size=3
  #V(gx)[which(abs(as.numeric(nodes$date - max_date)) < 2)]$size=5
  #V(gx)[which(abs(as.numeric(nodes$date - max_date)) < 1)]$size=3
  
  # get's rid of the not yet to be displayed nodes
  notyet <- hsv(1,1,1,alpha=0)
  
  V(gx)[which(degree(gx) < 1)]$frame.color=notyet 
  V(gx)[which(degree(gx) < 1)]$color=notyet 
  
  V(gx)[which(degree(gx) < 1)]$size=0
  
  bgcolor <- hsv(0.66,0.05,1)
  
  
  par(bg=bgcolor)
  dev.off()
}

# let's get it started!
start_date <- as.Date("2011-03-31")
for(i in 1:720) {
  create_graph_png(start_date + i, i)
}
