library(igraph)

library(tidygraph)
library(ggraph)
library(RColorBrewer)
library(magick)
library(wesanderson)
# plotting a simple ring graph, all default parameters, except the layout
g <- make_ring(10)
g$layout <- layout_in_circle
plot(g)
tkplot(g)
rglplot(g)

# plotting a random graph, set the parameters in the command arguments
g <- barabasi.game(100)
plot(g, layout=layout_with_fr, vertex.size=4,
     vertex.label.dist=0.5, vertex.color="red", edge.arrow.size=0.5)

# plot a random graph, different color for each component
g <- sample_gnp(100, 1/100)
comps <- components(g)$membership
colbar <- rainbow(max(comps)+1)
V(g)$color <- colbar[comps+1]
plot(g, layout=layout_with_fr, vertex.size=5, vertex.label=NA)

# plot communities in a graph
g <- make_full_graph(5) %du% make_full_graph(5) %du% make_full_graph(5)
g <- add_edges(g, c(1,6, 1,11, 6,11))
com <- cluster_spinglass(g, spins=5)
V(g)$color <- com$membership+1
g <- set_graph_attr(g, "layout", layout_with_kk(g))
plot(g, vertex.label.dist=1.5)

# draw a bunch of trees, fix layout
igraph_options(plot.layout=layout_as_tree)
plot(make_tree(20, 2))
plot(make_tree(50, 3), vertex.size=3, vertex.label=NA)
plot(make_tree(50, 2, mode="undirected"), vertex.size=10,
       vertex.color="green")


graph <- as_tbl_graph(
  data.frame(
    from = sample(100, 150, TRUE),
    to = sample(100, 150, TRUE),
    weight = runif(150)
  )
)


ggraph(graph,layout = 'fr', weights = weight) + 
  geom_edge_hive2() +
  geom_node_point(size=5)


## dynamic graph

#this version of the script has been tested on igraph 1.0.1
#load libraries


#load the edges with time stamp

#generate the full graph
g <- barabasi.game(500)

plot(degree(g))
#generate a cool palette for the graph (darker colors = older nodes)
pal <- colorRampPalette(wes_palette('Darjeeling1',n = 5))

# pal <- colorRampPalette(brewer.pal(8,"Set3"))
#colors for the nodes are chosen from the very beginning
V(g)$color <- rev(pal(vcount(g)))#[as.numeric(V(g)$name)]

#time in the edges goes from 1 to 300. We kick off at time 3
ti <- 3
#remove edges which are not present
gt <- delete_edges(g,which(E(g)$time > ti))
#generate first layout using graphopt with normalized coordinates. This places the initially connected set of nodes in the middle. If you use fruchterman.reingold it will place that initial set in the outer ring.
layout.old <- norm_coords(layout.graphopt(gt), xmin = -1, xmax = 1, ymin = -1, ymax = 1)

# E(g)$time <- sample(1:30,length(E(g)),replace = TRUE)
E(g)$time <- sort(rep(1:30, length.out=length(E(g))))


#total time of the dynamics
total_time <- max(E(g)$time)
#This is the time interval for the animation. In this case is taken to be 1/10
#of the time (i.e. 10 snapshots) between adding two consecutive nodes
dt <- 0.1
#Output for each frame will be a png with HD size 1600x900 :)
png(file="materials/30_SNA/img/animation2/example%03d.png", width=800,height=450,res = 100)
#Time loop starts
for(time in seq(3,total_time,dt)){
  #remove edges which are not present
  gt <- delete_edges(g,which(E(g)$time > time))
  #with the new graph, we update the layout a little bit
  layout.new <- layout_with_fr(gt,coords=layout.old,niter=10,start.temp=0.05,grid="nogrid")
  #plot the new graph
  plot(gt,layout=layout.new,
       vertex.label="",vertex.size=1+2*log(degree(gt)),
       vertex.frame.color=V(g)$color,edge.width=1,
       asp=9/16,margin=-0.15, edge.arrow.size=0.25)
  #use the new layout in the next round
  #use the new layout in the next round
  layout.old <- layout.new
}
dev.off()

## build gif

## list file names and read in
imgs <- list.files('materials/30_SNA/img/animation2/', full.names = TRUE)
img_list <- lapply(imgs, image_read)

## join the images together
img_joined <- image_join(img_list)

## animate at 2 frames per second
img_animated <- image_animate(img_joined, fps = 10)

## view animated image
img_animated

## save to disk
image_write(image = img_animated,
            path = "materials/30_SNA/img/growing_network2.gif")
