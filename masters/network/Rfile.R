##Why do we choose R?
# 1. R is a free software, which is kind to all people
# 2. It is easy to develop your own algorithm on R
# 3. There are many packages on R, including the most up-to-date results. 
#   It is import for a hot topic as Network analysis

# To use R, 
# download R from the link in slides
# It is recommended to download Rstudio at the same time
# (Warning: Download Rstudio only does not work!)
#######################################################
#######################################################

###############  Lecture 1  ##################
# Install the igraph package
install.packages("igraph")
library(igraph)

############## Basic Operations on Graphs #####################
# Create a graph 
# By identifying the nodes and edges
g <- graph.formula(1-2, 1-3, 2-3, 2-4, 3-5, 4-5, 4-6, 4-7, 5-6, 6-7)

# check the properties of g
summary(g) 
# return 
# First line: 1. undirected or directed; 2. Named graph or not; 3. Weighted graph or not; 4. Bipartite or not
# First line (cont'd): 5. #Nodes 6. #Edges; 
# Second line: Attributes and the propertis

V(g) #vertices of g
E(g) #edges of g
plot(g) #visualization of the graph

# Assign properties for each node, say, names
V(g)$name <- c("A", "B", "C", "D", "E", "F", "G")
summary(g)
V(g)
plot(g)

# Create a directed graph
dg <- graph.formula(1-+2, 1-+3, 2++3)
summary(dg)
V(dg)
E(dg)
plot(dg)

# Three kinds of representations of graphs
E(g)
get.edgelist(g)
get.adjacency(g)
# You can also build graphs with graph.edgelist, graph.adjacency

########## End of Operations on the graphs #####################


##########################################################
########## Decorations on the graphs #####################

# We want to add attributes to nodes
# Take the directed graph dg as an example
V(dg)$name <- c("Sam", "Mary", "Tom")
plot(dg)

# Add genders (note this gender is an attribute name we defined)
V(dg)$gender <- c("M", "F", "M")

# We can also add some properties to make the figure more beautiful
V(dg)$color <- "red"
plot(dg)

# Drop the direction info in directed graphs
udg <- as.undirected(dg)
plot(udg)

##########################################################
########## Properties of the graphs #####################

is.simple(g) #whether it is a simple graph or not
plot(g)
mg <- g+edge(2,3)
plot(mg)
is.simple(mg)

is.connected(g)

neighbors(g, "E")
deg = degree(g)
degree(dg, mode = "in")
degree(dg, mode = "out")
deg.dist <- degree.distribution(g, cumulative = TRUE)
plot(x = 0:max(deg), y = 1-deg.dist, cex = 1.2, xlab = "Degree", col = 2)


# generate a simple undirected graph, each pair of node is connected with 
# a constant probability
g <- sample_gnp(20, 1/10)
plot(g)
is.connected(g)
clu <- components(g) #return the nodes in each component
clu
groups(clu)

comp <- decompose.graph(g) #return the components, each as a graph
# Select the giant component
giantIndex <- which.max(sapply(comp, vcount))
GiantComp <- comp[[giantIndex]]; 


###### Get subgraphs and Combine two graphs #############

h1 <- induced.subgraph(g, c(3:5, 8:10, 12:16)) #Build a subgraph
plot(h1)
V(h1)$name <- c(c(3:5, 8:10, 12:16))

h2 <- g - vertices(c(2,7)) #Build a subgraph
plot(h2)

h2 <- graph.formula(5-21, 6-3, 8-12, 21-7)
h <- graph.union(h1, h2)
par(mfrow = c(1,3))
plot(h1)
plot(h2)
plot(h)
dev.off()



#### Distances #####
g <- sample_gnp(20, 1/5)

distance_table(g)
distance_table(dg, directed = TRUE)

mean_distance(g, unconnected = TRUE) #the pairs that not connected are not considered here
distances(g, v = 3, to = 18)

shortest_paths(g, from = 3, to = 18, output = "epath")
all_shortest_paths(g, from = 3, to = 18)

E(g)$weight <- runif(length(E(g)))
summary(g)
E(g)$label <- round(E(g)$weight, 2);
plot(g, edge.width=E(g)$weight*3, edge.label = E(g)$label, layout=layout_as_tree)
all_shortest_paths(g, from = 3, to = 18)
distances(g, v = 3, to = 18)


#### Cliques #####
cliques(g, min = 2)
max_cliques(g, min = 2)
clique_num(g) #the size of the largest clique


### Some special graphs ############
#Empty graph
eg <- make_empty_graph(40)
plot(eg, vertex.size=10, vertex.label=NA)

#Full graph

fg <- make_full_graph(40)
plot(fg, vertex.size=10, vertex.label=NA)

#Star graph

st <- make_star(20)
plot(st, vertex.size=10, vertex.label=NA) 

# Tree graph

tr <- make_tree(40, children = 3, mode = "undirected")
plot(tr, vertex.size=10, vertex.label=NA) 

# Ring graph

rn <- make_ring(40)
plot(rn, vertex.size=10, vertex.label=NA)

########## End of Lecture 1 ######################

##Question: 
#Import Zachary club data
library(igraphdata)
data(karate) #import the karate club data
#Now you have an "igraph" object, named as "karate"

#Please give a summary of Zachary club data, answering the following questions
summary(karate) #note the weightage
E(karate)$weight
#Name?
#Directed? Undirected? Other fatures?
#How many nodes? What attributes do they have?
#How many edges? What attributes do they have?

#Connected? 
#Components?

#Degree? 
#Which two persons have highest degrees?

#Adjacency matrix? 
get.adjacency(karate)

# shortest path from node 3 to nodes 5, 9, 12, 25
shortest_paths(karate, from = 3, to = c(5, 9, 12, 25))
# Coresponding distances
distances(karate, v = 3, to = c(5, 9, 12, 25))
distances(karate, v = 3, to = c(5, 9, 12, 25), weight = NA)
shortest_paths(karate, from = 3, to = c(5, 9, 12, 25), weight = NA)

# all shortest paths? 
all_shortest_paths(karate, from = 3, to = 9)
all_shortest_paths(karate, from = 3, to = 9, weight = NA)

plot(karate, vertex.label = V(karate)$label, edge.label = E(karate)$weight, layout = layout_nicely)
######################################################
################### Lecture 2 ########################
######################################################


### Induced-Subgraph Sampling
par(mai=c(0,0,1,0), mfrow = c(1, 3))

g <- sample_gnp(50, 1/8)
V(g)$color <- "white"
lo = layout_nicely(g)
plot(g, layout = lo)

V(g)$prop <- "pop" #Now all of the nodes are not chosen
sv <- sample(1:50, 20) #Sample 20 nodes without replacement
V(g)[sv]$prop <- "sample" # Some of them are chosen as sample
V(g)$color <- ifelse(V(g)$prop == "sample", "red", "grey")
plot(g, layout = lo)

ev <- get.edges(g, E(g)); 
se <- V(g)[ev[,1]]$prop == "sample" & V(g)[ev[,2]]$prop == "sample"; 
E(g)$color <- ifelse(se, "red", "grey")
plot(g, layout = lo, vertex.size = 8)

dev.off()

### Incident-Subgraph Sampling
par(mai=c(0,0,1,0), mfrow = c(1, 3))

g <- sample_gnp(50, 1/8)
V(g)$color <- "grey"
lo = layout_nicely(g)
plot(g, layout = lo)

e <- get.edges(g, E(g));
se <- sample(1:length(E(g)), 30)
col_edges <- rep("grey", length(E(g)))
col_edges[se] <- "red"
E(g)$color <- col_edges
plot(g, layout = lo)

col_nodes <- rep("grey", length(V(g)));
col_nodes[e[se,1]] <- "red"
col_nodes[e[se,2]] <- "red"
V(g)$color <- col_nodes
plot(g, layout = lo)

dev.off()


### Snowball Sampling
par(mfrow = c(2, 2), oma = c(5,4,0,0) + 0.1, mar = c(0,0,1,1) + 0.1)

g <- sample_gnp(50, 1/10)
V(g)$color <- "grey"
lo = layout_nicely(g)
plot(g, layout = lo)

V(g)$prop <- "pop" #Now all of the nodes are not chosen
s1 <- c(1, 20, 40)
V(g)[s1]$prop <- "sample" # Some of them are chosen as sample
V(g)$color <- ifelse(V(g)$prop == "sample", "red", "grey")
plot(g, layout = lo)

ev <- get.edges(g, E(g)); 
se <- xor(V(g)[ev[,1]]$prop == "sample", V(g)[ev[,2]]$prop == "sample"); 
E(g)$color <- ifelse(se, "red", "grey")
plot(g, layout = lo)

for(ii in 1:length(s1)){
  s2 <- neighbors(g, s1[ii])
  V(g)[s2]$prop <- "sample"
}
V(g)$color <- ifelse(V(g)$prop == "sample", "red", "grey")
se <- V(g)[ev[,1]]$prop == "sample" & V(g)[ev[,2]]$prop == "sample"; 
E(g)$color <- ifelse(se, "red", "grey")
plot(g, layout = lo)

dev.off()



### Respondent Driven Sampling
par(mfrow = c(2, 2), oma = c(5,4,0,0) + 0.1, mar = c(0,0,1,1) + 0.1)

k = 3; # Consider the respondent driven sampling method with at most 3 offsprings
g <- sample_gnp(50, 1/5)
V(g)$color <- "grey"
lo = layout_nicely(g)
plot(g, layout = lo)

V(g)$prop <- "pop" #Now all of the nodes are not chosen
s1 <- 20 #select one node to start
V(g)[s1]$prop <- "sample" # Some of them are chosen as sample
V(g)$color <- ifelse(V(g)$prop == "sample", "red", "grey")
plot(g, layout = lo)

ev <- get.edges(g, E(g)); 
se <- xor(V(g)[ev[,1]]$prop == "sample", V(g)[ev[,2]]$prop == "sample"); 
if(sum(se) <= k){
  E(g)$color <- ifelse(se, "red", "grey")
  s2 <- neighbors(g, s1)
  V(g)[s2]$prop <- "sample"
}
if(sum(se) > k){
  nospring <- sample(1:sum(se), sum(se) - k);
  se[which(se)[nospring]] = FALSE;
  E(g)$color <- ifelse(se, "red", "grey")
  s2 <- union(as.vector(s1), as.vector(ev[se, ]));
  s2 <- setdiff(s2, s1);
  V(g)[s2]$prop <- "sample"
}
V(g)$color <- ifelse(V(g)$prop == "sample", "red", "grey")
plot(g, layout = lo)


for(ii in 1:length(s2)){
  cur.s = s2[ii];
  ev <- get.edges(g, E(g)); 
  se <- xor(ev[,1] == cur.s & V(g)[ev[,2]]$prop != "sample", ev[,2] == cur.s & V(g)[ev[,1]]$prop != "sample"); 
  if(sum(se) <= k){
    news <- neighbors(g, cur.s)
    V(g)[news]$prop <- "sample"
    E(g)$color <- ifelse(se|E(g)$color == "red", "red", "grey")
  }
  if(sum(se) > k){
    nospring <- sample(1:sum(se), sum(se) - k);
    se[which(se)[nospring]] = FALSE;
    E(g)$color <- ifelse(se|E(g)$color == "red", "red", "grey")
    news <- union(as.vector(cur.s), as.vector(ev[se, ]));
    news <- setdiff(news, cur.s);
    V(g)[news]$prop <- "sample"
  }
  V(g)$color <- ifelse(V(g)$prop == "sample", "red", "grey")
}
plot(g, layout = lo)

dev.off()

#################### #################### ####################
################## Lecture 3: Descriptive Statistics #########
#################### #################### ####################
g <- graph.formula(A-B-C-D, A-C, A-F-E, C-E, C-F, B-F)
plot(g)

#degree distribution
deg = degree(g)
deg.dist <- degree.distribution(g)
plot(x = 0:max(deg), y = deg.dist, cex = 1.2, xlab = "Degree", col = 2, type = "l")

# #degree distribution example 2
# data(karate)
# deg = degree(karate)
# deg.dist <- degree.distribution(karate)
# plot(x = 0:max(deg), y = deg.dist, cex = 1.2, xlab = "Degree", col = 2, type = "l")

#average degree
mean(deg)

#Density for the whole network
edge_density(g)

#diameter
diameter(g)
diameter(karate, weight = NA)

# Cliques: maximal cliques (in Lecture 1)
# k-cores:
coreness(g)
k = 2; 
kvertex = which(coreness(g) >= k)
kcore <- induced.subgraph(g, vids = kvertex)
plot(kcore)

# coreness(karate)
# k = 4; 
# kvertex = which(coreness(karate) >= k)
# kcore <- induced.subgraph(karate, vids = kvertex)
# plot(kcore)
# Vertex connectivity
cohesion(g)
vertex.connectivity(g, source = "B", target = "E")
edge.connectivity(g)
#################### #################### ####################
############# Lecture 4: Descriptive Statistics: Nodes #######
#################### #################### ####################
g <- make_star(5, mode = "undirected")
plot(g)

#Closeness
closeness(g)*(length(V(g)) - 1)
#Betweenness
betweenness(g, directed = FALSE)
#Eigenvector Centrality
eigen_centrality(g)$vector


g <- make_tree(n = 40, children = 3, mode = "undirected")
plot(g)
#Closeness
closeness(g)*(length(V(g)) - 1)
#Betweenness
betweenness(g, directed = FALSE)
#Eigenvector Centrality
eigen_centrality(g)$vector

g <- graph.formula(A-B-C-D-F-J, A-E-C-D-G-B-A, A-H, A-I, F-K)
V(g)$color <- c("cyan", "red", "blue", "yellow", "green", "green", "green", "green", "green", "green", "green")
set.seed(2020)
lo = layout_nicely(g)
plot(g, layout = lo)
#Closeness
closeness(g)*(length(V(g)) - 1)
#Betweenness
betweenness(g, directed = FALSE)
#Eigenvector Centrality
eigen_centrality(g)$vector

g <- graph.formula(A-B-C-D, A-C, A-F-E, C-E)
plot(g)
#Density for each node
edge_density(induced_subgraph(g, c("A", neighbors(g, "A")$name)))
edge_density(induced_subgraph(g, c("B", neighbors(g, "B")$name)))
edge_density(induced_subgraph(g, c("C", neighbors(g, "C")$name)))
edge_density(induced_subgraph(g, c("F", neighbors(g, "F")$name)))

# Clustering coefficient for each node
transitivity(g, type = "local")
# For the whole graph
transitivity(g, type = "global")

# Edge Betweenness
bet = edge_betweenness(g, e = E(g), directed = FALSE);

# Graph Partitioning with betweenness
epol = E(g); gred = g;
while(components(gred)$no < 2){
  e1 = which.max(bet) # Find the edge with highest betweenness
  bet[e1] = 0; #Delete this edge from the betweenness vector
  gred = gred - epol[e1]; #Delete this edge from the graph
}
plot(gred)  # Plot the result

V(g)$color <- ifelse(components(gred)$membership ==1, "red", "yellow")
plot(g) #Plot the original graph with our partitioning

install.packages("sand") #a package containing the data sets for the book 'statistical analysis of network data with R'
library(sand)
data(karate) #Get the karate club data, saving with name 'karate'


### Hierarchical clustering with Euclidean distances
A = get.adjacency(karate) #get the adjacency matrix
d = dist(A) #calculate the Euclidean distance

par(mfrow = c(1,3))
clu = hclust(d, method = "complete")
plot(clu, xlab = "Karate club", main = "Cluster Dendrogram, Complete")

clu = hclust(d, method = "single")
plot(clu, xlab = "Karate club", main = "Cluster Dendrogram, Single")

clu = hclust(d, method = "average")
plot(clu, xlab = "Karate club", main = "Cluster Dendrogram, Average")

dev.off()

### Hierarchical clustering with modularity
kc <- fastgreedy.community(karate) #the result is communities object.
# we find that the method has declared there to be three communities.
length(kc) #number of communities
sizes(kc) #sizes of the communities
membership(kc) # membership for each node
plot(kc, karate) # plot the communities

kc2 = cut_at(kc, no = 2);
#Dendrogram
library(ape)
dendPlot(kc, mode="phylo")
