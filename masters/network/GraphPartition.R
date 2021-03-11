#################################################
################ Graph Partition ######################
#################################################

##### Code For Lecture###################
g <- graph.formula(A-B-C-D, A-C, A-F-E, C-E)
plot(g)

# Edge Betweenness
bet = edge_betweenness(g, e = E(g), directed = FALSE);

# Graph Partitioning with betweenness
gred = g;
while(components(gred)$no < 2){
  bet = edge_betweenness(gred, e = E(gred), directed = FALSE);
  e1 = which.max(bet) # Find the edge with highest betweenness
  epol = E(gred); 
  gred = gred - epol[e1]; #Delete this edge from the graph
}
plot(gred)  # Plot the result

V(g)$color <- ifelse(components(gred)$membership ==1, "red", "yellow")
plot(g) #Plot the original graph with our partitioning

### Note: you can use the following command directly for graph partitioning with betweenness
gc <- cluster_edge_betweenness(g)
#####End of code for Lecture###################



#### Graph Partitions with Karate Club Data set #####
install.packages("sand") #a package containing the data sets for the book 'statistical analysis of network data with R'
library(sand)
data(karate) #Get the karate club data, saving with name 'karate'
V(karate)$Faction
# Graph Partitioning with betweenness
kc <- cluster_edge_betweenness(karate) #returns the one with highest modularity
membet <- kc$membership # The true labels
table(membet, V(karate)$Faction) #compare the result with the truth

### Hierarchical clustering with Euclidean distances
A = get.adjacency(karate) #get the adjacency matrix

graphdist <- function(A){
  n = dim(A)[1]; p = dim(A)[2]
  distmatrix = matrix(0, nrow = n, ncol = n);
  for(i in 1:p){
    for(j in 1:i){
      distmatrix[i,j] = sqrt(sum((A[i,-c(i,j)] - A[j,-c(i,j)])^2));
    }
  }
  return(distmatrix)
}

d = graphdist(A) #calculate the Euclidean distance
d = as.dist(d);
?hclust
par(mfrow = c(1,3))
clu = hclust(d, method = "complete")
plot(clu, xlab = "Karate club", main = "Cluster Dendrogram, Complete")

clu = hclust(d, method = "single")
plot(clu, xlab = "Karate club", main = "Cluster Dendrogram, Single")

clu = hclust(d, method = "average")
plot(clu, xlab = "Karate club", main = "Cluster Dendrogram, Average")

dev.off()

### Compare the result with truth, given 2 clusters######
clu = hclust(d, method = "complete")
memb <- cutree(clu, k = 2)
table(memb, V(karate)$Faction)

### Hierarchical clustering with modularity
kc <- fastgreedy.community(karate)
# we find that the method has declared there to be three communities.
length(kc) #number of communities
sizes(kc) #sizes of the communities
membership(kc) # membership for each node
plot(kc, karate) # plot the communities
table(membership(kc), V(karate)$Faction)

kc2 = cut_at(kc, no = 2); 
#Cut the modularity dendrogram as 2 groups. 
#Note this cut function is different from the one for hclust

table(kc2, V(karate)$Faction)
modularity(karate, kc2, weights = NULL)
modularity(karate, V(karate)$Faction, weights = NULL)

#Dendrogram
library(ape)
dendPlot(kc, mode="phylo")