library(igraph)
library(data.table)

poll_file <- "/Users/jinwee/github/Return/data/network/polblogs.gml"

g <- read.graph(poll_file, format = c("gml"))

vertex_names <- as.character(c(1:length(V(g))))
V(g)$name <- vertex_names
V(g)$color <- "darkorange"
E(g)$color <- "grey"

?knitr::kable()

kableExtra::kable_styling()

?diameter
any_multiple(g)
?mean_distance

#Summary stats
#First line: 1. undirected or directed; 2. Named graph or not; 3. Weighted graph or not; 4. Bipartite or no
#IGRAPH f5e43cf D--- 1490 19090 --
summary(g)
V(g)
E(g)
is_directed(g)
is_simple(g) # FALSE not a simple graph
is_connected(g) # also not a connected graph
#get.edgelist(g)
#get.adjacency(g)
adjacency_mat <- get.adjacency(g)

plot(g, vertex.label=NA, vertex.size = 4)
?igraph::plot

?kable
?format

#Q1
g_s <- simplify(g)
g_s_comp_member <- components(g_s)$membership
#g_s_comp_member[g_s_comp_member==1]

#1a
#Density for the whole network
?edge_density
edge_density(g_s)
#diameter
diameter(g_s)
#Average distance
mean_distance(g_s, unconnected = TRUE)

#1b
summary(g_s)
g_s_deg <- degree(g_s, mode = "all")
g_s_deg_dist <- degree.distribution(g_s, mode = "all")
plot(x = 0:max(g_s_deg), y = g_s_deg_dist, cex = 1.2, xlab = "Degree", col = 2, type = "l")
plot(x = log(0:max(g_s_deg)), y = log(g_s_deg_dist), cex = 1.2, xlab = "Degree", col = 2, type = "l")

g_s_deg_dist_cul <- degree.distribution(g_s, cumulative = TRUE)
plot(x = 0:max(g_s_deg), y = g_s_deg_dist_cul, cex = 1.2, xlab = "Degree", col = 2, type = "l'")
plot(x = log(0:max(g_s_deg)), y = log(g_s_deg_dist_cul), cex = 1.2, xlab = "Degree", col = 2, type = "l'")



g_s_deg <- degree(g_s, mode = "all")
g_s_deg_add <- c(0:max(g_s_deg)) + 0.5 # adding 0.5 to shift from 0
cul_deg_dist_dt <- data.table("log_deg"= log(g_s_deg_add), "log_ccdf" = log(g_s_deg_dist_cul))

# ahhh ok ok 
m <- lm(formula = log_ccdf  ~ log_deg, data = cul_deg_dist_dt)
q1_a  <- m$coefficients["log_deg"] * -1


incident_sample_eids <- E(g_s)[sample(ecount(g_s), 3)]
incident_sample_graph <- subgraph.edges(g_s, eids = incident_sample_eids)
plot(incident_sample_graph)



#1c
g_s_in_deg <- degree(g_s, mode = "in")
g_s_out_deg <- degree(g_s, mode = "out")
g_s_between <- betweenness(g_s, directed = TRUE)
g_s_closeness <- closeness(g_s) 

top_in_deg <- head(g_s_in_deg[order(g_s_in_deg, decreasing = TRUE)])
top_out_deg <- head(g_s_out_deg[order(g_s_out_deg, decreasing = TRUE)])
top_between <- head(g_s_between[order(g_s_between, decreasing = TRUE)])
top_closeness <- head(g_s_closeness[order(g_s_closeness, decreasing = TRUE)])

head(g_s_closeness[order(g_s_closeness, decreasing = TRUE)], n=30)
?closeness

#neighbors(g, "123", mode = "all") # all neighbors of node 123
degree(g_s, 1051, mode = "in")

V(g_s)$label[c(1051, 855,1)]
vertex_attr(g_s, "label", index = c(1051, 855))

# centrality_dt <- data.table("Node rank" = c(1:6),
#                             "In-degree" = names(top_in_deg),
#                             "Out-degree" = names(top_out_deg),
#                             "Closeness" = names(top_closeness),
#                             "Betweenness" = names(top_between))

# degree(g_s, "855", mode = "out")

data.table("Node rank" = c(1:6),
           "In-degree" =  vertex_attr(g_s, "label", index = names(top_in_deg)),
           "Out-degree" = vertex_attr(g_s, "label", index = names(top_out_deg)),
           "Closeness" = vertex_attr(g_s, "label", index = names(top_between)),
           "Betweenness" = vertex_attr(g_s, "label", index = names(top_closeness)))


V(g)$label
f




#2 
q2_graph <- g_s
q2_adj <- as.matrix(get.adjacency(q2_graph))

# 2.1
h_score_igraph <- hub_score(q2_graph)$vector
top_h_score <- head(h_score_igraph[order(h_score_igraph, decreasing = TRUE)])

h_score <- eigen(q2_adj %*% t(q2_adj))$vectors[,1]
h_score_scale <- abs(h_score) / max(abs(h_score))
names(h_score_scale) <- vertex_names
h_score_scale_sort <- h_score_scale[order(h_score_scale, decreasing = TRUE)]

head(h_score_scale_sort)

# 2.2 
a_score_igraph <- authority_score(q2_graph)$vector
top_a_score <- head(a_score_igraph[order(a_score_igraph, decreasing = TRUE)])



a_score <- eigen(t(q2_adj) %*% q2_adj)$vectors[,1]
a_score_scale <- abs(a_score) / max(abs(a_score))
names(a_score_scale) <- vertex_names
a_score_scale_sort <- a_score_scale[order(a_score_scale, decreasing = TRUE)]

head(a_score_igraph_sort)
head(a_score_scale_sort)

#2.3 and 2.4
pr_99 <- page.rank(q2_graph, damping = 0.99)$vector
pr_85 <- page.rank(q2_graph, damping = 0.85)$vector

top_pr_99 <- head(pr_99[order(pr_99, decreasing = TRUE)])
top_pr_85 <- head(pr_85[order(pr_85, decreasing = TRUE)])



data.table("Node rank" = c(1:6),
           "Hub Score" =  vertex_attr(g_s, "label", index = names(top_h_score)),
           "Authority Score" = vertex_attr(g_s, "label", index = names(top_a_score)),
           "PageRank (0.99)" = vertex_attr(g_s, "label", index = names(top_pr_99)),
           "PageRank (0.85)" = vertex_attr(g_s, "label", index = names(top_pr_85)))




#3 
g_u <- as.undirected(g_s)
#resetting edge edge and vertex colors
V(g_u)$color <- "darkorange"
E(g_u)$color <- "grey"




#3a)

g_u_deg_dist <- degree.distribution(g_u, mode = "all")
plot(x = c(0:max(g_u_deg)), y = g_u_deg_dist, cex = 1.2, xlab = "Degree", col = 2, type = "l")
plot(x = log(c(0:max(g_u_deg))), y = log(g_u_deg_dist), cex = 1.2, xlab = "Degree", col = 2, type = "l")

g_u_deg_dist_cul <- degree.distribution(g_u, mode = "all", cumulative = TRUE)
#plot(x = c(0:max(g_u_deg)), y = 1-g_u_deg_dist_cul, cex = 1.2, xlab = "Degree", col = 2, type = "l")
plot(x = g_u_deg_add, y = g_u_deg_dist_cul, cex = 1.2, xlab = "Degree", col = 2, type = "l")
plot(x = log(c(0:max(g_u_deg))), y = log(g_u_deg_dist_cul), cex = 1.2, xlab = "Degree", col = 2, type = "l")
plot(x = log(g_u_deg_add), y = log(g_u_deg_dist_cul), cex = 1.2, xlab = "Degree", col = 2, type = "l")




#3b
is.connected(g_u)
g_u_comps <- decompose.graph(g_u) #return the components, each as a graph
# Select the giant component
giant_comp_idx <- which.max(sapply(g_u_comps, function(x) vcount(x)))
g_u_giant <- g_u_comps[[giant_comp_idx]]

#plot(g_u, vertex.label=NA, vertex.size = 4)
plot(g_u_giant, vertex.label=NA, vertex.size = 4)

length(V(g_u_giant))
length(V(g))

#3c
g_u_coreness <- coreness(g_u)
max_k <- max(g_u_coreness)
names(g_u_coreness[g_u_coreness >= max_k])

k_vertexes <- which(g_u_coreness >= max_k)

degree(g_u, v = as.character(k_vertexes))
length(k_vertexes)
kcore <- induced.subgraph(g, vids = names(g_u_coreness[g_u_coreness >= max_k]))
plot(kcore, vertex.label=NA, vertex.size = 4)



## test
# head(degree(g_u_giant)[order(degree(g_u_giant), decreasing = TRUE)])
# neighbors(g_u_giant, "155")






# 4
#4a Induced subgraph sampling
q4_graph <- g_u
E(q4_graph)$id <- seq_len(ecount(q4_graph))
E(q4_graph)$color <- "grey"

K <- 200
set.seed(12345)
induced_sample_vids <-  as.character(sample(length(V(q4_graph)), K))
induced_sample_graph <- induced.subgraph(q4_graph, vids = induced_sample_vids)

# V(q4a_graph)$color[E(induced_sample_graph)$id] <- "red"
# E(q4a_graph)$color[E(induced_sample_graph)$id] <- "red"
# plot(q4a_graph, vertex.label=NA, vertex.size = 4)

plot(induced_sample_graph, vertex.label=NA, vertex.size = 4)
transitivity(induced_sample_graph, type = "global")


#4b
set.seed(12345)
edge_num <- 125
incident_sample_eids <- E(q4_graph)[sample(length(E(q4_graph)), edge_num)]
incident_sample_graph <- subgraph.edges(q4_graph, eids = incident_sample_eids)
length(V(incident_sample_graph))
plot(incident_sample_graph,  vertex.label=NA, vertex.size = 4)
transitivity(incident_sample_graph, type = "global")


#4c
set.seed(12345)
snowball_v_set <- sample(V(q4_graph),1)
snowball_iterations <- 0
while(length(snowball_v_set) <= 200){
  snowball_nbs <- adjacent_vertices(q4_graph, snowball_v_set)
  
  for (i in 1:length(snowball_nbs)){
    snowball_v_set <- union(snowball_v_set, snowball_nbs[[i]])
  }
  
  
  snowball_iterations <- snowball_iterations + 1
}

snowball_sample_graph <- induced_subgraph(q4_graph, vids = snowball_v_set)
#length(V(snowball_sample_graph))
plot(snowball_sample_graph, vertex.label = NA, vertex.size = 4)
edge_density(snowball_sample_graph)
transitivity(snowball_sample_graph, type = "global")

#4d
res_sample_graph <- g_s # use simplified graph
set.seed(123456)
res_seed <- sample(v_set,1)
k <- 3
V(res_sample_graph)$color <- "grey"
E(res_sample_graph)$color <- "grey"
V(res_sample_graph)[res_seed]$color <- "red"
res_vec <- c(res_seed)
total_res_vec <- c(res_seed)
n_res <- length(which(V(res_sample_graph)$color == "red"))
edge_vec <- c()

while (n_res <= 200){
  new_res_vec <- c()
  total_res_vec <- c()
  for (res in res_vec){
    res_nbs <- neighbors(res_sample_graph, res, mode = "out")
    if (length(res_nbs) > k){
      res_nbs <- sample(res_nbs, k)
    }
    for (nb in res_nbs){
      V(res_sample_graph)[nb]$color <- "red"
      E(res_sample_graph)[res %->% nb]$color <- "red"
      
      if (!nb %in% total_res_vec) {
        total_res_vec <- c(total_res_vec, nb)
        new_res_vec <- c(new_res_vec, nb)
      }
    }
  }
  res_vec <- new_res_vec
  n_res <- length(which(V(res_sample_graph)$color == "red"))
}

length(which(V(res_sample_graph)$color == "red"))
length(which(E(res_sample_graph)$color == "red"))

res_sample_graph_sub <- delete.edges(res_sample_graph, E(res_sample_graph)[which(E(res_sample_graph)$color == "grey")])
res_sample_graph_sub <- delete.vertices(res_sample_graph_sub, V(res_sample_graph_sub)[which(V(res_sample_graph_sub)$color == "grey")])

E(res_sample_graph_sub)$color <- "grey"
plot(res_sample_graph_sub,vertex.label = NA, vertex.size = 4)
# 
# length(which(V(res_sample_graph)$color == "red"))
# length(which(E(res_sample_graph)$color == "red"))
# E(res_sample_graph)[E(res_sample_graph)$color == "red"]
# 
# igraph
# graph_from
# 
# plot(res_sample_graph, vertex.label = NA, vertex.size = 4)
# 
# ?inc()
# E(q4_graph)[res %->% nb]$color
# ?neighbors
# 
# intersect(unlist(incident_edges(q4_graph, "1084"))[[1]], incident_edges(q4_graph, "907")[[1]])
# 
# incident_edges(q4_graph, "1081")
# E(q4_graph)["1084" %<-% "907"]
# E(q4_graph)[c(907) %--% c(1081)]
# 
# 
# get.edge.ids(q4_graph, c("907"))
# 
# ?get.edge
# sample(c(1,2),3)
# 
# neighbors(q4_graph, "1084")
# 
# which(V(res_sample_graph)$color == "red")
# 
# is.directed(q4_graph)
# adjacent_vertices(q4_graph)


#q5
#g_u_giant
table(V(g_u_giant)$value)

#q5.1 betweeness 
betw_partition <- g_u_giant

V(g_u_giant)$name
#removed stuff
# betw_partition_check <- g_u_giant
# V(betw_partition_check)[c("794","820","821","1183")]$color <- "red"
# E(betw_partition_check)$color <- "grey"
# E(betw_partition_check)["600"%--%"1183"]$color <- "red"
# plot(betw_partition_check, vertex.label = NA, vertex.size = 4)
?edge_betweenness
head(edge_betweenness(betw_partition, directed = FALSE)[order(edge_betweenness(betw_partition, directed = FALSE), decreasing = TRUE)])

while(components(betw_partition)$no < 2){
  print(1)
  edges <- E(betw_partition)
  bet = edge_betweenness(betw_partition, e = edges, directed = FALSE);
  max_bet = which.max(bet)
  print(bet[max_bet])
  print(edges[max_bet])
  betw_partition = betw_partition - edges[max_bet];
}


betw_part_1 <- decompose.graph(betw_partition)[[1]]
betw_part_2 <- decompose.graph(betw_partition)[[2]]
table(V(betw_part_1)$value)
table(V(betw_part_2)$value)
V(betw_partition)$color <- "seagreen"
V(betw_partition)$color[which(V(betw_partition)$name %in% V(betw_part_2)$name)] <- "dodgerblue"
plot(betw_partition, vertex.label = NA, vertex.size = 4)



which(V(betw_partition)$name %in% V(betw_part_2)$name)


which(V(betw_partition) %in% V(betw_part_2))
table(V(betw_partition)$color)
f


# ?edge_betweenness()
# incident_edges(g_u_giant, v = "1183")
# E(g_u_giant)["600"%--%"1183"]
# edge_betweenness(g_u_giant, e = E(g_u_giant)["600"%--%"1183"], directed = FALSE)
# which.max(edge_betweenness(g_u_giant, directed = FALSE))
# edge_betweenness(g_u_giant, directed = FALSE)[11293]

#q5.2 euclidean distance
er_test <- sample_gnp(20, 1/10)
g_u_giant_adj <- get.adjacency(g_u_giant) 
#g_u_giant_adj <- get.adjacency(er_test) 

p <- dim(g_u_giant_adj)[1]
n <- dim(g_u_giant_adj)[2]
distmatrix <- matrix(0, nrow = n, ncol = n);
dummy <- function(i,j,mat,A) {
  mat[i,j] <- sqrt(sum((A[i,-c(i,j)] - A[j,-c(i,j)])^2));
}

dist_vec <- sapply(1:p, function(i) sapply(1:i, function(j) dummy(i, j, distmatrix,g_u_giant_adj)))
dist_mat <- plyr::ldply(dist_vec, rbind)
dist_mat[is.na(dist_mat)] <- 0
dist_mat_new <- as.dist(dist_mat)

g_u_giant_adj = get.adjacency(g_u_giant) #get the adjacency matrix

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

dist_mat <- graphdist(g_u_giant_adj) #calculate the Euclidean distance
d_mat <- as.dist(dist_mat)

complete_partition <- hclust(d_mat, method = "complete") %>%
  cutree(k=2)
table(V(g_u_giant)$value)
table(complete_partition-1)
table(complete_partition-1, V(g_u_giant)$value)

average_partition <- hclust(d_mat, method = "average") %>%
  cutree(k=2)
single_partition <- hclust(d_mat, method = "single") %>%
  cutree(k=2)

hier_graph <- g_u_giant
V(hier_graph)$complete <- complete_partition
V(hier_graph)$average <- average_partition
V(hier_graph)$single <- single_partition

V(hier_graph)$color[V(hier_graph)$complete == 1] <- "dodgerblue"
V(hier_graph)$color[V(hier_graph)$complete == 2] <- "seagreen"
plot(hier_graph,  vertex.label = NA, vertex.size = 4)

table(V(hier_graph)$complete)
table(V(hier_graph)$average)
table(V(hier_graph)$single)

# head(dist_mat)[,1]
# save(dist_mat, file = "/Users/jinwee/github/Return/masters/network/dist_mat.RData")

#q5.3 modularity
### Hierarchical clustering with modularity
true_graph <- g_u_giant
q5_mod_graph <- g_u_giant
mod_community <- fastgreedy.community(q5_mod_graph)
length(mod_community)
sizes(mod_community)
mod_community_2 <- cut_at(mod_community, no = 2)
V(q5_mod_graph)$mod_value <- sapply(mod_community_2, function(x) ifelse(x == 2, 0, 1)) # renaming assigned communities

modularity(q5_mod_graph, V(q5_mod_graph)$mod_value+1)# adding 1 to shift membership
modularity(q5_mod_graph, V(q5_mod_graph)$value+1) # adding 1 to shift membership

#plotting membership
V(true_graph)$color[V(true_graph)$value == 0] <- "dodgerblue"
V(true_graph)$color[V(true_graph)$value == 1] <- "seagreen"
plot(true_graph,  vertex.label = NA, vertex.size = 4)
plot(g_u_giant,  vertex.label = NA, vertex.size = 4)



get_err_rate <- function(g, value){
  vertex_set <- V(g_u_giant)
  n_vertex <- length(vertex_set)
  if (value == "mod"){
      diff <- sum(abs(V(g_u_giant)$value - V(g_u_giant)$mod_value))
    }else{
      diff <- sum(abs(V(g_u_giant)$value - V(g_u_giant)$kmeans_value))
    }
  
  err <- diff/n_vertex
  return(min(err, 1 - err))
}

get_err_rate(mod_graph, value = "mod")

table(V(g_u_giant)$value)
mod_lab_mat <- as.matrix(table(V(mod_graph)$mod_value, V(g_u_giant)$value))
(mod_lab_mat[1,2] + mod_lab_mat[2,1]) / vcount(mod_graph)

V(mod_graph)$color[V(mod_graph)$mod_value == 0] <- "seagreen"
V(mod_graph)$color[V(mod_graph)$mod_value == 1] <- "dodgerblue"
plot(mod_graph,  vertex.label = NA, vertex.size = 4)



?diag
#SBM
kmeans_graph <- g_u_giant
g_u_giant_adj <- get.adjacency(g_u_giant)
v1 <- eigen(g_u_giant_adj)$vectors[,1]
v2 <- eigen(g_u_giant_adj)$vectors[,2]
r <- v2/v1
kmeans <- kmeans(r, centers =2)
kmeans_commumity <- sapply(kmeans$cluster, function(x) ifelse(x == 2, 0, 1))
V(kmeans_graph)$kmeans_value <- kmeans_commumity

kmeans_lab_mat <- as.matrix(table(V(kmeans_graph)$kmeans_value, V(kmeans_graph)$value))
kmeans_err_stat <- (kmeans_lab_mat[1,2] + kmeans_lab_mat[2,1]) / vcount(kmeans_graph)
kmeans_err_rate <- min(1-kmeans_err_stat , kmeans_err_stat)
 
get_err_rate(kmeans_graph, value = "kmeans")





#SBM estimation
# only need to estimate b11, b22, b12
comm_0_vertices <- V(g_u_giant)[V(g_u_giant)$value == 0]
comm_0 <- induced_subgraph(g_u_giant, vids = comm_0_vertices)
comm_0_edges <- E(comm_0)
comm_1_vertices <- V(g_u_giant)[V(g_u_giant)$value == 1]
comm_1 <- induced_subgraph(g_u_giant, vids = comm_1_vertices)
comm_1_edges <- E(comm_1)

b11 <- length(E(comm_1)) / choose(length(V(comm_1)),2)
b00 <- length(E(comm_0)) / choose(length(V(comm_0)),2)

e12 <- length(E(g_u_giant)) - length(comm_1_edges) - length(comm_0_edges)
b12 <- e12 / (length(comm_0_vertices) * length(comm_1_vertices))

#nice
V(g_u_giant)$color <-  sapply(V(g_u_giant)$value, function(x) ifelse(x == 0, "blue", "green"))

plot(g_u_giant, vertex.label = NA, vertex.size = 4)
plot(comm_0, vertex.label = NA, vertex.size = 4)
plot(comm_1, vertex.label = NA, vertex.size = 4)


     