#################################################
################ WWW network ######################
#################################################

#Create the NUS network in class
nus = graph.formula(A:B:C:D-+NTU, D:E:wiki:CNA-+NUS, E:F:G-+wiki, H:I:J:K:L-+CNA)
lo = layout.davidson.harel(nus)
plot(nus, layout = lo)
A = get.adjacency(nus); ##Get the adjacency matrix
A = as.matrix(A)

#Check hub and authority score we calculated in our class
hub = rep(1, length(V(nus)))
auth = t(A)%*%hub;
hub = A%*%auth;
auth = t(A)%*%hub

##Calculate the hub score and authority score with eigenvectors
hub = eigen(A%*%t(A))$vectors[,1] ## In vectors, the first column is the hub score
hub = abs(hub)/max(abs(hub)) ##Scale the hub vector as the scaling way in R
names(hub) = V(nus)$name
hub

auth = eigen(t(A)%*%A)$vectors[,1] ## In vectors, the first column is the hub score
auth = abs(auth)/max(abs(auth)) ##Scale the hub vector as the scaling way in R
names(auth) = V(nus)$name
auth

## Apply the ready-to-use functions directly
hub.score(nus)$vector
authority.score(nus)$vector

#### PageRank Method #########
g = graph.formula(A-+B:C, B-+D:E, C-+F:G, D:E-+H, D:E:F:G:H-+A) ##Create the example network
plot(g)

A = get.adjacency(g); 
A = as.matrix(A);
deg = degree(g, mode = "out")  ## Get the out-degree vector

P = diag(1./deg)%*%A; ##Get the transition probability matrix via matrix calculation
r = rep(1/length(V(g)), length(V(g))); ## The starting distribution

r%*%P   ## The distribution after 1 step
r%*%P%*%P   ## The distribution after 2 steps
#Note: In R, you cannot use r%*%P^2, since P^2 means sqaure of each element of P, not P%*%
page.rank(g, damping = 1) ## Classical PageRank
page.rank(g, damping = 0.9) ## Scaled PageRank, with the scale parameter 0.9 (parameter s in class)


#Consider the network NUS
A = get.adjacency(nus); 
A = as.matrix(A);
deg = degree(nus, mode = "out")  ## Get the out-degree vector

P = diag(1./deg)%*%A; ##Get the transition probability matrix via matrix calculation
P[is.na(P)] = 0;
r = rep(1/length(V(nus)), length(V(nus))); ## The starting distribution

r%*%P   ## The distribution after 1 step
r%*%P%*%P   ## The distribution after 2 steps
#Note: In R, you cannot use r%*%P^2, since P^2 means sqaure of each element of P, not P%*%
page.rank(nus, damping = 1) ## Classical PageRank
page.rank(nus, damping = 0.9) ## Scaled PageRank, with the scale parameter 0.9 (parameter s in class)
