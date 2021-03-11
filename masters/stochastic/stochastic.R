library(markovchain)

### Stationary distribution of discrete-time Markov chain
###  (uses eigenvectors)
###
stationary <- function(mat) {
x = eigen(t(mat))$vectors[,1]
as.double(x/sum(x))
}


test <- matrix(c(1/3,2/3,2/3,1/3), ncol=2, byrow=TRUE)
test2 <- matrix(c(1/4,3/4,1/5,4/5), ncol=2, byrow=TRUE)
q3 <- matrix(c(3/4,1/4,1/8,7/8), ncol=2, byrow=TRUE)

stationary(test)
stationary(test2)
stationary(q3)

q1 <- matrix(c(0,1/2,0,1/2,0,0,1,0,0,0,0,1,0.5,0,0,0.5), ncol = 4, byrow=TRUE)
q1_mc <- new("markovchain", transitionMatrix = q1,
             states = c("0", "1", "2", "3"))
firstPassage(q1_mc, "0", 4)



q3 <- matrix(c(3/4), ncol = 2, byrow=TRUE)
q3_mc <- new("markovchain", transitionMatrix = q3,
             states = c("0", "1"))

#f00
hittingProbabilities(q3_mc)


#q3 full
q3_full <-  matrix(c(1,0,0,0,0,0,
                     0,3/4,1/4,0,0,0,
                     0,1/8,7/8,0,0,0,
                     1/4,1/4,0,1/8,3/8,0,
                     1/3,0,1/6,1/6,1/3,0,
                     0,0,0,0,0,1),
                   ncol = 6, byrow=TRUE)


q3_mc_full <- new("markovchain", transitionMatrix = q3_full,
             states = c("0", "1", "2", "3", "4", "5"))



q2 <- matrix(c(1 - exp(-1),exp(-1),0,0,
              1 - exp(-1/2),0,exp(-1/2),0,
              1 - exp(-1/3),0, 0,exp(-1/3)),
             ncol = 4, byrow=TRUE)




stationary(t(q2))








?absorbingStates
recurrentClasses(q3_mc_full)
transientClasses(q3_mc_full)
absorbingStates(q3_mc_full)
absorptionProbabilities(q3_mc_full)
transitionProbability(q3_mc_full,3,1)
