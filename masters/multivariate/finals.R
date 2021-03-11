library(mvtnorm)
library(ICSNP)
library(jocre)
library(matlib)
library(data.table)
library(ggplot2)
library(scatterplot3d)
library(andrews)


#Q3
q3_sig <- matrix(c(2,0,-1,0,5,0,-1,0,3), ncol=3, byrow=TRUE)
A <- matrix(c(1,2,3,0,0,1), ncol=3, byrow=TRUE)
A %*% q3_sig %*% t(A)



#Q5 x=2
q5_mat <- matrix(c(), ncol=3, byrow=TRUE)
c(8,-4,2) %*% c(1,3,1)
c(-4,0,-2) %*% c(1,3,1)

#Q6 mat
q6_mat <- matrix(c(7,12,4,2,8,3,7,6,0,5,4,2,9,10,1), ncol=3, byrow=TRUE)
colMeans(q6_mat)






#Q8 
eval_3 <- 5 - 2.413 - 1.002 - 0.571 -0.401


evec_3 <- c(-0.786,0.550,0.075,0.273,-0.006)
x2_vec <- c(0.351,1,0.164,0.190,0.181)
x4_vec <- c(0.320,0.190,0.595,1,0.464)
x5_vec <- c(0.329,0.181,0.470,0.464,1)

(x2_vec %*% evec_3) / evec_3[2]
(x4_vec %*% evec_3) / evec_3[4]
(x5_vec %*% evec_3) / evec_3[5]

#8b
q <- c(-0.5,-0.75,0.4,0.5,0.6)
evec_2 <- c(-0.415,-0.777,0.303,0.291,0.217)
q %*% evec_2



#Q9
L <- c(0.8,0.6,0.9,0.5)
1 - L^2
sum(L^2) / 4



#Q11
mul <- c(2,1,1)
sig <- matrix(c(4,0,-1,0,2,0,-1,0,5), ncol=3, byrow=TRUE)
A <- matrix(c(1,0,0,0,0,1), ncol=3, byrow=TRUE)

mul_13 <- A %*% mul
sig_13 <- A %*% sig %*% t(A)




#Q13
rm(list=ls())
calc_T2 <- function(sample_mean, sample_cov, n, mul0){
  mean_diff <- sample_mean - mul0
  t2_stat  <- n * t(mean_diff) %*% inv(sample_cov) %*% mean_diff
  return(t2_stat)
}

n <- 100
p <- 2
s_mean <- c(0,-1)
s_cov <- matrix(c(4,-3,-3,9), ncol=2, byrow=TRUE)
inv(s_cov)
mul0 <- c(0,0)

calc_T2(s_mean, s_cov, n, mul0)
qf(0.95, df1 = p , df2 = n-p) *
  (((n-1) * p) / (n-p))


f


