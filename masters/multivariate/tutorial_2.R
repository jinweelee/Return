library(data.table)
library(matlib)

#T1
#Q1a
a <- c(2,-1,3)
q1_mul <- c(3,1,4)
q1_sig <- matrix(c(6,1,-2,1,13,3,-2,3,4), ncol=3, byrow=TRUE)
a %*% q1_mul
t(a) %*% q1_sig %*% a
#Q1b
A <- matrix(c(1,1,1,-1,1,2), ncol=3, byrow=FALSE)
A %*% q1_mul
A %*% q1_sig %*% t(A)
#Q1c
B <- matrix(c(1,0,0,0,0,1), ncol=3, byrow=FALSE)
B %*% q1_mul
B %*% q1_sig %*% t(B)
#1d
C <- matrix(c(1,0,0.5,0,0,0.5, 0, 1,0), ncol=3, byrow=FALSE)
C %*% q1_mul
C %*% q1_sig %*% t(C)


#Q2
#Q2a
q2_sig <- matrix(c(16,-2,3,-2,4,1,3,1,9), ncol = 3, byrow = TRUE)
V_12 <- sqrt(diag(diag(q2_sig)))
P <- inv(V_12) %*% q2_sig %*% inv(V_12)
#cov2cor(q2_sig)
#Q2b
D <- matrix(c(1,1,1,-2,0,1), ncol=3, byrow=FALSE)
d <- c(-3,1)
D %*% q2_sig %*% t(D)

#Q3
q3_sig_yy <- matrix(c(7,3,3,6), ncol = 2, byrow = TRUE)
q3_sig_xx <- matrix(c(5,-2,-2,4), ncol = 2, byrow = TRUE)
q3_sig_yx <- matrix(c(-3,2,0,3), ncol = 2, byrow = TRUE)

y_mul <- c(3,-1)
x_mul <- c(2,1)

x_given <- c(3,1)

cond_mul <- y_mul  + q3_sig_yx %*% inv(q3_sig_xx) %*% (x_given - x_mul)
cond_sig <- q3_sig_yy - q3_sig_yx %*% inv(q3_sig_xx) %*% t(q3_sig_yx)







