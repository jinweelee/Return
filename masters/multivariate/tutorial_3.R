library(data.table)
library(matlib)
library(mvtnorm)
library(ICSNP)
library(jocre)

#Q1a
A <- matrix(c(1,1,0,1), ncol=2, byrow=FALSE)
b <- c(0,0)
O <- matrix(c(1,0,0,1), ncol=2, byrow=FALSE)
mul_x <- c(1,2)
sig_x <- matrix(c(2,1,1,2), ncol=2, byrow=FALSE)

mul_y <- A %*% mul_x
sig_y <- O + (A %*% sig_x %*% t(A))

mul_v <- c(mul_x, mul_y)
sig_v <- rbind(cbind(sig_x, sig_x %*% t(A)),cbind(t(sig_x %*% t(A)), sig_y))

#q1b
mul_y
sig_y


#q1c
W <- matrix(c(1,0,0,1,-1,0,0,-1), ncol=4, byrow=FALSE)
W %*% mul_v
W %*% sig_v %*% t(W)




#Q3
#Q3a 1 sample t-test, variance unknown
s_cov <- matrix(c(2,-1,-1,2), ncol=2, byrow=TRUE)
s_mean <- c(1, 0.5)
mul_0 <- c(0.8,0.6)
n <- 50
p <- 2

calc_T2 <- function(sample_mean, sample_cov, n, mul0){
  mean_diff <- sample_mean - mul0
  t2_stat  <- n * t(mean_diff) %*% inv(sample_cov) %*% mean_diff
  return(t2_stat)
}
# F-cdf quantile
# Since T2 < F-stat, do not reject
qf(0.95, df1 = p , df2 = n-p) *
  (((n-1) * p) / (n-p))

#Q3b GLRH test on variance, mul unknown.
sig_0 <- matrix(c(2,0,0,2), ncol=2, byrow=TRUE)
m <- p/2 * (p+1)
glhr_var_stat <-  (n * tr(inv(sig_0) %*% s_cov)) - (n * log(det(inv(sig_0) %*% s_cov))) - (n * p)
qchisq(0.95, df = m, lower.tail = TRUE)


#Q4
mul <- c(3,1)  # mean vector
sil <- matrix(c(4,2,2,9), ncol =2) # cov matrix
set.seed(123456)
simunorm <- as.data.table(rmvnorm(100, mean = mul, sigma = sil))

sample_mean <- apply(simunorm, 2, mean)
sample_cov <- var(simunorm)
sample_cov_inv <- inv(sample_cov)
mul_0 <- c(3,1)

calc_T2(sample_mean, sample_cov, 100, mul_0)
qf(0.95, df1 = 2 , df2 = 100-p) *
  (((100-1) * 2) / (100-2))

HotellingsT2(simunorm, mu = mul_0, test= "chi")

#Q4c, simul cfintervals NOTE the change in formualte 
a <- c(1,0)
alpha <- 0.05
n <- 100 
p <- 2
f_stat <- qf(0.95, df1 = p , df2 = n-p) *
  (((n-1) * p) / (n * (n-p)))

bound_value <- sqrt((t(a) %*% sample_cov %*% a) * f_stat)

c((t(a) %*% sample_mean) - bound_value, (t(a) %*% sample_mean) + bound_value)






