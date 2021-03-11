library(data.table)
library(ggplot2)


set.seed(123)
ex_5_vec <- c(1:1000)

ex_5_a_dt <- data.table("Sn" = sapply(ex_5_vec, function(x) sum(runif(x))),
                      "n" = ex_5_vec)

ex_5_b_dt <- data.table("Sn" = sapply(ex_5_vec, function(x) sum(runif(x)) / x),
                      "n" = ex_5_vec)

ex_5_c_dt <- data.table("Sn" = sapply(ex_5_vec, function(x) sum(runif(x)) - (x/2)),
                      "n" = ex_5_vec)


ex_5_d_dt <- data.table("Sn" = sapply(ex_5_vec,
                                      function(x) (sum(runif(x)) - (x/2)) / x),
                      "n" = ex_5_vec)

ex_5_e_dt <- data.table("Sn" = sapply(ex_5_vec,
                                      function(x) (sum(runif(x)) - (x/2)) / sqrt(x)),
                      "n" = ex_5_vec)

ggplot(ex_5_a_dt, aes(n, Sn)) +
  geom_line(stat = "identity")

ggplot(ex_5_b_dt, aes(n, Sn)) +
  geom_line(stat = "identity")

ggplot(ex_5_c_dt, aes(n, Sn)) +
  geom_line(stat = "identity")

ggplot(ex_5_d_dt, aes(n, Sn)) +
  geom_line(stat = "identity")

ggplot(ex_5_e_dt, aes(n, Sn)) +
  geom_line(stat = "identity")


#### Q1
library(foreach)
library(doParallel)
set.seed(1234)
cores=detectCores()
cl <- makeCluster(6)
registerDoParallel(cl)

test <- foreach(i=1:10, .combine = sum) %dopar% {
  n <- 1001000100
  uni <- sum(runif(n)) / 10010001000
  
  uni
}

test/100



n <- 1001000100
n<- 1001000100
(sum(sapply(runif(n), function(x) sqrt(4 - x^2) - 2 + x))) * 2/n

test


##
?qnorm


z <- qnorm(1-a)
r_region <- mul_0 + (z * (sqrt(var)/ sqrt(n)))


n <- 100
var <- 100
a <- 0.1
mul_0 <- 0
mul_vec <- seq(-10,10,by = 0.01)
delta_vec <- sapply(mul_vec, function(x) (mul_0 - x) / sqrt(var))
power_vec <- sapply(delta_vec, function(x) pnorm(sqrt(n)*x - qnorm(1 - (a/2))) + 
                      pnorm(-sqrt(n)*x - qnorm(1 - (a/2))))


plot(x = mul_vec, y = power_vec, cex = 1.2, xlab = "Mul", ylab = "Power", col = 2, type = "l")

plot(delta,pnorm(sqrt(10)*delta-qnorm(0.975))+
pnorm(-sqrt(10)*delta-qnorm(0.975)), xlab=delta,type=’l’, ylim=c(0,1) ,ylab=’’)




pnorm(1-0.53)








