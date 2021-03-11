library(mvtnorm)
library(ICSNP)
library(jocre)
library(matlib)
library(data.table)

data_dir <- "/Users/jinwee/github/Return/data/multivariate_analysis/"
turnip_data <- fread(file.path(data_dir, "T3_3_CALCIUM.DAT"))
colnames(turnip_data) <- c("location", "available", "exchangeable", "turnip") # calcium in each source
swiss_data <- fread(file.path(data_dir, "banknote.txt"))

#  One sample t-test on mean, sigma unkown
## Turnip pg 134
dim(turnip_dt)
turnip_dt <- turnip_data[,!c("location")]
turnip_mean <- apply(turnip_dt, 2, mean)
turnip_var <- var(turnip_dt)
turnip_mul_0 <- c(15, 6, 2.85)
# Method based on chi-sauare gives tb result.
turnip_t2 <- HotellingsT2(turnip_dt, mu = turnip_mul_0, test= "chi")
qf(0.95, df1 = ncol(turnip_dt), df2 = nrow(turnip_dt) - ncol(turnip_dt))

## Slides example, banknote
## NOTE, FIRST 100 are real, second 100 are fake
## Only testing on fake notes.
swiss_dt <- swiss_data[101:200, !c("BN")]
swiss_p <- ncol(swiss_dt)
swiss_n <- nrow(swiss_dt)
swiss_v <- nrow(swiss_dt) - ncol(swiss_dt)
swiss_mean <- round(apply(swiss_dt, 2, mean),1)
swiss_var <- round(var(swiss_dt),3)
swiss_mul_0 <- c(214.9, 129.9, 129.7, 8.3,10.1,141.5)
swiss_mean_diff_0 <- swiss_mean - swiss_mul_0

100 * swiss_mean_diff_0 %*% inv(swiss_var) %*% swiss_mean_diff_0

swiss_t2 <- HotellingsT2(swiss_dt, mu = swiss_mul_0, test= "chi")
swiss_fvalue <- qf(0.95, df1 = swiss_p , df2 = swiss_v) *
  ((swiss_v * swiss_p) / (swiss_v - swiss_p + 1))

## One sample t-test on mean, sigma known
swiss_sigma_vec <- c(0.15,0.058,0.057,0.057,0.014,0.005,
                     0.058,0.133,0.086,0.057,0.049,-0.043,
                     0.057, 0.086, 0.126, 0.058, 0.031, -0.024,
                     0.057,0.057,0.058,0.413,-0.263, 0,
                     0.014, 0.049, 0.031,-0.263,0.421, -0.075,
                     0.005,-0.043,-0.024,0,-0.075,0.2
                     )
swiss_sigma <- matrix(swiss_sigma_vec, ncol = 6,
                      byrow = TRUE, dimnames = list(colnames(swiss_dt), colnames(swiss_dt)))

swiss_sigma_inv <- inv(swiss_sigma)
swiss_mean_diff <- swiss_mean - swiss_mul_0

swiss_test_stat <-  swiss_n * (swiss_mean_diff %*% swiss_sigma_inv %*% swiss_mean_diff)

swiss_test_stat
qchisq(0.95, df = swiss_p, lower.tail = TRUE)

# One sample t-test on sigma, mu unknown
swiss_dt_sub <- swiss_dt[,c(1:3)]
swiss_mean_sub <- apply(swiss_dt_sub, 2, mean)
swiss_var_sub <- var(swiss_dt_sub)


# Confidence intervals/ simulconfidence intervals
# Confidence interval for mul

a_t <- c(0,0,0,1,-1,0)
a_t_swiss_mean <-  a_t %*% swiss_mean
a_t_swiss_variance <- a_t %*% swiss_var %*% a_t
t_value <- qt(0.025, df = swiss_n - 1, lower.tail = FALSE)
a_t_swiss_tstat_ci <- c(a_t_swiss_mean - (t_value * (sqrt(a_t_swiss_variance) / sqrt(swiss_n))),
                        a_t_swiss_mean + (t_value * (sqrt(a_t_swiss_variance) / sqrt(swiss_n))))

f_value <- qf(0.95, df1 = 1 , df2 = swiss_n - 1)
a_t_swiss_fstat_ci <- c(a_t_swiss_mean - sqrt(f_value * a_t_swiss_variance / swiss_n),
                        a_t_swiss_mean + sqrt(f_value * a_t_swiss_variance / swiss_n))
         
## same result               
a_t_swiss_tstat_ci
a_t_swiss_fstat_ci


# using simultaneous CI for all a_t
# note result is different / less precise than using t-statistic
simul_f_coe <- ((swiss_n -1) * swiss_p) / (swiss_n * (swiss_n - swiss_p))
simul_f_value <- qf(0.95, df1 = swiss_p , df2 = swiss_n - swiss_p)
a_t_swiss_simul_ci <- c(a_t_swiss_mean - sqrt(simul_f_coe * simul_f_value * a_t_swiss_variance),
                        a_t_swiss_mean + sqrt(simul_f_coe * simul_f_value * a_t_swiss_variance))
a_t_swiss_simul_ci                        


# f_coe <- ((100 - 1) * 6) / (100 * 94)
# 
# a_t_swiss_mean - sqrt( f_coe * qf(0.95, df1 = 1 , df2 = swiss_n -1) *
#                         (a_t_swiss_variance / swiss_n))
# 

?qt, loe

c("F", 3) %*% c(4,4)
A <- matrix(c(1,0,0,0,0,1), ncol = 3, byrow = TRUE)
A
A %*% matrix(c(4,0,-1,0,5,0,-1,0,2), ncol = 3, byrow = TRUE) %*% t(A)


B <- matrix(c(9,12,3,0,8,4,6,6,0,5,4,2,8,10,1), ncol = 3, byrow = TRUE)
var(B)
apply(B,2,mean)


C <- matrix(c(3,10,6,12,5,14,10,9), ncol = 2, byrow = TRUE)
var(C)
C_mean <- apply(C,2,mean)
C_var <- matrix(c(26/3,-8/3,-8/3,14.75/3), ncol = 2, byrow = TRUE)
C_mul_0 <- c(6,11)
C_mean_diff_0 <- C_mean - C_mul_0

5 * (C_mean_diff_0 %*% inv(C_var) %*% C_mean_diff_0)


HotellingsT2(C, mu = C_mul_0, test= "chi")

9/319.5 * matrix(c(14.75/3, 8/3,8/3, 26/3), ncol = 2, byrow = TRUE)


5 * C_mean_diff_0 %*% inv(C_var) %*% C_mean_diff_0

5 * C_mean_diff_0 %*% temp
 # cset(swiss_dt, method = "hotelling", alpha = 0.05)
# cset(simunorm, method = "hotelling", alpha = 0.05)
# 
# cset(as.matrix(swiss_dt[,c(1,2)]), method = "hotelling", alpha = 0.1)
# ?cset()


swiss_t2 
swiss_fvalue

### 


?HotellingsT2
turnip_t2
f








mul <- c(3,1)  # mean vector
sil <- matrix(c(4,2,2,9), ncol =2) # cov matrix
set.seed(123456)
simunorm <- as.data.table(rmvnorm(100, mean = mul, sigma = sil))

sample_mean <- apply(simunorm, 2, mean)
sample_var <- var(simunorm)
sample_var_inv <- inv(sample_var)
mul_0 <- c(3,1)

sample_mean_mul0 <- sample_mean - mul_0


c(1,0) %*% sample_var %*% c(1,0)

100 * sample_mean_mul0 %*% sample_var_inv %*% sample_mean_mul0

0.63962 * 2.0204
# Calculating T2
head(simunorm)
HotellingsT2(simunorm, mu = mul_0, test= "f")

#simultanous C.I
cset(simunorm, method = "hotelling", alpha = 0.05)


#misc prep for quiz



