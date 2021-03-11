library(mvtnorm)
library(ICSNP)
library(jocre)
library(matlib)
library(data.table)
library(dplyr)

data_dir <- "/Users/jinwee/github/Return/data/multivariate_analysis/"
q3_data <- fread(file.path(data_dir, "t4q3.txt"))
q4_data <- fread(file.path(data_dir, "t4q4.txt"))
q5_data <- fread(file.path(data_dir, "t4q5.txt"))


#Q1 2 sample, treatments independant, normally distributed, with equal variance.
#Q1a)
x1 <- matrix(c(6,6,2,10,4,8), ncol = 2, byrow = TRUE)
x2 <- matrix(c(4,6,12,8,8,10), ncol = 2, byrow = TRUE)
n <- 3
m <- 3
p <- 2
mul1 <- c(4,8)
mul2 <- c(8,8)
S1 <- matrix(c(4,-4,-4,4), ncol = 2, byrow = TRUE)
S2 <- matrix(c(16,4,4,4), ncol = 2, byrow = TRUE)

Spooled <- ((n - 1) * S1 + (m - 1) * S2) / (n + m - 2)
mul_diff <- mul1 - mul2
T2 <- t(mul_diff) %*% inv(((1/n) + (1/m)) * Spooled) %*% mul_diff
f_coeff <- (n + m - 2) * p / (n + m - 1 - p)
f_value <- f_coeff * qf(0.95, df1 = p, df2 = n + m - 1 - p) # critical value

HotellingsT2(x1,x2, test = "chi")

#Q1b
# Hmm just follow formula in slides (pg25), expand out.


#Q4 independant, 2 sample t-test on mean,
#Q4a
q4_1_dt <- q4_data[group == 1, !c("group")]
q4_2_dt <- q4_data[group == 2, !c("group")]
q4_n <- 20
q4_p <- 6
q4_1_mean <- colMeans(q4_1_dt)
q4_2_mean <- colMeans(q4_2_dt)
q4_1_var <- var(q4_1_dt)
q4_2_var <- var(q4_2_dt)

q4_mul_diff <- q4_1_mean - q4_2_mean
q4_Spooled <- ((q4_n - 1) * q4_1_var + (q4_n - 1) * q4_2_var) / (q4_n + q4_n - 2)

q4_T2 <- t(q4_mul_diff) %*% inv(((1/q4_n) + (1/q4_n)) * q4_Spooled) %*% q4_mul_diff
f_coeff <- (q4_n + q4_n - 2) * q4_p / (q4_n + q4_n - 1 - q4_p)
f_value <- f_coeff * qf(0.95, df1 = q4_p, df2 =  q4_n + q4_n - 1 - q4_p) # critical value


HotellingsT2(q4_1_dt, q4_2_dt, test = "chi")
#HotellingsT2(q4_1_dt, q4_2_dt, test = "f")
#q4_T2 / f_coeff <- thats the T.2 value when using test= "f"

#Q4b Boneferroni simultanoues C.Is for each mean difference
# Note, q4_n is doubled here, since n and m are the same
alpha <- 0.05
alpha_prime <- alpha/q4_p
q4_t_value <- qt(1 - (alpha_prime /2), df = q4_n + q4_n -2)
q4_pooled_var_diag_sqrt <- sqrt(((1/q4_n) + (1/q4_n)) * diag(q4_pooled_var))

q4_bf_simu_ci <- sapply(c(1:q4_p), function(x) c(q4_mul_diff[x] - (q4_t_value * q4_pooled_var_diag_sqrt[x]),
                             q4_mul_diff[x] + (q4_t_value * q4_pooled_var_diag_sqrt[x])))


f





#Q3 paired, 2 sample t-test on mean
y_dt <- q3_data[,c("y1","y2")]
x_dt <- q3_data[,c("x1","x2")]
q3_p <- ncol(y_dt)
q3_n <- nrow(y_dt)
q3_d_dt <- y_dt - x_dt
q3_d_mean <- colMeans(q3_d_dt)
q3_d_var <- var(q3_d_dt)

q3_mul_0 <- c(0,0)

q3_T2 <- q3_n * q3_d_mean %*% inv(q3_d_var) %*% q3_d_mean
q3_f_coef <- q3_p * (q3_n - 1)  / (q3_n - q3_p)
q3_f_value <- q3_f_coef * qf(0.95, df1 = q3_p, df2 = q3_n - q3_p)
q3_f_value

HotellingsT2(q3_d_dt, mu = q3_mul_0, test = "chi")


#Q2 custom MANOVA
rm(list=ls())
data <- matrix(c(6,7,5,9,7,5,3,3,1,5,2,4,2,3,6,4,4,5), ncol = 2, byrow = TRUE) %>%
  as.data.table()
data$group <- c(1,1,1,2,2,2,3,3,3)
dt <-  data[,!c("group")]

overall_mean <- colMeans(dt)
overall_cov <- var(dt)

group_data_vec <- lapply(unique(data$group),
                         function(x) data[group == x, !c("group")])

#Custom residuals
group_sscp_vec <- lapply(group_data_vec, function(x) (nrow(x)-1) * var(x))
group_residual <- Reduce('+',group_sscp_vec) # Sum across list

#custom treatment
group_treatment_vec <- lapply(group_data_vec,
                              function(x) nrow(x) * (colMeans(x) - overall_mean) %*% t(colMeans(x) - overall_mean))
group_treatment <- Reduce('+', group_treatment_vec)


group_total <- group_residual + group_treatment

manova_result <- manova(cbind(V1,V2)~factor(group), data = data)
manova_sscp_res <- summary.manova(manova_result)$SS$Residuals
manova_sscp_trt <- summary.manova(manova_result)$SS$`factor(group)`



#Q5 MANOVA
colnames(q5_data) <- c("group" ,"y1", "y2", "y3", "y4")
head(q5_data)
q5_manova <- manova(cbind(y1,y2,y3,y4)~factor(group), data = q5_data)

summary(q5_manova, test = "Wilks")
summary.manova(q5_manova)$SS
qf(0.95, 8, 60)


##MANOVA test from slides
root_data <- fread(file.path(data_dir, "MANOVA2.txt"))
root_dt <- root_data[,!c("rootstock")] 
root_p <- ncol(root_dt)
root_a <- length(unique(root_data$rootstock))
root_n <- nrow(root_dt)

root_manova <- manova(cbind(y1,y2,y3,y4)~factor(rootstock), data = root_data)

summary(root_manova, test = "Wilks")
summary.manova(root_manova)$SS



