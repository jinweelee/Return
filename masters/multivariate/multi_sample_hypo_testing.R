library(ICSNP)
library(data.table)
library(matlib)
library(dplyr)


data_dir <- "/Users/jinwee/github/Return/data/multivariate_analysis/"
psych_data <- fread(file.path(data_dir, "psych.txt"))
q3_data <- fread(file.path(data_dir, "t4q3.txt"))
q4_data <- fread(file.path(data_dir, "t4q4.txt"))
q5_data <- fread(file.path(data_dir, "t4q5.txt"))


# Testing on psych
colnames(psych_data) <- c("gender", "pictorial", "paper", "recognition", "vocab")
m_dt <- psych_data[gender == 1, !c("gender")]
f_dt <- psych_data[gender == 2, !c("gender")]
m_mean <- colMeans(m_dt)
f_mean <- colMeans(f_dt)
m_var <- var(m_dt)
f_var <- var(f_dt)
psych_diff_mean <- m_mean - f_mean
psych_pooled_var <- ((32 - 1) * m_var  + (32 - 1) * f_var) / (32 + 32 - 2)

psych_t2 <- psych_diff_mean %*% inv(((1/32) + (1/32)) * psych_pooled_var) %*% psych_diff_mean
HotellingsT2(m_dt, f_dt, test = "chi")
psych_f_coeff <- (32 + 32 - 2) * 4 / (32 + 32 - 1 - 4)
psych_f_value <- psych_f_coeff  * qf(0.99, df1 = 4, df2 = 32 + 32 - 1 - 4)


psych_f_value
# low p-value we reject

#Q1
#Assume treatments independant, normally distributed, with equal variance.
n1 <- 3
n2 <- 3
x1 <- matrix(c(6,6,2,10,4,8), ncol = 2, byrow = TRUE)
x2 <- matrix(c(4,6,12,8,8,10), ncol = 2, byrow = TRUE)
mu_x1 <- apply(x1, 2, mean)
mu_x2 <- apply(x2, 2, mean)
var1 <- var(x1)
var2 <- var(x2)

q1_diff_mean <- mu_x1 - mu_x2
q1_pooled_var <-  ((3 - 1) * var1 + (3 - 1) * var2) / (3 + 3 - 2)
eigen(q1_pooled_var)

q1_T2 <- q1_diff_mean %*%  inv(((1/3) + (1/3)) * q1_pooled_var) %*% q1_diff_mean
q1_T2
HotellingsT2(x1,x2, test = "chi")
q1_f_coeff <- (3 + 3 - 2) * 2 / (3 + 3 - 1 - 2)
q1_f_value <- q1_f_coeff * qf(0.95, df1 = 2, df2 = 3 + 3 - 1 - 2) # critical value
q1_f_value


q1_f_coeff * qf(0.70, df1 = 2, df2 = 3 + 3 - 1 - 2)
q1_T2 > q1_f_value



# paired samples test
pipe_data <- fread(file.path(data_dir, "pipe.txt"))
pipe1_dt <- pipe_data[Coating == 1, !c("Location", "Coating")]
pipe2_dt <- pipe_data[Coating == 2, !c("Location", "Coating")]
pipe_diff_dt <- pipe1_dt - pipe2_dt
pipe_d_mean <- colMeans(pipe_diff_dt)
pipe_d_var <- var(pipe_diff_dt)

15 * pipe_d_mean %*% inv(pipe_d_var) %*% pipe_d_mean

pipe_mul_0 <- c(0,0)
HotellingsT2(pipe_diff_dt, mu = pipe_mul_0, test = "chi")

pipe_f_coef <- 2 * (15 -1) / (15 -2)
pipe_f_coef * qf(0.95, df1 = 2, df2 = 13) # critical value

pipe_f_coef * qf(0.996, df1 = 2, df2 = 13) # critical value
qchisq(0.995526, 2)

#Yup p-values are based on chi square approximation.



#Q3, paired samples
y_dt <- q3_data[,c("y1","y2")]
x_dt <- q3_data[,c("x1","x2")]
q3_p <- ncol(y_dt)
q3_n <- nrow(y_dt)
q3_d_dt <- y_dt - x_dt
q3_d_mean <- colMeans(q3_d_dt)
q3_d_var <- var(q3_d_dt)

q3_mul_0 <- c(0,0)

q3_T2 <- q3_n * q3_d_mean %*% inv(q3_d_var) %*% q3_d_mean
HotellingsT2(q3_d_dt, mu = q3_mul_0, test = "chi")

q3_f_coef <- q3_p * (q3_n - 1)  / (q3_n - q3_p)
q3_f_value <- q3_f_coef * qf(0.95, df1 = q3_p, df2 = q3_n - q3_p)
q3_f_value


#Q4 Independant samples
q4_1_dt <- q4_data[group == 1, !c("group")]
q4_2_dt <- q4_data[group == 2, !c("group")]
q4_n <- 20
q4_p <- 6
q4_1_mean <- colMeans(q4_1_dt)
q4_2_mean <- colMeans(q4_2_dt)
q4_1_var <- var(q4_1_dt)
q4_2_var <- var(q4_2_dt)
q4_diff_mean <- q4_1_mean - q4_2_mean
q4_pooled_var <- ((q4_n - 1) * q4_1_var + (q4_n - 1) * q4_2_var) / (q4_n + q4_n - 2)


HotellingsT2(q4_1_dt,q4_2_dt, test = "chi")
q4_T2 <- q4_diff_mean %*%  inv(((1/q4_n) + (1/q4_n)) * q4_pooled_var) %*% q4_diff_mean
q4_T2 
q4_f_coeff <- (q4_n + q4_n - 2) * q4_p / (q4_n + q4_n - 1 - q4_p)
q4_f_value <- q1_f_coeff * qf(0.95, df1 = q4_p, df2 = q4_n + q4_n - 1 - q4_p) # critical value

alpha_prime <- 0.05/6
q4_t_value <- qt(1 - (alpha_prime /2), df = 38)
q4_pooled_var_diag <- ((1/q4_n) + (1/q4_n)) * diag(q4_pooled_var)

q4_ci <- sapply(c(1:6), function(x) c(q4_diff_mean[x] - (q4_t_value * sqrt(q4_pooled_var_diag[x])),
                             q4_diff_mean[x] + (q4_t_value * sqrt(q4_pooled_var_diag[x]))))

-4.8 + (2.7835 * sqrt(536.03/10))





#MANOVA example
root_data <- fread(file.path(data_dir, "MANOVA2.txt"))
root_dt <- root_data[,!c("rootstock")] 
root_p <- ncol(root_dt)
root_a <- length(unique(root_data$rootstock))
root_n <- nrow(root_dt)

#factor by group.
root_manova <- manova(cbind(y1,y2,y3,y4)~factor(rootstock), data = root_data)

summary(root_manova, test = "Wilks")
summary.manova(root_manova)$SS

# custom residuals
root_group_vec <- lapply(unique(root_data$rootstock),
                         function(x) root_data[rootstock == x, !c("rootstock")])
root_group_sscp_vec <- lapply(root_group_vec, function(x) (nrow(x)-1) * var(x))
root_group_residual <- Reduce('+',root_group_sscp_vec)

# custom treatment /factor
overall_mean <- colMeans(root_dt)
root_group_treatment_vec <- lapply(root_group_vec,
                              function(x) nrow(x) * (colMeans(x) - overall_mean) %*% t(colMeans(x) - overall_mean))
root_group_treatment <- Reduce('+', root_group_treatment_vec)

# custom wilks stat
root_group_total <- root_group_residual + root_group_treatment
root_wilks_stat <- det(root_group_residual) / det(root_group_total)
root_w_coef <- ((root_p - (root_a - 1) + 1) / 2) - (root_n - root_a)
root_w_stat <- root_w_coef * log(root_wilks_stat)

# approx to F distribution
root_dtrt <- root_a - 1
root_dres <- root_n - root_a
root_w <- root_dres + root_dtrt - (0.5 * (root_p + root_dtrt + 1))
root_t <- sqrt(((root_p^2 * root_dtrt^2) - 4) / 
                 (root_p^2 +  root_dtrt^2 -5 )) #tbc
root_df1 <- root_p * (root_dtrt)
root_df2 <- (root_w * root_t) - 0.5 *((root_p * root_dtrt) - 0.5)

root_cfm_df1 <- 20
root_cfm_df2 <- 130





#Q5
q5_manova <- manova(cbind(y1,y2,y3,y4)~factor(method), data = q5_data)

summary(q5_manova, test = "Wilks")
summary.manova(q5_manova)$SS

#checking custom residuals
q5_group_vec <- lapply(unique(q5_data$method),
                         function(x) q5_data[method == x, !c("method")])

q5_group_mean_vec <- lapply(q5_group_vec, function(x) colMeans(x))

q5_dt <- q5_data[,!c("method")]
q5_a <- length(unique(q5_data$method))
q5_p <- ncol(q5_dt)
q5_n <- nrow(q5_dt)

q5_group_sscp_vec <- lapply(q5_group_vec, function(x) (nrow(x)-1) * var(x))
q5_group_residual <- Reduce('+',q5_group_sscp_vec)

qf(0.95, df1= 4, df2= 10)
qf(0.95, 4,8)

calc_f_coef <- function(a,p,n){
  dtrt <- a - 1
  dres <- n - a
  w <- dres + dtrt - (0.5 * (p + dtrt + 1))
  #print(w)
  t <- sqrt(((p^2 * dtrt^2) - 4) / 
                 (p^2 + dtrt^2 - 5))
  #print(t)
  
  df1 <- p * (dtrt)
  df2 <- (w * t) - (0.5 * ((p * dtrt) - 2))
  
  return(c(df1,df2))
  
}



#q1
q1_data <- matrix(c(6,7,5,9,7,5,3,3,1,5,2,4,2,3,6,4,4,5), ncol = 2, byrow = TRUE) %>%
  as.data.table()
q1_data$group <- c(1,1,1,2,2,2,3,3,3)

q1_dt <- q1_data[,!c("group")]
q1_a <- length(unique(q1_data$group))
q1_p <- ncol(q1_dt)
q1_n <- nrow(q1_dt)


q1_manova <- manova(cbind(V1,V2)~factor(group), data = q1_data)

summary(q1_manova, test = "Wilks")
summary.manova(q1_manova)$SS

q1_group_vec <- lapply(unique(q1_data$group),
                         function(x) q1_data[group == x, !c("group")])
q1_group_mean_vec <- lapply(q1_group_vec, function(x) colMeans(x))
q1_overall_mean <- colMeans(q1_dt)
q1_overall_var <- var(q1_dt)

#custom residuals
q1_group_sscp_vec <- lapply(q1_group_vec, function(x) (nrow(x)-1) * var(x))
q1_group_residual <- Reduce('+',q1_group_sscp_vec)


#custom treatment
q1_group_treatment_vec <- lapply(q1_group_vec,
                              function(x) nrow(x) * (colMeans(x) - q1_overall_mean) %*% t(colMeans(x) - q1_overall_mean))
q1_group_treatment <- Reduce('+', q1_group_treatment_vec)

#custom total
q1_group_treatment + q1_group_residual


q1_tot_diff <- apply(q1_dt, 1, function (x) x - q1_overall_mean)



# wilks
q1_wilks <- det(q1_group_residual) / det(q1_group_treatment + q1_group_residual)

calc_f_coef(q1_a,q1_p, q1_n)


(1 - sqrt(q1_wilks)) / sqrt(q1_wilks)





