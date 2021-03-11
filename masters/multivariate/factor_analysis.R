library(data.table)
library(ggplot2)
library(ggfortify)
library(eigen)
library(dplyr)
library(matlib)
library(mvtnorm)


data_dir <- "/Users/jinwee/github/Return/data/multivariate_analysis/"

football_data <- fread(file.path(data_dir, "T8_3_FOOTBALL.DAT"))
colnames(football_data) <- c("Group", "Width", "Cir", "Fb", "Eye", "Ear", "Jaw")
football_data <- football_data[!Group == 1] # Removing group 1, ref Methods Ex 8.8
football_data$Group <- sapply(football_data$Group, function(x) ifelse(x == 2, "College", "Non"))
football_dt <- football_data[,!c("Group")]


?princomp
football_cor <- cor(football_dt)
football_cov <- cov(football_dt)
football_pc <- princomp(cor = football_cor, covmat = football_cov) # returns list 
football_eigen_var_dt <- data.table("Var" = football_pc$sdev^2,
                               "Eigenvalue_num"= c(1:length(football_pc[[1]])))
football_pc_loadings <- football_pc$loadings

n_factors <- sum(football_eigen_var_dt$Var >= 1)

selected_loadings <- football_pc_loadings[,1:n_factors]

selected_loadings %*% diag(football_pc$sdev[1:n_factors])

rotated_loadings <- varimax()
loadings <- prc$loadings[,1:factors]
   coefficients <- loadings[,1:factors]%*%diag(prc$sdev[1:factors])
   rotated <- varimax(coefficients)$loadings 
   
   
   
   
   
t5_data <- fread(file.path(data_dir, "t5q4.txt"))
colnames(t5_data) <- c("method","aroma", "flavour", "texture", "moisture", "appearance")
t5_dt <- t5_data[,!c("method")]
t5_cov <- cov(t5_dt)
t5_cor <- cor(t5_dt)
t5_mean <- colMeans(t5_dt)
t5_var  <- diag(t5_cov)


t5_mle <- factanal(t5_dt, factors = 2,
                        rotation = "none", method = "mle", scores = "regression")

t5_mle


?factanal
t5_mle_rotate <- factanal(t5_dt, factors = 2,
                        rotation = "varimax", method = "mle", scores = "regression")


loadings <- t5_mle_rotate$loadings
loading_mat <- matrix(as.numeric(loadings),attributes(loadings)$dim)


t5_data
t(loading_mat) %*% inv(t5_cov) %*% t(as.matrix(t5_dt[1,]) - t5_mean)

t(t5_mle_rotate$loadings) %*% inv(t5_cor) %*% t(as.matrix(t5_dt[1,]) - t5_mean)


(as.matrix(t5_dt[1,]) - t5_mean) / t5_var


Y <- t(apply(t5_dt, 1, function(x) (x - t5_mean)/sqrt(t5_var)))
Yc <- t(apply(t5_dt, 1, function(x) (x - t5_mean)))


t5_mle_rotate$scores
#Yc  %*% inv(t5_cov) %*% t5_mle_rotate$loadings
# ahh yes since cov matrix was used
fact_scores <- t5_mle_rotate$scores
fact_scores_dt <- as.data.table(fact_scores)
fact_scores_dt$method <- t5_data$method

manova_res <- manova(cbind(Factor1,Factor2)~factor(method), data = fact_scores_dt)
summary(manova_res , test = "Wilks") #wilks number
summary.manova(manova_res)$SS




##approximate using regression method
t5_mle_rotate$loadings %*% inv(t5_cov)







