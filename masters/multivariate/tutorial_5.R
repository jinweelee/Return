library(data.table)
library(dplyr)
library(matlib)
library(mvtnorm)


data_dir <- "/Users/jinwee/github/Return/data/multivariate_analysis/"
q2_data <- fread(file.path(data_dir, "t5q2.txt"))
q4_data <- fread(file.path(data_dir, "t5q4.txt"))

#Q1
sig <- matrix(c(4,-1,-1,4), ncol =2, byrow=TRUE)
eigen(sig)
R <- cov2cor(sig)
eigen(R) # ah yes sum to 2
5/8
1.25/2

#Q2
head(q2_data)
q2_cor <- cor(q2_data)
q2_eigen <- eigen(q2_cor)
q2_eval <- q2_eigen$values
q2_evec <- q2_eigen$vectors
q2_eval / sum(q2_eval)




#Q3
R <- matrix(c(1,0.9,0.7,0.9,1,0.4,0.7,0.4,1), ncol =3, byrow=TRUE)
q3_eigen <- eigen(R)
q3_eval <- q3_eigen$values
q3_evec <- q3_eigen$vectors

L <- sqrt(q3_eval[1]) * q3_evec[,1]
L
L^2
diag(1 - L^2)
sum(L^2)/3


#Q4
head(q4_data)
q4_dt <- q4_data[,!c("method")]
q4_mle_rotate <- factanal(q4_dt, factors = 2,
                          rotation = "varimax", method = "mle", scores = "regression")


# how to extract loadings from factanal
q4_mle_loadings <- data.frame(matrix(as.numeric(q4_mle_rotate$loadings),
                  attributes(q4_mle_rotate$loadings)$dim,
                  dimnames=attributes(q4_mle_rotate$loadings)$dimnames))

q4_mle_fact_scores <- as.data.table(q4_mle_rotate$scores)


#using prcomp to get loading mat + others
rm(list=ls())
stock_data <- fread("/Users/jinwee/github/Return/data/multivariate_analysis/ch8ex1.txt")
stock_data <- stock_data[,c(1:5)]
colnames(stock_data) <- c("JPM", "Citi", 'Wells', "Shell", "Exxon")

n_factors <-  2
stock_cor <- cor(stock_data)
princ <- princomp(covmat = stock_cor , cor = TRUE)
factor_evals <- unname(princ$sdev^2)[1:n_factors]
factor_evecs <- princ$loadings[,1:n_factors]
loading_mat <- diag(princ$sdev[1:n_factors]) %*% t(factor_evecs) %>%
    t()
colnames(loading_mat) <-  paste("Factor", seq(1:n_factors), sep = " ")

comm <- apply(loading_mat, 1, function(x) sum(x^2))
specific_var <- 1- comm

var_explained <- apply(loading_mat, 2, function(x) sum(x^2) / ncol(stock_data)) #since cor used









