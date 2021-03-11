library(data.table)
library(matlib)

factanalpc <- function(xmat, factors=NULL, cor=TRUE) {
   prc <- princomp (covmat = xmat ,cor = cor)  # Principal components    
   eig <- prc$sdev^2  # variance of each of the Principal Components
   if (is.null(factors)) factors <- sum(eig >= 1)
   # get the number of factors with variance of the PC >= 1 
   loadings <- prc$loadings[,1:factors]
   coefficients <- loadings[,1:factors]%*%diag(prc$sdev[1:factors])
   #rotated <- varimax(coefficients)$loadings 
   ## output the loadings with rotation using varimax method
   rotated <- coefficients ## output the loadings without rotation
   fct.ss  <- apply(rotated,2,function(x) {sum(x^2)} )
   pct.ss  <- fct.ss / sum(eig)
   cum.ss  <- cumsum (pct.ss)
   ss <- t( cbind(fct.ss, pct.ss, cum.ss) )
   #dum <- c(?Factor 1?,?Factor 2?,?Factor 3?,?Factor 4?,?Factor 5?)
   colnames(rotated) <- paste("Factor", seq(1:factors), sep = " ")
   # colnames(ss) <- colnames(rotated)
   # rownames(ss) <- c("Var explained", "Prop of var expl", "Cum percent of var expl")
   # out <- list(rotated,ss)
   # return(out)
   return(rotated)
}

paste("Factor", seq(1:4), sep = " ")


stock_data <- fread("/Users/jinwee/github/Return/data/multivariate_analysis/ch8ex1.txt")
stock_data <- stock_data[,c(1:5)]
colnames(stock_data) <- c("JPM", "Citi", 'Wells', "Shell", "Exxon")
head(stock_data)

stockcor <- cor(stock_data)

seed_cor <- stockcor
n_factors <- 4
seed_princ <- princomp(covmat = seed_cor , cor = TRUE)
seed_factor_evals <- unname(seed_princ$sdev^2)[1:n_factors]
seed_factor_evecs <- seed_princ$loadings[,1:n_factors]
seed_loading_mat <- diag(seed_princ$sdev[1:n_factors]) %*% t(seed_factor_evecs) %>%
   t()
colnames(seed_loading_mat) <-  paste("Factor", seq(1:n_factors), sep = "_")

factanal(stock_data, factors =2, rotation = "none", method = "mle", scores = "regression")

seed_manova_res <- manova(cbind(Factor1,Factor2)~factor(group), data = seed_mle_fact_scores)
summary(seed_manova_res , test = "Wilks") #wilks number

seed_manova_res_stats <- summary(seed_manova_res , test = "Wilks")$stats
as.data.table(seed_manova_res_stats)
names(summary(seed_manova_res , test = "Wilks"))
?manova

nrow(seed_dt)
f_value <- qf(0.95, df1 = 4 , df2 = 412)

# ```{r re-ordering by corr, echo = FALSE}
# reord_var <- c("length_k","length_k_g","perimeter","area","width_k", "compact", "asym")
# kable(seed_cor[reord_var,reord_var], align = "c")
# ```
# 
# ```{r residual matrix analysis, echo =FALSE}
# # Since cor matrix used, use 1 instead of sii
# seed_specific_var <- 1 - apply(seed_loading_mat_rotated, 1, function(x) sum(x^2))
# seed_factor_cor <- (seed_loading_mat_rotated %*% t(seed_loading_mat_rotated)) + diag(seed_specific_var)
# seed_loading_residual <- seed_cor - seed_factor_cor
# kable(seed_loading_residual, align = "c")
# ```

