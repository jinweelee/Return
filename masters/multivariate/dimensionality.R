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


# Starting PCA 
football_pc <- prcomp(football_dt, scale=TRUE) # returns list 
football_pc_sdev <- data.table("Sdev" = football_pc[[1]],
                               "Eigenvalue_num"= c(1:length(football_pc[[1]])))






summary(football_pc)

football_pc[[1]]
football_cov <- var(football_dt)
football_cor <- cor(football_dt)
football_cov_eigen <- eigen(football_cov) ## $vectors columns are normalized eigenvectors 
football_cor_eigen <- eigen(football_cor)

# Note, sqrt(football_cov_eigen$values) is diff from prcomp$stdev,scale =TRUE
# when scale = FALSE, results are same
# 

## Standardized sample PCs
football_mean <- apply(football_dt, 2, mean)
football_sdev <- sqrt(diag(football_cov))
football_sdev_iden <- inv(diag(sqrt(diag(football_cov)))) # equivalent to "D^-1/2"

football_dt_sub_mean <- sweep(football_dt, 2, football_mean, FUN = `-`)
football_dt_std <- sweep(football_dt, 2, football_mean, FUN = `-`) %>% 
  sweep(2, football_sdev, FUN = `/`)

#x <- as.matrix(sweep(football_dt, 2, football_mean, FUN = `-`))
#cov(x %*% football_sdev_iden)

## equal to the unstandardized correlation matrix
summary(prcomp(football_dt, scale=TRUE))
summary(prcomp(football_dt_std, scale = FALSE))
football_dt_std_cov <- cov(football_dt_std) 


## Interpretation of sample PCs
# Pending questions about PCs:
# Trying out simulated, atypical covariance matrices.
# mul <- c(3,1,5)  # mean vector
# sil <- matrix(c(4,2,5, 2, 6, 3, 5,3,10), ncol =3) # cov matrix
mul <- c(3,1)  # mean vector
sil <- matrix(c(0,0,0,0), ncol =2) # cov matrix, hmnn
sil
set.seed(123456)
simunorm <- as.data.table(rmvnorm(100, mean = mul, sigma = sil))
simu_cov <- var(simunorm)
simu_pc <- prcomp(simunorm, scale =TRUE) # hmm intresting something to do with scaling?
summary(simu_pc)

# Vizualization of PCs
autoplot(simu_pc, loadings = TRUE)

autoplot(football_pc, data = football_data, colour = "Group",
         loadings =TRUE)


# Scree graph / proportion of pop variance
ggplot(football_pc_sdev, aes(Eigenvalue_num, Sdev)) + 
  geom_bar(stat = "identity") +
  theme_bw()


