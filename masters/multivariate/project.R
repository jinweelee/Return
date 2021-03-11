library(data.table)
library(ggplot2)
library(ggfortify)
library(eigen)
library(dplyr)
library(matlib)
library(mvtnorm)
library(pca3d)
library(rgl)
library(knitr)


?kable


data_dir <- "/Users/jinwee/github/Return/data/multivariate_analysis/"
seed_data <- fread(file.path(data_dir, "seeds.txt"))
seed_dt <- seed_data[, !c("group")]
col_palette <- c("dodgerblue", "orangered", "seagreen")

seed_pc <- prcomp(seed_data, scale=TRUE)
seed_pc_unscale <- prcomp(seed_data, scale=FALSE)
seed_group <- factor(seed_data$group)
summary(seed_group)
summary(seed_pc)


seed_pc_var_dt <- data.table("Variance explained" = seed_pc[[1]] ^2,
                               "PC"= c(1:length(seed_pc[[1]])))


cumsum(seed_pc_var_dt$`Variance explained`)/ sum(seed_pc_var_dt$`Variance explained`)
seed_pc_var_dt$`Culmulative Proportion` <- cumsum(seed_pc_var_dt$`Variance explained`)/ sum(seed_pc_var_dt$`Variance explained`)

eigen(seed_cor)
#?autoplot
seed_data$group_factor <-  factor(seed_data$group)
autoplot(seed_pc, data = seed_data, colour = "group_factor") + 
  labs(color = "Group") +
  scale_colour_manual(values = col_palette)
  #scale_fill_discrete(name = "New Legend Title")
  #theme(legend.title = "Group")
  
?pca3d
pca3d(seed_pc, group = seed_group, legend = "right", palette = col_palette)
rglwidget()


# Scree graph / proportion of pop variance
ggplot(seed_pc_sdev_dt, aes(PC,Variance_explained)) + 
  geom_line(stat = "identity") +
  theme_bw()

#Checking eigenvalues
seed_cov <- cov(seed_dt)
seed_cor <- cor(seed_dt)
seed_cov_eigen <- eigen(seed_cov)
seed_cor_eigen <- eigen(seed_cor)

seed_pc

seed_eigen1 <- seed_pc$vectors[,1]
seed_eigen2 <- seed_pc$vectors[,2]
seed_eigen3 <- seed_pc$vectors[,3]


data.table("Variable" = colnames(seed_dt),
           "Eigenvector 1" = seed_eigen1,
           "Eigenvector 2" = seed_eigen2,
           "Eigenvector 3" = seed_eigen3)

print(seed_eigen1)
print(seed_eigen2)

# By examining the first 2 eigenvectors, we can see that PC1 is a general measure of
# the area, perimeter, kernel length, width and groove length. Whereas the main contribution to
# PC2 is by the asymmetry coefficient and to a lesser extent, kernel length and groove length.

#Factor analysis
# Principal components
n_factors <- 3
seed_princ <- princomp(covmat = seed_cor , cor = TRUE)
seed_factor_evals <- unname(seed_princ$sdev^2)[1:n_factors]
seed_factor_evecs <- seed_princ$loadings[,1:n_factors]
seed_loading_mat <- diag(seed_princ$sdev[1:n_factors]) %*% t(seed_factor_evecs) %>%
   t()
colnames(seed_loading_mat) <-  paste("Factor", seq(1:n_factors), sep = "_")

seed_loading_mat_rotated <- varimax(seed_loading_mat)$loadings
?varimax
?loadings


get_factor_var_table <- function(cor_mat, loading_mat){
  princ <- princomp(covmat = cor_mat , cor = TRUE)
  factor_variance <- apply(loading_mat,2,function(x) {sum(x^2)})
  factor_variance_prop <- factor_variance / sum(unname(princ$sdev^2))
  factor_variance_prop_cul <- cumsum(factor_variance_prop)
  factor_variance_dt <- t(data.table("Variance explained" = factor_variance,
                      "Proportion of total variance" = factor_variance_prop,
                      "Culmulative proportion of total variance" = factor_variance_prop_cul))
  colnames(factor_variance_dt) <- paste("Factor", seq(1:n_factors), sep = "_")
  return(factor_variance_dt)
}

get_factor_var_table(seed_cor, seed_loading_mat)
get_factor_var_table(seed_cor, seed_loading_mat_rotated)


#
plot_loading_strength <- function(loading_mat){
  loading_dt <- data.table(matrix(as.numeric(loading_mat),
                                attributes(loading_mat)$dim,
                                dimnames=attributes(loading_mat)$dimnames))
  loading_dt$Variable <- rownames(loading_mat)
  
  loading_dt_melt <- melt(loading_dt,id="Variable", variable.name = 'Factor', value.name = "Loading")
  
  loading_dt_melt$Variable
  
  factor_plot <- ggplot(loading_dt_melt, aes(Variable, abs(Loading), fill=Loading)) + 
    facet_wrap(~ Factor, nrow=1) + #place the factors in separate facets
    geom_bar(stat="identity") + #make the bars
    coord_flip() + #flip the axes so the test names can be horizontal  
    scale_fill_gradient2(name = "Loading", 
                         high = "blue", mid = "white", low = "red", 
                         midpoint=0, guide="colourbar") +
    ylab("Absolute Loading Strength") + #improve y-axis label
    theme_bw(base_size=10) + 
    theme(axis.title.x = element_text(size = 14),
          axis.text.x = element_text(size = 10),
          axis.title.y = element_text(size = 14),
          axis.text.y = element_text(size = 10),
          strip.text.x = element_text(size = 10),
          legend.title = element_blank())
  return(factor_plot)
}


loading_mat <- seed_loading_mat_rotated



## Grouping variables with similar factor loadings
## c("area","perimeter","length_k","width_k", "compact", "asym")
reord_var <- c("area","perimeter","length_k","width_k", "compact", "asym")
seed_cor[reord_var,reord_var]


# Since cor matrix used, use 1 instead of sii
seed_specific_var <- 1 - apply(seed_loading_rotated, 1, function(x) sum(x^2))
seed_factor_cor <- (seed_loading_mat_rotated %*% t(seed_loading_mat_rotated)) + diag(seed_specific_var)
seed_cor - seed_factor_cor

#TODO
# factor estimation
# MANOVA


![](/Users/jinwee/github/Return/pca_3d_plot.png)


