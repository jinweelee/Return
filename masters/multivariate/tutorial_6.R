library(data.table)
library(matlib)
library(dplyr)
library(MASS)
#rm(list=ls())
source("/Users/jinwee/github/Return/masters/multivariate/classify.R")

data_dir <- "/Users/jinwee/github/Return/data/multivariate_analysis/"
q2_data <- fread(file.path(data_dir, "t6q2.txt"))
q3_data <- fread(file.path(data_dir, "t6q3.txt"))



#Q1 Equal missclass cost, equal priors, equal variance 
mul1 <- c(3,6)
mul2 <- c(5,8)
Sp <- matrix(c(1,1,1,2), byrow=TRUE,ncol=2)

a_hat <- t(mul1-mul2) %*% inv(Sp)
log_prob <- log(2) #rhs
x0 <- c(2,7)
log_likely <- (a_hat %*% x0) - (0.5 * (a_hat %*% (mul1+mul2)))


#Q1A with pecm
rm(list=ls())
# q1_dt <- matrix(c(3,7,2,4,4,7,6,9,5,7,4,8), byrow=TRUE,ncol=2) %>%
#   as.data.table()
# q1_grp <- c(1,1,1,2,2,2)
# priors <- c(0.5,0.5)
# C <- matrix(c(0,1,2,0), byrow=TRUE,ncol=2)
# q1_x0 <- data.table("V1" = 2, "V2" = 7) 
# 
# q1_pecm <- pecm(q1_dt, grouping = q1_grp, newdata = q1_x0, cmat = C, prior = priors)
# classify(q1_pecm)
v

#Q2
?lda
# 3 populations, 2 variables.
grp <- q2_data$grp
q2_dt <- q2_data[,!c("grp")]

#Q2a estimated priors and missclass cost 
#Plotting 
#classplot(q2_dt, grp, pch=grp,col=grp)

#q2a_pecm <- pecm(q2_dt, grouping = grp) # returns list
#names(q2a_pecm) #"groups" "means"  "prior"  "pecm"
#classify(q2a_pecm)
head(q2_dt)
q2a_x0 <- data.table("x1" = 5.78, "x2" = 0.16) 

q2a_pecm <- pecm(q2_dt, grouping = grp, newdata = q2a_x0)
classify(q2a_pecm)

#Q2b custom missclass matrix
C <- matrix(c(0,1,1,2,0,1,4,1,0), byrow=TRUE,ncol=3)
q2b_pecm <- pecm(q2_dt, grouping = grp, cmat = C)
q2b_pecm$prior
classify(q2b_pecm)

#Additional extensions 
# custom priors
priors <- c(0.4,0.4,0.2)
q2c_pecm <- pecm(q2_dt, grouping = grp, cmat = C, prior = priors)
# cmvar = FALSE <- different variance -> therefore quadratic ?
# cv <- cross validation


#Q3
head(q3_data)
?dist
d_man <- dist(q3_data, method = "manhattan")
d_eu <- dist(q3_data, method = "euclidean")
d_max <- dist(q3_data, method = "maximum")


hc_man <- hclust(d_man, method = "average")
hc_man_5 <- cutree(hc_man,5) # returns labels

kmean <- kmeans(q3_data, 5)
#kmean$cluster #use this to color




?hclust
hc_man$labels
summary(hc_man)
plot(hc_man)
plot(hc_man_5)
?plot





