# Functions for multivariate course.
# Classification.

# Calculates posterior ecms
pecm <- function(x, grouping, prior, cmvar = TRUE, cv = FALSE,
   cmat = NULL, newdata = NULL) {
  grouping <- as.factor(grouping)
  if(missing(prior)) 
    prior <- as.numeric(table(grouping)/length(grouping))
  if(cmvar) {
    dalst <- lda(x = x, grouping = grouping, prior = prior)
    dalstcv <- lda(x = x, grouping = grouping, prior = prior, CV = TRUE)
  } 
  else {
    dalst <- qda(x = x, grouping = grouping, prior = prior)
    dalstcv <- qda(x = x, grouping = grouping, prior = prior, CV = TRUE)
  }
  dalst2 <- list(groups = grouping, means = dalst$means, prior = 
    dalst$prior, pecm = predict(dalst)$posterior)
  if(cv) dalst2$pecm <- dalstcv$posterior
  if(!is.null(newdata)) {
    dalst2$pecm <- predict(dalst, newdata = newdata)$posterior
    dalst2$newdata <- newdata
  }
  if(is.null(cmat)) {
    cmat <- 1 - diag(ncol(dalst2$pecm))  
  }
  else {
    if(any(diag(cmat)!=0)) stop("diagonal of cost matrix must be zero")
  }
  dalst2$pecm <- dalst2$pecm %*% t(cmat)
  dalst2
}

# Classifications and misclass matrix from pecm output
classify <- function(x) {
  grps <- levels(x$groups)
  grps <- factor(grps, levels = unique(grps))
  indx <- apply(x$pecm, 1, which.min)
  if(!is.null(x$newdata)) return(as.factor(grps[indx]))
  list(class = as.factor(grps[indx]), 
     classmat = table(x$groups,grps[indx]))
}

# Classification plot for two dimensional data
classplot <- function(x, grouping, prior, cmvar = TRUE, cv = FALSE, 
    cmat = NULL, newdata = NULL, nn1 = FALSE, ctree = FALSE, add = FALSE, 
    len = 250, main = "Classification Regions", xlab = colnames(x)[1], 
    ylab = colnames(x)[2], ...)
{
    if(missing(x) || ncol(x) != 2)
      stop("Dear NUS student: data must have two columns")
    if(!missing(cv))
       stop("Dear NUS student: what are you plotting here?")
    if(!missing(newdata))
       stop("Dear NUS student: newdata argument not allowed")

    if(!add) plot(x, main = main, xlab = xlab, ylab = ylab, ...)
  
    xp <- seq(min(x[,1],na.rm=TRUE), max(x[,1],na.rm=TRUE), length=len)
    yp <- seq(min(x[,2],na.rm=TRUE), max(x[,2],na.rm=TRUE), length=len)
    gridv <- expand.grid(xp, yp)
    colnames(gridv) <- colnames(x)
    if(nn1) {
      Z <- 1-class.ind(knn1(x, gridv, grouping))
      ng <- ncol(Z)
      if(ng <= 1) stop("must have more than one group")
      for(i in 1:ng) {
        zp <- apply(Z[,-i,drop=FALSE], 1, min) - Z[,i]
        contour(xp, yp, matrix(zp, len), add = T, levels = 0, drawlabels = FALSE)
      }
      return(invisible())
    }
    if(ctree) {
      grouping <- as.factor(grouping)
      levels(grouping) <- LETTERS[1:length(levels(grouping))]
      mdat <<- cbind(x, grouping = grouping)
      cush.tr <- tree(grouping ~ Tetrahydrocortisone + Pregnanetriol, mdat)
      partition.tree(cush.tr, add = TRUE)
      return(invisible())
    }
    Z <- pecm(x = x, grouping = grouping, prior = prior, cmvar = cmvar,
      cv = FALSE, cmat = cmat, newdata = gridv)$pecm
    ng <- ncol(Z)
    if(ng <= 1) stop("must have more than one group") 
    for(i in 1:ng) {
      zp <- apply(Z[,-i,drop=FALSE], 1, min) - Z[,i]
      contour(xp, yp, matrix(zp, len), add = T, levels = 0, drawlabels = FALSE)
    }
    
    invisible()
}

#From library nnet
class.ind <- function (cl) 
{
    n <- length(cl)
    cl <- as.factor(cl)
    x <- matrix(0, n, length(levels(cl)))
    x[(1:n) + n * (unclass(cl) - 1)] <- 1
    dimnames(x) <- list(names(cl), levels(cl))
    x
}
