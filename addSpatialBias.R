##define sampling bias

#inputs:
# - strata - output from genstratumLam
# - probs - either NULL, then probabilities are generated randomly, or a vector of length equal to number of strata. Probabilities are then assigned in order e.g. stratum 1 gets the first value in the vector of probs



addSpatialBias <- function(strata = strata, 
                           maxprob = NULL, 
                           correlated = FALSE,
                           rho = NULL){ # included rho as an input so we can change it
  
  
  dim = c(max(strata$x), max(strata$y))
  minprob = maxprob/10 # keep the relative difference between maximum and minimum probabilities the same across different scenarios of maxprob (i.e. strength of bias is the same)
  
  
  
  if (correlated == FALSE){
    probseq <-  exp(seq(log(maxprob), log(minprob), length.out = dim[1]))
    lookup <- data.frame(grid = 1:dim[1], probs = probseq)
    
    n     <- dim[1]                    # length of vector
    rho   <- rho                   # desired correlation = cos(angle)
    theta <- acos(rho)             # corresponding angle
    x1    <- probseq       # fixed given data
    x2    <- rnorm(n, 2, 0.5)      # new random data
    X     <- cbind(x1, x2)         # matrix
    Xctr  <- scale(X, center=TRUE, scale=FALSE)   # centered columns (mean 0)
    
    Id   <- diag(n)                               # identity matrix
    Q    <- qr.Q(qr(Xctr[ , 1, drop=FALSE]))      # QR-decomposition, just matrix Q
    P    <- tcrossprod(Q)          # = Q Q'       # projection onto space defined by x1
    x2o  <- (Id-P) %*% Xctr[ , 2]                 # x2ctr made orthogonal to x1ctr
    Xc2  <- cbind(Xctr[ , 1], x2o)                # bind to matrix
    Y    <- Xc2 %*% diag(1/sqrt(colSums(Xc2^2)))  # scale columns to length 1
    
    x <- Y[ , 2] + (1 / tan(theta)) * Y[ , 1]     # final new vector
    cor(x1, x)                                    # check correlation = rho
    
    lookup$covariate <- x+max(x) # ensure positive
    
    for(i in 1:dim[1]){
      strata$stratprobs[strata$x == i] <- lookup$probs[i]
      strata$covariate[strata$x == i] <- lookup$covariate[i]
    }
    
  }
  
  if (correlated == TRUE){
    probseq <-  exp(seq(log(maxprob), log(minprob), length.out = dim[2]))
    lookup <- data.frame(grid = dim[2]:1, probs = probseq)#positively correlated
    
    #define new variable for covariate to explain bias - correlated with true sampling probability
    
    n     <- dim[2]                    # length of vector
    rho   <- rho                  # desired correlation = cos(angle)
    theta <- acos(rho)             # corresponding angle
    x1    <- probseq       # fixed given data
    x2    <- rnorm(n, 2, 0.5)      # new random data
    X     <- cbind(x1, x2)         # matrix
    Xctr  <- scale(X, center=TRUE, scale=FALSE)   # centered columns (mean 0)
    
    Id   <- diag(n)                               # identity matrix
    Q    <- qr.Q(qr(Xctr[ , 1, drop=FALSE]))      # QR-decomposition, just matrix Q
    P    <- tcrossprod(Q)          # = Q Q'       # projection onto space defined by x1
    x2o  <- (Id-P) %*% Xctr[ , 2]                 # x2ctr made orthogonal to x1ctr
    Xc2  <- cbind(Xctr[ , 1], x2o)                # bind to matrix
    Y    <- Xc2 %*% diag(1/sqrt(colSums(Xc2^2)))  # scale columns to length 1
    
    x <- Y[ , 2] + (1 / tan(theta)) * Y[ , 1]     # final new vector
    cor(x1, x)                                    # check correlation = rho
    
    lookup$covariate <- x + max(x) #ensure positive
    
    for(i in 1:dim[2]){
      strata$stratprobs[strata$y == i] <- lookup$probs[i]
      strata$covariate[strata$y == i] <- lookup$covariate[i]
    }
  }
  
  # add detection probability per point defined by which grid square it is in
  return(strata)
  
  
}




