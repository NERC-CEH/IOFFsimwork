##define sampling bias


addSpatialBias <- function(strata = strata, 
                           maxprob = NULL, 
                           correlated = FALSE,
                           rho = NULL){ # included rho as an input so we can change it
  
  
  dim = c(max(strata$x), max(strata$y))
  minprob = maxprob/10 # keep the relative difference between maximum and minimum probabilities the same across different scenarios of maxprob (i.e. strength of bias is the same)
  
  
  if (correlated == FALSE){
    probseq <-  exp(seq(log(maxprob), log(minprob), length.out = dim[1]))
    
    if(rho == 1){
      lookup <- data.frame(grid = 1:dim[1], probs = probseq)
      lookup$covariate <- lookup$probs
    } else {
    
    
    #reorder to avoid perfect gradient
    #probseq <- probseq[c(2,5,1,3,6,9,7,10,8,4,20,15,16,11,13,14,12,19,17,18,29,27,22,21,23,26,25,30,24,28,38,32,39,33,37,34,36,35,31,40,44,50,46,43,48,41,47,49,45,42,60,59,51,56,54,52,53,55,58,57,63,68,65,66,69,64,62,67,61,70,79,71,78,75,72,80,74,73,77,76,89,85,88,87,81,83,84,90,82,86,95,93,96,94,99,97,100,91,92,98,109,107,110,108,105,103,104,102,106,101,116,115,113,120,118,117,111,114,119,112,129,125,130,122,127,121,126,124,123,128,131,140,138,135,132,139,136,133,134,137,147,144,149,145,142,148,141,143,146,150,154,153,151,160,157,156,158,152,159,155,163,165,162,169,164,161,170,166,168,167,179,172,171,177,175,176,173,174,180,178,185,183,188,186,190,182,184,189,181,187,192,191,200,197,195,198,193,194,196,199,207,203,202,206,204,208,201,205,209,210,220,219,217,215,212,218,214,216,211,213,230,229,227,224,228,225,226,221,222,223,237,239,234,233,240,231,235,238,236,232,243,246,242,248,247,244,241,245,250,249,255,259,253,256,258,251,257,254,260,252,269,265,261,262,263,270,264,268,267,266,276,278,274,280,277,271,279,273,275,272,283,282,289,287,285,284,290,288,281,286,292,297,293,296,295,298,291,294,300,299)]
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
    }
    
    for(i in 1:dim[1]){
      strata$stratprobs[strata$x == i] <- lookup$probs[i]
      strata$covariate[strata$x == i] <- lookup$covariate[i]
    }
    
    
  }
  
  if (correlated == TRUE){
    
    probseq <-  exp(seq(log(maxprob), log(minprob), length.out = dim[2]))
    
    
    if(rho == 1){
      lookup <- data.frame(grid = dim[2]:1, probs = probseq)#positively correlated
      lookup$covariate <- lookup$probs
    } else {
    
    
    #reorder to avoid perfect gradient
    #probseq <- probseq[c(2,5,1,3,6,9,7,10,8,4,20,15,16,11,13,14,12,19,17,18,29,27,22,21,23,26,25,30,24,28,38,32,39,33,37,34,36,35,31,40,44,50,46,43,48,41,47,49,45,42,60,59,51,56,54,52,53,55,58,57,63,68,65,66,69,64,62,67,61,70,79,71,78,75,72,80,74,73,77,76,89,85,88,87,81,83,84,90,82,86,95,93,96,94,99,97,100,91,92,98,109,107,110,108,105,103,104,102,106,101,116,115,113,120,118,117,111,114,119,112,129,125,130,122,127,121,126,124,123,128,131,140,138,135,132,139,136,133,134,137,147,144,149,145,142,148,141,143,146,150,154,153,151,160,157,156,158,152,159,155,163,165,162,169,164,161,170,166,168,167,179,172,171,177,175,176,173,174,180,178,185,183,188,186,190,182,184,189,181,187,192,191,200,197,195,198,193,194,196,199,207,203,202,206,204,208,201,205,209,210,220,219,217,215,212,218,214,216,211,213,230,229,227,224,228,225,226,221,222,223,237,239,234,233,240,231,235,238,236,232,243,246,242,248,247,244,241,245,250,249,255,259,253,256,258,251,257,254,260,252,269,265,261,262,263,270,264,268,267,266,276,278,274,280,277,271,279,273,275,272,283,282,289,287,285,284,290,288,281,286,292,297,293,296,295,298,291,294,300,299)]
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
    
    }
    
    for(i in 1:dim[2]){
      strata$stratprobs[strata$y == i] <- lookup$probs[i]
      strata$covariate[strata$y == i] <- lookup$covariate[i]
    }
  }
  
  # add detection probability per point defined by which grid square it is in
  return(strata)
  
  
}




