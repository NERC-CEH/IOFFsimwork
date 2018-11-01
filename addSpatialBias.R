##define sampling bias

#inputs:
# - strata - output from genstratumLam
# - probs - either NULL, then probabilities are generated randomly, or a vector of length equal to number of strata. Probabilities are then assigned in order e.g. stratum 1 gets the first value in the vector of probs



addSpatialBias <- function(strata = strata, probs = NULL){
  
  nstrata = length(unique(strata$stratum))
  
  lookup <- data.frame(stratum = 1:nstrata, probs = NA)
  
  if(is.null(probs)){
    lookup$probs <- runif(nrow(lookup), min = 0.1, max = 0.9)
  } else {
    lookup$probs <- probs
  }
  
  #add probabilites to strata output
  
  for(i in 1:nstrata){
    strata$stratprobs[strata$stratum == i] <- lookup$probs[i]
  }
  
  # add detection probability per point defined by which grid square it is in
  return(strata)

}




