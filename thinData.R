# thin data using stratum probabilities

#input:
# - PPdat - output from genData
# - strata - output from addSpatialBias


thinData <- function(PPdat = PPdat, bias = bias){


  #put points on same scale as strata
  pp1 <- as.data.frame(PPdat$xy*100)
  names(pp1) <- c("x", "y")
  
  #add spatial bias info to PP data
  pp2 <- merge(round(pp1), bias, by.x = c("x","y"), by.y = c("x","y"))
  
  #thin points using the stratum-based detection probability
  #reducing also to presence absence here not abundance
  pp2$presence <- rbinom(nrow(pp2),1,pp2$stratprobs)
  
  # make it presence only data
  pp3 <- pp2[pp2$presence == 1,]
  
  
  image.plot(list(x=PPdat$Lam$xcol, y=PPdat$Lam$yrow, z=t(PPdat$rf.s)), main='log-Lambda', asp=1) 
  points(pp2$x/100, pp2$y/100, pch = 20, col = "white")
  points(pp3$x/100, pp3$y/100, pch = 20)#note rescale again - plotting back on original

  thinpp <- pp3
  return(thinpp)
  
}