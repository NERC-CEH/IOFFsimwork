#function to generate structured data samples equally split between strata


sampleStructured <- function(PPdat, biasfield, nsamp = NULL, qsize = NULL, plotdat = TRUE){
  source("Sample from strata.R")
  biasfield$sim2 <- biasfield$sim1 #no extra error added
  
  success <- FALSE
  
  while(!success) {
  
  s1 <- sampleStrata(biasfield, nsamp = nsamp, type = "Stratified", plot = FALSE)
  
  s1$Stratified$samp_id <- 1:nrow(s1$Stratified)
  
  #add neighbourhood - these reflect the sampling areas
  
  buffer <- (qsize-1)/2
  
  
  s2 <- data.frame()
  for(i in 1:nrow(s1$Stratified)){
    s3 <- biasfield[biasfield$x %in% seq(s1$Stratified$x[i]-buffer, s1$Stratified$x[i]+buffer) & biasfield$y %in% seq(s1$Stratified$y[i]-buffer, s1$Stratified$y[i]+buffer),]
    s3$samp_id <- s1$Stratified$samp_id[i]
    s2 <- rbind(s2, s3)
  }
  
  ##need to generate new points! - otherwise we assume the same individuals are observed with both processes which is unrealistic. 
  
  
  
  newpoints <- rpoispp(lambda = PPdat$Lam)
  #see which points are observed 	
  newpoints_sc <- data.frame(x1 = round(newpoints$x), y1 = round(newpoints$y))
  dat2 <- merge(newpoints_sc, s2, by.x = c("x1", "y1"), by.y = c("x","y"))
  names(dat2) <- c("x1", "y1", "sim1", "stratum", "stratprobs", "covariate", "sim2","samp_id")
  #head(dat2)
  dat2$x_sc <- dat2$x
  dat2$y_sc <- dat2$y
  
  
  #dat2 now holds locations of points observed in structured survey
  
  #add absences to structured data
  
  all_samps <- unique(s2$samp_id)
  
  struct_dat <- data.frame(samp_id = all_samps)
  struct_dat$x <- s1$Stratified$x[match(struct_dat$samp_id, s1$Stratified$samp_id)]
  struct_dat$y <- s1$Stratified$y[match(struct_dat$samp_id, s1$Stratified$samp_id)]
  
  
  struct_dat$presence <- match(all_samps, dat2$samp_id)
  struct_dat$presence[!is.na(struct_dat$presence)] <- 1 #convert to p/a
  struct_dat$presence[is.na(struct_dat$presence)] <- 0
  
  success <- sum(struct_dat$presence) >= 0.1*nsamp 
  
  }
  
  #add env covariate to structured data
  if(!is.null(PPdat$gridcov)){
  
  struct_dat$env <- PPdat$gridcov[as.matrix(struct_dat[,3:2])]
  }
  
  
  if (plotdat == TRUE){
    par(mfrow=c(1,1))
    image.plot(list(x=PPdat$Lam$xcol, y=PPdat$Lam$yrow, z=t(PPdat$rf.s)), main='log-Lambda', asp=1) 
    points(s2$x, s2$y, pch = 20, col = "blue")#note rescale again
    points(newpoints$y ~ newpoints$x, pch = 20, col = "white")
    points(dat2$y_sc ~ dat2$x_sc, pch = 20, col = "red")
    
  }
  
  return(struct_dat)


}