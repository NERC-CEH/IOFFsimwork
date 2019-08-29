#function to generate structured data samples equally split between strata


sampleStructured <- function(PPdat, biasfield, nsamp = NULL, qsize = NULL, plotdat = TRUE){
  
  source("Sample from strata - subsetting.R")
  biasfield$sim2 <- biasfield$sim1 #no extra error added
  
  success <- FALSE
  
  while(!success) {
  
  s1 <- sampleStrata(biasfield, nsamp = nsamp, type = "Stratified")
  
  # will now return a list of dataframes
  
  # sample from strata nsamp/nstrata times
  # sim2 now has extra error
  
  ##need to generate new points! - otherwise we assume the same individuals are observed with both processes which is unrealistic. 
  
  newpoints <- rpoispp(lambda = PPdat$Lam) # generate random point pattern 
  #see which points are observed 	
  newpoints_sc <- data.frame(x1 = round(newpoints$x), y1 = round(newpoints$y))
  
  #for each dataframe in list s1
  struct_dat_list <- list()
  for(j in 1:length(s1$Stratified)){
    
    s1$Stratified[[j]]$samp_id <- 1:nrow(s1$Stratified[[j]])
    # give each sample an id
  
    #add neighbourhood - these reflect the sampling areas
    buffer <- (qsize-1)/2
  
    s2 <- data.frame()
    for(i in 1:nrow(s1$Stratified[[j]])){
    
      s3 <- biasfield[biasfield$x %in% seq(s1$Stratified[[j]]$x[i]-buffer, 
                                           s1$Stratified[[j]]$x[i]+buffer) 
                      & biasfield$y %in% seq(s1$Stratified[[j]]$y[i]-buffer,
                                             s1$Stratified[[j]]$y[i]+buffer),]
    
    #take the one with no extra error added since taken from biasfield
    
      s3$samp_id <- s1$Stratified[[j]]$samp_id[i]
    
      s2 <- rbind(s2, s3)
    }
    
      dat2 <- merge(newpoints_sc, s2, by.x = c("x1", "y1"), by.y = c("x","y"))
      names(dat2) <- c("x1", "y1", "sim1", "stratum", "stratprobs", "covariate", "sim2","samp_id")
  
      dat2$x_sc <- dat2$x
      dat2$y_sc <- dat2$y
  
      #dat2 now holds presence locations of points observed in structured survey
  
      #add absences to structured data
  
      all_samps <- unique(s2$samp_id)
  
      struct_dat <- data.frame(samp_id = all_samps)
      struct_dat$x <- s1$Stratified[[j]]$x[match(struct_dat$samp_id, s1$Stratified[[j]]$samp_id)]
      struct_dat$y <- s1$Stratified[[j]]$y[match(struct_dat$samp_id, s1$Stratified[[j]]$samp_id)]
  
  
      struct_dat$presence <- match(all_samps, dat2$samp_id)
      struct_dat$presence[!is.na(struct_dat$presence)] <- 1 #convert to p/a
      struct_dat$presence[is.na(struct_dat$presence)] <- 0
  

      success <- sum(struct_dat$presence) > 0
   
      #add env covariate to structured data
      if(!is.null(PPdat$gridcov)){
        
        struct_dat$env <- PPdat$gridcov[as.matrix(struct_dat[,3:2])]    
        # with y-coor, get env cov from gridcov
      }
        
      if (plotdat == TRUE){
        
        par(mfrow=c(1,1))
        image.plot(list(x=PPdat$Lam$xcol, y=PPdat$Lam$yrow, z=t(PPdat$rf.s)), main='log-Lambda', asp=1) 
        points(s2$x, s2$y, pch = 20, col = "blue")# structured sample
        #points(newpoints$y ~ newpoints$x, pch = 20, col = "white") #new rpoispp
        points(dat2$y_sc ~ dat2$x_sc, pch = 20, col = "red") #presence
      
        }
    
      struct_dat_list[[j]] <- struct_dat 
    }
    
  }
  

  names(struct_dat_list) <- c("left", "right", "midleft", "midright", "bottom", "top", "midbottom", "midtop", "whole")
  
  return(struct_dat_list)

}