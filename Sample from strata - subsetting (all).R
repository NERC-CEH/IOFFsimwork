is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol

sampleStrata <- function(strata, 
                         nsamp = 100, 
                         type = c("Stratified", "Unstructured", "Intelligent"),
                         serror = 0.01, 
                         uerror= 0.1, 
                         ubias = 0.05, 
                         plot = TRUE)
  {
  
  if(any(nsamp > nrow(strata))) 
  {stop("Too many samples for data, reduce nsamp")}
  
  if(any(nsamp == 0)) 
  {stop("Specify more than 0 samples per required method")}
  
  if(is.null(strata$stratprobs))
  {stop("Must provide stratum probabilities from stratumProbs function")}
  
  outlist <- list()
  
  if("Stratified" %in% type){
    
    sp <- split(strata, list(strata$stratum))  # split data according to stratum
    
    nstrata <- length(unique(strata$stratum))
    
    if(nsamp[1] < nstrata)
    {stop("Number of samples is smaller than number of strata")}
    
    if(!is.wholenumber(nsamp[1]/nstrata))
    {warning("Stratified method: Number of samples does not divide exactly by number of strata, number of samples per strata will be rounded up to the nearest integer")}
    

    nsampstrat <- ceiling(nsamp[1]/nstrata)#note rounds up to nearest integer
    
    samples <- lapply(sp, function(x) x[sample(1:nrow(x), nsampstrat, FALSE),])
    # in each stratum, sample equal number of times (nsampstrat)

    Strat_samp <- do.call(rbind, samples)  
    # combine all the samples taken in each stratum
    
    Strat_samp$sim2 <- Strat_samp$sim2 + rnorm(length(Strat_samp$sim2),0,serror)
    #add error
    
    
    # filter to take required strata only - to make restricted dat
    
    #make a list of all possible combination of restricted area
    res_list = list()
    
    # collect all posible horizontal strips
    count = 1
    for(j in 1:(rows-1)){
      for(r in 1:(rows-(j-1))){
        # horizontal strips
        res_list[[count]] <- (cols*(r-1)+1):((cols*(r-1)+1)+(cols*j)-1)
        count = count + 1
      }
    }
    
    # collect all possible vertical strips
    for(j in 1:(cols-1)){
      for(r in 1:(cols-(j-1))){
        # vertical strips
        
        all_seq = vector()
        for(k in 1:j){
          new_seq = seq(from = r+k-1, to = (cols*(rows-1)+r+k-1), by = cols)
          all_seq = c(all_seq, new_seq)
        }
        
        res_list[[count]] <- all_seq
        count = count + 1
      }
    }
    
    #create another list of all combination samples
    # combine samples from wanted strata only
    samp_list <- list()
    for(s in 1:length(res_list)){
      
      res_samp <- data.frame()
    
      for(m in res_list[[s]]){
        res_samp <- rbind(res_samp, Strat_samp[Strat_samp$stratum == m,])
      }
      
      samp_list[[s]] <- res_samp 
      
    }
    
    samp_list[[length(res_list)+1]] <- Strat_samp 
    
   
    
    ##############################################################
    
    
    if(plot == TRUE){
      par(mfrow=c(3,3))
      for(p in 1:length(samp_list)){
        plot(strata$y ~ strata$x, pch = 20, col = factor(strata$stratprobs), main = "Stratified")
        points(samp_list[[p]]$y ~ samp_list[[p]]$x, col = "white", pch = 20)
      }
      
    }
    
    outlist[["Stratified"]] <- samp_list
  }
  
  
  if("Unstructured" %in% type){
    #take NMPS sample - weighted random sampling
    #use 24 samples (roughly proportional to CS sample size)
    if(length(nsamp) == 1){nunstr = nsamp[1]} else {nunstr = nsamp[2]}
    Unstr_samp <- strata[sample(1:nrow(strata),nunstr, FALSE, prob = strata$stratprobs),]
    Unstr_samp$sim2 <- Unstr_samp$sim2 + rnorm(length(Unstr_samp$sim2),0,uerror) + ubias #add error and bias
    if(plot == TRUE){
      plot(strata$y ~ strata$x, pch = 20, col = factor(strata$stratprobs), main = "Unstructured")
      points(Unstr_samp$y ~ Unstr_samp$x, col = "white", pch = 20)
    }
    outlist[["Unstructured"]] <- Unstr_samp
  }
  
  if("Intelligent" %in% type){
    #take NMPS sample - weighted random sampling
    #use 24 samples (roughly proportional to CS sample size)
    if(length(nsamp) == 1){nunstr = nsamp[1]
    } else if(length(nsamp) == 3) {nunstr = nsamp[3]
    } else nunstr = nsamp[2]
    probint <- 1/strata$stratprobs
    stratintprob <- as.numeric(tapply(probint,strata$stratum,sum)/sum(probint))
    nstrata <- length(unique(strata$stratum))
    Int_samp <- list()
    if(any(!is.wholenumber(nunstr*stratintprob))){warning("Intelligent method: Non-integer sample numbers have been produced so sample numbers will be rounded up to the nearest integer")}
    for(i in 1:nstrata){
      Int_samp[[i]] <- strata[strata$stratum == i,][sample(1:nrow(strata[strata$stratum == i,]), ceiling(nunstr*stratintprob[i]), FALSE),]
    }
    Int_samp <- do.call(rbind, Int_samp)
    Int_samp$sim2 <- Int_samp$sim2 + rnorm(length(Int_samp$sim2),0,serror)#add error
    if(plot == TRUE){
      plot(strata$y ~ strata$x, pch = 20, col = factor(strata$stratprobs), main = "Intelligent")
      points(Int_samp$y ~ Int_samp$x, col = "white", pch = 20)
    }
    outlist[["Intelligent"]] <- Int_samp
  }
  
  return(outlist)
}
