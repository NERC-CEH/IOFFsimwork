# Function to generate data for simulations

#function returns a simulated log cox gaussian process with or without the mean of this process related to an environmental variable. 

#inputs:
#lambda - mean of random field
#env.beta - coefficient of env variable
#plotdat - logical, plot data or not
#seed - value to set seed

###Functionality to add:

# - env correlated with spatial bias or not
# - vary complexity of environmental surface(?)



genData <- function(dim = c(100,300), lambda = 2, env.beta = 0.3, plotdat = TRUE, seed = 1, sigma2x = 0.2, kappa = 2){
 
  library(spatstat) 
  # owin creates an object of class "owin" which is an observation window in 2D
  # specify x and y coordinates
  win <- owin(c(0,dim[1]), c(0,dim[2])) # keeping this rectangular is really important to check for errors in the code - otherwise easy to get x and y confused
  
  # set number of pixels
  spatstat.options(npixel=c(dim[1],dim[2]))
  
  ##Creating environmental covariate and altering truth based on this
  # using Simpson tutorial again
  
  # can add covariate at the beginning - create arificial covariate
  # create y0 and x0 separately as rectangle
  y0 <- seq(win$yrange[1], win$yrange[2],
            length=spatstat.options()$npixel[2])
  x0 <- seq(win$xrange[1], win$xrange[2],
            length=spatstat.options()$npixel[1])
  # bit of a fudge but rounding this does work to give 3 levels (0,1,2)
  gridcov <- round(outer(y0, x0, function(x,y) cos(x) - sin(y-2))) 
  
  
  beta0 <- lambda # intercept/mu (increased to 5 to increase no.obs)
  beta1 <- env.beta # slope of relationship to environment
  
  sigma2x <- sigma2x;      kappa <- kappa
  
  library(RandomFields) 
  set.seed(seed) 
  # below generates a random point pattern (Log-Gaussian Cox process)
  # exponential of this is taken as the intensity of Poisson point process
  
  if(is.null(beta1)){
  
    lg.s <- rLGCP('matern', beta0, 
                var=sigma2x, scale=1/kappa, nu=1, win=win)
  } else {
  
  # with environmental covariate
    lg.s <- rLGCP('matern', im(beta0 + beta1*gridcov, xcol=x0, yrow=y0),
                  var=sigma2x, scale=1/kappa, nu=1, win=win)
  }
  
  # takes the coordinates of the randomly generated points
  xy <- cbind(lg.s$x, lg.s$y)
  
  # access attribute (Lambda) of lg.s object and create Lam 
  Lam <- attr(lg.s, 'Lambda') 
  summary(as.vector(rf.s <- log(Lam$v)))
  # can convert this to a Poisson point process with below
  #ppLam <- rpoispp(Lam)
  
  # plot the intensity and the points (don't think we do need to plot the points here - only sampled points)
  if(plotdat == TRUE){
    par(mfrow=c(1,1)) 
    library(fields) 
    image.plot(list(x=Lam$xcol, y=Lam$yrow, z=t(rf.s)), main='log-Lambda', asp=1) 
    #points(xy, pch=19)
  }
  
  if (plotdat == TRUE & !is.null(env.beta)){
  # covariate and point process
    par(mfrow=c(1,2), mar=c(2,2,1,1), mgp=c(1,0.5,0))
    image.plot(list(x=x0, y=y0, z=t(gridcov)), main='Covariate', asp=1)
    image.plot(list(x=x0, y=y0, z=t(rf.s)),
               main='log-Lambda', asp=1)
    #points(xy, pch=19)
  }
  
  dataout <- list(Lam = Lam, xy = xy, rf.s = rf.s, gridcov = gridcov)
  return(dataout)
  
}





  
  
  
  
