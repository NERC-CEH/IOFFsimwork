##generate spatial field from log gaussian cox process
##this code is taken from the Simpson tutorial 


library(spatstat) 
# owin creates an object of class "owin" which is an observation window in 2D
# specify x and y coordinates
win <- owin(c(0,1), c(0,3)) # keeping this rectangular is really important to check for errors in the code - otherwise easy to get x and y confused

# set number of pixels
spatstat.options(npixel=c(100,300))


beta0 <- 3 # intercept/mu

sigma2x <- 0.2;      kappa <- 2

library(RandomFields) 
set.seed(1) 
# below generates a random point pattern (Log-Gaussian Cox process)
# exponential of this is taken as the intensity of Poisson point process
lg.s <- rLGCP('matern', beta0, 
              var=sigma2x, scale=1/kappa, nu=1, win=win)

# takes the coordinates of the randomly generated points
xy <- cbind(lg.s$x, lg.s$y)

# access attribute (Lambda) of lg.s object and create Lam 
Lam <- attr(lg.s, 'Lambda') 
summary(as.vector(rf.s <- log(Lam$v)))
# can convert this to a Poisson point process with below
ppLam <- rpoispp(Lam)

# plot the intensity and the points (realisations?)
par(mfrow=c(1,1)) 
library(fields) 
image.plot(list(x=Lam$xcol, y=Lam$yrow, z=t(rf.s)), main='log-Lambda', asp=1) 
points(xy, pch=19)


##function splits the surface into a grid of defined size
source('Generate strata levels Lam.R')
par(mfrow = c(1,1))
# creates a dataframe of x and y coordinates, intensity and assigned grid square
strata1 <- genStrataLam(Lam, strata = 8, rows = 4, cols = 2)


## Create lookup table between strata and probabilities
## NOTE this section must be adjusted if number of strata change and/or desired probabilities change
# lookup is another dataframe of stratum number and associated probability
lookup <- data.frame(stratum = 1:8, probs = NA)

## Specify probs for each stratum (note this is effectively a detection probability per point NOT a probability of each cell being sampled i.e. does not sum to 1)
lookup$probs[lookup$stratum %in% c(1:4)] <- 0.8
lookup$probs[lookup$stratum %in% c(5,6)] <- 0.4
lookup$probs[lookup$stratum %in% c(7,8)] <- 0.2


#add probabilites to strata output
stratumProbs <- function(strata, probs){
  ncells <- nrow(strata)
  nstrata <- length(unique(strata$stratum))
  nperstratum <- ncells/nstrata
  for(i in 1:nstrata){
    strata$stratprobs[strata$stratum == i] <- probs[i]
  }
  return(strata)
}
# add detection probability per point defined by which grid square it is in
dat <- stratumProbs(strata1, lookup$probs)


par(mfrow=c(1,1))
plot(dat$y ~ dat$x, pch = 20, col = factor(dat$stratprobs))
points(xy*100, pch=19, col= "white") 
# strata appears to be original scale * 100 not sure why

###now need to use strata in a different way to previously...sample from the points generated in the first section (xy) according to associated strata prob

#put points on same scale as strata
pp1 <- as.data.frame(xy*100)
names(pp1) <- c("x", "y")

pp2 <- merge(round(pp1), dat, by.x = c("x","y"), by.y = c("x","y"))
par(mfrow=c(1,1))
plot(dat$y ~ dat$x, pch = 20, col = factor(dat$stratprobs))
points(pp2, pch=19, col= pp2$stratprobs*10)

#thin points using the stratum-based detection probability
#reducing also to presence absence here not abundance
pp2$presence <- rbinom(nrow(pp2),1,pp2$stratprobs)

# make it presence only data
pp3 <- pp2[pp2$presence == 1,]


image.plot(list(x=Lam$xcol, y=Lam$yrow, z=t(rf.s)), main='log-Lambda', asp=1) 
points(pp3$x/100, pp3$y/100, pch = 20)#note rescale again - plotting back on original

#distribution of points is now biased to the lower half of the region. Therefore pp3 could be unstructured data collection

#how to simulate structured? 

#basic approach - simulate stratified random set of sampling areas (5*5 points)

source("Sample from strata.R")
dat$sim2 <- dat$sim1 #no extra error added

s1 <- sampleStrata(dat, nsamp = 24, type = "Stratified")

#add neighbourhood - areas of human occupancy?

s2 <- data.frame()
for(i in 1:nrow(s1$Stratified)){
  s3 <- dat[dat$x %in% seq(s1$Stratified$x[i]-2, s1$Stratified$x[i]+2) & dat$y %in% seq(s1$Stratified$y[i]-2, s1$Stratified$y[i]+2),]
  s2 <- rbind(s2, s3)
}

par(mfrow=c(1,1))
image.plot(list(x=Lam$xcol, y=Lam$yrow, z=t(rf.s)), main='log-Lambda', asp=1) 
points(s2$x/100, s2$y/100, pch = 20, col = "blue")#note rescale again
points(xy, pch=20, col= "white")



#see which points are observed 

dat2 <- merge(pp2, s2, by.x = c("x", "x"), by.y = c("y","y"))


##need to generate new points? - otherwise we assume the same individuals are observed with both processes which is unrealistic. 
# 
# newpoints <- rpoispp(lambda = Lam)	
# points(newpoints$y ~ newpoints$x, pch = 20, col = "white")
# 
# #see which points are observed 	#see which points are observed 
# newpoints_sc <- data.frame(x1 = round(newpoints$x*100), y1 = round(newpoints$y*100))	dat2 <- merge(pp2, s2, by.x = c("x", "x"), by.y = c("y","y"))
# dat2 <- merge(newpoints_sc, s2, by.x = c("x1", "y1"), by.y = c("x","y"))	
# names(dat2) <- c("x1", "y1", "sim1", "stratum", "stratprobs", "sim2")	
# head(dat2)	
# dat2$x_sc <- dat2$x/100	
# dat2$y_sc <- dat2$y/100	
# points(dat2$y_sc ~ dat2$x_sc, pch = 20, col = "red")	
# #dat2 now holds locations of points observed in structured survey





