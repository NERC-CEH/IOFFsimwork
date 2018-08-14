##generate spatial field from log gaussian cox process
##this code is taken from the Simpson tutorial 


library(spatstat) 
win <- owin(c(0,3), c(0,3))

spatstat.options(npixel=300)

beta0 <- 3

sigma2x <- 0.2;      kappa <- 2

library(RandomFields) 
set.seed(1) 
lg.s <- rLGCP('matern', beta0, 
              var=sigma2x, scale=1/kappa, nu=1, win=win)

xy <- cbind(lg.s$x, lg.s$y)[,2:1]

Lam <- attr(lg.s, 'Lambda') 
summary(as.vector(rf.s <- log(Lam$v)))

par(mfrow=c(1,1)) 
library(fields) 
image.plot(list(x=Lam$yrow, y=Lam$xcol, z=rf.s), main='log-Lambda', asp=1) 
points(xy, pch=19)



##function splits the surface into a grid of defined size
source('Generate strata levels Lam.R')
par(mfrow = c(1,1))
strata1 <- genStrataLam(Lam, strata = 8, rows = 4, cols = 2)


## Create lookup table between strata and probabilities
## NOTE this section must be adjusted if number of strata change and/or desired probabilities change
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
dat <- stratumProbs(strata1, lookup$probs)


par(mfrow=c(1,1))
plot(dat$y ~ dat$x, pch = 20, col = factor(dat$stratprobs))
points(xy*100, pch=19, col= "white")

###now need to use strata in a different way to previously...sample from the points generated in the first section (xy) according to associated strata prob

#put points on same scale as strata
pp1 <- as.data.frame(xy*100)
names(pp1) <- c("x", "y")

pp2 <- merge(round(pp1), dat, by.x = c("x","y"), by.y = c("x","y"))
par(mfrow=c(1,1))
plot(dat$y ~ dat$x, pch = 20, col = factor(dat$stratprobs))
points(pp2, pch=19, col= pp2$stratprobs*10)

#thin points using the stratum-based detection probability
pp2$presence <- rbinom(nrow(pp2),1,pp2$stratprobs)

pp3 <- pp2[pp2$presence == 1,]


image.plot(list(x=Lam$yrow, y=Lam$xcol, z=rf.s), main='log-Lambda', asp=1) 
points(pp3$x/100, pp3$y/100, pch = 20)#note rescale again

#distribution of points is now biased to the lower half of the region. Therefore pp3 could be unstructured data collection

#how to simulate structured?

#basic approach - simulate stratified random set of sampling areas (5*5 points)

source("Sample from strata.R")
dat$sim2 <- dat$sim1 #no extra error added

s1 <- sampleStrata(dat, nsamp = 24, type = "Stratified")

#add neighbourhood

s2 <- data.frame()
for(i in 1:nrow(s1$Stratified)){
  s3 <- dat[dat$x %in% seq(s1$Stratified$x[i]-2, s1$Stratified$x[i]+2) & dat$y %in% seq(s1$Stratified$y[i]-2, s1$Stratified$y[i]+2),]
s2 <- rbind(s2, s3)
}

par(mfrow=c(1,1))
image.plot(list(x=Lam$yrow, y=Lam$xcol, z=rf.s), main='log-Lambda', asp=1) 
points(s2$x/100, s2$y/100, pch = 20, col = "blue")#note rescale again
points(xy, pch=20, col= "white")



#see which points are observed 

dat2 <- merge(pp2, s2, by.x = c("x", "x"), by.y = c("y","y"))

##need to generate new points?



