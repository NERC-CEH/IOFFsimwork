##generate spatial field from log gaussian cox process
##this code is taken from the Simpson tutorial 


library(spatstat) 
# owin creates an object of class "owin" which is an observation window in 2D
# specify x and y coordinates
win <- owin(c(0,3), c(0,3)) # keeping this rectangular is really important to check for errors in the code - otherwise easy to get x and y confused

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


##Creating environmental covariate and altering truth based on this
# using Simpson tutorial again

# can add covariate at the beginning - create arificial covariate
# create y0 and x0 separately as rectangle
y0 <- x0 <- seq(win$xrange[1], win$xrange[2],
                length=spatstat.options()$npixel)
# bit of a fudge but rounding this does work to give 3 levels (0,1,2)
gridcov <- round(outer(x0, y0, function(x,y) cos(x) - sin(y-2))) 
set.seed(1)
lg.s.c <- rLGCP('matern', im(beta0 + beta1*gridcov, xcol=x0, yrow=y0),
                var=sigma2x, scale=1/kappa, nu=1, win=win)

##Mesh building
library(INLA)
# loc.d must be the outside corners of the region
loc.d <- data.frame(x=c(win$xrange, win$xrange[2], win$xrange[1], win$xrange[1]), 
                    y=c(rep(win$yrange[1],2), rep(win$yrange[2],2), win$yrange[1]))
mesh <- inla.mesh.2d(loc.domain=loc.d, offset=c(.4,.1), max.edge=c(0.1,1.5), cutoff=0.25) # not quite sure why mesh not rectangular

par(mar=c(0,0,0,0))
plot(mesh, main='')
points(xy, col=4, pch=19); lines(loc.d, col=3)




