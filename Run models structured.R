## Models with simulated data

#packages
library(INLA)
library(reshape2)

#import data
source("Generate field and sample.R") # note - once we've got the code finalised these need to be turned into functions

head(struct_dat) #this is the structured data


head(pp3) # this is the unstructured data


# Need to run several models...
# 1) unstructured only
# 2) structured only
# 3) joint model

#preparation - mesh construction - use the loc.domain argument

mesh <- inla.mesh.2d(loc.domain = dat[,c(1,2)],max.edge=c(10,20),cutoff=2, offset = c(5,20))
#plot the mesh to see what it looks like
plot(mesh)

##set the spde representation to be the mesh just created
spde <- inla.spde2.matern(mesh)

#make A matrix for structured data - for projection
struct_dat_A <- inla.spde.make.A(mesh = mesh, loc = as.matrix(struct_dat[,2:3]))

#make A matrix for unstructured data - for projection
pp3_A <- inla.spde.make.A(mesh = mesh, loc = as.matrix(pp3[,1:2]))

## Structured only

# create stack including presence data from structured, have Ntrials instead of expected
stk_struct <- inla.stack(data=list(y=struct_dat$presence, Ntrials = rep(1, nrow(struct_dat))),
                         effects=list(data.frame(interceptA=rep(1,length(struct_dat$x)), env = struct_dat$env), 
                                      Bnodes=1:spde$n.spde),
                         A=list(1,struct_dat_A),
                         tag="struct")

# what is Bnodes?
formulaN = y ~  interceptA + env + f(Bnodes, model = spde) -1

# Binomial cloglog link model
result.struct.binom <- inla(formulaN,family="binomial",
                            data=inla.stack.data(stk_struct),
                            control.predictor=list(A=inla.stack.A(stk_struct)),
                            control.family = list(link = "cloglog"),
                            Ntrials = inla.stack.data(stk_struct)$Ntrials
)




##project the mesh onto the initial simulated grid 100x100 cells in dimension
loglog <- function(x){return(1-exp(-exp(x)))}

proj1.struct.binom <- inla.mesh.projector(mesh,ylim=c(1,300),xlim=c(1,100),dims=c(100,300))
# need to maybe back transform in both ways - the cloglog = log() so then exp
xmean1.struct.binom <- exp(loglog(inla.mesh.project(proj1.struct.binom, result.struct.binom$summary.random$Bnodes$mean)))

##plot the estimated random field 
# plot with the original
library(fields)
# some of the commands below were giving warnings as not graphical parameters - I have fixed what I can
# scales and col.region did nothing on my version
par(mfrow=c(1,3))
image.plot(1:100,1:300,xmean1.struct.binom, col=tim.colors(),xlab='', ylab='',main="mean of r.f",asp=1)
image.plot(list(x=Lam$xcol*100, y=Lam$yrow*100, z=t(rf.s.c)), main='Truth', asp=1) # make sure scale = same
points(struct_dat[struct_dat[,4] %in% 0,2:3], pch=16, col='white') # many absences, few presences
points(struct_dat[struct_dat[,4] %in% 1,2:3], pch=16, col='black')

##plot the standard deviation of random field
xsd1 <- inla.mesh.project(proj1.struct.binom, result.struct$summary.random$Bnodes$sd)
#library(fields)
image.plot(1:100,1:300,xsd1, col=tim.colors(),xlab='', ylab='', main="sd of r.f",asp=1)


#biased to bottom of grid 

result.struct.binom$summary.fixed

#estimated intercept
int_est.struct <- result.struct.binom$summary.fixed[1,1] # way too low - assuming same scale (does it need back converting? Think so)

#estimated covariate value
cov_est.struct.binom <- result.struct.binom$summary.fixed[2,1] # wrong sign!! 


## VALIDATION

# look at area under the curve - think we need to create an ROC curve
# used to assess the accuracy of a continuous measurement for predicting a binary outcome... we maybe want the opposite?



# look at presences

# grid it and compare average abundance
# set up grid of 10X10 pixels
grid_points <- matrix(c(rep(rep(1:30,each=10),10)), ncol=100, nrow=300, byrow=F)
# show grid
plot(dat$y ~ dat$x, col = grid_points)

# sum average abundance by grid square for truth and predicted
grid_average <- function(grid_points, data){
  output <- rep(NA, length(1:max(grid_points)))
  data <- data-mean(data)
  for(i in 1:max(grid_points)){
    marker <- which(grid_points==i)
    output[i] <- mean(data[marker])
  }
  return(output)
}


# make sure mean scaled as we cannot accurately assess mean abundance
difference_struct_binom <- grid_average(grid_points, xmean1.struct.binom)-grid_average(grid_points, rf.s.c)

# now have difference in relative abundance per grid square
hist(difference_struct_binom)
