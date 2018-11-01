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
struct_dat_A <- inla.spde.make.A(mesh = mesh, loc = as.matrix(struct_dat[,3:4]))

#make A matrix for unstructured data - for projection
pp3_A <- inla.spde.make.A(mesh = mesh, loc = as.matrix(pp3[,1:2]))

## Structured only

# create stack including presence data from structured, e = expected vector of 0s, and effects
stk_struct <- inla.stack(data=list(y=struct_dat$presence, e = rep(0, nrow(struct_dat_A))),
                      effects=list(data.frame(interceptB=rep(1,length(struct_dat$x)), env = struct_dat$env), 
                                   Bnodes=1:spde$n.spde),
                      A=list(1,struct_dat_A),
                      tag="struct")	

# what is Bnodes?
formulaN = y ~  interceptB + env + f(Bnodes, model = spde) -1

# looks like this should predict abundance as it is Poisson? But we are using binomial data to do so?
result.struct <- inla(formulaN,family="poisson",
               data=inla.stack.data(stk_struct),
               control.predictor=list(A=inla.stack.A(stk_struct)),
               control.family = list(link = "log"),
               E = inla.stack.data(stk_struct)$e
)

# try just predicting presence/absence
result.struct.binom <- inla(formulaN,family="binomial",
                      data=inla.stack.data(stk_struct),
                      control.predictor=list(A=inla.stack.A(stk_struct)),
                      control.family = list(link = "logit"),
                      E = inla.stack.data(stk_struct)$e
)

##project the mesh onto the initial simulated grid 100x100 cells in dimension
proj1.struct<-inla.mesh.projector(mesh,ylim=c(1,300),xlim=c(1,100),dims=c(100,300))
##pull out the mean of the random field for the NPMS model
# think you might need to back transform - for binomial projection makes more sense transformed
xmean1.struct <- exp(inla.mesh.project(proj1, result.struct$summary.random$Bnodes$mean))

# binomial
logistic <- function(x){u <- exp(x)
return(u/(1+u))}
proj1.struct.binom <- inla.mesh.projector(mesh,ylim=c(1,300),xlim=c(1,100),dims=c(100,300))
xmean1.struct.binom <- logistic(inla.mesh.project(proj1, result.struct.binom$summary.random$Bnodes$mean))

##plot the estimated random field 
# plot with the original
library(fields)
# some of the commands below were giving warnings as not graphical parameters - I have fixed what I can
# scales and col.region did nothing on my version
par(mfrow=c(1,3))
image.plot(1:100,1:300,xmean1, col=tim.colors(),xlab='', ylab='',main="mean of r.f",asp=1)
image.plot(list(x=Lam$xcol*100, y=Lam$yrow*100, z=t(rf.s)), main='Truth', asp=1) # make sure scale = same
points(struct_dat[,2:3], pch=16)

##plot the standard deviation of random field
xsd1 <- inla.mesh.project(proj1, result.struct$summary.random$Bnodes$sd)
#library(fields)
image.plot(1:100,1:300,xsd1, col=tim.colors(),xlab='', ylab='', main="sd of r.f",asp=1)

## plotting the binomial vs the poisson - very different!
par(mfrow=c(1,3))
image.plot(1:100,1:300,xmean1.struct, col=tim.colors(),xlab='', ylab='',main="mean of Poisson",asp=1)
image.plot(1:100,1:300,xmean1.struct.binom, col=tim.colors(),xlab='', ylab='',main="mean of Binom",asp=1)
image.plot(list(x=Lam$xcol*100, y=Lam$yrow*100, z=t(rf.s)), main='Truth', asp=1) # make sure scale = same
points(struct_dat[struct_dat$presence %in% 0,2:3], pch=16, col="white") #absences
points(struct_dat[struct_dat$presence %in% 1,2:3], pch=16, col="black") #presences

#biased to bottom of grid - opposite directions depending on type of model

result.struct$summary.fixed

#estimated intercept
int_est <- result.struct$summary.fixed[1,1] # way too low - assuming same scale (does it need back converting?)

#estimated covariate value
cov_est <- result.struct$summary.fixed[2,1] # wrong sign!! 


## VALIDATION

# look at area under the curve

# look at presences

# grid it and compare average abundance
library(reshape2)
truefield <- melt(rf.s.c)
estimatedfield <- melt(xmean1)
covartable <- melt(gridcov)