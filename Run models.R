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

#make A matrix for structured data
struct_dat_A <- inla.spde.make.A(mesh = mesh, loc = as.matrix(struct_dat[,3:4]))

#make A matrix for unstructured data
pp3_A <- inla.spde.make.A(mesh = mesh, loc = as.matrix(pp3[,1:2]))


# Unstructured only


stk_pp3 <- inla.stack(data=list(y=pp3$presence, e = rep(0, nrow(pp3))),
                      effects=list(data.frame(interceptB=rep(1,length(pp3$x)), env = pp3$env), 
                                   Bnodes=1:spde$n.spde),
                      A=list(1,pp3_A),
                      tag="pp3")	


formulaN = y ~  interceptB + env + f(Bnodes, model = spde) -1


result <- inla(formulaN,family="poisson",
               data=inla.stack.data(stk_pp3),
               control.predictor=list(A=inla.stack.A(stk_pp3)),
               control.family = list(link = "log"),
               E = inla.stack.data(stk_pp3)$e
)


##project the mesh onto the initial simulated grid 100x100 cells in dimension
proj1<-inla.mesh.projector(mesh,ylim=c(1,300),xlim=c(1,100),dims=c(100,300))
##pull out the mean of the random field for the NPMS model
xmean1 <- inla.mesh.project(proj1, result$summary.random$Bnodes$mean)

##plot the estimated random field 
# plot with the original
library(fields)
# some of the commands below were giving warnings as not graphical parameters - I have fixed what I can
# scales and col.region did nothing on my version
par(mfrow=c(1,3))
image.plot(1:100,1:300,xmean1, col=tim.colors(),xlab='', ylab='',main="mean of r.f",asp=1)
image.plot(list(x=Lam$xcol*100, y=Lam$yrow*100, z=t(rf.s.c)), main='Truth', asp=1) # make sure scale = same
points(pp3[,1:2], pch=16)

##plot the standard deviation of random field
xsd1 <- inla.mesh.project(proj1, result$summary.random$Bnodes$sd)
#library(fields)
image.plot(1:100,1:300,xsd1, col=tim.colors(),xlab='', ylab='', main="sd of r.f",asp=1)


#biased to bottom of grid

result$summary.fixed

#estimated intercept
int_est <- result$summary.fixed[1,1]

#estimated covariate value
cov_est <- result$summary.fixed[2,1]

library(reshape2)
truefield <- melt(rf.s.c)
estimatedfield <- melt(xmean1)
covartable <- melt(gridcov)

# this looks almost like someone's ribcage 
# shows the difference between estimate and true value?
plot(exp(int_est + cov_est*covartable$value + estimatedfield$value) ~ truefield$value, col = covartable$value*2)

# Structured only


# Joint (multiple versions possible)


