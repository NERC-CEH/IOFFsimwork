## Models with simulated data

#packages
library(INLA)
library(reshape2)

#import data
source("Functions to generate data and sample.R") # note - once we've got the code finalised these need to be turned into functions

head(structured_data) #this is the structured data


head(unstructured_data) # this is the unstructured data


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

#make A matrix for structured data - should this be pulling the x and y coordinates for the location?
structured_data_A <- inla.spde.make.A(mesh = mesh, loc = as.matrix(structured_data[,3:4]))

#make A matrix for unstructured data
unstructured_data_A <- inla.spde.make.A(mesh = mesh, loc = as.matrix(unstructured_data[,1:2]))


# Joint model

# One spatial field
# Uses (as far as I can tell) Simpson approach for PP data
# Binomial model for PA data
# Using cloglog


stk_unstructured_data <- inla.stack(data=list(y=cbind(unstructured_data$presence, NA), e = rep(0, nrow(unstructured_data))),
                      effects=list(data.frame(interceptB=rep(1,length(unstructured_data$x)), env = unstructured_data$env), 
                                   Bnodes=1:spde$n.spde),
                      A=list(1,unstructured_data_A),
                      tag="unstructured_data")	


#note intercept with different name


stk_struct <- inla.stack(data=list(y=cbind(NA, structured_data$presence), Ntrials = rep(1, nrow(structured_data))),
                      effects=list(data.frame(interceptA=rep(1,length(structured_data$x)), env = structured_data$env), 
                                   Bnodes=1:spde$n.spde),
                      A=list(1,structured_data_A),
                      tag="struct")

##NOTE: doesn't use the copy function initially


stk <- inla.stack(stk_unstructured_data, stk_struct)


formulaJ = y ~  interceptB + interceptA + env + f(Bnodes, model = spde) -1


result <- inla(formulaJ,family=c("poisson", "binomial"),
               data=inla.stack.data(stk),
               control.predictor=list(A=inla.stack.A(stk)),
               control.family = list(list(link = "log"), 
                                     list(link = "cloglog")),
               E = inla.stack.data(stk)$e,
               Ntrials = inla.stack.data(stk)$Ntrials
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
image.plot(1:100,1:300,exp(xmean1), col=tim.colors(),xlab='', ylab='',main="mean of r.f",asp=1)
image.plot(list(x=Lam$xcol*100, y=Lam$yrow*100, z=t(rf.s.c)), main='Truth', asp=1) # make sure scale = same

##plot the standard deviation of random field
xsd1 <- inla.mesh.project(proj1, result$summary.random$Bnodes$sd)

image.plot(1:100,1:300,xsd1, col=tim.colors(),xlab='', ylab='', main="sd of r.f",asp=1)


#biased to bottom of grid - any chance the environmental variable was included? 

result$summary.fixed


#estimated covariate value
cov_est <- result$summary.fixed[3,1]


truefield <- melt(rf.s.c)
estimatedfield <- melt(xmean1)
covartable <- melt(gridcov)

par(mfrow=c(1,1))
plot(exp(cov_est*covartable$value + estimatedfield$value) ~ truefield$value, col = covartable$value+2)

# Structured only


# Joint (multiple versions possible)

# VALIDATION - grid based approach

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
difference_joint <- grid_average(grid_points, xmean1)-grid_average(grid_points, rf.s.c)


