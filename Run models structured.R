## Models with simulated data

structured_model <- function(structured_data, dat1, biasfield){
  

#packages
library(INLA)
library(reshape2)
library(fields)
  

max_x <- max(biasfield$x)
max_y <- max(biasfield$y)
  

#preparation - mesh construction - use the loc.domain argument

mesh <- inla.mesh.2d(loc.domain = biasfield[,c(1,2)],max.edge=c(10,20),cutoff=2, offset = c(5,20))
#plot the mesh to see what it looks like
plot(mesh)

##set the spde representation to be the mesh just created
spde <- inla.spde2.matern(mesh)

#make A matrix for structured data - for projection
structured_data_A <- inla.spde.make.A(mesh = mesh, loc = as.matrix(structured_data[,2:3]))


## Structured only

# create stack including presence data from structured, have Ntrials instead of expected
stk_structured <- inla.stack(data=list(y=structured_data$presence, Ntrials = rep(1, nrow(structured_data))),
                         effects=list(data.frame(interceptA=rep(1,length(structured_data$x)), env = structured_data$env), 
                                      Bnodes=1:spde$n.spde),
                         A=list(1,structured_data_A),
                         tag="structured")


source("Create prediction stack.R")

join.stack <- create_prediction_stack(stk_structured, c(10,10), biasfield = biasfield, dat1 = dat1)


# what is Bnodes?
formulaN = y ~  interceptA + env + f(Bnodes, model = spde) -1

# Binomial cloglog link model
result.struct.binom <- inla(formulaN,family="binomial",
                            data=inla.stack.data(join.stack),
                            control.predictor=list(A=inla.stack.A(join.stack), compute=TRUE),
                            control.family = list(link = "cloglog"),
                            Ntrials = inla.stack.data(join.stack)$Ntrials
)


##project the mesh onto the initial simulated grid 
loglog <- function(x){return(1-exp(-exp(x)))}

proj1.struct.binom <- inla.mesh.projector(mesh,ylim=c(1,max_y),xlim=c(1,max_x),dims=c(max_x,max_y))
# need to maybe back transform in both ways - the cloglog = log() so then exp
xmean1.struct.binom <- exp(loglog(inla.mesh.project(proj1.struct.binom, result.struct.binom$summary.random$Bnodes$mean)))

##plot the estimated random field 
# plot with the original

# some of the commands below were giving warnings as not graphical parameters - I have fixed what I can
# scales and col.region did nothing on my version
par(mfrow=c(1,3))
image.plot(1:max_x,1:max_y,xmean1.struct.binom, col=tim.colors(),xlab='', ylab='',main="mean of r.f",asp=1)
image.plot(list(x=dat1$Lam$xcol*100, y=dat1$Lam$yrow*100, z=t(dat1$rf.s)), main='Truth', asp=1) # make sure scale = same
points(structured_data[structured_data[,4] %in% 0,2:3], pch=16, col='white') # many absences, few presences
points(structured_data[structured_data[,4] %in% 1,2:3], pch=16, col='black')

##plot the standard deviation of random field
xsd1 <- inla.mesh.project(proj1.struct.binom, result.struct.binom$summary.random$Bnodes$sd)
#library(fields)
image.plot(1:max_x,1:max_y,xsd1, col=tim.colors(),xlab='', ylab='', main="sd of r.f",asp=1)


#biased to bottom of grid 

result.struct.binom$summary.fixed

return(list(join.stack = join.stack, result = result.struct.binom))

}
