## Models with simulated data

unstructured_model <- function(unstructured_data, dat1, biasfield){

#packages
library(INLA)
library(reshape2)
library(deldir)
library(rgeos)
library(fields)
  
#preparation - mesh construction - use the loc.domain argument

mesh <- inla.mesh.2d(loc.domain = biasfield[,c(1,2)],max.edge=c(10,20),cutoff=2, offset = c(5,20))
#plot the mesh to see what it looks like
#plot(mesh)

##set the spde representation to be the mesh just created
spde <- inla.spde2.matern(mesh)

#make A matrix for unstructured data
unstructured_data_A <- inla.spde.make.A(mesh = mesh, loc = as.matrix(unstructured_data[,1:2]))

#make integration stack for unstructured data

#get dimensions
max_x <- max(biasfield$x)
max_y <- max(biasfield$y)

loc.d <- t(matrix(c(0,0,max_x,0,max_x,max_y,0,max_y,0,0), 2))

#make dual mesh
dd <- deldir::deldir(mesh$loc[, 1], mesh$loc[, 2])
tiles <- deldir::tile.list(dd)

#make domain into spatial polygon
domainSP <- SpatialPolygons(list(Polygons(list(Polygon(loc.d)), '0')))

#intersection between domain and dual mesh

poly.gpc <- as(domainSP@polygons[[1]]@Polygons[[1]]@coords, "gpc.poly")

# w now contains area of voronoi polygons
w <- sapply(tiles, function(p) rgeos::area.poly(rgeos::intersect(as(cbind(p$x,p$y), "gpc.poly"), poly.gpc)))

#check some have 0 weight
table(w>0)

# ##plot stuff
# par(mfrow=c(1,1))
# plot(mesh$loc, asp=1, col=(w==0)+1, pch=19, xlab='', ylab='')
# plot(dd, add=TRUE)
# lines(loc.d, col=3)

nv <- mesh$n
n <- nrow(unstructured_data)


#change data to include 0s for nodes and 1s for presences
y.pp <- rep(0:1, c(nv, n))

#add expectation vector (area for integration points/nodes and 0 for presences)
e.pp <- c(w, rep(0, n))

#diagonal matrix for integration point A matrix
imat <- Diagonal(nv, rep(1, nv))

#combine integration point A matrix with data A matrix
A.pp <- rbind(imat, unstructured_data_A)


#get covariate for integration points

covariate = dat1$gridcov[Reduce('cbind', nearest.pixel(
  mesh$loc[,1], mesh$loc[,2],
  im(dat1$gridcov)))]


# Create data stack
stk_unstructured_data <- inla.stack(data=list(y=y.pp, e = e.pp),
                      effects=list(list(data.frame(interceptB=rep(1,nv+n))), list(Bnodes=1:spde$n.spde)),
                      A=list(1,A.pp),
                      tag="unstructured_data")	

source("Create prediction stack.R")

join.stack <- create_prediction_stack(stk_unstructured_data, c(10,10), biasfield = biasfield, dat1 = dat1, mesh, spde)

formulaN = y ~  -1 + interceptB + f(Bnodes, model = spde)


result <- inla(formulaN,family="poisson",
               data=inla.stack.data(join.stack),
               control.predictor=list(A=inla.stack.A(join.stack), compute=TRUE),
               control.family = list(link = "log"),
               E = inla.stack.data(join.stack)$e
)

result$summary.fixed

# plot(result$marginals.fix[[1]], type='l', 
#      xlab="Log-lambda", ylab='Density') 
# abline(v=lambda, col=2) 


##project the mesh onto the initial simulated grid 
proj1<- inla.mesh.projector(mesh,ylim=c(1,max_y),xlim=c(1,max_x),dims=c(max_x,max_y))
##pull out the mean of the random field 
xmean1 <- inla.mesh.project(proj1, result$summary.random$Bnodes$mean)

##plot the estimated random field 
# plot with the original

# some of the commands below were giving warnings as not graphical parameters - I have fixed what I can
# scales and col.region did nothing on my version
par(mfrow=c(1,3))
image.plot(1:100,1:300,xmean1, col=tim.colors(),xlab='', ylab='',main="mean of r.f",asp=1)
#plot truth
image.plot(list(x=dat1$Lam$xcol*100, y=dat1$Lam$yrow*100, z=t(dat1$rf.s)), main='Truth', asp=1) # make sure scale = same
points(unstructured_data[,1:2], pch=16)

##plot the standard deviation of random field
xsd1 <- inla.mesh.project(proj1, result$summary.random$Bnodes$sd)
#library(fields)
image.plot(1:100,1:300,xsd1, col=tim.colors(),xlab='', ylab='', main="sd of r.f",asp=1)


#return from function
return(list(join.stack = join.stack, result = result))

}