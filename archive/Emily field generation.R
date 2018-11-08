## Code to manually create truth intensity

# I prefer to load all packages first
library(INLA)
library(gstat)
library(sp)
library(RColorBrewer)

#### Create mesh - then surface
# then need to simulate values at mesh points
# inc environmental covariate and average abundance

# create 100 by 100 square simulated area
xy <- expand.grid(seq(1, 100, 1),seq(1, 100, 1))
names(xy) <- c('x', 'y') # name the coordinates

# create a distance matrix to allow spatial autocorrelation to be included
distance_xy <- dist(xy, method = 'euclidean')
distance_xy <- distance_xy/max(distance_xy)
distance_xy <- as.matrix(distance_xy)

# then add in environmental covariate (values = 1:3) - with spatial autocorrelation
env_levels <- c(1,2,3)
xy$env[1] <- 1
for(i in 2:10000){
# scale probability of each environment level by distance
sampler <- c(rep(xy$env[i-1], 1000*(1-distance_xy[i,i-1])), 
             rep(env_levels[-which(env_levels == xy$env[i-1])], 1000*distance_xy[i,i-1]))
xy$env[i] <- sample(sampler, size=1)
}

# add abundance of species - beta for env = 0.2
# with poisson error
xy$count <- 1 + (0.2*xy$env)
xy$count <- rpois(10000, xy$count)
count <- data.frame(z = xy$count)
env <- data.frame(z = xy$env)

# create an inla mesh for the area
try_mesh <- inla.mesh.2d(loc = xy[,1:2], max.edge=7, cutoff=2)
# and plot the mesh to check it looks ok
plot(try_mesh)

# try to truth
abundance <- SpatialPointsDataFrame(coords=xy[,1:2], data=count)
# count map
spplot(obj=abundance, col.regions=brewer.pal(5, "Reds"), layout = c(2,1), cuts = 5, pretty=TRUE)
# env map
environment <- SpatialPointsDataFrame(coords=xy[,1:2], data=env)
spplot(obj=environment, col.regions=brewer.pal(3, "Greens"), layout = c(2,1), cuts = 5, pretty=TRUE)

## VERY MUCH NOT AS PRETTY AS FROM THE SPATSTAT PACKAGE!

# create the spde from mesh
spde <- inla.spde2.matern(try_mesh)

#### Using PointedSDMs 
# start by defining the polygon
Projection <- CRS("+proj=longlat +datum=WGS84")

# THIS STEP DOES NOT CURRENTLY WORK DUE TO LONG AND LAT
Pgon <- Polygons(list(region=Polygon(coords=xy[,1:2])), ID="region")
