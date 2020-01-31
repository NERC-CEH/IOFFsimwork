## Models with simulated data

joint_model2 <- function(structured_data, unstructured_data, dat1, biasfield){
  
  #packages
  library(INLA)
  library(reshape2)
  library(rgeos)
  library(fields)
  
  max_x <- max(biasfield$x)
  max_y <- max(biasfield$y)
  
  
  #preparation - mesh construction - use the loc.domain argument
  
  mesh <- inla.mesh.2d(loc.domain = biasfield[,c(1,2)],max.edge=c(20,40),cutoff=2, offset = c(5,20))
  #plot the mesh to see what it looks like
  #plot(mesh)
  
  ##set the spde representation to be the mesh just created
  spde <- inla.spde2.matern(mesh)
  
  #make A matrix for structured data - should this be pulling the x and y coordinates for the location?
  structured_data_A <- inla.spde.make.A(mesh = mesh, loc = as.matrix(structured_data[,2:3]))
  
  #make A matrix for unstructured data
  unstructured_data_A <- inla.spde.make.A(mesh = mesh, loc = as.matrix(unstructured_data[,1:2]))
  
  
  # Joint model
  
  # One spatial field
  # Uses Simpson approach for PP data
  # Binomial model for PA data
  # Using cloglog
  
  
  # create integration stack
  
  loc.d <- t(matrix(c(0,0,max_x,0,max_x,max_y,0,max_y,0,0), 2))
  
  #make dual mesh
  dd <- deldir::deldir(mesh$loc[, 1], mesh$loc[, 2])
  tiles <- deldir::tile.list(dd)
  
  #make domain into spatial polygon
  domainSP <- SpatialPolygons(list(Polygons(
    list(Polygon(loc.d)), '0')))
  
  #intersection between domain and dual mesh
  poly.gpc <- as(domainSP@polygons[[1]]@Polygons[[1]]@coords, "gpc.poly")
  
  # w now contains area of voronoi polygons
  w <- sapply(tiles, function(p) rgeos::area.poly(rgeos::intersect(as(cbind(p$x, p$y), "gpc.poly"), poly.gpc)))
  
  #check some have 0 weight
  table(w>0)
  
  ##plot stuff
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
  
  A.pp <- rBind(imat, unstructured_data_A)
  
  #get covariate for integration points
  
  covariate = dat1$gridcov[Reduce('cbind', nearest.pixel(mesh$loc[,1], mesh$loc[,2], im(dat1$gridcov)))]
  
  
  #unstructured data stack with integration points
  
  stk_unstructured_data <- inla.stack(data=list(y=cbind(y.pp, NA), e = e.pp),
                                      effects=list(list(data.frame(interceptB=rep(1,nv+n)), env = c(covariate, unstructured_data$env)), list(uns_field=1:spde$n.spde, bias_field = 1:spde$n.spde)),
                                      A=list(1,A.pp),
                                      tag="unstructured_data")	
  
  #stack for structured data
  #note intercept with different name
  
  stk_structured_data <- inla.stack(data=list(y=cbind(NA, structured_data$presence), Ntrials = rep(1, nrow(structured_data))),
                                    effects=list(list(data.frame(interceptA=rep(1,length(structured_data$x)), env = structured_data$env)), list(str_field=1:spde$n.spde)),
                                    A=list(1,structured_data_A),
                                    tag="structured_data")
  
  ##NOTE: doesn't use the copy function initially
  
  stk <- inla.stack(stk_unstructured_data, stk_structured_data)
  
  # join.stack <- stk
  # 
  source("Create prediction stack.R")
  
  join.stack <- create_prediction_stack(stk, c(10,10), biasfield = biasfield, dat1 = dat1, mesh, spde)
  
  
  formulaJ = y ~  interceptA + interceptB + env + f(uns_field, model = spde) + f(str_field, copy = "uns_field", fixed = TRUE) + f(bias_field, model = spde) -1
  
  
  result <- inla(formulaJ,family=c("poisson", "binomial"),
                 data=inla.stack.data(join.stack),
                 control.predictor=list(A=inla.stack.A(join.stack), compute=TRUE),
                 control.family = list(list(link = "log"), 
                                       list(link = "cloglog")),
                 E = inla.stack.data(join.stack)$e,
                 Ntrials = inla.stack.data(join.stack)$Ntrials,
                 control.compute = list(cpo=TRUE, waic = TRUE, dic = TRUE)
  )
  
  
  ##project the mesh onto the initial simulated grid 100x100 cells in dimension
  proj1<-inla.mesh.projector(mesh,ylim=c(1,max_y),xlim=c(1,max_x),dims=c(max_x,max_y))
  ##pull out the mean of the random field for the NPMS model
  xmean1 <- inla.mesh.project(proj1, result$summary.random$uns_field$mean)
  xmean2 <- inla.mesh.project(proj1, result$summary.random$bias_field$mean)
  
  
  ##plot the estimated random field 
  # plot with the original
  library(fields)
  # some of the commands below were giving warnings as not graphical parameters - I have fixed what I can
  # scales and col.region did nothing on my version
  png("joint two fields model.png", height = 1000, width = 3000, pointsize = 30)
  par(mfrow=c(1,4))
  image.plot(1:max_x,1:max_y,xmean1, col=tim.colors(),xlab='', ylab='',main="mean of shared r.f",asp=1)
  image.plot(1:max_x,1:max_y,xmean2, col=tim.colors(),xlab='', ylab='',main="mean of bias r.f",asp=1)
  image.plot(list(x=dat1$Lam$xcol*100, y=dat1$Lam$yrow*100, z=t(dat1$rf.s)), main='Truth', asp=1) # make sure scale = same
  points(structured_data[structured_data[,4] %in% 0,2:3], pch=16, col='white') #absences
  points(structured_data[structured_data[,4] %in% 1,2:3], pch=16, col='black')
  
  ##plot the standard deviation of random field
  xsd1 <- inla.mesh.project(proj1, result$summary.random$uns_field$sd)
  
  image.plot(1:max_x,1:max_y,xsd1, col=tim.colors(),xlab='', ylab='', main="sd of r.f",asp=1)
  dev.off()
  
  result$summary.fixed
  
  return(list(join.stack = join.stack, result = result))
}