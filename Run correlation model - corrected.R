
# Function to run joint model with spatial correlation.
#
# Function will return a list of data stack and the fitted model.

correlation_model <- function(unstructured_data,
                              structured_data,
                              dat1,
                              biasfield,
                              dim,
                              plotting = FALSE,
                              mesh.edge = c(20,40),
                              mesh.offset = c(5,20),
                              resolution = c(10,10)){
  
  # packages
  library(INLA)
  library(reshape2)
  library(rgeos)
  library(fields)
  library(deldir)
  
  ###############################
  
  # preparation - mesh contruction - use the loc.domain argument ####
  
  mesh <- inla.mesh.2d(loc.domain = biasfield[, c(1,2)],
                       max.edge = mesh.edge,
                       cutoff = 2,
                       offset = mesh.offset)
  
  # make SPDE ####
  spde <- inla.spde2.matern(mesh = mesh, alpha = 2)
  
  
  # make A matrix
  structured_data_A <- inla.spde.make.A(mesh = mesh,
                                        loc = as.matrix(structured_data[, 2:3]))
  
  unstructured_data_A <- inla.spde.make.A(mesh = mesh,
                                          loc = as.matrix(unstructured_data[ , 1:2]))
  
  
  # integration stack for unstructured data ####
  max_x <- max(biasfield$x)
  max_y <- max(biasfield$y)
  
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
  
  
  nv <- mesh$n
  n <- nrow(unstructured_data)
  
  #change data to include 0s for nodes and 1s for presences
  y.pp <- rep(0:1, c(nv, n))
  
  #add expectation vector (area for integration points/nodes and 0 for presences)
  e.pp <- c(w, rep(0, n))
  
  #diagonal matrix for integration point A matrix
  imat <- Diagonal(nv, rep(1, nv))
  
  A.pp <- rBind(imat, unstructured_data_A)  # new A matrix for unstructured
  
  #get covariate for integration points
  #refer plot 'covariate'
  
  covariate = dat1$gridcov[Reduce('cbind', 
                                  nearest.pixel(mesh$loc[,1], 
                                                mesh$loc[,2], 
                                                im(dat1$gridcov)))]
  
  # create data stacks ####
  
  #unstructured data stack with integration points
  stk_unstructured_data <- inla.stack(
    
    data = list(y = cbind(y.pp, NA),
                e = e.pp),
    
    effects = list(list(data.frame(interceptB = rep(1, nv+n)),
                        env = c(covariate, unstructured_data$env)),
                   
                   list(data.frame(uns_field = 1:spde$n.spde),
                        uns_field.group = rep(1, spde$n.spde))),  # named Group 1
    
    A = list(1, A.pp),
    
    tag = "unstructured_data")
  
  #structured data stack
  stk_structured_data <- inla.stack(
    
    data = list(y = cbind(NA, structured_data$presence),
                Ntrials = rep(1, nrow(structured_data))),
    
    effects = list(list(data.frame(interceptA = rep(1, length(structured_data$x)),
                                   env = structured_data$env)),
                   
                   list(data.frame(uns_field = 1:spde$n.spde),
                        uns_field.group = rep(2, spde$n.spde))), # named Group 2
    
    A = list(1, structured_data_A),
    
    tag = "structured_data"
  )
  
  # combine stacks
  stk <- inla.stack(stk_unstructured_data, stk_structured_data)
  
  # prediction stack ####
  source("Create prediction stack for correlation model.R")
  
  join.stack <- create_prediction_stack_corr(data_stack = stk,
                                        resolution = resolution,
                                        biasfield = biasfield,
                                        dat1 = dat1,
                                        mesh = mesh,
                                        spde = spde)
  
  
  ##############################
  
  # fit model ####
  
  formulaC = y ~ -1 + interceptA + interceptB + env +
    f(uns_field, model = spde, group = uns_field.group, control.group = list(model = 'exchangeable'))
  
  result <- inla(formulaC, 
                 family = c("poisson", "binomial"),
                 data = inla.stack.data(join.stack),
                 control.predictor = list(A = inla.stack.A(join.stack),
                                          compute = TRUE),
                 control.family = list(list(link = "log"),
                                       list(link = "cloglog")),
                 E = inla.stack.data(join.stack)$e,
                 Ntrials = inla.stack.data(join.stack)$Ntrials,
                 control.compute = list(dic = FALSE, 
                                        cpo = FALSE,
                                        waic = FALSE))
  
  # no plotting for grouped model
  
  return(list(join.stack = join.stack, result = result))
  
}