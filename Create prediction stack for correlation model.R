# Function to create the prediction stack

## Inputs:

#data_stack - stack of data for model
#resolution
#biasfield
#dat1 - original datafile of environmental covariate and bias
#spde

create_prediction_stack_corr <- function(data_stack, 
                                    resolution, 
                                    biasfield, 
                                    dat1, 
                                    mesh, 
                                    spde){
  
  pred.grid <- expand.grid(x=seq(resolution[1]/2,
                                 max(biasfield$x),
                                 resolution[1]), 
                           y=seq(resolution[2]/2, 
                                 max(biasfield$y),
                                 resolution[2])) # make grid
  
  dim(pred.grid) 
  
  # extract covariate values at these points
  pred.grid$cov <- dat1$gridcov[Reduce('cbind', 
                                       nearest.pixel(pred.grid[,1], 
                                                     pred.grid[,2],
                                                     im(dat1$gridcov))
                                       )]
  
  # need to create a new A matrix for the prediction data
  
  np = length(pred.grid[,1]) # number of points
  
  A.pred <- inla.spde.make.A(mesh, loc=as.matrix(pred.grid[,1:2]))
  
  # if clauses to automate correct creation depending on what is in the data stack
  if(length(data_stack$data$names$y) > 1){
    
    ys <- cbind(rep(NA, nrow(pred.grid)), rep(NA, nrow(pred.grid)))
    
    stack.pred.response <- 
      
      inla.stack(data=list(y=ys),
                 
                 effects = list(list(data.frame(interceptA=rep(1,np))),
                                env = pred.grid$cov, 
                                list(data.frame(uns_field=1:spde$n.spde),
                                uns_field.group = rep(1, spde$n.spde))),
                 
                 A=list(1,1, A.pred),
                 
                 tag='pred.unstructured')
    
    stack.pred.response2 <- 
      
      inla.stack(data=list(y=ys),
                 
                 effects = list(list(data.frame(interceptA=rep(1,np))),
                                env = pred.grid$cov, 
                                list(data.frame(uns_field=1:spde$n.spde),
                                uns_field.group = rep(2, spde$n.spde))),
                 
                 A=list(1,1, A.pred),
                 
                 tag='pred.structured')
    
  } 
  
  
  # make inla stack using proper tag (pred.response)
  
  
  # join this stack and previous stack
  
  join.stack <- inla.stack(data_stack, stack.pred.response, stack.pred.response2)
  return(join.stack)
}
