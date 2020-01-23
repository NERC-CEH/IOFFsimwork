# Function to create the prediction stack

## Inputs:

#data_stack - stack of data for model
#resolution
#biasfield
#dat1 - original datafile of environmental covariate and bias
#mesh
#spde

create_prediction_stack <- function(data_stack, resolution, biasfield, dat1, mesh, spde){
  
  # Create a grid and sample at each grid corner
  pred.grid <- expand.grid(x=seq(resolution[1]/2,max(biasfield$x),resolution[1]), 
                           y=seq(resolution[2]/2, max(biasfield$y),resolution[2])) 
  # make grid
  dim(pred.grid) 
  
  # extract covariate values at these points
  pred.grid$cov <- dat1$gridcov[Reduce('cbind', nearest.pixel(
    pred.grid[,1], pred.grid[,2],
    im(dat1$gridcov)))]
  
  # need to create a new A matrix for the prediction data
  
  np = length(pred.grid[,1]) # number of points
  
  A.pred <- inla.spde.make.A(mesh, loc=as.matrix(pred.grid[,1:2]))
  
  # if clauses to automate correct creation depending on what is in the data stack
  if(length(data_stack$data$names$y) > 1){
    ys <- cbind(rep(NA, nrow(pred.grid)), rep(NA, nrow(pred.grid)))
    stack.pred.response <- inla.stack(data=list(y=ys),
                                      effects = list(list(data.frame(interceptA=rep(1,np))), env = pred.grid$cov, list(uns_field=1:spde$n.spde)),
                                      A=list(1,1, A.pred),
                                      tag='pred.response')
  } else if("Ntrials" %in% data_stack$data$names) {
    stack.pred.response <- inla.stack(data=list(y=NA, Ntrials = rep(1,np)),
                                      effects = list(list(data.frame(interceptA=rep(1,np))), env = pred.grid$cov, list(Bnodes=1:spde$n.spde)),
                                      A=list(1,1, A.pred),
                                      tag='pred.response')
  } else {
    stack.pred.response <- inla.stack(data=list(y=NA),
                                      effects = list(list(data.frame(interceptB=rep(1,np))), env = pred.grid$cov, list(Bnodes=1:spde$n.spde)),
                                      A=list(1,1, A.pred),
                                      tag='pred.response')   
    
      }
  
  
  # make inla stack using proper tag (pred.response)
  
  
  # join this stack and previous stack
  
  join.stack <- inla.stack(data_stack, stack.pred.response)
  return(join.stack)
}
