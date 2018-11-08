### This script contains the functions needed to produce validation outputs

# these will be a table of coefficient values and uncertainty
# figure of predicted intensity and difference to observed
# table of root mean square error


## CREATE PREDICTION DATA STACK ##

# set up prediction grid, same size as original BUT lower resolution 
# one point in centre of each 10X10
# these are the NAs we want to predict
create_prediction_stack <- function(data_stack, resolution){

pred.grid <- expand.grid(x=seq(resolution[1]/2,100,resolution[1]), y=seq(resolution[2]/2,300,resolution[2])) # make grid
dim(pred.grid) # 300,2 = much better than before

# extract covariate values at these points
pred.grid$cov <- dat1$gridcov[Reduce('cbind', nearest.pixel(
  pred.grid[,1], pred.grid[,2],
  im(dat1$gridcov)))]

# need to create a new A matrix for the prediction data

np = length(pred.grid[,1]) # number of points

A.pred <- inla.spde.make.A(mesh, loc=as.matrix(pred.grid[,1:2]))

table(apply(A.pred,1,sum)) # a check that it is formatted correctly. First row should = 1

# make inla stack using proper tag (pred.response)
stack.pred.response <- inla.stack(data=list(y=NA),
                                  effects = list(list(data.frame(interceptP=rep(1,np))), env = pred.grid$cov, list(Bnodes=1:spde$n.spde)),
                                  A=list(1,1,A.pred),
                                  tag='pred.response')

# join this stack and previous stack

join.stack <- inla.stack(data_stack, stack.pred.response)
return(join_stack)
}

## run the combined model ### WILL NOT BE A FUNCTION HERE

join.output <- inla(formulaN,family="poisson",
                    data=inla.stack.data(join.stack),
                    control.predictor=list(A=inla.stack.A(join.stack), compute=TRUE),
                    control.family = list(link = "log")
)

## EXTRACT DIFFERENCES AND PLOT ##

# function to make a grid of the truth data to compare to predicted
# will be the average intensity of each 10X10
make_truth_grid <- function(resolution, dat1, dimensions){
  
  grid = matrix(NA, nrow=dimensions[2], ncol=dimensions[1])
  grid_numbers <- 1:prod(dimensions/resolution)
  # loop for y values
  for(j in 1:(dimensions[2]/resolution[2])){
    index.y <- seq((j*resolution[2]-9),(j*resolution[2]),1)
    temp_grid_numbers <- grid_numbers[index.y]
    # loop for x values
    for(i in 1:(dimensions[1]/resolution[1])){
      index.x <- seq((i*resolution[1]-9),(i*resolution[1]),1)
      grid[index.y,index.x] <- temp_grid_numbers[i]
    }
  }
  
  # sum average abundance by grid square for truth
  output <- rep(NA, length(1:max(grid)))
  data <- dat1$rf.s-mean(dat1$rf.s)
  for(i in 1:max(grid)){
    marker <- which(grid==i)
    output[i] <- mean(data[marker])
  }
  return(output)
}

# result = model output
# resolution = c(x,y)
# join_stack = joint stack of data and predictions
# model_type = indication which model result came from
# dat1 = original spatial field (truth)
# unstructured_data 
# structured_data
# choose table and/or plot
validation <- function(result, resolution, join_stack, model_type = c("unstructured", "structured", "joint"), unstructured_data=NULL,
                             structured_data=NULL, dat1,
                             plot = F, table = F){

index.pred.response <- inla.stack.index(join.stack, tag="pred.response")$data

m.prd <- result$summary.fitted.values$mean[index.pred.response]
sd.prd <- result$summary.fitted.values$sd[index.pred.response]

if(plot == T){
par(mfrow=c(1,4))
#truth
image.plot(list(x=dat1$Lam$xcol*100, y=dat1$Lam$yrow*100, z=t(dat1$rf.s)), main='Truth', asp=1) # make sure scale = same
if(is.null(unstructured_data)==FALSE){points(unstructured_data[,1:2], pch=16, col="grey")}
if(is.null(structured_data)==FALSE){points(structured_data[structured_data[,4] %in% 0,2:3], pch=16, col='white') #absences
  points(structured_data[structured_data[,4] %in% 1,2:3], pch=16, col='black')}
#predicted mean
image.plot(seq(resolution[1]/2,100,resolution[1]),seq(resolution[2]/2,300,resolution[2]), 
           matrix(exp(m.prd), ncol=30, nrow=10), col=tim.colors(),xlab='', ylab='',main="Predicted mean intensity",asp=1)
image.plot(seq(resolution[1]/2,100,resolution[1]),seq(resolution[2]/2,300,resolution[2]), 
           matrix(exp(sd.prd), ncol=30, nrow=10), col=tim.colors(),xlab='', ylab='',main="Predicted sd intensity",asp=1)
# relative differences
truth_grid <- make_truth_grid(c(10,10), dat1, c(100,300)) # grid truth and take averaged
differences <- (exp(m.prd)-mean(exp(m.prd)))-truth_grid # calculate differences
image.plot(seq(resolution[1]/2,100,resolution[1]),seq(resolution[2]/2,300,resolution[2]), 
           matrix(differences, ncol=30, nrow=10), col=tim.colors(),xlab='', ylab='',main="Relative differences",asp=1)
}

if(table == T){
  table = list(data.frame(Model = model_type,
                     RMSE = mean(sqrt(differences^2))),
               differences,
               worst_areas <- unique(grid[which(differences>(mean(differences)+sd(differences)))]),
               best_areas <- unique(grid[which(differences<(mean(differences)-sd(differences)))])
               )
  names(table) <- c("Error", "All_differences", "Worst_grid_cells", "Best_grid_cells")
  return(table)
}

}