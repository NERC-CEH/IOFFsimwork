# function to make a grid of the truth data to compare to predicted
# will be the average intensity of each grid square
make_truth_grid <- function(resolution, dat1, 
                            dimensions, 
                            type=c('truth', 'grid'), 
                            absolute = TRUE){
  
  # set up the same as prediction stack
  grid <- expand.grid(x=seq(resolution[1]/2, max(dimensions),resolution[1]), 
                     y=seq(resolution[2]/2, max(dimensions),resolution[2])) 
  
  # extract abundance by point for truth
  output <- rep(NA, length(1:max(grid)))
  if(absolute == TRUE){
    data <- dat1$rf.s
  }
  if(absolute == FALSE){
    data <- dat1$rf.s-mean(dat1$rf.s)
  }
  # extract abundance values from the truth
  grid$abundance <- data[Reduce('cbind', nearest.pixel(
  grid[,1], grid[,2],
  im(data)))]

  if(type=='truth'){return(grid$abundance)}
  if(type=='grid'){return(grid)}
}