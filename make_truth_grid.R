# function to make a grid of the truth data to compare to predicted
# will be the average intensity of each grid square
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