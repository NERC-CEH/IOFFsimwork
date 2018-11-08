# FUNCTION FOR VALIDATION
# result = model output
# resolution = c(x,y)
# join_stack = joint stack of data and predictions
# model_type = indication which model result came from
# dat1 = original spatial field (truth)
# unstructured_data 
# structured_data
# choose table and/or plot
validation_function <- function(result, resolution, join_stack, model_type = c("unstructured", "structured", "joint"), unstructured_data=NULL,
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
source('make_truth_grid.R')
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