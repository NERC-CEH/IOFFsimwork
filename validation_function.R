# FUNCTION FOR VALIDATION

# result = model output
# resolution = c(x,y)
# join.stack = joint stack of data and predictions
# model_type = indication which model result came from
# dat1 = original spatial field (truth)
# unstructured_data 
# structured_data
# choose table and/or plot

validation_function <- function(result, resolution, join.stack, model_type = c("unstructured", "structured", "joint", "joint2"), unstructured_data=NULL,
                             structured_data=NULL, dat1,
                             plotting = FALSE, summary_results = FALSE, qsize = qsize, absolute = TRUE, dim = dim){
  
#All comparisons are on the same scale as the truth is logged too!
  
# create index to extract predictions
index.pred.response <- inla.stack.index(join.stack, tag="pred.response")$data

# find the mean of the result and the standard deviation of predictions
m.prd <- result$summary.fitted.values$mean[index.pred.response]
sd.prd <- result$summary.fitted.values$sd[index.pred.response]

# if the model was structured then need to account for area it represents
# if we use 1 - should not be problem
#if(model_type == "structured"){
  #EZ <- (1-exp(-exp(m.prd)))-0.00000000000000008 #minus tiny number to avoid infinite values
  #psi <- (1-(1-EZ)^(1/(qsize^2)))
  #m.prd <- log(-log(1-psi))
  #}


# calculate differences
source('make_truth_grid.R')
if(absolute == TRUE){truth_grid <- make_truth_grid(c(10,10), dat1, c(dim[1],dim[2]), type='truth', absolute=TRUE)}
else{truth_grid <- make_truth_grid(c(10,10), dat1, c(dim[1],dim[2]), type='truth', absolute=FALSE)}

if(absolute == TRUE){
  differences <- m.prd-truth_grid # calculate differences
  method = "Absolute"
}
if(absolute == FALSE){
  differences <- (m.prd-mean(m.prd))-truth_grid
  m.prd <- m.prd - mean(m.prd)
  sd.prd <- sd.prd - mean(sd.prd)
  method = "Relative"
  }



if(plotting == TRUE){
#png(paste0(model_type, " ", method, " validation.png"), height = 1000, width = 3100, pointsize = 30)
par(mfrow=c(2,2))
par(mar = c(5.1, 4.1, 4.1, 3.5))
# Plot truth on grid scale
image.plot(seq(resolution[1]/2,dim[1],resolution[1]),seq(resolution[2]/2,dim[2],resolution[2]), 
             matrix(truth_grid, ncol=dim[2]/10, nrow=dim[1]/10), col=tim.colors(), xlab='', ylab='',main="Averaged truth",asp=1)
#predicted mean
image.plot(seq(resolution[1]/2,dim[1],resolution[1]),seq(resolution[2]/2,dim[2],resolution[2]), 
           matrix(m.prd, ncol=dim[2]/10, nrow=dim[1]/10), col=tim.colors(),xlab='', ylab='',main="Predicted mean intensity",asp=1)
image.plot(seq(resolution[1]/2,dim[1],resolution[1]),seq(resolution[2]/2,dim[2],resolution[2]), 
           matrix(sd.prd, ncol=dim[2]/10, nrow=dim[1]/10), col=tim.colors(),xlab='', ylab='',main="Predicted sd intensity",asp=1)
# relative differences
image.plot(seq(resolution[1]/2,dim[1],resolution[1]),seq(resolution[2]/2,dim[2],resolution[2]), 
           matrix(differences, ncol=dim[2]/10, nrow=dim[1]/10), col=tim.colors(),xlab='', ylab='',main=paste0(model_type, " ", method, "\ndifferences"),asp=1)
#dev.off()
}

if(summary_results == TRUE){
  MAE_differences <- abs(differences)
  correlation <- cor(m.prd, truth_grid)
  grid <- make_truth_grid(c(10,10), dat1, c(dim[1],dim[2]), type='grid')
  coefficients <- result$summary.fixed
  #ONLY want to transform predictions NOT coefficients
  
  summary_results = list(data.frame(Model = model_type,
                     MAE = mean(MAE_differences)),
                     correlation = correlation,
               coefficients = coefficients[,c(1,3,5,6)],     
               differences,
               worst_areas = unique(grid[which(MAE_differences>(mean(MAE_differences)+sd(MAE_differences)))]),
               best_areas = unique(grid[which(MAE_differences<(mean(MAE_differences)-sd(MAE_differences)))])
               )
  names(summary_results) <- c("Proto-table", "correlation", "coefficients", "All_differences", "Worst_grid_cells", "Best_grid_cells")
  return(summary_results)
}

}
