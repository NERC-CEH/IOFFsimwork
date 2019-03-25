# FUNCTION FOR VALIDATION
# result = model output
# resolution = c(x,y)
# join.stack = joint stack of data and predictions
# model_type = indication which model result came from
# dat1 = original spatial field (truth)
# unstructured_data 
# structured_data
# choose table and/or plot
validation_function <- function(result, resolution, join.stack, model_type = c("unstructured", "structured", "joint"), unstructured_data=NULL,
                             structured_data=NULL, dat1,
                             plot = F, summary_results = F, qsize = qsize){
  
if(model_type == "structured"){transformation <- function(x){1-exp(-exp(x))}}else{transformation <- function(x){exp(x)}}
  
index.pred.response <- inla.stack.index(join.stack, tag="pred.response")$data

m.prd <- result$summary.fitted.values$mean[index.pred.response]
sd.prd <- result$summary.fitted.values$sd[index.pred.response]

if(model_type == "structured"){
  EZ <- (1-exp(-exp(m.prd)))-0.00000000000000008 #minus tiny number to avoid infinite values
  psi <- (1-(1-EZ)^(1/(qsize^2)))
  m.prd <- log(-log(1-psi))
  }



# calculate differences
source('make_truth_grid.R')
truth_grid <- make_truth_grid(c(10,10), dat1, c(100,300), type='truth') # grid truth and take averaged
differences <- m.prd-truth_grid # calculate differences

if(plot == T){
par(mfrow=c(1,4))
# #truth
# image.plot(list(x=dat1$Lam$xcol*100, y=dat1$Lam$yrow*100, z=t(dat1$rf.s)), main='Truth', asp=1) # make sure scale = same
# if(is.null(unstructured_data)==FALSE){points(unstructured_data[,1:2], pch=16, col="grey")}
# if(is.null(structured_data)==FALSE){points(structured_data[structured_data[,4] %in% 0,2:3], pch=16, col='white') #absences
#   points(structured_data[structured_data[,4] %in% 1,2:3], pch=16, col='black')}
image.plot(seq(resolution[1]/2,100,resolution[1]),seq(resolution[2]/2,300,resolution[2]), 
             matrix(truth_grid, ncol=30, nrow=10), col=tim.colors(),xlab='', ylab='',main="Averaged truth",asp=1)
#predicted mean
image.plot(seq(resolution[1]/2,100,resolution[1]),seq(resolution[2]/2,300,resolution[2]), 
           matrix(m.prd, ncol=30, nrow=10), col=tim.colors(),xlab='', ylab='',main="Predicted log lambda",asp=1)
image.plot(seq(resolution[1]/2,100,resolution[1]),seq(resolution[2]/2,300,resolution[2]), 
           matrix(sd.prd, ncol=30, nrow=10), col=tim.colors(),xlab='', ylab='',main="Predicted sd intensity",asp=1)
# relative differences
image.plot(seq(resolution[1]/2,100,resolution[1]),seq(resolution[2]/2,300,resolution[2]), 
           matrix(differences, ncol=30, nrow=10), col=tim.colors(),xlab='', ylab='',main="Relative differences",asp=1)
}

if(summary_results == T){
  RSE_differences <- sqrt(differences^2)
  grid <- make_truth_grid(c(10,10), dat1, c(100,300), type='grid')
  coefficients <- result$summary.fixed
  #back transform all of the coefficient values so they are comparable
  if(length(which(row.names(coefficients) %in% "interceptA")==T)>0){coefficients[row.names(coefficients) %in% "interceptA",] <- 
    1-exp(-exp(coefficients[row.names(coefficients) %in% "interceptA",]))}
  if(length(which(row.names(coefficients) %in% "interceptB")==T)>0){coefficients[row.names(coefficients) %in% "interceptB",] <- 
    exp(coefficients[row.names(coefficients) %in% "interceptB",])}
  if(length(which(row.names(coefficients) %in% "interceptB")==T)==0){coefficients[row.names(coefficients) %in% "env",] <- 
    1-exp(-exp(coefficients[row.names(coefficients) %in% "env",]))}else{coefficients[row.names(coefficients) %in% "env",] <- 
      exp(coefficients[row.names(coefficients) %in% "env",])}
  
  summary_results = list(data.frame(Model = model_type,
                     RMSE = mean(RSE_differences)),
               coefficients = coefficients[,c(1,3,5,6)],     
               differences,
               worst_areas = unique(grid[which(RSE_differences>(mean(RSE_differences)+sd(RSE_differences)))]),
               best_areas = unique(grid[which(RSE_differences<(mean(RSE_differences)-sd(RSE_differences)))])
               )
  names(summary_results) <- c("Proto-table", "coefficients", "All_differences", "Worst_grid_cells", "Best_grid_cells")
  return(summary_results)
}

}
