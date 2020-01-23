# FUNCTION FOR VALIDATION

# result = model output
# resolution = c(x,y)
# join.stack = joint stack of data and predictions
# model_type = indication which model result came from
# dat1 = original spatial field (truth)
# unstructured_data 
# structured_data
# choose table and/or plot
# qsize - quadrat size
# absolute - absolute or relative to mean
# dim - dimensions of domain

validation_function <- function(result, resolution, join.stack, model_type = c("unstructured", "unstructuredcov", "unstructuredsf", 
                                                                               "structured", "joint", "jointcov", "jointtwo"), 
                                unstructured_data=NULL, structured_data=NULL, dat1,
                                plotting = FALSE, summary_results = FALSE, qsize = qsize, 
                                absolute = TRUE, dim = dim){
  
#All comparisons are on the same scale as the truth is logged too!
  
# create index to extract predictions
index.pred.response <- inla.stack.index(join.stack, tag="pred.response")$data

# find the mean of the result and the standard deviation of predictions
m.prd <- result$summary.fitted.values$mean[index.pred.response]
sd.prd <- result$summary.fitted.values$sd[index.pred.response]


# calculate differences
source('make_truth_grid.R')
if(absolute == TRUE){truth_grid <- make_truth_grid(resolution, dat1, c(dim[1],dim[2]), type='truth', absolute=TRUE)} else 
{truth_grid <- make_truth_grid(resolution, dat1, c(dim[1],dim[2]), type='truth', absolute=FALSE)}

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
png(paste0(model_type, " ", method, " validation.png"), height = 1000, width = 1000, pointsize = 25)
par(mfrow=c(2,2))
par(mar = c(5.1, 4.1, 4.1, 3.5))
# Plot truth on grid scale
image.plot(seq(resolution[1]/2,dim[1],resolution[1]),seq(resolution[2]/2,dim[2],resolution[2]), 
             matrix(truth_grid, ncol=dim[2]/resolution[2], nrow=dim[1]/resolution[1]), col=tim.colors(), xlab='', ylab='',main="Averaged truth",asp=1)
#predicted mean
image.plot(seq(resolution[1]/2,dim[1],resolution[1]),seq(resolution[2]/2,dim[2],resolution[2]), 
           matrix(m.prd, ncol=dim[2]/resolution[2], nrow=dim[1]/resolution[1]), col=tim.colors(),xlab='', ylab='',main="Predicted mean intensity",asp=1)
image.plot(seq(resolution[1]/2,dim[1],resolution[1]),seq(resolution[2]/2,dim[2],resolution[2]), 
           matrix(sd.prd, ncol=dim[2]/resolution[2], nrow=dim[1]/resolution[1]), col=tim.colors(),xlab='', ylab='',main="Predicted sd intensity",asp=1)
# relative differences
image.plot(seq(resolution[1]/2,dim[1],resolution[1]),seq(resolution[2]/2,dim[2],resolution[2]), 
           matrix(differences, ncol=dim[2]/resolution[2], nrow=dim[1]/resolution[2]), col=tim.colors(),xlab='', ylab='',main=paste0(model_type, " ", method, "\ndifferences"),asp=1)
dev.off()
}

if(plotting == FALSE){
  output <- list(truth_grid, m.prd)
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
               coefficients = coefficients[,c(1,3,5,6)]
               )
  names(summary_results) <- c("Proto-table", "correlation", "coefficients")
  if(plotting == TRUE){return(summary_results)}else{return(c(summary_results, output))}
}

}
