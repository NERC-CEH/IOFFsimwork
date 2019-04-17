# the following needs to be repeated 100 times

run_function_multiple <- function(resolution=c(10,10), model_type = c("unstructured", "structured", "joint"), unstructured_data=NULL,
                                   structured_data=NULL, dat1,
                                   plotting = FALSE, summary_results = FALSE, biasfield = NULL){

source("Functions to generate data and sample.R")

if(model_type == "structured"){
source("Run models structured.R")
mod_1 <- structured_model(structured_data, dat1, biasfield, plotting = FALSE)

source("validation_function.R")
validation <- validation_function(result=mod_1[[2]], resolution=c(10,10), join.stack=mod_1[[1]], model_type="structured", 
                                    structured_data = structured_data, dat1 = dat1, summary_results=T, qsize = 1, absolute=TRUE, dim = dim, plotting = FALSE)
validation_r <- validation_function(result=mod_1[[2]], resolution=c(10,10), join.stack=mod_1[[1]], model_type="structured", 
                                      structured_data = structured_data, dat1 = dat1, summary_results=T, qsize = 1, absolute=FALSE, plotting = FALSE, dim = dim)
}
  
if(model_type == "unstructured"){
source("Run models.R")
mod_2 <- unstructured_model(unstructured_data, dat1, biasfield, dim = dim, plotting = FALSE)

source("validation_function.R")
validation <- validation_function(result=mod_2[[2]], resolution=c(10,10), join.stack=mod_2[[1]], model_type="unstructured", 
                                    unstructured_data = unstructured_data, dat1 = dat1, summary_results=T, absolute=TRUE, dim = dim, plotting = FALSE)
validation_r <- validation_function(result=mod_2[[2]], resolution=c(10,10), join.stack=mod_2[[1]], model_type="unstructured", 
                                      unstructured_data = unstructured_data, dat1 = dat1, summary_results=T, absolute=FALSE, plotting = FALSE, dim = dim)
}
  
if(model_type == "joint"){
source("Run models joint.R")
mod_3 <- joint_model(structured_data, unstructured_data, dat1, biasfield, plotting = FALSE)

source("validation_function.R")
validation <- validation_function(result=mod_3[[2]], resolution=c(10,10), join.stack=mod_3[[1]], model_type="joint", 
                                    unstructured_data = unstructured_data, structured_data = structured_data,
                                    dat1 = dat1, summary_results=T, absolute=TRUE, dim = dim, plotting = FALSE)
validation_r <- validation_function(result=mod_3[[2]], resolution=c(10,10), join.stack=mod_3[[1]], model_type="joint", 
                                      unstructured_data = unstructured_data, structured_data = structured_data,
                                      dat1 = dat1, summary_results=T, absolute=FALSE, dim = dim, plotting = FALSE)
}

return(list(validation, validation_r))
}