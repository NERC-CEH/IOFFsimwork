# the following needs to be repeated 100 times

run_function_multiple <- function(resolution=c(10,10), model_type = c("unstructured", "structured", "joint"), unstructured_data=NULL,
                                   structured_data=NULL, dat1,
                                   plot = F, summary_results = F, biasfield = NULL){

# generate all the data
source("setParams.R")  
source("Functions to generate data and sample.R")

if(model_type == "structured"){
source("Run models structured.R")
mod_1 <- structured_model(structured_data, dat1, biasfield)

source("validation_function_multiple.R")
validation <- validation_function_multiple(result=mod_1[[2]], resolution=c(10,10), join.stack=mod_1[[1]], model_type="structured", 
                                             structured_data = structured_data, dat1 = dat1, plot=F, summary_results=T)
}
  
if(model_type == "unstructured"){
source("Run models.R")
mod_2 <- unstructured_model(unstructured_data, dat1, biasfield)

source("validation_function.R")
validation <- validation_function(result=mod_2[[2]], resolution=c(10,10), join.stack=mod_2[[1]], model_type="unstructured", 
                                      unstructured_data = unstructured_data, dat1 = dat1, plot=T, summary_results=T)
}
  
if(model_type == "joint"){
source("Run models joint.R")
mod_joint <- joint_model(structured_data, unstructured_data, dat1, biasfield)

source("validation_function.R")
validation <- validation_function(result=mod_joint[[2]], resolution=c(10,10), join.stack=mod_joint[[1]], model_type="joint", 
                                      unstructured_data = unstructured_data, structured_data = structured_data,
                                      dat1 = dat1, plot=T, summary_results=T)
}

return(validation)
}