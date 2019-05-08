# the following needs to be repeated 100 times

run_function_multiple <- function(resolution=c(10,10), model_type = c("unstructured", "structured", "joint", "unstructuredcov", "jointcov", "joint2"), unstructured_data=NULL,
                                   structured_data=NULL, dat1,
                                   plotting = FALSE, summary_results = FALSE, biasfield = NULL,
                                   nsamp = NULL, seed = NULL){
  
# removing so they can all have same truth
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

if(model_type == "unstructuredcov"){
source("Run models unstructured bias covariate.R")
mod_4 <- unstructured_model_cov(unstructured_data, dat1, biasfield, dim = dim, plotting = TRUE)

source("validation_function.R")
validation <- validation_function(result=mod_4[[2]], resolution=c(10,10), join.stack=mod_4[[1]], model_type="unstructuredcov", 
                                      unstructured_data = unstructured_data, dat1 = dat1, summary_results=T, absolute=TRUE, dim = dim, plotting = TRUE)
validation_r <- validation_function(result=mod_4[[2]], resolution=c(10,10), join.stack=mod_4[[1]], model_type="unstructuredcov", 
                                        unstructured_data = unstructured_data, dat1 = dat1, summary_results=T, absolute=FALSE, dim = dim, plotting = TRUE)
}  
  
if(model_type == "jointcov"){
source("Run models joint covariate for bias.R")
mod_5 <- joint_model_cov(structured_data, unstructured_data, dat1, biasfield)

source("validation_function.R")
validation <- validation_function(result=mod_5[[2]], resolution=c(10,10), join.stack=mod_5[[1]], model_type="jointcov", 
                                      unstructured_data = unstructured_data, structured_data = structured_data,
                                      dat1 = dat1, summary_results=T, absolute = TRUE, dim = dim, plotting = TRUE)
validation_r <- validation_function(result=mod_5[[2]], resolution=c(10,10), join.stack=mod_5[[1]], model_type="jointcov", 
                                        unstructured_data = unstructured_data, structured_data = structured_data,
                                        dat1 = dat1, summary_results=T, absolute = FALSE, dim = dim, plotting = TRUE)
}  
  
if(model_type == "joint2"){
source("Run models joint second field.R")
mod_6 <- joint_model2(structured_data, unstructured_data, dat1, biasfield)

source("validation_function.R")
validation <- validation_function(result=mod_6[[2]], resolution=c(10,10), join.stack=mod_6[[1]], model_type="joint2", 
                                      unstructured_data = unstructured_data, structured_data = structured_data,
                                      dat1 = dat1, summary_results=T, absolute = TRUE, dim = dim, plotting = TRUE)
validation_r <- validation_function(result=mod_6[[2]], resolution=c(10,10), join.stack=mod_6[[1]], model_type="joint2", 
                                        unstructured_data = unstructured_data, structured_data = structured_data,
                                        dat1 = dat1, summary_results=T, absolute = FALSE, dim = dim, plotting = TRUE)
}

return(list(validation_r, nsamp))
}