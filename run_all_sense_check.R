# function to run the 3 types of models and validation


source("Run models structured.R")
mod_1 <- structured_model(structured_data, dat1, biasfield)
source("validation_function.R")
validation_1 <- validation_function(result=mod_1[[2]], resolution=c(10,10), join.stack=mod_1[[1]], model_type="structured", 
                                    structured_data = structured_data, dat1 = dat1, plot=T, summary_results=T)
validation_1$coefficients


source("Run models.R")
mod_2 <- unstructured_model(unstructured_data, dat1, biasfield)
source("validation_function.R")
validation_2 <- validation_function(result=mod_2[[2]], resolution=c(10,10), join.stack=mod_2[[1]], model_type="unstructured", 
                                    unstructured_data = unstructured_data, dat1 = dat1, plot=T, summary_results=T)
validation_2$coefficients


source("Run models joint.R")
mod_joint <- joint_model(structured_data, unstructured_data, dat1, biasfield)
source("validation_function.R")
validation_3 <- validation_function(result=mod_joint[[2]], resolution=c(10,10), join.stack=mod_joint[[1]], model_type="joint", 
                                    unstructured_data = unstructured_data, structured_data = structured_data,
                                    dat1 = dat1, plot=T, summary_results=T)

validation_3$coefficients