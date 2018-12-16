# run all multiple times and save output

# need to decide what output to save
# - the estimates of coefficients (untransformed)
# - intensity at each grid square
# - truth at each grid square
# - label all and save as a list -> can concatenate in foreach loop

library(foreach)
library(doParallel)

### STRUCTURED

cl = makeCluster(2)
registerDoParallel(cl)

# set parameters - fixed for all runs
source("setParams.R")
source("run_function_multiple.R")

simulation_output_structured = foreach(i=1:100,.combine = c,.errorhandling = 'pass') %dopar% { 
  run_function_multiple(resolution=c(10,10), model_type="structured", 
                        structured_data = structured_data, dat1 = dat1, 
                        plot=F, summary_results=T)
}

stopCluster(cl)

### UNSTRUCTURED

cl = makeCluster(2)
registerDoParallel(cl)

# set parameters - fixed for all runs
source("setParams.R")
source("run_function_multiple.R")

simulation_output_unstructured = foreach(i=1:2,.combine = c,.packages=c("rgeos"),.errorhandling = 'pass') %dopar% { 
  run_function_multiple(resolution=c(10,10), model_type="unstructured", 
                        unstructured_data = unstructured_data, dat1 = dat1, 
                        plot=F, summary_results=T, biasfield = biasfield)
}

stopCluster(cl)

### JOINT

cl = makeCluster(2)
registerDoParallel(cl)

# set parameters - fixed for all runs
source("setParams.R")
source("run_function_multiple.R")

simulation_output_joint = foreach(i=1:2,.combine = c,.errorhandling = 'pass') %dopar% { 
  run_function_multiple(resolution=c(10,10), model_type="joint", 
                        unstructured_data = unstructured_data, structured_data = structured_data,dat1 = dat1, 
                        plot=F, summary_results=T, biasfield = biasfield)
}

stopCluster(cl)



