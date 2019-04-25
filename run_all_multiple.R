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
strt = Sys.time()

simulation_output_structured = foreach(i=1:2,
                                       .combine=c,
                                       .multicombine = TRUE,
                                       .packages=c("rgeos", "INLA", "reshape2", "fields"),
                                       .errorhandling = 'pass') %dopar% { 
  # set parameters - fixed for all runs
  source("setParams.R")
  source("run_function_multiple.R")
  run_function_multiple(resolution=c(10,10), model_type="structured", 
                        structured_data = structured_data, dat1 = dat1, 
                        plotting=FALSE, summary_results=TRUE, biasfield = biasfield)
}

stopCluster(cl)
print(Sys.time()-strt)

save(simulation_output_structured, file="structured_output_parallel.RData")

### UNSTRUCTURED

cl = makeCluster(2)
registerDoParallel(cl)

# set parameters - fixed for all runs
source("setParams.R")
source("run_function_multiple.R")

simulation_output_unstructured = foreach(i=1:2,
                                         .combine=c,
                                         .multicombine = TRUE,
                                         .packages=c("rgeos", "INLA", "reshape2", "deldir", "fields"),
                                         .errorhandling = 'pass') %dopar%  {
  # set parameters - fixed for all runs
  source("setParams.R")
  source("run_function_multiple.R")                                         
  run_function_multiple(resolution=c(10,10), model_type="unstructured", 
                        unstructured_data = unstructured_data, dat1 = dat1, 
                        plotting=FALSE, summary_results=TRUE, biasfield = biasfield)
}

stopCluster(cl)
print(Sys.time()-strt)

save(simulation_output_unstructured, file="unstructured_output_parallel.RData")

### JOINT

cl = makeCluster(2)
registerDoParallel(cl)

# set parameters - fixed for all runs
source("setParams.R")
source("run_function_multiple.R")

simulation_output_joint = foreach(i=1:2,
                                  .combine=c,
                                  .multicombine = TRUE,
                                  .packages=c("rgeos", "INLA", "reshape2", "deldir", "fields"),
                                  .errorhandling = 'pass') %dopar% { 
  # set parameters - fixed for all runs
  source("setParams.R")
  source("run_function_multiple.R")                                    
  run_function_multiple(resolution=c(10,10), model_type="joint", 
                        unstructured_data = unstructured_data, structured_data = structured_data,dat1 = dat1, 
                        plotting=FALSE, summary_results=TRUE, biasfield = biasfield)
}

stopCluster(cl)
print(Sys.time()-strt)

save(simulation_output_joint, file="joint_output_parallel.RData")

### UNSTRUCT COV

cl = makeCluster(2)
registerDoParallel(cl)

# set parameters - fixed for all runs
source("setParams.R")
source("run_function_multiple.R")

simulation_output_unstructuredcov = foreach(i=1:2,
                                  .combine=c,
                                  .multicombine = TRUE,
                                  .packages=c("rgeos", "INLA", "reshape2", "deldir", "fields"),
                                  .errorhandling = 'pass') %dopar% { 
                                    # set parameters - fixed for all runs
                                    source("setParams.R")
                                    source("run_function_multiple.R")                                    
                                    run_function_multiple(resolution=c(10,10), model_type="unstructuredcov", 
                                                          unstructured_data = unstructured_data, dat1 = dat1, 
                                                          plotting=FALSE, summary_results=TRUE, biasfield = biasfield)
                                  }

stopCluster(cl)
print(Sys.time()-strt)

save(simulation_output_unstructuredcov, file="unstructuredcov_output_parallel.RData")

### JOINT COV

cl = makeCluster(2)
registerDoParallel(cl)

# set parameters - fixed for all runs
source("setParams.R")
source("run_function_multiple.R")

simulation_output_jointcov = foreach(i=1:2,
                                            .combine=c,
                                            .multicombine = TRUE,
                                            .packages=c("rgeos", "INLA", "reshape2", "deldir", "fields"),
                                            .errorhandling = 'pass') %dopar% { 
                                              # set parameters - fixed for all runs
                                              source("setParams.R")
                                              source("run_function_multiple.R")                                    
                                              run_function_multiple(resolution=c(10,10), model_type="jointcov", 
                                                                    unstructured_data = unstructured_data, structured_data = structured_data, dat1 = dat1, 
                                                                    plotting=FALSE, summary_results=TRUE, biasfield = biasfield)
                                            }

stopCluster(cl)
print(Sys.time()-strt)

save(simulation_output_jointcov, file="jointcov_output_parallel.RData")

### JOINT2

cl = makeCluster(2)
registerDoParallel(cl)

# set parameters - fixed for all runs
source("setParams.R")
source("run_function_multiple.R")

simulation_output_joint2 = foreach(i=1:2,
                                     .combine=c,
                                     .multicombine = TRUE,
                                     .packages=c("rgeos", "INLA", "reshape2", "deldir", "fields"),
                                     .errorhandling = 'pass') %dopar% { 
                                       # set parameters - fixed for all runs
                                       source("setParams.R")
                                       source("run_function_multiple.R")                                    
                                       run_function_multiple(resolution=c(10,10), model_type="joint2", 
                                                             unstructured_data = unstructured_data, structured_data = structured_data, dat1 = dat1, 
                                                             plotting=FALSE, summary_results=TRUE, biasfield = biasfield)
                                     }

stopCluster(cl)
print(Sys.time()-strt)

save(simulation_output_joint2, file="joint2_output_parallel.RData")

