#### SAMPLE SIZE SCENARIO!

# run all multiple times and save output

# need to decide what output to save
# - the estimates of coefficients (untransformed)
# - intensity at each grid square
# - truth at each grid square
# - label all and save as a list -> can concatenate in foreach loop

library(foreach)
library(doParallel)

structured_sample <- c(25,50,100,150,500)

#set number of runs desired here
n_runs <- 4

# create a randomly generated string of seeds
#seed <- sample(1:10000000000000,n_runs,replace=F)
seed <- NULL


### STRUCTURED
cl = makeCluster(4)
registerDoParallel(cl)
strt = Sys.time()

simulation_output_structured_v_low = foreach(i=1:n_runs,
                                           .combine=c,
                                           .multicombine = TRUE,
                                           .packages=c("rgeos", "INLA", "reshape2", "fields"),
                                           .errorhandling = 'pass') %dopar% { 
                                             # set parameters - fixed for all runs
                                             source("setParams.R")
                                             source("run_function_multiple.R")
                                             run_function_multiple(resolution=c(10,10), model_type="structured", plotting=FALSE, summary_results=TRUE,  nsamp = structured_sample[1], seed = seed[i], dim = dim, lambda = lambda, env.beta = env.beta, kappa = kappa,  sigma2x = sigma2x, strata = strata,  rows = rows,cols = cols,  probs = probs,  plot = FALSE,  plotdat = FALSE, qsize = qsize                                    
            )
                                           }

stopCluster(cl)
print(Sys.time()-strt)

save(simulation_output_structured_v_low, file="structured_output_v_low_parallel.RData")


cl = makeCluster(4)
registerDoParallel(cl)
strt = Sys.time()

simulation_output_structured_low = foreach(i=1:n_runs,
                                       .combine=c,
                                       .multicombine = TRUE,
                                       .packages=c("rgeos", "INLA", "reshape2", "fields"),
                                       .errorhandling = 'pass') %dopar% { 
                                         # set parameters - fixed for all runs
                                         source("setParams.R")
                                         source("run_function_multiple.R")
                                         run_function_multiple(resolution=c(10,10), model_type="structured", 
                                                              
                                                               plotting=FALSE, summary_results=TRUE, 
                                                               nsamp = structured_sample[2], seed = seed[i],dim = dim, lambda = lambda, env.beta = env.beta, kappa = kappa,  sigma2x = sigma2x, strata = strata,  rows = rows,cols = cols,  probs = probs,  plot = FALSE,  plotdat = FALSE, qsize = qsize  )
                                       }

stopCluster(cl)
print(Sys.time()-strt)

save(simulation_output_structured_low, file="structured_output_low_parallel.RData")

cl = makeCluster(4)
registerDoParallel(cl)
strt = Sys.time()


simulation_output_structured_mid = foreach(i=1:n_runs,
                                           .combine=c,
                                           .multicombine = TRUE,
                                           .packages=c("rgeos", "INLA", "reshape2", "fields"),
                                           .errorhandling = 'pass') %dopar% { 
                                             # set parameters - fixed for all runs
                                             source("setParams.R")
                                             source("run_function_multiple.R")
                                             run_function_multiple(resolution=c(10,10), model_type="structured", 
                                                                   
                                                                   plotting=FALSE, summary_results=TRUE,
                                                                   nsamp = structured_sample[3], seed = seed[i],dim = dim, lambda = lambda, env.beta = env.beta, kappa = kappa,  sigma2x = sigma2x, strata = strata,  rows = rows,cols = cols,  probs = probs,  plot = FALSE,  plotdat = FALSE, qsize = qsize  )
                                           }

stopCluster(cl)
print(Sys.time()-strt)

save(simulation_output_structured_mid, file="structured_output_mid_parallel.RData")

cl = makeCluster(4)
registerDoParallel(cl)
strt = Sys.time()


simulation_output_structured_high = foreach(i=1:n_runs,
                                           .combine=c,
                                           .multicombine = TRUE,
                                           .packages=c("rgeos", "INLA", "reshape2", "fields"),
                                           .errorhandling = 'pass') %dopar% { 
                                             # set parameters - fixed for all runs
                                             source("setParams.R")
                                             source("run_function_multiple.R")
                                             run_function_multiple(resolution=c(10,10), model_type="structured", 
                                                                   
                                                                   plotting=FALSE, summary_results=TRUE,
                                                                   nsamp = structured_sample[4], seed = seed[i],dim = dim, lambda = lambda, env.beta = env.beta, kappa = kappa,  sigma2x = sigma2x, strata = strata,  rows = rows,cols = cols,  probs = probs,  plot = FALSE,  plotdat = FALSE, qsize = qsize  )
                                           }

stopCluster(cl)
print(Sys.time()-strt)

save(simulation_output_structured_high, file="structured_output_high_parallel.RData")

cl = makeCluster(4)
registerDoParallel(cl)
strt = Sys.time()


simulation_output_structured_v_high = foreach(i=1:n_runs,
                                            .combine=c,
                                            .multicombine = TRUE,
                                            .packages=c("rgeos", "INLA", "reshape2", "fields"),
                                            .errorhandling = 'pass') %dopar% { 
                                              # set parameters - fixed for all runs
                                              source("setParams.R")
                                              source("run_function_multiple.R")
                                              run_function_multiple(resolution=c(10,10), model_type="structured", 
                                                                   
                                                                    plotting=FALSE, summary_results=TRUE, 
                                                                    nsamp = structured_sample[5], seed = seed[i],dim = dim, lambda = lambda, env.beta = env.beta, kappa = kappa,  sigma2x = sigma2x, strata = strata,  rows = rows,cols = cols,  probs = probs,  plot = FALSE,  plotdat = FALSE, qsize = qsize  )
                                            }

stopCluster(cl)
print(Sys.time()-strt)

save(simulation_output_structured_v_high, file="structured_output_v_high_parallel.RData")

### UNSTRUCTURED

cl = makeCluster(2)
registerDoParallel(cl)

# set parameters - fixed for all runs
source("setParams.R")
source("run_function_multiple.R")

simulation_output_unstructured = foreach(i=1:n_runs,
                                         .combine=c,
                                         .multicombine = TRUE,
                                         .packages=c("rgeos", "INLA", "reshape2", "deldir", "fields"),
                                         .errorhandling = 'pass') %dopar%  {
                                           # set parameters - fixed for all runs
                                           source("setParams.R")
                                           source("run_function_multiple.R")                                         
                                           run_function_multiple(resolution=c(10,10), model_type="unstructured", 
                                                               
                                                                 plotting=FALSE, summary_results=TRUE, 
                                                                 seed = seed[i],dim = dim, lambda = lambda, env.beta = env.beta, kappa = kappa,  sigma2x = sigma2x, strata = strata,  rows = rows,cols = cols,  probs = probs,  plot = FALSE,  plotdat = FALSE, qsize = qsize  )
                                         }

stopCluster(cl)
print(Sys.time()-strt)

save(simulation_output_unstructured, file="unstructured_output_parallel.RData")

### JOINT
cl = makeCluster(4)
registerDoParallel(cl)

# set parameters - fixed for all runs
source("setParams.R")
source("run_function_multiple.R")

simulation_output_joint_v_low = foreach(i=1:n_runs,
                                      .combine=c,
                                      .multicombine = TRUE,
                                      .packages=c("rgeos", "INLA", "reshape2", "deldir", "fields"),
                                      .errorhandling = 'pass') %dopar% { 
                                        # set parameters - fixed for all runs
                                        source("setParams.R")
                                        source("run_function_multiple.R")                                    
                                        run_function_multiple(resolution=c(10,10), model_type="joint", 
                                                             
                                                              plotting=FALSE, summary_results=TRUE,
                                                              nsamp = structured_sample[1], seed = seed[i],dim = dim, lambda = lambda, env.beta = env.beta, kappa = kappa,  sigma2x = sigma2x, strata = strata,  rows = rows,cols = cols,  probs = probs,  plot = FALSE,  plotdat = FALSE, qsize = qsize  )
                                      }

stopCluster(cl)
print(Sys.time()-strt)

save(simulation_output_joint_v_low, file="joint_output_v_low_parallel.RData")

cl = makeCluster(4)
registerDoParallel(cl)

# set parameters - fixed for all runs
source("setParams.R")
source("run_function_multiple.R")

simulation_output_joint_low = foreach(i=1:n_runs,
                                  .combine=c,
                                  .multicombine = TRUE,
                                  .packages=c("rgeos", "INLA", "reshape2", "deldir", "fields"),
                                  .errorhandling = 'pass') %dopar% { 
                                    # set parameters - fixed for all runs
                                    source("setParams.R")
                                    source("run_function_multiple.R")                                    
                                    run_function_multiple(resolution=c(10,10), model_type="joint", 
                                                         
                                                          plotting=FALSE, summary_results=TRUE,
                                                          nsamp = structured_sample[2], seed = seed[i],dim = dim, lambda = lambda, env.beta = env.beta, kappa = kappa,  sigma2x = sigma2x, strata = strata,  rows = rows,cols = cols,  probs = probs,  plot = FALSE,  plotdat = FALSE, qsize = qsize  )
                                  }

stopCluster(cl)
print(Sys.time()-strt)

save(simulation_output_joint_low, file="joint_output_low_parallel.RData")

cl = makeCluster(4)
registerDoParallel(cl)

# set parameters - fixed for all runs
source("setParams.R")
source("run_function_multiple.R")

simulation_output_joint_mid = foreach(i=1:n_runs,
                                      .combine=c,
                                      .multicombine = TRUE,
                                      .packages=c("rgeos", "INLA", "reshape2", "deldir", "fields"),
                                      .errorhandling = 'pass') %dopar% { 
                                        # set parameters - fixed for all runs
                                        source("setParams.R")
                                        source("run_function_multiple.R")                                    
                                        run_function_multiple(resolution=c(10,10), model_type="joint", 
                                                             
                                                              plotting=FALSE, summary_results=TRUE, 
                                                              nsamp = structured_sample[3], seed = seed[i],dim = dim, lambda = lambda, env.beta = env.beta, kappa = kappa,  sigma2x = sigma2x, strata = strata,  rows = rows,cols = cols,  probs = probs,  plot = FALSE,  plotdat = FALSE, qsize = qsize  )
                                      }

stopCluster(cl)
print(Sys.time()-strt)

save(simulation_output_joint_mid, file="joint_output_mid_parallel.RData")

cl = makeCluster(4)
registerDoParallel(cl)

# set parameters - fixed for all runs
source("setParams.R")
source("run_function_multiple.R")

simulation_output_joint_high = foreach(i=1:n_runs,
                                      .combine=c,
                                      .multicombine = TRUE,
                                      .packages=c("rgeos", "INLA", "reshape2", "deldir", "fields"),
                                      .errorhandling = 'pass') %dopar% { 
                                        # set parameters - fixed for all runs
                                        source("setParams.R")
                                        source("run_function_multiple.R")                                    
                                        run_function_multiple(resolution=c(10,10), model_type="joint", 
                                                             
                                                              plotting=FALSE, summary_results=TRUE,
                                                              nsamp = structured_sample[4], seed = seed[i],dim = dim, lambda = lambda, env.beta = env.beta, kappa = kappa,  sigma2x = sigma2x, strata = strata,  rows = rows,cols = cols,  probs = probs,  plot = FALSE,  plotdat = FALSE, qsize = qsize  )
                                      }

stopCluster(cl)
print(Sys.time()-strt)

save(simulation_output_joint_high, file="joint_output_high_parallel.RData")

cl = makeCluster(4)
registerDoParallel(cl)

# set parameters - fixed for all runs
source("setParams.R")
source("run_function_multiple.R")

simulation_output_joint_v_high = foreach(i=1:n_runs,
                                       .combine=c,
                                       .multicombine = TRUE,
                                       .packages=c("rgeos", "INLA", "reshape2", "deldir", "fields"),
                                       .errorhandling = 'pass') %dopar% { 
                                         # set parameters - fixed for all runs
                                         source("setParams.R")
                                         source("run_function_multiple.R")                                    
                                         run_function_multiple(resolution=c(10,10), model_type="joint", 
                                                              
                                                               plotting=FALSE, summary_results=TRUE, 
                                                               nsamp = structured_sample[5], seed = seed[i],dim = dim, lambda = lambda, env.beta = env.beta, kappa = kappa,  sigma2x = sigma2x, strata = strata,  rows = rows,cols = cols,  probs = probs,  plot = FALSE,  plotdat = FALSE, qsize = qsize  )
                                       }

stopCluster(cl)
print(Sys.time()-strt)

save(simulation_output_joint_v_high, file="joint_output_v_high_parallel.RData")


### JOINT COV
cl = makeCluster(4)
registerDoParallel(cl)

# set parameters - fixed for all runs
source("setParams.R")
source("run_function_multiple.R")

simulation_output_jointcov_v_low = foreach(i=1:n_runs,
                                         .combine=c,
                                         .multicombine = TRUE,
                                         .packages=c("rgeos", "INLA", "reshape2", "deldir", "fields"),
                                         .errorhandling = 'pass') %dopar% { 
                                           # set parameters - fixed for all runs
                                           source("setParams.R")
                                           source("run_function_multiple.R")                                    
                                           run_function_multiple(resolution=c(10,10), model_type="jointcov", 
                                                                 
                                                                 plotting=FALSE, summary_results=TRUE, 
                                                                 nsamp = structured_sample[1], seed = seed[i],dim = dim, lambda = lambda, env.beta = env.beta, kappa = kappa,  sigma2x = sigma2x, strata = strata,  rows = rows,cols = cols,  probs = probs,  plot = FALSE,  plotdat = FALSE, qsize = qsize  )
                                         }

stopCluster(cl)
print(Sys.time()-strt)

save(simulation_output_jointcov_v_low, file="jointcov_output_v_low_parallel.RData")

cl = makeCluster(4)
registerDoParallel(cl)

# set parameters - fixed for all runs
source("setParams.R")
source("run_function_multiple.R")

simulation_output_jointcov_low = foreach(i=1:n_runs,
                                     .combine=c,
                                     .multicombine = TRUE,
                                     .packages=c("rgeos", "INLA", "reshape2", "deldir", "fields"),
                                     .errorhandling = 'pass') %dopar% { 
                                       # set parameters - fixed for all runs
                                       source("setParams.R")
                                       source("run_function_multiple.R")                                    
                                       run_function_multiple(resolution=c(10,10), model_type="jointcov", 
                                                             
                                                             plotting=FALSE, summary_results=TRUE, 
                                                             nsamp = structured_sample[2], seed = seed[i],dim = dim, lambda = lambda, env.beta = env.beta, kappa = kappa,  sigma2x = sigma2x, strata = strata,  rows = rows,cols = cols,  probs = probs,  plot = FALSE,  plotdat = FALSE, qsize = qsize  )
                                     }

stopCluster(cl)
print(Sys.time()-strt)

save(simulation_output_jointcov_low, file="jointcov_output_low_parallel.RData")

cl = makeCluster(4)
registerDoParallel(cl)

# set parameters - fixed for all runs
source("setParams.R")
source("run_function_multiple.R")

simulation_output_jointcov_mid = foreach(i=1:n_runs,
                                         .combine=c,
                                         .multicombine = TRUE,
                                         .packages=c("rgeos", "INLA", "reshape2", "deldir", "fields"),
                                         .errorhandling = 'pass') %dopar% { 
                                           # set parameters - fixed for all runs
                                           source("setParams.R")
                                           source("run_function_multiple.R")                                    
                                           run_function_multiple(resolution=c(10,10), model_type="jointcov", 
                                                                
                                                                 plotting=FALSE, summary_results=TRUE,
                                                                 nsamp = structured_sample[3], seed = seed[i],dim = dim, lambda = lambda, env.beta = env.beta, kappa = kappa,  sigma2x = sigma2x, strata = strata,  rows = rows,cols = cols,  probs = probs,  plot = FALSE,  plotdat = FALSE, qsize = qsize  )
                                         }

stopCluster(cl)
print(Sys.time()-strt)

save(simulation_output_jointcov_mid, file="jointcov_output_mid_parallel.RData")

cl = makeCluster(4)
registerDoParallel(cl)

# set parameters - fixed for all runs
source("setParams.R")
source("run_function_multiple.R")

simulation_output_jointcov_high = foreach(i=1:n_runs,
                                         .combine=c,
                                         .multicombine = TRUE,
                                         .packages=c("rgeos", "INLA", "reshape2", "deldir", "fields"),
                                         .errorhandling = 'pass') %dopar% { 
                                           # set parameters - fixed for all runs
                                           source("setParams.R")
                                           source("run_function_multiple.R")                                    
                                           run_function_multiple(resolution=c(10,10), model_type="jointcov", 
                                                                  
                                                                 plotting=FALSE, summary_results=TRUE, 
                                                                 nsamp = structured_sample[4], seed = seed[i],dim = dim, lambda = lambda, env.beta = env.beta, kappa = kappa,  sigma2x = sigma2x, strata = strata,  rows = rows,cols = cols,  probs = probs,  plot = FALSE,  plotdat = FALSE, qsize = qsize  )
                                         }

stopCluster(cl)
print(Sys.time()-strt)

save(simulation_output_jointcov_high, file="jointcov_output_high_parallel.RData")

cl = makeCluster(4)
registerDoParallel(cl)

# set parameters - fixed for all runs
source("setParams.R")
source("run_function_multiple.R")

simulation_output_jointcov_v_high = foreach(i=1:n_runs,
                                          .combine=c,
                                          .multicombine = TRUE,
                                          .packages=c("rgeos", "INLA", "reshape2", "deldir", "fields"),
                                          .errorhandling = 'pass') %dopar% { 
                                            # set parameters - fixed for all runs
                                            source("setParams.R")
                                            source("run_function_multiple.R")                                    
                                            run_function_multiple(resolution=c(10,10), model_type="jointcov", 
                                                                
                                                                  plotting=FALSE, summary_results=TRUE, 
                                                                  nsamp = structured_sample[5], seed = seed[i],dim = dim, lambda = lambda, env.beta = env.beta, kappa = kappa,  sigma2x = sigma2x, strata = strata,  rows = rows,cols = cols,  probs = probs,  plot = FALSE,  plotdat = FALSE, qsize = qsize  )
                                          }

stopCluster(cl)
print(Sys.time()-strt)

save(simulation_output_jointcov_v_high, file="jointcov_output_v_high_parallel.RData")

### JOINT2
cl = makeCluster(4)
registerDoParallel(cl)

# set parameters - fixed for all runs
source("setParams.R")
source("run_function_multiple.R")

simulation_output_joint2_v_low = foreach(i=1:n_runs,
                                       .combine=c,
                                       .multicombine = TRUE,
                                       .packages=c("rgeos", "INLA", "reshape2", "deldir", "fields"),
                                       .errorhandling = 'pass') %dopar% { 
                                         # set parameters - fixed for all runs
                                         source("setParams.R")
                                         source("run_function_multiple.R")                                    
                                         run_function_multiple(resolution=c(10,10), model_type="joint2", 
                                                               
                                                               plotting=FALSE, summary_results=TRUE,
                                                               nsamp = structured_sample[1], seed = seed[i],dim = dim, lambda = lambda, env.beta = env.beta, kappa = kappa,  sigma2x = sigma2x, strata = strata,  rows = rows,cols = cols,  probs = probs,  plot = FALSE,  plotdat = FALSE, qsize = qsize  )
                                       }

stopCluster(cl)
print(Sys.time()-strt)

save(simulation_output_joint2_v_low, file="joint2_output_v_low_parallel.RData")

cl = makeCluster(4)
registerDoParallel(cl)

# set parameters - fixed for all runs
source("setParams.R")
source("run_function_multiple.R")

simulation_output_joint2_low = foreach(i=1:n_runs,
                                   .combine=c,
                                   .multicombine = TRUE,
                                   .packages=c("rgeos", "INLA", "reshape2", "deldir", "fields"),
                                   .errorhandling = 'pass') %dopar% { 
                                     # set parameters - fixed for all runs
                                     source("setParams.R")
                                     source("run_function_multiple.R")                                    
                                     run_function_multiple(resolution=c(10,10), model_type="joint2", 
                                                           
                                                           plotting=FALSE, summary_results=TRUE, 
                                                           nsamp = structured_sample[2], seed = seed[i],dim = dim, lambda = lambda, env.beta = env.beta, kappa = kappa,  sigma2x = sigma2x, strata = strata,  rows = rows,cols = cols,  probs = probs,  plot = FALSE,  plotdat = FALSE, qsize = qsize  )
                                   }

stopCluster(cl)
print(Sys.time()-strt)

save(simulation_output_joint2_low, file="joint2_output_low_parallel.RData")

cl = makeCluster(4)
registerDoParallel(cl)

# set parameters - fixed for all runs
source("setParams.R")
source("run_function_multiple.R")

simulation_output_joint2_mid = foreach(i=1:n_runs,
                                   .combine=c,
                                   .multicombine = TRUE,
                                   .packages=c("rgeos", "INLA", "reshape2", "deldir", "fields"),
                                   .errorhandling = 'pass') %dopar% { 
                                     # set parameters - fixed for all runs
                                     source("setParams.R")
                                     source("run_function_multiple.R")                                    
                                     run_function_multiple(resolution=c(10,10), model_type="joint2", 
                                                           
                                                           plotting=FALSE, summary_results=TRUE,
                                                           nsamp = structured_sample[3], seed = seed[i],dim = dim, lambda = lambda, env.beta = env.beta, kappa = kappa,  sigma2x = sigma2x, strata = strata,  rows = rows,cols = cols,  probs = probs,  plot = FALSE,  plotdat = FALSE, qsize = qsize  )
                                   }

stopCluster(cl)
print(Sys.time()-strt)

save(simulation_output_joint2_mid, file="joint2_output_mid_parallel.RData")

cl = makeCluster(4)
registerDoParallel(cl)

# set parameters - fixed for all runs
source("setParams.R")
source("run_function_multiple.R")

simulation_output_joint2_high = foreach(i=1:n_runs,
                                   .combine=c,
                                   .multicombine = TRUE,
                                   .packages=c("rgeos", "INLA", "reshape2", "deldir", "fields"),
                                   .errorhandling = 'pass') %dopar% { 
                                     # set parameters - fixed for all runs
                                     source("setParams.R")
                                     nsamp <- structured_sample[3]
                                     source("run_function_multiple.R")                                    
                                     run_function_multiple(resolution=c(10,10), model_type="joint2", 
                                                           
                                                           plotting=FALSE, summary_results=TRUE,
                                                           nsamp = structured_sample[4], seed = seed[i],dim = dim, lambda = lambda, env.beta = env.beta, kappa = kappa,  sigma2x = sigma2x, strata = strata,  rows = rows,cols = cols,  probs = probs,  plot = FALSE,  plotdat = FALSE, qsize = qsize  )
                                   }

stopCluster(cl)
print(Sys.time()-strt)

save(simulation_output_joint2_high, file="joint2_output_high_parallel.RData")

cl = makeCluster(4)
registerDoParallel(cl)

# set parameters - fixed for all runs
source("setParams.R")
source("run_function_multiple.R")

simulation_output_joint2_v_high = foreach(i=1:n_runs,
                                        .combine=c,
                                        .multicombine = TRUE,
                                        .packages=c("rgeos", "INLA", "reshape2", "deldir", "fields"),
                                        .errorhandling = 'pass') %dopar% { 
                                          # set parameters - fixed for all runs
                                          source("setParams.R")
                                          nsamp <- structured_sample[3]
                                          source("run_function_multiple.R")                                    
                                          run_function_multiple(resolution=c(10,10), model_type="joint2", 
                                                               
                                                                plotting=FALSE, summary_results=TRUE, 
                                                                nsamp = structured_sample[5], seed = seed[i],dim = dim, lambda = lambda, env.beta = env.beta, kappa = kappa,  sigma2x = sigma2x, strata = strata,  rows = rows,cols = cols,  probs = probs,  plot = FALSE,  plotdat = FALSE, qsize = qsize  )
                                        }

stopCluster(cl)
print(Sys.time()-strt)

save(simulation_output_joint2_v_high, file="joint2_output_v_high_parallel.RData")