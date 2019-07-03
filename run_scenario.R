# function to run any scenario/model in parallel

run_scenario <- function(resolution, 
                         model_type=c("structured", "unstructured", "unstructuredcov", "joint", "jointcov", "joint2"), 
                         plotting=FALSE, 
                         summary_results=FALSE,  
                         nsamp = NULL, 
                         seed = NULL, 
                         dim = NULL, 
                         lambda = NULL, 
                         env.beta = NULL, 
                         kappa = NULL,  
                         sigma2x = NULL, 
                         strata = NULL,  
                         rows = NULL,
                         cols = NULL,  
                         probs = NULL,  
                         plot = FALSE, 
                         plotdat = FALSE, 
                         qsize = qsize, 
                         rho = rho, 
                         correlation = FALSE, 
                         n_runs = NULL,
                         scenario_name = NULL,
                         parameter = NULL){
  
  
  # run the model in a parallel loop
  
  # create a cluster
  cl = makeCluster(2)
  registerDoParallel(cl)
  strt = Sys.time() # record system time
  
  model_output = foreach(i=1:n_runs,
                         .combine=c,
                         .multicombine = TRUE,
                         .packages=c("rgeos", "INLA", "reshape2", "fields"),
                         .errorhandling = 'pass') %dopar% { 
                           source("run_function_multiple.R")
                           run_function_multiple(resolution=resolution, 
                                                 model_type=model_type, 
                                                 plotting=FALSE, summary_results=TRUE,  
                                                 nsamp = nsamp, 
                                                 seed = seed[i], 
                                                 dim = dim, 
                                                 lambda = lambda, 
                                                 env.beta = env.beta, 
                                                 kappa = kappa,  
                                                 sigma2x = sigma2x, 
                                                 strata = strata,  
                                                 rows = rows,
                                                 cols = cols,  
                                                 probs = probs,  
                                                 plot = FALSE, 
                                                 plotdat = FALSE, 
                                                 qsize = qsize, rho = rho                                    
                           )
                         }
  
  # want to save the output named by the scenario and value
  
  stopCluster(cl)
  print(Sys.time()-strt)
  
  save(model_output, file = paste(scenario_name, model_type, parameter, ".RData", sep=""))
}
