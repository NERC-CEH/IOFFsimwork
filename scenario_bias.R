#### Code to run scenarios - BIAS

# Lambda default = -2
# prob = 0.2
# env = 1.2

# run all multiple times and save output
# all run as functions 

# load packages needed to run in parallel
library(foreach)
library(doParallel)

# choose number of times to run
n_runs <- 500

# create a randomly generated string of seeds
# seed must be integer
seed <- sample(round(1:100000000),n_runs,replace=F)
#seed <- NULL


# set up the scenario with parameters that need to be changed

# set parameters
#source("setParams.R") # this is now redundant
source("run_scenario.R")

# change those that need changing
## BIAS
probs <- list(0.2,0.16,0.12,0.08,0.04,0.02,0.004) # 7 scenarios

# structured model
mapply(FUN = run_scenario,
       probs = probs[1], 
       parameter = probs[1],
       MoreArgs = list(
         model_type="structured", 
         plotting=FALSE, 
         summary_results=FALSE,  
         seed = seed, 
         plot = FALSE, 
         n_runs = n_runs,
         scenario_name = "Bias_",
         dim = c(300,300),
         env.beta = 2,
         plotdat = TRUE,
         sigma2x = 0.5,
         kappa = 0.05,
         strata = 25,
         rows = 5,
         cols = 5,
         qsize = 1,
         rho = 0.99,
         lambda = -2,
         nsamp = 150,
         resolution = c(10,10))) # to use the function you must put in all parameters it is expecting

# unstructured model
mapply(FUN = run_scenario,
       probs = probs, 
       parameter = probs,
       MoreArgs = list(
         model_type="unstructured", 
         plotting=FALSE, 
         summary_results=FALSE,  
         seed = seed, 
         plot = FALSE, 
         n_runs = n_runs,
         scenario_name = "Bias_",
         dim = c(300,300),
         env.beta = 2,
         plotdat = TRUE,
         sigma2x = 0.5,
         kappa = 0.05,
         strata = 25,
         rows = 5,
         cols = 5,
         qsize = 1,
         rho = 0.99,
         lambda = -2,
         nsamp = 150,
         resolution = c(10,10))) # to use the function you must put in all parameters it is expecting


# unstructuredcov model
mapply(FUN = run_scenario,
       probs = probs, 
       parameter = probs,
       MoreArgs = list(
         model_type = "unstructuredcov", 
         plotting=FALSE, 
         summary_results=FALSE,  
         seed = seed, 
         plot = FALSE, 
         n_runs = n_runs,
         scenario_name = "Bias_",
         dim = c(300,300),
         env.beta = 2,
         plotdat = TRUE,
         sigma2x = 0.5,
         kappa = 0.05,
         strata = 25,
         rows = 5,
         cols = 5,
         qsize = 1,
         rho = 0.99,
         lambda = -2,
         nsamp = 150,
         resolution = c(10,10))) # to use the function you must put in all parameters it is expecting

# joint model
mapply(FUN = run_scenario,
       probs = probs, 
       parameter = probs,
       MoreArgs = list(
         model_type="joint", 
         plotting=FALSE, 
         summary_results=FALSE,  
         seed = seed, 
         plot = FALSE, 
         n_runs = n_runs,
         scenario_name = "Bias_",
         dim = c(300,300),
         env.beta = 2,
         plotdat = TRUE,
         sigma2x = 0.5,
         kappa = 0.05,
         strata = 25,
         rows = 5,
         cols = 5,
         qsize = 1,
         rho = 0.99,
         lambda = -2,
         nsamp = 150,
         resolution = c(10,10))) # to use the function you must put in all parameters it is expecting

# jointcov model
mapply(FUN = run_scenario,
       probs = probs,
       parameter = probs,
       MoreArgs = list(
         model_type="jointcov", 
         plotting=FALSE, 
         summary_results=FALSE,  
         seed = seed, 
         plot = FALSE, 
         n_runs = n_runs,
         scenario_name = "Bias_",
         dim = c(300,300),
         env.beta = 2,
         plotdat = TRUE,
         sigma2x = 0.5,
         kappa = 0.05,
         strata = 25,
         rows = 5,
         cols = 5,
         qsize = 1,
         rho = 0.99,
         lambda = -2,
         nsamp = 150,
         resolution = c(10,10))) # to use the function you must put in all parameters it is expecting


# joint2 model
mapply(FUN = run_scenario,
       probs = probs,
       parameter = probs,
       MoreArgs = list(
         model_type="jointtwo", 
         plotting=FALSE, 
         summary_results=FALSE,  
         seed = seed, 
         plot = FALSE, 
         n_runs = n_runs,
         scenario_name = "Bias_",
         dim = c(300,300),
         env.beta = 2,
         plotdat = TRUE,
         sigma2x = 0.5,
         kappa = 0.05,
         strata = 25,
         rows = 5,
         cols = 5,
         qsize = 1,
         rho = 0.99,
         lambda = -2,
         nsamp = 150,
         resolution = c(10,10))) # to use the function you must put in all parameters it is expecting

