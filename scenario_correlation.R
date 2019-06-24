#### Code to run scenarios - CORRELATION

# run all multiple times and save output
# all run as functions 

# load packages needed to run in parallel
library(foreach)
library(doParallel)

# choose number of times to run
n_runs <- 50

# create a randomly generated string of seeds
# seed must be integer
seed <- sample(round(1:100000000),n_runs,replace=F)
#seed <- NULL


# set up the scenario with parameters that need to be changed

# set parameters
#source("setParams.R") # this is now redundant
source("run_scenario.R")

# change those that need changing
## CORRELATION

# structured model
run_scenario(parameter = "correlation",
         model_type="structured", 
         plotting=FALSE, 
         summary_results=FALSE,  
         seed = seed, 
         plot = FALSE, 
         n_runs = n_runs,
         scenario_name = "Correlation",
         dim = c(300,300),
         env.beta = 1.2,
         plotdat = TRUE,
         sigma2x = 0.5,
         kappa = 0.05,
         strata = 25,
         rows = 5,
         cols = 5,
         probs = 0.5,
         qsize = 1,
         rho = 0.8,
         lambda = -1,
         nsamp = 150,
         correlated = TRUE,
         resolution = c(10,10)) # to use the function you must put in all parameters it is expecting

# unstructured model
run_scenario(parameter = "correlation",
         model_type="unstructured", 
         plotting=FALSE, 
         summary_results=FALSE,  
         seed = seed, 
         plot = FALSE, 
         n_runs = n_runs,
         scenario_name = "Correlation",
         dim = c(300,300),
         env.beta = 1.2,
         plotdat = TRUE,
         sigma2x = 0.5,
         kappa = 0.05,
         strata = 25,
         rows = 5,
         cols = 5,
         probs = 0.5,
         qsize = 1,
         rho = 0.8,
         nsamp = 150,
         correlated = TRUE,
         resolution = c(10,10))

# unstructuredcov model
run_scenario(parameter = "correlation",
         model_type = "unstructuredcov", 
         plotting = FALSE, 
         summary_results = FALSE,  
         seed = seed, 
         plot = FALSE, 
         n_runs = n_runs,
         scenario_name = "Correlation",
         dim = c(300,300),
         env.beta = 1.2,
         plotdat = TRUE,
         sigma2x = 0.5,
         kappa = 0.05,
         strata = 25,
         rows = 5,
         cols = 5,
         probs = 0.5,
         qsize = 1,
         rho = 0.8,
         nsamp = 150,
         correlated = TRUE,
         resolution = c(10,10))

# joint model
run_scenario(parameter = "correlation",
         model_type="joint", 
         plotting=FALSE, 
         summary_results=FALSE,  
         seed = seed, 
         plot = FALSE, 
         n_runs = n_runs,
         scenario_name = "Correlation",
         dim = c(300,300),
         env.beta = 1.2,
         plotdat = TRUE,
         sigma2x = 0.5,
         kappa = 0.05,
         strata = 25,
         rows = 5,
         cols = 5,
         probs = 0.5,
         qsize = 1,
         rho = 0.8,
         nsamp = 150,
         correlated = TRUE,
         resolution = c(10,10)) # to use the function you must put in all parameters it is expecting

# jointcov model
run_scenarion(parameter = "correlation",
         model_type="jointcov", 
         plotting=FALSE, 
         summary_results=FALSE,  
         seed = seed, 
         plot = FALSE, 
         n_runs = n_runs,
         scenario_name = "Correlation",
         dim = c(300,300),
         env.beta = 1.2,
         plotdat = TRUE,
         sigma2x = 0.5,
         kappa = 0.05,
         strata = 25,
         rows = 5,
         cols = 5,
         probs = 0.5,
         qsize = 1,
         rho = 0.8,
         nsamp = 150, 
         correlated = TRUE,
         resolution = c(10,10))

# joint2 model
run_scenario(parameter = "correlation",
         model_type="joint2", 
         plotting=FALSE, 
         summary_results=FALSE,  
         seed = seed, 
         plot = FALSE, 
         n_runs = n_runs,
         scenario_name = "Correlation",
         dim = c(300,300),
         env.beta = 1.2,
         plotdat = TRUE,
         sigma2x = 0.5,
         kappa = 0.05,
         strata = 25,
         rows = 5,
         cols = 5,
         probs = 0.5,
         qsize = 1,
         rho = 0.8,
         nsamp = 150,
         correlated = TRUE,
         resolution = c(10,10)) 
