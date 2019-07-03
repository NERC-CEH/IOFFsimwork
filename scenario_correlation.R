#### Code to run scenarios - CORRELATION

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
## CORRELATION

# structured model
run_scenario(parameter = "TRUE",
         model_type="structured", 
         plotting=FALSE, 
         summary_results=FALSE,  
         seed = seed, 
         plot = FALSE, 
         n_runs = n_runs,
         scenario_name = "Correlation_",
         dim = c(300,300),
         env.beta = 1.2,
         plotdat = TRUE,
         sigma2x = 0.5,
         kappa = 0.05,
         strata = 25,
         rows = 5,
         cols = 5,
         probs = c(rep(seq(0.5, 0.01, length.out = 5),5)),
         qsize = 1,
         rho = 0.8,
         lambda = -3,
         nsamp = 150,
         correlation = TRUE,
         resolution = c(10,10)) # to use the function you must put in all parameters it is expecting

# unstructured model
run_scenario(parameter = "TRUE",
         model_type="unstructured", 
         plotting=FALSE, 
         summary_results=FALSE,  
         seed = seed, 
         plot = FALSE, 
         n_runs = n_runs,
         scenario_name = "Correlation_",
         dim = c(300,300),
         env.beta = 1.2,
         plotdat = TRUE,
         sigma2x = 0.5,
         kappa = 0.05,
         strata = 25,
         rows = 5,
         cols = 5,
         probs = c(rep(seq(0.5, 0.01, length.out = 5),5)),
         qsize = 1,
         rho = 0.8,
         nsamp = 150,
         correlation = TRUE,
         lambda = -3,
         resolution = c(10,10))

# unstructuredcov model
run_scenario(parameter = "TRUE",
         model_type = "unstructuredcov", 
         plotting = FALSE, 
         summary_results = FALSE,  
         seed = seed, 
         plot = FALSE, 
         n_runs = n_runs,
         scenario_name = "Correlation_",
         dim = c(300,300),
         env.beta = 1.2,
         plotdat = TRUE,
         sigma2x = 0.5,
         kappa = 0.05,
         strata = 25,
         rows = 5,
         cols = 5,
         probs = c(rep(seq(0.5, 0.01, length.out = 5),5)),
         qsize = 1,
         rho = 0.8,
         nsamp = 150,
         lambda = -3,
         correlation = TRUE,
         resolution = c(10,10))

# joint model
run_scenario(parameter = "TRUE",
         model_type="joint", 
         plotting=FALSE, 
         summary_results=FALSE,  
         seed = seed, 
         plot = FALSE, 
         n_runs = n_runs,
         scenario_name = "Correlation_",
         dim = c(300,300),
         env.beta = 1.2,
         plotdat = TRUE,
         sigma2x = 0.5,
         kappa = 0.05,
         strata = 25,
         rows = 5,
         cols = 5,
         probs = c(rep(seq(0.5, 0.01, length.out = 5),5)),
         qsize = 1,
         rho = 0.8,
         nsamp = 150,
         correlation = TRUE,
         lambda = -3,
         resolution = c(10,10)) # to use the function you must put in all parameters it is expecting

# jointcov model
run_scenario(parameter = "TRUE",
         model_type="jointcov", 
         plotting=FALSE, 
         summary_results=FALSE,  
         seed = seed, 
         plot = FALSE, 
         n_runs = n_runs,
         scenario_name = "Correlation_",
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
         lambda = -3,
         correlation = TRUE,
         resolution = c(10,10))

# joint2 model
run_scenario(parameter = "TRUE",
         model_type="joint2", 
         plotting=FALSE, 
         summary_results=FALSE,  
         seed = seed, 
         plot = FALSE, 
         n_runs = n_runs,
         scenario_name = "Correlation_",
         dim = c(300,300),
         env.beta = 1.2,
         plotdat = TRUE,
         sigma2x = 0.5,
         kappa = 0.05,
         strata = 25,
         rows = 5,
         cols = 5,
         probs = rep(seq(0.5, 0.01, length.out = 5),5),
         qsize = 1,
         rho = 0.8,
         nsamp = 150,
         correlation = TRUE,
         lambda = -3,
         resolution = c(10,10)) 
