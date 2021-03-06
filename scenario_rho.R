#### Code to run scenarios - RHO

# run all multiple times and save output
# all run as functions 

# load packages needed to run in parallel
library(foreach)
library(doParallel)

# choose number of times to run
n_runs <- 500
#n_runs<-1

# create a randomly generated string of seeds
# seed must be integer
set.seed(1)
seed <- sample(round(1:100000000),n_runs,replace=F)
#seed <- NULL


# set up the scenario with parameters that need to be changed

# set parameters
#source("setParams.R") # this is now redundant
source("run_scenario.R")

# change those that need changing
## STRUCTURED SAMPLE SIZE
rho_param <- c(0.90,0.95,0.99,1) # 10 scenarios

# 
# unstructuredcov model
 mapply(FUN = run_scenario,
        rho = rho_param, 
        parameter = rho_param,
        MoreArgs = list(
          model_type="unstructuredcov", 
          plotting=FALSE, 
          summary_results=FALSE,  
          seed = seed, 
          plot = FALSE, 
          n_runs = n_runs,
          scenario_name = "Rho_",
          dim = c(300,300),
          lambda = -3,
          env.beta = 2,
          plotdat = TRUE,
          sigma2x = 0.5,
          kappa = 0.05,
          strata = 25,
          rows = 5,
          cols = 5,
          probs = 0.2,
          qsize = 1,
          nsamp = 150,
          resolution = c(10,10),
          correlation = FALSE))

# jointcov model
mapply(FUN = run_scenario,
       rho = rho_param, 
       parameter = rho_param,
       MoreArgs = list(
         model_type="jointcov", 
         plotting=FALSE, 
         summary_results=FALSE,  
         seed = seed, 
         plot = FALSE, 
         n_runs = n_runs,
         scenario_name = "Rho_",
         dim = c(300,300),
         lambda = -2,
         env.beta = 2,
         plotdat = TRUE,
         sigma2x = 0.5,
         kappa = 0.05,
         strata = 25,
         rows = 5,
         cols = 5,
         probs = 0.2,
         qsize = 1,
         nsamp = 150,
         resolution = c(10,10),
         correlation = FALSE)) # to use the function you must put in all parameters it is expecting
