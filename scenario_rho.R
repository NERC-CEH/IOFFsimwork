#### Code to run scenarios - RHO

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
## STRUCTURED SAMPLE SIZE
rho_param <- c(0.90,0.95,0.99,1) # 10 scenarios

# # structured model
# mapply(FUN = run_scenario,
#        rho = rho_param, 
#        parameter = rho_param,
#        MoreArgs = list(
#              model_type="structured", 
#              plotting=FALSE, 
#              summary_results=FALSE,  
#              seed = seed, 
#              plot = FALSE, 
#              n_runs = n_runs,
#              scenario_name = "Rho_",
#              dim = c(300,300),
#              lambda = -1,
#              env.beta = 1.2,
#              plotdat = TRUE,
#              sigma2x = 0.5,
#              kappa = 0.05,
#              strata = 25,
#              rows = 5,
#              cols = 5,
#              probs = c(rep(seq(0.5, 0.01, length.out = 5),5)),
#              qsize = 1,
#              nsamp = 150,
#              resolution = c(10,10),
#              correlation = FALSE)) # to use the function you must put in all parameters it is expecting
# 
# # unstructured model
# mapply(FUN = run_scenario,
#        rho = rho_param, 
#        parameter = rho_param,
#        MoreArgs = list(
#          model_type="unstructured", 
#          plotting=FALSE, 
#          summary_results=FALSE,  
#          seed = seed, 
#          plot = FALSE, 
#          n_runs = n_runs,
#          scenario_name = "Rho_",
#          dim = c(300,300),
#          lambda = -1,
#          env.beta = 1.2,
#          plotdat = TRUE,
#          sigma2x = 0.5,
#          kappa = 0.05,
#          strata = 25,
#          rows = 5,
#          cols = 5,
#          probs = c(rep(seq(0.5, 0.01, length.out = 5),5)),
#          qsize = 1,
#          nsamp = 150,
#          resolution = c(10,10),
#          correlation = FALSE))
# 
# # unstructuredcov model
# mapply(FUN = run_scenario,
#        rho = rho_param, 
#        parameter = rho_param,
#        MoreArgs = list(
#          model_type="unstructuredcov", 
#          plotting=FALSE, 
#          summary_results=FALSE,  
#          seed = seed, 
#          plot = FALSE, 
#          n_runs = n_runs,
#          scenario_name = "Rho_",
#          dim = c(300,300),
#          lambda = -1,
#          env.beta = 1.2,
#          plotdat = TRUE,
#          sigma2x = 0.5,
#          kappa = 0.05,
#          strata = 25,
#          rows = 5,
#          cols = 5,
#          probs = c(rep(seq(0.5, 0.01, length.out = 5),5)),
#          qsize = 1,
#          nsamp = 150,
#          resolution = c(10,10),
#          correlation = FALSE))
# 
# joint model
mapply(FUN = run_scenario,
       rho = rho_param, 
       parameter = rho_param,
       MoreArgs = list(
         model_type="joint", 
         plotting=FALSE, 
         summary_results=FALSE,  
         seed = seed, 
         plot = FALSE, 
         n_runs = n_runs,
         scenario_name = "Rho_",
         dim = c(300,300),
         lambda = -3,
         env.beta = 1.2,
         plotdat = TRUE,
         sigma2x = 0.5,
         kappa = 0.05,
         strata = 25,
         rows = 5,
         cols = 5,
         probs = c(rep(seq(0.5, 0.01, length.out = 5),5)),
         qsize = 1,
         nsamp = 150,
         resolution = c(10,10),
         correlation = FALSE)) # to use the function you must put in all parameters it is expecting

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
         lambda = -3,
         env.beta = 1.2,
         plotdat = TRUE,
         sigma2x = 0.5,
         kappa = 0.05,
         strata = 25,
         rows = 5,
         cols = 5,
         probs = c(rep(seq(0.5, 0.01, length.out = 5),5)),
         qsize = 1,
         nsamp = 150,
         resolution = c(10,10),
         correlation = FALSE)) # to use the function you must put in all parameters it is expecting

# joint2 model
mapply(FUN = run_scenario,
       rho = rho_param, 
       parameter = rho_param,
       MoreArgs = list(
         model_type="jointtwo", 
         plotting=FALSE, 
         summary_results=FALSE,  
         seed = seed, 
         plot = FALSE, 
         n_runs = n_runs,
         scenario_name = "Rho_",
         dim = c(300,300),
         lambda = -3,
         env.beta = 1.2,
         plotdat = TRUE,
         sigma2x = 0.5,
         kappa = 0.05,
         strata = 25,
         rows = 5,
         cols = 5,
         probs = c(rep(seq(0.5, 0.01, length.out = 5),5)),
         qsize = 1,
         nsamp = 150,
         resolution = c(10,10),
         correlation = FALSE)) # to use the function you must put in all parameters it is expecting
