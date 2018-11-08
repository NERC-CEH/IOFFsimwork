#' Demonstrate different parameter combinations for models
#' 
#' Import default parameters
#' 
source("setParams.R")

#' The default parameters are as follows:
#' 
#' - simulation is conducted on a grid of 100*300  
#' - environmental covariate coefficient of 0.2  
#' - scale parameter kappa for matern covariance of 0.3  
#' - variance parameter sigma2x of matern covariance of 0.2  
#' - mean intensity of point process of 5  
#' - 50 structured samples  
#' - 3 strata  
#' - probability of sampling strata at 0.2, 0.8 and 0.5 used for thinning point process  

#' Here we can change default parameters:

#e.g. we want to simulate a point process with a smaller mean intensity

lambda <- 5

#' ## Generate data from parameters.
#' 
#' This script generates data using the parameters specified above. The key steps are:
#' 
#' 1. Simulate a log Gaussian Cox process with the lambda, kappa, sigma2x and dimensions specified above. This creates a point process and a realisation from this point process which is then thinned to create the unstructured data. If the env.beta parameter is not set to NULL then the point process is simulated to be dependent on an environmental covariate with the coefficient of this effect specified by env.beta. The environmental covariate is simple and has three levels distributed equally across the surface.
#' 
#' 2. Thin the point process to create spatially biased unstructured data. Currently this is done by strata, and the user can specify the number of strata and the probabilities of sampling each point in each stratum.
#' 
#' 3. Create a new realisation from the same point process which will become the structured data. 
#' 
#' 4. Sample the new points over an area (roughly speaking) by generating stratified random points (to ensure global coverage) and then denoting presence as the presence of one or more point process realisation in the 5x5 neighbourhood surrounding the stratified random points. Absence is denoted as the absence of any point process realisation in this "quadrat".
#' 
source("Functions to generate data and sample.R")

#' ## Run individual data models
#' 
#' Models are run for each data type separately first then a joint model is computed. All models are run in INLA. 
#' 
#' These models have the following assumptions:
#' 
#' 1. Unstructured data is presence only and can be modelled as a Poisson point process
#' 
#' 2. Structured data is presence/absence can be modelled as a binomial 
#' 
#' 3. A single environmental covariate determines intensity and is included in the model
#' 
#' 4. In joint models the true species occurrence can be represented with a shared random spatial field
#' 
#' 
source("Run models structured.R")
mod_1 <- structured_model(structured_data, dat1, biasfield)
validation_tab

source("Run models.R")
mod_2 <- unstructured_model(unstructured_data, dat1, biasfield)
source("validation_function.R")
validation_2 <- validation_function(result=mod_2[[2]], resolution=c(10,10), join.stack=mod_2[[1]], model_type="unstructured", 
                                    unstructured_data = unstructured_data, dat1 = dat1, plot=T, summary_results=T)

#' Run joint model
source("Run models joint.R")
mod_joint <- joint_model(structured_data, unstructured_data, dat1, biasfield)
result = mod_joint[[2]]
join.stack = mod_joint[[1]]



