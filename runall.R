#' Demonstrate different parameter combinations for models
#' 
#' Import default parameters
#' 
source("setParams.R")

#' The default parameters are as follows:
#' 
#' - simulation is conducted on a grid of 100*300  
#' - environmental covariate coefficient of NULL
#' - scale parameter kappa for matern covariance of 0.05  
#' - variance parameter sigma2x of matern covariance of 2  
#' - mean log intensity of point process of -3  
#' - 500 structured samples  - to simulate "perfect" coverage
#' - 3 strata  
#' - probability of sampling strata at 1, 1 and 1 used for thinning point process (i.e no thinning)  
#' - qsize of 5

#' Here we can change default parameters:

#e.g. we want to simulate a point process with a smaller mean intensity

nsamp = 250
probs = c(rep(0.5,5), rep(0.3, 5), rep(0.1,5), rep(0.05,5), rep(0.01,5))

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
#+ warning = FALSE, message = FALSE, error = FALSE, include = FALSE
source("Functions to generate data and sample.R")

#' Visualise the random field and covariate pattern
#+ echo = FALSE 
par(mfrow=c(1,3)) 
image.plot(list(x=dat1$Lam$xcol, y=dat1$Lam$yrow, z=t(dat1$rf.s)), main='log-Lambda', asp=1) 
points(dat1$xy, pch=19)
image.plot(list(x=dat1$Lam$xcol, y=dat1$Lam$yrow, z=t(dat1$gridcov)), main='Covariate', asp=1)
image.plot(list(x=dat1$Lam$xcol, y=dat1$Lam$yrow, z=t(biascov)), main='Bias covariate', asp=1)

#' Visualise the strata and associated probabilities of sampling
#+ echo = FALSE 
par(mfrow=c(1,1), xpd = TRUE) 
par(mar=c(4,4,4,7))
plot(strata1$y ~ strata1$x, col = strata1$stratum)
legend(110,300, fill = unique(strata1$stratum), legend = probs, title = "Probability")

#' Visualise thinned unstructured data
#+ echo = FALSE 
par(mfrow=c(1,1))
image.plot(list(x=dat1$Lam$xcol, y=dat1$Lam$yrow, z=t(dat1$rf.s)), main='Thinned unstructured data', asp=1) 
points(thin1$x, thin1$y, pch = 20)#note rescale again - plotting back on original

#' Visualise structured data
#+ echo = FALSE  
par(mfrow=c(1,1))
image.plot(list(x=dat1$Lam$xcol, y=dat1$Lam$yrow, z=t(dat1$rf.s)), main='Structured data', asp=1) 
points(structured_data$x,structured_data$y, pch = 21, bg = structured_data$presence, col = "black")
legend(1.2,2.5,c("Absence", "Presence"), pch = 21, col = "black", pt.bg = c(0,1))


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
#' ## Model outputs
#' 
#' Model outputs follow below (in the order of structured only, unstructured only, and then the joint model):
#' 
#' 1. The mean of the estimated intensity from the model AFTER accounting for the environment
#' 
#' 2. The 'true' species intensity driven by environmental covariate
#' 
#' 3. The standard deviation of the estimated intensity from the model AFTER accounting for the evnironment
#' 
#' Then we have some model validation plots:
#' 
#' 4. The 'true' species intensity driven by environmental covariate
#' 
#' 5. The predicted mean intensity back in environmental space
#' 
#' 6. The predicted standard deviation of intensity in environmental space
#' 
#' 7. The relative difference between the 'true intensity' and the predicted intensity in environmental space. 
#' 
#' Relative differences are calculated for 10X10 squares across the space. 
#' Predictions generated at centre of these squares.
#' True intensity averaged across each square.
#' Both have mean intensity subtracted, then deviation from mean compared. 
#' 
#+ warning = FALSE, message = FALSE, error = FALSE, figure.align = "center"
source("Run models structured.R")
mod_1 <- structured_model(structured_data, dat1, biasfield)
source("validation_function.R")
validation_1 <- validation_function(result=mod_1[[2]], resolution=c(10,10), join.stack=mod_1[[1]], model_type="structured", 
                                    structured_data = structured_data, dat1 = dat1, plot=T, summary_results=T, qsize = 5, method = "absolute", dim = dim)

#+ warning = FALSE, message = FALSE, error = FALSE
source("Run models.R")
mod_2 <- unstructured_model(unstructured_data, dat1, biasfield, dim = dim)
source("validation_function.R")
validation_2 <- validation_function(result=mod_2[[2]], resolution=c(10,10), join.stack=mod_2[[1]], model_type="unstructured", 
                                    unstructured_data = unstructured_data, dat1 = dat1, plot=T, summary_results=T, method = "absolute", dim = dim)

#+ warning = FALSE, message = FALSE, error = FALSE
#joint model (no covariate on bias)
source("Run models joint.R")
mod_3 <- joint_model(structured_data, unstructured_data, dat1, biasfield)
source("validation_function.R")
validation_3 <- validation_function(result=mod_3[[2]], resolution=c(10,10), join.stack=mod_3[[1]], model_type="joint", 
                                    unstructured_data = unstructured_data, structured_data = structured_data,
                                    dat1 = dat1, plot=T, summary_results=T, method = "absolute", dim = dim)

#joint model (covariate on bias)
source("Run models joint covariate for bias.R")
mod_4 <- joint_model_cov(structured_data, unstructured_data, dat1, biasfield)
source("validation_function.R")
validation_4 <- validation_function(result=mod_4[[2]], resolution=c(10,10), join.stack=mod_4[[1]], model_type="jointcov", 
                                    unstructured_data = unstructured_data, structured_data = structured_data,
                                    dat1 = dat1, plot=T, summary_results=T, method = "absolute", dim = dim)


##joint model with second spatial field
source("Run models joint second field.R")
mod_5 <- joint_model2(structured_data, unstructured_data, dat1, biasfield)
source("validation_function.R")
validation_5 <- validation_function(result=mod_5[[2]], resolution=c(10,10), join.stack=mod_5[[1]], model_type="joint2", 
                                    unstructured_data = unstructured_data, structured_data = structured_data,
                                    dat1 = dat1, plot=T, summary_results=T, method = "absolute", dim = dim)

validation_1$'Proto-table'
validation_2$'Proto-table'
validation_3$'Proto-table'
validation_4$'Proto-table'
validation_5$'Proto-table'

mod_1$result$summary.fixed
mod_2$result$summary.fixed
mod_3$result$summary.fixed
mod_4$result$summary.fixed
mod_5$result$summary.fixed

validation_1$correlation
validation_2$correlation
validation_3$correlation
validation_4$correlation
validation_5$correlation

# #relative validation
# 
# source("validation_function.R")
# validation_1_r <- validation_function(result=mod_1[[2]], resolution=c(10,10), join.stack=mod_1[[1]], model_type="structured", 
#                                     structured_data = structured_data, dat1 = dat1, plot=T, summary_results=T, qsize = 5, method = "relative", dim = dim)
# 
# validation_2_r <- validation_function(result=mod_2[[2]], resolution=c(10,10), join.stack=mod_2[[1]], model_type="unstructured", 
#                                     unstructured_data = unstructured_data, dat1 = dat1, plot=T, summary_results=T, method = "relative", dim = dim)
# 
# 
# validation_3_r <- validation_function(result=mod_joint[[2]], resolution=c(10,10), join.stack=mod_joint[[1]], model_type="joint", 
#                                     unstructured_data = unstructured_data, structured_data = structured_data,
#                                     dat1 = dat1, plot=T, summary_results=T, method = "relative", dim = dim)
# 
# 
# validation_4_r <- validation_function(result=mod_joint2[[2]], resolution=c(10,10), join.stack=mod_joint2[[1]], model_type="joint2", 
#                                     unstructured_data = unstructured_data, structured_data = structured_data,
#                                     dat1 = dat1, plot=T, summary_results=T, method = "relative", dim = dim)
# 
# validation_1_r$'Proto-table'
# validation_2_r$'Proto-table'
# validation_3_r$'Proto-table'
# validation_4_r$'Proto-table'




