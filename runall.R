#' Demonstrate different parameter combinations for models
#' 
#' Import default parameters
#' 
source("setParams.R")

library(viridis)

lambda <- -3 #manageable number
#env.beta <- 3 #stronger env effect

#' The default parameters are as follows:
#' 
#' - simulation is conducted on a grid of 300*300  
#' - environmental covariate coefficient of NULL
#' - scale parameter kappa for matern covariance of 0.05  
#' - variance parameter sigma2x of matern covariance of 2  
#' - mean log intensity of point process of -3  
#' - 500 structured samples  - to simulate "perfect" coverage
#' - 3 strata  
#' - probability of sampling strata at 1, 1 and 1 used for thinning point process (i.e no thinning)  
#' - qsize of 1

#' Here we can change default parameters:
#' 
#' e.g. we want to simulate a point process with a smaller mean intensity

lambda <- -3

#' ## Generate data from parameters

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
#' 
#' ### Plots of the data generation process
#' 
#+ warning = FALSE, message = FALSE, error = FALSE, include = FALSE
source("Functions to generate data and sample.R")

g1 <- genDataFunctions(dim = dim, lambda = lambda, env.beta = env.beta, seed = seed, kappa = kappa, sigma2x = sigma2x, strata = strata, rows = rows, cols = cols, probs = probs, nsamp = nsamp, qsize = 1, rho = 0.99)

structured_data <- g1$structured_data
unstructured_data <- g1$unstructured_data
biasfield <- g1$biasfield
dat1 <- g1$dat1
biascov <- g1$biascov
strata1 <- g1$strata1


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
palette(viridis(50))
plot(biasfield$y ~ biasfield$x, col = biasfield$stratprobs*100)
legend(330,300, col = c(50,40,30,20,10), pch = 20, legend = c("0.4-0.5", "0.3-0.4", "0.2-0.3", "0.1-0.2", "0.01-0.1"), title = "Probability")

#' Visualise thinned unstructured data
#+ echo = FALSE 
par(mfrow=c(1,1))
image.plot(list(x=dat1$Lam$xcol, y=dat1$Lam$yrow, z=t(dat1$rf.s)), main='Thinned unstructured data', asp=1) 
points(unstructured_data$x, unstructured_data$y, pch = 20)#note rescale again - plotting back on original

#' Visualise structured data
#+ echo = FALSE  
par(mfrow=c(1,1))
image.plot(list(x=dat1$Lam$xcol, y=dat1$Lam$yrow, z=t(dat1$rf.s)), main='Structured data', asp=1) 
points(structured_data$x,structured_data$y, pch = 21, bg = structured_data$presence, col = "black")
par(xpd = TRUE)
legend(-10,360,c("Absence", "Presence"), pch = 21, col = "black", pt.bg = c(0,1))

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
#' 5. The predicted mean intensity back in environmental space (averaged on a grid system)
#' 
#' 6. The predicted standard deviation of intensity in environmental space (averaged on a grid system)
#' 
#' 7. The relative difference between the 'true intensity' and the predicted intensity in environmental space (averaged on a grid system)
#' 
#' Relative differences are calculated for 10X10 squares across the space. 
#' Predictions generated at centre of these squares.
#' True intensity averaged across each square.
#' 
#' ### Structured model 
#' 
# #+ warning = FALSE, message = FALSE, error = FALSE, figure.align = "center"
source("Run models structured.R")
mod_1 <- structured_model(structured_data, dat1, biasfield, plotting = TRUE)
source("validation_function.R")
validation_1 <- validation_function(result=mod_1[[2]], resolution=c(10,10), join.stack=mod_1[[1]], model_type="structured",
                                    structured_data = structured_data, dat1 = dat1, summary_results=T, qsize = 1, absolute=TRUE, dim = dim, plotting = TRUE)
validation_1_r <- validation_function(result=mod_1[[2]], resolution=c(10,10), join.stack=mod_1[[1]], model_type="structured",
                                      structured_data = structured_data, dat1 = dat1, summary_results=T, qsize = 1, absolute=FALSE, dim = dim, plotting = TRUE)

#' ### Unstructured model
#' 
#+ warning = FALSE, message = FALSE, error = FALSE
source("Run models.R")
mod_2 <- unstructured_model(unstructured_data, dat1, biasfield, dim = dim, plotting = TRUE)
source("validation_function.R")
validation_2 <- validation_function(result=mod_2[[2]], resolution=c(10,10), join.stack=mod_2[[1]], model_type="unstructured", 
                                    unstructured_data = unstructured_data, dat1 = dat1, summary_results=T, absolute=TRUE, dim = dim, plotting = TRUE)
validation_2_r <- validation_function(result=mod_2[[2]], resolution=c(10,10), join.stack=mod_2[[1]], model_type="unstructured", 
                                      unstructured_data = unstructured_data, dat1 = dat1, summary_results=T, absolute=FALSE, dim = dim, plotting = TRUE)




#' ### Joint model
#'
#+ warning = FALSE, message = FALSE, error = FALSE
#joint model (no covariate on bias)
source("Run models joint.R")
mod_3 <- joint_model(structured_data, unstructured_data, dat1, biasfield)
source("validation_function.R")
validation_3 <- validation_function(result=mod_3[[2]], resolution=c(10,10), join.stack=mod_3[[1]], model_type="joint",
                                    unstructured_data = unstructured_data, structured_data = structured_data,
                                    dat1 = dat1, summary_results=T, absolute=TRUE, dim = dim, plotting = TRUE)
validation_3_r <- validation_function(result=mod_3[[2]], resolution=c(10,10), join.stack=mod_3[[1]], model_type="joint",
                                      unstructured_data = unstructured_data, structured_data = structured_data,
                                      dat1 = dat1, summary_results=T, absolute=FALSE, dim = dim, plotting = TRUE)


#' ### Unstructured model with covariate for bias
#' 
#+ warning = FALSE, message = FALSE, error = FALSE
source("Run models unstructured bias covariate.R")
mod_4 <- unstructured_model_cov(unstructured_data, dat1, biasfield, dim = dim, plotting = TRUE, biascov = biascov)
source("validation_function.R")
validation_4 <- validation_function(result=mod_4[[2]], resolution=c(10,10), join.stack=mod_4[[1]], model_type="unstructuredcov", 
                                     unstructured_data = unstructured_data, dat1 = dat1, summary_results=T, absolute=TRUE, dim = dim, plotting = TRUE)
validation_4_r <- validation_function(result=mod_4[[2]], resolution=c(10,10), join.stack=mod_4[[1]], model_type="unstructuredcov", 
                                       unstructured_data = unstructured_data, dat1 = dat1, summary_results=T, absolute=FALSE, dim = dim, plotting = TRUE)

#
#joint model (covariate on bias)
source("Run models joint covariate for bias.R")
mod_5 <- joint_model_cov(structured_data, unstructured_data, dat1, biasfield, resolution = c(10,10), biascov = biascov)
source("validation_function.R")
validation_5 <- validation_function(result=mod_5[[2]], resolution=c(10,10), join.stack=mod_5[[1]], model_type="jointcov",
                                    unstructured_data = unstructured_data, structured_data = structured_data,
                                    dat1 = dat1, summary_results=T, absolute = TRUE, dim = dim, plotting = TRUE)
validation_5_r <- validation_function(result=mod_5[[2]], resolution=c(10,10), join.stack=mod_5[[1]], model_type="jointcov",
                                    unstructured_data = unstructured_data, structured_data = structured_data,
                                    dat1 = dat1, summary_results=T, absolute = FALSE, dim = dim, plotting = TRUE)


##joint model with second spatial field
source("run models joint second field.R")
mod_6 <- joint_model2(structured_data, unstructured_data, dat1, biasfield)
source("validation_function.R")
validation_6 <- validation_function(result=mod_6[[2]], resolution=c(10,10), join.stack=mod_6[[1]], model_type="joint2",
                                    unstructured_data = unstructured_data, structured_data = structured_data,
                                    dat1 = dat1, summary_results=T, absolute = TRUE, dim = dim, plotting = TRUE)
validation_6_r <- validation_function(result=mod_6[[2]], resolution=c(10,10), join.stack=mod_6[[1]], model_type="joint2",
                                    unstructured_data = unstructured_data, structured_data = structured_data,
                                    dat1 = dat1, summary_results=T, absolute = FALSE, dim = dim, plotting = TRUE)


##covariate model
source("Run cov model all eff prior.R")
mod_7 <- covariate_model(structured_data = structured_data, unstructured_data = unstructured_data, unstr_mod_type = "unstructured only", dat1, biasfield, dim = dim)
source("validation_function.R")
validation_7 <- validation_function(result=mod_7[[2]], resolution=c(10,10), join.stack=mod_7[[1]], model_type="covariate",
                                    unstructured_data = unstructured_data, structured_data = structured_data,
                                    dat1 = dat1, summary_results=T, absolute = TRUE, dim = dim, plotting = TRUE)
validation_7_r <- validation_function(result=mod_7[[2]], resolution=c(10,10), join.stack=mod_7[[1]], model_type="covariate",
                                      unstructured_data = unstructured_data, structured_data = structured_data,
                                      dat1 = dat1, summary_results=T, absolute = FALSE, dim = dim, plotting = TRUE)


##correlation model
source("Run correlation model.R")
mod_8 <- correlation_model(structured_data = structured_data, unstructured_data = unstructured_data, dat1, biasfield, dim = dim)
source("validation_function.R")
validation_8 <- validation_function(result=mod_8[[2]], resolution=c(10,10), join.stack=mod_8[[1]], model_type="correlation",
                                    unstructured_data = unstructured_data, structured_data = structured_data,
                                    dat1 = dat1, summary_results=T, absolute = TRUE, dim = dim, plotting = TRUE)
validation_8_r <- validation_function(result=mod_8[[2]], resolution=c(10,10), join.stack=mod_8[[1]], model_type="correlation",
                                      unstructured_data = unstructured_data, structured_data = structured_data,
                                      dat1 = dat1, summary_results=T, absolute = FALSE, dim = dim, plotting = TRUE)




#note these are now from the relative validation


## Compare Mean Absolute Error between models
validation_1_r$'Proto-table'
validation_2_r$'Proto-table'
validation_3_r$'Proto-table'
validation_4_r$'Proto-table'
validation_5_r$'Proto-table'
validation_6_r$'Proto-table'
validation_7_r$`Proto-table`
validation_8_r$`Proto-table`

#
# ## Compare estimation of environmental covariate between models
mod_1$result$summary.fixed
mod_2$result$summary.fixed
mod_3$result$summary.fixed
mod_4$result$summary.fixed
mod_5$result$summary.fixed
mod_6$result$summary.fixed
mod_7$result$summary.fixed
mod_8$result$summary.fixed

## Compare correlation of true vs estimated relative intensities
validation_1_r$correlation
validation_2_r$correlation
validation_3_r$correlation
validation_4_r$correlation
validation_5_r$correlation
validation_6_r$correlation
validation_7_r$correlation
validation_8_r$correlation



