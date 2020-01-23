#' Demonstrate different parameter combinations for models
#' 
#' Import default parameters
#' 
source("setParams.R")

library(viridis)
library(reshape)


#' The default parameters are as follows:
#' 
#' - simulation is conducted on a grid of 300*300  
#' - environmental covariate coefficient of NULL
#' - scale parameter kappa for matern covariance of 0.05  
#' - variance parameter sigma2x of matern covariance of 2  
#' - mean log intensity of point process of -2  
#' - 150 structured samples 
#' - 25 strata  
#' - maximum probability of detection of 0.2 


#' Here we can change default parameters:
#' 
#' e.g. we want to simulate a point process with a smaller mean intensity

lambda <- -3


#' ## Generate data from parameters

#' 
#' This script generates data using the parameters specified above. The key steps are:
#' 
#' 1. Simulate a log Gaussian Cox process with the lambda, kappa, sigma2x and dimensions specified above. This creates a point process and a realisation from this point process which is then thinned to create the unstructured data. If the env.beta parameter is not set to NULL then the point process is simulated to be dependent on an environmental covariate with the coefficient of this effect specified by env.beta. The covariate has a linear spatial pattern with highest covariate values at the top of the domain.
#' 
#' 2. Thin the point process to create spatially biased unstructured data. The thinning is performed by creating a surface which defines the probability of each point being sampled. Probability decreases along the surface from a maximum value, by default in a direction orthogonal to the environmental covariate.
#' 
#' 3. Create a new realisation from the same point process which will become the structured data. 
#' 
#' 4. Sample the new points generating stratified random points "quadrats" (to ensure global coverage) and then denoting presence as the intersection of these quadrats with the point process realisation. By default quadrat size is 1
#' 
#' 
#' ### Plots of the data generation process
#' 
#+ warning = FALSE, message = FALSE, error = FALSE, include = FALSE
source("Functions to generate data and sample.R")

g1 <- genDataFunctions(dim = dim, lambda = lambda, env.beta = env.beta, seed = seed, kappa = kappa, sigma2x = sigma2x, strata = strata, rows = rows, cols = cols, probs = probs, nsamp = nsamp, qsize = 1, rho = 0.99, correlated = FALSE)

structured_data <- g1$structured_data
unstructured_data <- g1$unstructured_data
biasfield <- g1$biasfield
dat1 <- g1$dat1
biascov <- g1$biascov
strata1 <- g1$strata1

#' 
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
#' 
#' Visualise structured data
#+ echo = FALSE
par(mfrow=c(1,1))
image.plot(list(x=dat1$Lam$xcol, y=dat1$Lam$yrow, z=t(dat1$rf.s)), main='Structured data', asp=1)
points(structured_data$x,structured_data$y, pch = 21, bg = structured_data$presence, col = "black")
par(xpd = TRUE)
legend(-10,360,c("Absence", "Presence"), pch = 21, col = "black", pt.bg = c(0,1))


### To create Figure 1

# png("Figure 1.png", height = 1200, width = 1200, units = "mm", pointsize = 60, res= 30)
# 
# par(mfrow=c(2,2))
# 
# #Environmental covariate
# image.plot(list(x=dat1$Lam$xcol, y=dat1$Lam$yrow, z=t(dat1$gridcov)), main=expression(bold(paste('Environmental covariate ', bolditalic("x(s)")))), asp=1, col = viridis(50))
# mtext("a)", side = 3, line = 2, outer = FALSE, at = -10)
# 
# #Example truth
# image.plot(list(x=dat1$Lam$xcol, y=dat1$Lam$yrow, z=t(dat1$rf.s)), main=expression(bold(paste("Species intensity (log ", lambda, "(s) )" ))), asp=1 , col = viridis(50))
# mtext("b)", side = 3, line = 2, outer = FALSE, at = -10)
# 
# 
# #Bias
# palette(viridis(50))
# c1 <- cast(biasfield, y ~ x, value = "stratprobs", fun.aggregate = mean)
# c1 <- as.matrix(c1[,-1])
# image.plot(list(x=dat1$Lam$xcol, y=dat1$Lam$yrow, z=t(c1)), main = expression(bold(paste("Detection probability ", bolditalic("p(s)")))), asp = 1, col = viridis(50))
# mtext("c)", side = 3, line = 2, outer = FALSE, at = -10)
# 
# 
# #Example samples
# image.plot(list(x=dat1$Lam$xcol, y=dat1$Lam$yrow, z=t(dat1$rf.s)), main='Sampled data', asp=1, col = viridis(50))
# points(unstructured_data$x, unstructured_data$y, pch = 20, col = "grey25")
# points(structured_data$x,structured_data$y, pch = 22, bg = structured_data$presence, col = "black", cex = 1.2)
# par(xpd = TRUE)
# legend(250,360,c("Absence", "Presence"), pch = 22, col = "black", pt.bg = c(0,1))
# mtext("d)", side = 3, line = 2, outer = FALSE, at = -10)
# 
# 
# dev.off()



#' ## Run models

#' Six types of models are run: PA only, PO only, IDM, PO with bias covariate, IDM with bias covariate, IDM with second spatial field. All models are run in INLA.
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

 

### (A) PA only model

# #+ warning = FALSE, message = FALSE, error = FALSE, figure.align = "center"
source("Run models structured.R")
mod_1 <- structured_model(structured_data, dat1, biasfield, plotting = FALSE)
source("validation_function.R")
validation_1_r <- validation_function(result=mod_1[[2]], resolution=c(10,10), join.stack=mod_1[[1]], model_type="structured",
                                      structured_data = structured_data, dat1 = dat1, summary_results=T, qsize = 1, absolute=FALSE, dim = dim, plotting = FALSE)

#' ### (B) PO only model
#'
#+ warning = FALSE, message = FALSE, error = FALSE
source("Run models.R")
mod_2 <- unstructured_model(unstructured_data, dat1, biasfield, dim = dim, plotting = TRUE)
source("validation_function.R")
validation_2_r <- validation_function(result=mod_2[[2]], resolution=c(10,10), join.stack=mod_2[[1]], model_type="unstructured",
                                      unstructured_data = unstructured_data, dat1 = dat1, summary_results=T, absolute=FALSE, dim = dim, plotting = FALSE)


#' ### (C) IDM
#'
#+ warning = FALSE, message = FALSE, error = FALSE
#joint model (no covariate on bias)
source("Run models joint.R")
mod_3 <- joint_model(structured_data, unstructured_data, dat1, biasfield)
source("validation_function.R")
validation_3_r <- validation_function(result=mod_3[[2]], resolution=c(10,10), join.stack=mod_3[[1]], model_type="joint",
                                      unstructured_data = unstructured_data, structured_data = structured_data,
                                      dat1 = dat1, summary_results=T, absolute=FALSE, dim = dim, plotting = FALSE)


#' ### (D) PO only with covariate for bias
#' 
#+ warning = FALSE, message = FALSE, error = FALSE
source("Run models unstructured bias covariate.R")
mod_4 <- unstructured_model_cov(unstructured_data, dat1, biasfield, dim = dim, plotting = TRUE, biascov = biascov)
source("validation_function.R")
validation_4_r <- validation_function(result=mod_4[[2]], resolution=c(10,10), join.stack=mod_4[[1]], model_type="unstructuredcov", 
                                       unstructured_data = unstructured_data, dat1 = dat1, summary_results=T, absolute=FALSE, dim = dim, plotting = FALSE)


mod_4$result$summary.fixed
validation_4_r$'Proto-table'


#' ### (E) IDM with covariate for bias
source("Run models joint covariate for bias.R")
mod_5 <- joint_model_cov(structured_data, unstructured_data, dat1, biasfield, resolution = c(10,10), biascov = biascov)
source("validation_function.R")
validation_5_r <- validation_function(result=mod_5[[2]], resolution=c(10,10), join.stack=mod_5[[1]], model_type="jointcov",
                                    unstructured_data = unstructured_data, structured_data = structured_data,
                                    dat1 = dat1, summary_results=T, absolute = FALSE, dim = dim, plotting = FALSE)


#'### (F) IDM with second spatial field
source("Run models joint second spatial field.R")
mod_6 <- joint_model2(structured_data, unstructured_data, dat1, biasfield)
source("validation_function.R")
validation_6_r <- validation_function(result=mod_6[[2]], resolution=c(10,10), join.stack=mod_6[[1]], model_type="joint2",
                                    unstructured_data = unstructured_data, structured_data = structured_data,
                                    dat1 = dat1, summary_results=T, absolute = FALSE, dim = dim, plotting = FALSE)



## Compare Mean Absolute Error between models
validation_1_r$'Proto-table'
validation_2_r$'Proto-table'
validation_3_r$'Proto-table'
validation_4_r$'Proto-table'
validation_5_r$'Proto-table'
validation_6_r$'Proto-table'

## Compare estimation of environmental covariate between models
mod_1$result$summary.fixed
mod_2$result$summary.fixed
mod_3$result$summary.fixed
mod_4$result$summary.fixed
mod_5$result$summary.fixed
mod_6$result$summary.fixed

## Compare correlation of true vs estimated relative intensities
validation_1_r$correlation
validation_2_r$correlation
validation_3_r$correlation
validation_4_r$correlation
validation_5_r$correlation
validation_6_r$correlation


