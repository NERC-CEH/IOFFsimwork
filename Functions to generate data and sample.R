# Code that calls other functions to generate observed data

## 'truth' data generation 

source("genData.R")
#source("genData_stochastic.R") # stochastic version for multiple runs

dat1 <- genData(dim = dim, lambda = lambda, env.beta = env.beta, seed = seed, kappa = kappa, sigma2x = sigma2x)

######## PREPARATION #########

##sampling bias
source('Generate strata levels Lam.R')

#generate a stratification to use for sampling bias
strata1 <- genStrataLam(dat1$Lam, strata = strata, rows = rows, cols = cols)


source("addSpatialBias.R")
#no spatial bias
#biasfield <- addSpatialBias(strata1, probs = c(0.5, 0.5, 0.5))
#spatial bias not correlated with environmental covariate
biasfield <- addSpatialBias(strata1, probs = probs)
#spatial bias correlated with environmental covariate
#biasfield <- addSpatialBias(strata1, probs = c(0.8, 0.5, 0.2))


####### UNSTRUCTURED DATA ##########

##thinning

source("thinData.R")

#thin data according to biasfield
thin1 <- thinData(dat1, biasfield)

#add env covariate
thin1$env <- dat1$gridcov[as.matrix(thin1[,2:1])]

#use thinned data as the unstructured data sample
unstructured_data <- thin1


####### STRUCTURED DATA ##########

# Set up structured sample, depends on number of samples and quadrat size

source("sampleStructured.R")

structured_data <- sampleStructured(dat1, biasfield, nsamp = nsamp, plotdat = plotdat, qsize = qsize)
  
  



