
##data generation 

source("genData.R")

dat1 <- genData(lambda = 5, env.beta = 0.2, seed = 1)

######## PREPARATION #########


##sampling bias
source('Generate strata levels Lam.R')

#generate a stratification to use for sampling bias
strata1 <- genStrataLam(dat1$Lam, strata = 3, rows = 3, cols = 1)


source("addSpatialBias.R")
#no spatial bias
#biasfield <- addSpatialBias(strata1, probs = c(0.5, 0.5, 0.5))
#spatial bias not correlated with environmental covariate
biasfield <- addSpatialBias(strata1, probs = c(0.2, 0.8, 0.5))
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


source("sampleStructured.R")

structured_data <- sampleStructured(dat1, biasfield, nsamp = 50, plotdat = TRUE)
  
  



