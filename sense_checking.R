#' #Try out altered scenarios to test models perform correctly
#'
#' The dimensions remain the same for all scenarios but other parameters change.
#' 
#+ echo = FALSE 

source("setParams.R")

#'
#' # Scenario one - CHANGE ENVIRONMENT RELATIONSHIP
#'
#' ## Reduce relationship between environment and abundance to 0. 
#'
#+ warning = FALSE, message = FALSE, error = FALSE, include = FALSE

lambda = 5
env.beta = NULL
source("Functions to generate data and sample.R")

#' ## Visualise the random field and covariate pattern
#' Setting the seed means that even without a relationship to the environment abundance is still higher in bottom segment.
#+ echo = FALSE 

par(mfrow=c(1,2)) 
image.plot(list(x=dat1$Lam$xcol, y=dat1$Lam$yrow, z=t(dat1$rf.s)), main='log-Lambda', asp=1) 
#points(dat1$xy, pch=19)
image.plot(list(x=dat1$Lam$xcol, y=dat1$Lam$yrow, z=t(dat1$gridcov)), main='Covariate', asp=1)

#+ warning = FALSE, message = FALSE, error = FALSE, figure.align = "center"
source("run_all_sense_check.R")

#' ## Reduce relationship between environment and abundance to -0.5. 
#'
#+ warning = FALSE, message = FALSE, error = FALSE, include = FALSE

env.beta = -0.5
source("Functions to generate data and sample.R")

#+ warning = FALSE, message = FALSE, error = FALSE, figure.align = "center"
source("run_all_sense_check.R")

#' ## Increase relationship between environment and abundance to 0.9. 
#'
#+ warning = FALSE, message = FALSE, error = FALSE, include = FALSE

env.beta = 0.9
source("Functions to generate data and sample.R")

#+ warning = FALSE, message = FALSE, error = FALSE, figure.align = "center"

source("run_all_sense_check.R")

#'
#' # Scenario two - CHANGE NUMBER OF STRUCTURED SAMPLES
#'
#+ warning = FALSE, message = FALSE, error = FALSE, include = FALSE 

source("setParams.R")
nsamp = 1000

#' ## Increase structured sample size to 1000 
#'
#+ warning = FALSE, message = FALSE, error = FALSE, include = FALSE

source("Functions to generate data and sample.R")

#+ warning = FALSE, message = FALSE, error = FALSE, figure.align = "center"
source("run_all_sense_check.R")

#'
#' # Scenario three - CHANGE BIAS
#'
#+ echo = FALSE 

source("setParams.R")
probs = c(0.5, 0.5, 0.5)

#' ## Same probability everywhere
#'
#+ warning = FALSE, message = FALSE, error = FALSE, include = FALSE

source("Functions to generate data and sample.R")

#+ warning = FALSE, message = FALSE, error = FALSE, figure.align = "center"

source("run_all_sense_check.R")

#+ echo = FALSE 

source("setParams.R")
probs = c(0.8, 0.2, 0.5)

#' ## Top is most probable
#'
#+ warning = FALSE, message = FALSE, error = FALSE, include = FALSE

source("Functions to generate data and sample.R")

#+ warning = FALSE, message = FALSE, error = FALSE, figure.align = "center"
source("run_all_sense_check.R")

#'
#' # Scenario four - CHANGE NUMBER OF STRATA
#'
#+ echo = FALSE 

source("setParams.R")
strata = 6
rows = 3
cols = 2

#' ## Strata in a 3x2 so 6 in total
#'
#+ warning = FALSE, message = FALSE, error = FALSE, include = FALSE

source("Functions to generate data and sample.R")

#+ warning = FALSE, message = FALSE, error = FALSE, figure.align = "center"
source("run_all_sense_check.R")
