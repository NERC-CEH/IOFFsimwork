
# Function to run structured only models using informative priors obtained from fitted unstructured only model.
#
# Priors include: 
# - Gaussian distribution parameters(mean, stdev) for intercept and coefficient for covariates
# - Hyperparameters (tau, kappa) for random spatial effect (SPDE)
#
# Function will return a list of data stack and the fitted model.

covariate_model <- function(unstructured_data, 
                            structured_data, 
                            unstr_mod_type = c("unstructured only",
                                               "unstructured with bias"),
                            dat1, 
                            biasfield,
                            dim,
                            plotting = FALSE,
                            mesh.edge = c(20,40),
                            mesh.offset = c(5,20),
                            resolution = c(10,10)){
  
  # packages
  library(INLA)
  library(reshape2)
  library(fields)
  library(rgeos)
  
  ##############################
  
  #preparation - mesh construction - use the loc.domain argument
  
  mesh <- inla.mesh.2d(loc.domain = biasfield[,c(1,2)],
                       max.edge=mesh.edge,
                       cutoff=2, 
                       offset = mesh.offset)
  
  # make A matrix for structured data for projection
  structured_data_A <- inla.spde.make.A(mesh = mesh, 
                                        loc = as.matrix(structured_data[, 2:3]))
  
  if(unstr_mod_type == "unstructured only"){
    
    # unstructured model
    source("Run models.R")
    unstr_model <- unstructured_model(unstructured_data, 
                                      dat1, 
                                      biasfield, 
                                      dim = dim, 
                                      plotting = FALSE,
                                      mesh.edge = mesh.edge,
                                      mesh.offset = mesh.offset,
                                      resolution = resolution)
  }
  
  if(unstr_mod_type == "unstructured with bias"){
    
    #  Unstructured model with covariate for bias
    source("Run models unstructured bias covariate.R")
    unstr_model <- unstructured_model_cov(unstructured_data, 
                                    dat1, 
                                    biasfield, 
                                    dim = dim, 
                                    plotting = FALSE,
                                    mesh.edge = mesh.edge,
                                    mesh.offset = mesh.offset,
                                    resolution = resolution)
  }
  
  cpu.first <- unstr_model$result$cpu.used
  
  ##############################
  
  # unstructured model parameter distribution (posterior) 
  # to be used as prior distribution for structured data
  
  ## to be commented out if structured model has more covariate (other than env) 
  #   n.cov <- nrow(unstr_model$result$summary.fixed) - 1
  
  # fixed effect parameters  
  unstr_coef_mean <- unstr_model$result$summary.fixed["env", 1]   #[2:(n.cov+1), 1]
  unstr_coef_sd <- unstr_model$result$summary.fixed["env", 2]   #[2:(n.cov+1),2]    
  
  ## choice to use median instead of mean
  #   unstr_int_mean <- result_un$summary.fixed[1,4]
  #   unstr_coef_mean <- result_un$summary.fixed[2:(n.cov+1),4]
  
  
  ##############################
  
  # hyperprior precision matrix
  cov_matrix <- unstr_model$result$misc$cov.intern # internal parameterisation of the hyperpar
  prec_from_cov <- solve(cov_matrix)       # precision = cov^(-1)
  
  ##set the spde representation to be the mesh 
  # with prior settings
  spde <- inla.spde2.matern(mesh = mesh,
                            alpha = 2, # smoothing parameter. default is 2
                            theta.prior.mean = unstr_model$result$summary.hyperpar$mean,     # hyperprior mean
                            theta.prior.prec = prec_from_cov # hyperprior precision
  )
  
  # create stack with presence data and Ntrials
  stk_structured <- inla.stack(data = list(y = structured_data$presence,
                                           Ntrials = rep(1, 
                                                         nrow(structured_data))),
                               effects = list(data.frame(interceptA = rep(1, length(structured_data$x)),
                                                         env = structured_data$env),
                                              Bnodes = 1:spde$n.spde),
                               A = list(1, structured_data_A),
                               tag = "structured_data")
  
  
  
  source("Create prediction stack.R")
  
  join.stack <- create_prediction_stack(data_stack = stk_structured, 
                                        resolution = resolution, 
                                        biasfield = biasfield, 
                                        dat1 = dat1, 
                                        mesh = mesh, 
                                        spde = spde)
  
  ##############################
  
  # set prior on the fixed effects
  prior.fixed <- list(#mean.intercept = unstr_int_mean,
                      #prec.intercept = 1/unstr_int_sd^2, 
                      mean = unstr_coef_mean,   
                      prec = 1/unstr_coef_sd^2)  
  
  # model formula
  # if want to add more covariate (not just env), change this formula 
  # and stk_structured
  formulaN = y ~ -1 + interceptA + env + f(Bnodes, model = spde)
  
  # make INLA call
  result.struct.binom <- inla(formula = formulaN, 
                              family = "binomial",
                              data = inla.stack.data(join.stack),
                              control.predictor = list(A = inla.stack.A(join.stack
                              ),
                              compute = TRUE), 
                              control.family = list(link = "cloglog"),
                              control.fixed = prior.fixed,  # set prior
                              Ntrials = inla.stack.data(join.stack)$Ntrials,
                              control.compute = list(dic = FALSE, 
                                                     cpo = FALSE,   
                                                     waic = FALSE) 
                              )
  
  # project mesh
  max_x <- max(biasfield$x)
  max_y <- max(biasfield$y)
  
  proj1.struct.binom <- inla.mesh.projector(mesh,
                                            ylim = c(1, max_y),
                                            xlim = c(1, max_x),
                                            dims = c(max_x, max_y))
  
  # mean of fitted model intensity
  xmean1.struct.binom <- inla.mesh.project(proj1.struct.binom,
                                           result.struct.binom$summary.random$Bnodes$mean)
  
  # stdev of fitted model intensity
  xsd1.struct.binom <- inla.mesh.project(proj1.struct.binom,
                                         result.struct.binom$summary.random$Bnodes$sd)
  
  # plot the mean and stdev of posterior intensity
  if(plotting == TRUE){
    par(mfrow=c(1,1))
    image.plot(1:max_x, 1:max_y,
               xmean1.struct.binom,
               col = tim.colors(),
               xlab = '', ylab = '',
               main = "Covariate-mean of r.f",
               asp=1
               #, zlim=c(-4,1)
    )
    
    
    #plot truth
        image.plot(list(x=dat1$Lam$xcol,#*100, 
                        y=dat1$Lam$yrow,#*100, 
                        z=t(dat1$rf.s)), 
                   main='Truth', 
                   asp=1
                   #, zlim=c(-4,1) # make sure scale = same
        ) 
    # plot truth points
    #points(structured_data[structured_data[,4] %in% 0,2:3], pch=16, col='white')     # absences
    #points(structured_data[structured_data[,4] %in% 1,2:3], pch=16, col='black')     # presences
    
    image.plot(1:max_x, 1:max_y,
               xsd1.struct.binom,
               col = tim.colors(),
               xlab = '', ylab = '',
               main = "Covariate-sd of r.f",
               asp = 1)
    
  }
  
  result.struct.binom$cpu.used <- result.struct.binom$cpu.used + cpu.first
  
  return(list(join.stack = join.stack, result = result.struct.binom))
  
}
