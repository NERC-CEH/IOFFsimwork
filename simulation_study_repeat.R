# simulation - zero bias (env covariate)

# simulation study

# load libraries ####

library(INLA)
INLA:::inla.dynload.workaround() 
library(reshape2)
library(deldir)
library(rgeos)
library(fields)
library(RColorBrewer)
#display.brewer.all(colorblindFriendly = TRUE)

### Set parameters ####

#genData
dim = c(100,100)   
lambda = -3    
env.beta = 1.2 
plotdat = FALSE
sigma2x = 1     # var in rLGCP
kappa = 0.05    # =1/scale in rLGCP  [0.02, 0.1] good range

#genStrataLam
strata = 25
rows = 5
cols = 5
plot = TRUE

#addSpatialBias - detection
# this is where we can control bias
probs = rep(c(0.5, 0.3, 0.1, 0.05, 0.01),5) #default
#probs = rep(c(0.2, 0.2, 0.2, 0.2, 0.2),5) # unbiased
#probs = rep(c(0.5, 0.4, 0.1, 0.01, 0.001),5) # very biased

#sampleStructured
#nsamp = 100  
#nsamp = 50   #small
nsamp = 200  #large
plotdat = TRUE
qsize = 1          #~~~ neighborhood; buffer <- (qsize-1)/2

#~~~ mesh
mesh.edge = c(7, 14)   
mesh.offset = c(2, 7)   

#~~~ dimension
resolution = c(5,5)


# Generate data #####

# change the seed for each run
ResultList <- list()
seed_all <- sample(round(1:1000000),50, replace = F)


start_time_ori <- Sys.time()
for(i in 1:50){
  start_time <- Sys.time()
  #seed = seed_all[i]
  
  source("Functions to generate data and sample.R")
  g1 <- genDataFunctions(dim = dim, 
                         lambda = lambda, 
                         env.beta = env.beta, 
                         seed = seed_all[i], 
                         kappa = kappa, 
                         sigma2x = sigma2x, 
                         strata = strata, 
                         rows = rows, 
                         cols = cols, 
                         probs = probs, 
                         nsamp = nsamp,
                         plot = F,
                         plotdat = F,
                         qsize = 1)
  
  structured_data <- g1$structured_data
  unstructured_data <- g1$unstructured_data
  biasfield <- g1$biasfield
  dat1 <- g1$dat1
  biascov <- g1$biascov
  strata1 <- g1$strata1
  
  
  #' Visualise thinned unstructured data
  #+ echo = FALSE 
  #par(mfrow=c(1,1))
  #  image.plot(list(x=dat1$Lam$xcol, y=dat1$Lam$yrow, z=t(dat1$rf.s)), 
  #             main='Thinned unstructured data', asp=1,
  #             col = hcl.colors(12, "RdBu", rev = TRUE)) 
  #  points(unstructured_data$x, unstructured_data$y, pch = 20)#note rescale again - plotting back on original
  
  #' Visualise structured data
  #+ echo = FALSE  
  #par(mfrow=c(1,1))
   # image.plot(list(x=dat1$Lam$xcol, y=dat1$Lam$yrow, z=t(dat1$rf.s)), 
   #            main='Structured data', asp=1,
   #            col = hcl.colors(12, "RdBu", rev = TRUE)) 
   # points(structured_data$x,structured_data$y, pch = 21, bg = structured_data$presence, col = "black")
    #par(xpd = TRUE)
    #legend(0,115,c("Absence", "Presence"), pch = 21, col = "black", pt.bg = c(0,1))
  
  ##########################################################
  #structured model
  source("Run models structured.R")
  mod_1 <- structured_model(structured_data, 
                            dat1, 
                            biasfield, 
                            plotting = F,
                            mesh.edge = mesh.edge,
                            mesh.offset = mesh.offset,
                            resolution = resolution
  )
  
  source("validation_function.R")
  validation_1 <- validation_function(result=mod_1[[2]], 
                                      resolution=resolution, 
                                      join.stack=mod_1[[1]], 
                                      model_type="structured", 
                                      structured_data = structured_data, 
                                      dat1 = dat1, 
                                      summary_results=T, 
                                      qsize = 1, 
                                      absolute=TRUE, 
                                      dim = dim, 
                                      plotting = F)

  
  #' ### Unstructured model 
  #' 
  #+ warning = FALSE, message = FALSE, error = FALSE
  source("Run models.R")
  mod_2 <- unstructured_model(unstructured_data, 
                              dat1, 
                              biasfield, 
                              dim = dim, 
                              plotting = F,
                              mesh.edge = mesh.edge,
                              mesh.offset = mesh.offset,
                              resolution = resolution)
  
  source("validation_function.R")
  validation_2 <- validation_function(result=mod_2[[2]], 
                                      resolution=resolution, 
                                      join.stack=mod_2[[1]], 
                                      model_type="unstructured", 
                                      unstructured_data = unstructured_data, 
                                      dat1 = dat1, 
                                      summary_results=T, 
                                      absolute=TRUE, 
                                      dim = dim, 
                                      plotting = F)
  

  #' ### Joint model 
  #' 
  #+ warning = FALSE, message = FALSE, error = FALSE
  #joint model (no covariate on bias)
  source("Run models joint.R")
  mod_3 <- joint_model(structured_data, 
                       unstructured_data, 
                       dat1, 
                       biasfield,
                       plotting = F,
                       mesh.edge = mesh.edge,
                       mesh.offset = mesh.offset,
                       resolution = resolution)
  
  source("validation_function.R")
  validation_3 <- validation_function(result=mod_3[[2]], 
                                      resolution=resolution, 
                                      join.stack=mod_3[[1]], 
                                      model_type="joint", 
                                      unstructured_data = unstructured_data,
                                      structured_data = structured_data,
                                      dat1 = dat1, 
                                      summary_results=T, 
                                      absolute=TRUE, 
                                      dim = dim, 
                                      plotting = F)
  
  #' ### Unstructured model with covariate for bias
  #' 
  #+ warning = FALSE, message = FALSE, error = FALSE
  source("Run models unstructured bias covariate.R")
  mod_4 <- unstructured_model_cov(unstructured_data, 
                                  dat1, 
                                  biasfield, 
                                  dim = dim, 
                                  plotting = F,
                                  mesh.edge = mesh.edge,
                                  mesh.offset = mesh.offset,
                                  resolution = resolution)
  
  source("validation_function.R")
  validation_4 <- validation_function(result=mod_4[[2]], 
                                      resolution=resolution, 
                                      join.stack=mod_4[[1]], 
                                      model_type="unstructuredcov", 
                                      unstructured_data = unstructured_data, 
                                      dat1 = dat1, 
                                      summary_results=T, 
                                      absolute=TRUE, 
                                      dim = dim, 
                                      plotting = F)
  

    #joint model (covariate on bias)
  source("Run models joint covariate for bias.R")
  mod_5 <- joint_model_cov(structured_data, 
                           unstructured_data, 
                           dat1, 
                           biasfield,
                           resolution = resolution, 
                           biascov,
                           plotting = F,
                           mesh.edge = mesh.edge,
                           mesh.offset = mesh.offset)
  
  source("validation_function.R")
  validation_5 <- validation_function(result=mod_5[[2]], 
                                      resolution=resolution, 
                                      join.stack=mod_5[[1]], 
                                      model_type="jointcov", 
                                      unstructured_data = unstructured_data,
                                      structured_data = structured_data,
                                      dat1 = dat1, 
                                      summary_results=T, 
                                      absolute = TRUE, 
                                      dim = dim, 
                                      plotting = F)
  
 
  #' ### Covariate model 
  #' set prior for fixed effect 
  #' 
  source("Run cov model all eff prior.R")
  mod_8 <- covariate_model(unstructured_data = unstructured_data, 
                           structured_data = structured_data,
                           unstr_mod_type = "unstructured only",
                           dat1, 
                           biasfield,
                           dim,
                           plotting=F,
                           mesh.edge = mesh.edge,
                           mesh.offset = mesh.offset,
                           resolution = resolution)
  
  source("validation_function.R")
  validation_8 <- validation_function(result=mod_8[[2]], 
                                      resolution=resolution, 
                                      join.stack=mod_8[[1]], 
                                      model_type="covariate", 
                                      unstructured_data = unstructured_data,
                                      structured_data = structured_data,
                                      dat1 = dat1, 
                                      summary_results=T, 
                                      absolute=TRUE, 
                                      dim = dim, 
                                      plotting = F)
  
  
  #' ### Covariate model 
  #' set prior for fixed effect 
  #' 
  source("Run cov model all eff prior.R")
  mod_9 <- covariate_model(unstructured_data = unstructured_data, 
                           structured_data = structured_data, 
                           unstr_mod_type = "unstructured with bias",
                           dat1, 
                           biasfield,
                           dim,
                           plotting=F,
                           mesh.edge = mesh.edge,
                           mesh.offset = mesh.offset,
                           resolution = resolution)
  
  source("validation_function.R")
  validation_9 <- validation_function(result=mod_9[[2]], 
                                      resolution=resolution, 
                                      join.stack=mod_9[[1]], 
                                      model_type="covariatebias", 
                                      unstructured_data = unstructured_data,
                                      structured_data = structured_data,
                                      dat1 = dat1, 
                                      summary_results=T, 
                                      absolute=TRUE, 
                                      dim = dim, 
                                      plotting = F)

  
  #' ### Correlation model 
  #' set prior for fixed effect 
  #' 
  source("Run correlation model - corrected.R")
  mod_10 <- correlation_model(unstructured_data = unstructured_data, 
                              structured_data = structured_data,
                              dat1, 
                              biasfield,
                              dim,
                              plotting=F,
                              mesh.edge = mesh.edge,
                              mesh.offset = mesh.offset,
                              resolution = resolution)
  
  source("validation_function for correlation.R")
  validation_10 <- validation_function_str(result=mod_10[[2]], 
                                       resolution=resolution, 
                                       join.stack=mod_10[[1]], 
                                       model_type="correlation_str", 
                                       unstructured_data = unstructured_data,
                                       structured_data = structured_data,
                                       dat1 = dat1, 
                                       summary_results=T, 
                                       absolute=TRUE, 
                                       dim = dim, 
                                       plotting = F)
  
  validation_11 <- validation_function_uns(result=mod_10[[2]], 
                                       resolution=resolution, 
                                       join.stack=mod_10[[1]], 
                                       model_type="correlation_uns", 
                                       unstructured_data = unstructured_data,
                                       structured_data = structured_data,
                                       dat1 = dat1, 
                                       summary_results=T, 
                                       absolute=TRUE, 
                                       dim = dim, 
                                       plotting = F)
  
  #' ### Correlation model 
  #' set prior for fixed effect 
  #' 
  source("Run correlation-bias model - corrected.R")
  mod_12 <- correlationbias_model(unstructured_data = unstructured_data, 
                                  structured_data = structured_data,
                                  dat1, 
                                  biasfield,
                                  biascov,
                                  dim,
                                  plotting=F,
                                  mesh.edge = mesh.edge,
                                  mesh.offset = mesh.offset,
                                  resolution = resolution)
  
  source("validation_function for correlation.R")
  validation_12 <- validation_function_str(result=mod_12[[2]], 
                                       resolution=resolution, 
                                       join.stack=mod_12[[1]], 
                                       model_type="correlationbias_str", 
                                       unstructured_data = unstructured_data,
                                       structured_data = structured_data,
                                       dat1 = dat1, 
                                       summary_results=T, 
                                       absolute=TRUE, 
                                       dim = dim, 
                                       plotting = F)

  validation_13 <- validation_function_uns(result=mod_12[[2]], 
                                       resolution=resolution, 
                                       join.stack=mod_12[[1]], 
                                       model_type="correlationbias_uns", 
                                       unstructured_data = unstructured_data,
                                       structured_data = structured_data,
                                       dat1 = dat1, 
                                       summary_results=T, 
                                       absolute=TRUE, 
                                       dim = dim, 
                                       plotting = F)
  
  
  ResultList[[i]] <- list(param = list(lambda=lambda, 
                                       env.beta = env.beta,
                                       seed = seed_all[i], 
                                       sigma2x = sigma2x, 
                                       kappa = kappa, 
                                       nsamp = nsamp,
                                       npres = length(which(structured_data$presence==1)),
                                       uns_nsamp = nrow(unstructured_data)),
                          str_v = validation_1,
                          uns_v = validation_2, 
                          joi_v = validation_3, 
                          unsbias_v = validation_4, 
                          joibias_v = validation_5, 
                          cov_v = validation_8, 
                          covbias_v = validation_9, 
                          cor_v_str = validation_10, 
                          cor_v_uns = validation_11, 
                          corbias_v_str = validation_12,
                          corbias_v_uns = validation_13)
  
  print(i) 
  end_time <- Sys.time()
  
  print(end_time - start_time)
}

end_time - start_time_ori

#save the simulation result
save(ResultList, file = "ResultList_largesample_repeat50.Rdata")

#results
sim_results <- data.frame()
for(i in 1:50){     # for each simulation
  for(j in 2:12){   # for each model absolute difference
    
    sim_results <- rbind(sim_results,
                         rbind(cbind(ResultList[[i]]$param,
                                     ResultList[[i]][[j]]$result$`Proto-table`,
                                     correlation = ResultList[[i]][[j]]$result$correlation,
                                     worst = paste(x=(ResultList[[i]][[j]]$result$Worst_grid_cells), collapse = ', '),
                                     best = paste(x=(ResultList[[i]][[j]]$result$Best_grid_cells), collapse = ', '),
                                     #waic = ResultList[[i]][[j]]$result$WAIC,
                                     tot_cpu = ResultList[[i]][[j]]$result$CPU[4],
                                     sim = i)))
    
  }
  
}

write.csv(sim_results, file = "sim_results.csv")


# summary.fixed
sim_fixed <- data.frame()
for(i in 1:50){
  sim_fixed <- rbind(sim_fixed,
                     rbind(cbind(ResultList[[i]]$str_v$result$coefficients,
                                 model = "structured",
                                 sim = i),
                           cbind(ResultList[[i]]$uns_v$result$coefficients,
                                 model = "unstructured",
                                 sim = i),
                           cbind(ResultList[[i]]$joi_v$result$coefficients,
                                 model = "joint",
                                 sim = i),
                           cbind(ResultList[[i]]$unsbias_v$result$coefficients,
                                 model = "unstructuredbias",
                                 sim = i),
                           cbind(ResultList[[i]]$joibias_v$result$coefficients,
                                 model = "jointbias",
                                 sim = i),
                           cbind(ResultList[[i]]$cov_v$result$coefficients,
                                 model = "covariate",
                                 sim = i),
                           cbind(ResultList[[i]]$covbias_v$result$coefficients,
                                 model = "covariatebias",
                                 sim = i),
                           cbind(ResultList[[i]]$cor_v_str$result$coefficients,
                                 model = "correlation",
                                 sim = i),
                           cbind(ResultList[[i]]$corbias_v_str$result$coefficients,
                                 model = "correlationbias",
                                 sim = i))
  )
}
write.csv(sim_fixed, file = "sim_fixed.csv")

# summary.hyperpar
sim_hyperpar <- data.frame()
for(i in 1:50){
  sim_hyperpar <- rbind(sim_hyperpar,
                        rbind(cbind(ResultList[[i]]$str_v$result$hyperparameters,
                                    model = "structured",
                                    sim = i),
                              cbind(ResultList[[i]]$uns_v$result$hyperparameters,
                                    model = "unstructured",
                                    sim = i),
                              cbind(ResultList[[i]]$joi_v$result$hyperparameters,
                                    model = "joint",
                                    sim = i),
                              cbind(ResultList[[i]]$unsbias_v$result$hyperparameters,
                                    model = "unstructuredbias",
                                    sim = i),
                              cbind(ResultList[[i]]$joibias_v$result$hyperparameters,
                                    model = "jointbias",
                                    sim = i),
                              cbind(ResultList[[i]]$cov_v$result$hyperparameters,
                                    model = "covariate",
                                    sim = i),
                              cbind(ResultList[[i]]$covbias_v$result$hyperparameters,
                                    model = "covariatebias",
                                    sim = i),
                              cbind(ResultList[[i]]$cor_v_str$result$hyperparameters,
                                    model = "correlation",
                                    sim = i),
                              cbind(ResultList[[i]]$corbias_v_str$result$hyperparameters,
                                    model = "correlationbias",
                                    sim = i))
  )
}
write.csv(sim_hyperpar, file = "sim_hyperpar.csv")

