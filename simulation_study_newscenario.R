# new scenarios - simulation study

# load libraries ####

library(INLA)
#INLA:::inla.dynload.workaround() 
library(reshape2)
library(deldir)
library(rgeos)
library(fields)
library(RColorBrewer)

### Set parameters ####

#genData
dim = c(100,100)   
lambda = -3        
env.beta = 1.2       
plotdat = FALSE
sigma2x = 1 
kappa = 0.05      # =1/scale in rLGCP  [0.02, 0.1] good range

#genStrataLam
strata = 25
rows = 5
cols = 5
plot = TRUE

#addSpatialBias - detection
# this is where we can control bias
probs = rep(c(0.5, 0.3, 0.1, 0.05, 0.01),5)   #default

#sampleStructured
nsamp = 250  
plotdat = TRUE
qsize = 1          # neighborhood; buffer <- (qsize-1)/2

#~~~ mesh
mesh.edge = c(7, 14)   
mesh.offset = c(2, 7)  

#~~~ dimension
resolution = c(5,5)


# Generate data #####

# change the seed for each run
seed_all = sample(round(1:1000000), 50, replace = F)


ResultList <- list()
counter = 1
start_time_ori <- Sys.time()
for(i in 1:50){

  source("Functions to generate data and sample - new scenario.R")
  g1 <- genDataFunctions_newscenario(dim = dim, 
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
  
  #check for 0 presence in sections of structured data
  numpres = c()
  for(k in 1:length(structured_data)){
    numpres = rbind(numpres,
                    length(which(structured_data[[k]]$presence==1)))
  }
  
  if(0 %in% numpres){
    print(c("presence=0 in structured data", i))
  } else{
 
 
  ##########################################################
  
  default_structured = length(structured_data)  # length of list
  
  for(k in 1:length(structured_data)){  # for each scenarios (9)
    start_time <- Sys.time()
    
    #structured model
    source("Run models structured.R")
    mod_1 <- structured_model(structured_data[[k]], 
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
                                        structured_data = structured_data[[k]], 
                                        dat1 = dat1, 
                                        summary_results=T, 
                                        qsize = 1, 
                                        absolute=TRUE, 
                                        dim = dim, 
                                        plotting = F)
    
   
    #' ### Joint model 
    #' 
    #+ warning = FALSE, message = FALSE, error = FALSE
    #joint model (no covariate on bias)
    source("Run models joint.R")
    mod_3 <- joint_model(structured_data[[k]], 
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
                                        structured_data = structured_data[[k]],
                                        dat1 = dat1, 
                                        summary_results=T, 
                                        absolute=TRUE, 
                                        dim = dim, 
                                        plotting = F)
    
    #joint model (covariate on bias)
    source("Run models joint covariate for bias.R")
    mod_5 <- joint_model_cov(structured_data[[k]], 
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
                                        structured_data = structured_data[[k]],
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
                             structured_data = structured_data[[k]],
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
                                        structured_data = structured_data[[k]],
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
                             structured_data = structured_data[[k]], 
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
                                        structured_data = structured_data[[k]],
                                        dat1 = dat1, 
                                        summary_results=T, 
                                        absolute=TRUE, 
                                        dim = dim, 
                                        plotting = F)
    
    #' ### Correlation model 
    #'  
    #' 
    source("Run correlation model - corrected.R")
    mod_10 <- correlation_model(unstructured_data = unstructured_data, 
                                structured_data = structured_data[[k]],
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
                                         structured_data = structured_data[[k]],
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
                                           structured_data = structured_data[[k]],
                                           dat1 = dat1, 
                                           summary_results=T, 
                                           absolute=FALSE, 
                                           dim = dim, 
                                           plotting = F)
    
    #' ### Correlation model with bias covariate
    #' 
    #' 
    source("Run correlation-bias model - corrected.R")
    mod_12 <- correlationbias_model(unstructured_data = unstructured_data, 
                                    structured_data = structured_data[[k]],
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
                                         structured_data = structured_data[[k]],
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
                                           structured_data = structured_data[[k]],
                                           dat1 = dat1, 
                                           summary_results=T, 
                                           absolute=FALSE, 
                                           dim = dim, 
                                           plotting = F)
    
    # only run unstructured model when structured data is whole
    if(k == default_structured){    
      
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
      
    }else{
      validation_2 = NA
      validation_4 = NA
    }
    
    # compile results
    ResultList[[counter]] <- list(param = list(lambda=lambda, 
                                         env.beta = env.beta, 
                                         seed = seed_all[i], 
                                         sigma2x = sigma2x, 
                                         kappa = kappa, 
                                         nsamp = nsamp,
                                         section = names(structured_data[k]),
                                         npres = length(which(structured_data[[k]]$presence==1)),
                                         uns_nsamp = nrow(unstructured_data)),                                                    #1
                            str_v = validation_1,          #2
                            uns_v = validation_2,             #3
                            joi_v = validation_3,          #4
                            unsbias_v = validation_4,         #5
                            joibias_v = validation_5,      #6
                            cov_v = validation_8,          #7
                            covbias_v = validation_9,      #8
                            cor_v_str = validation_10,     #9
                            cor_v_uns = validation_11,     #10
                            corbias_v_str = validation_12, #11
                            corbias_v_uns = validation_13) #12
    
    counter = counter + 1
    
    print(i) 
    end_time <- Sys.time()
    
    print(end_time - start_time)
  }
  }
}

end_time - start_time_ori

#save global environment of simulation result
save(ResultList, file = "ResultList_newscenario.Rdata")

#results sans unstructured and unstructuredbias
sim_results <- data.frame()
for(i in 1:450){      # for each simulation
  for(j in c(2,4,6,7,8,9,10,11,12)){
    
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

# unstructured data results
sim_results_unst <- data.frame()
for(i in seq(9, 450, 9)){      # for each 9th simulation (whole)
  for(j in c(3,5)){
    
    sim_results_unst <- rbind(sim_results_unst,
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

sim_results_full <- rbind(sim_results, sim_results_unst)

write.csv(sim_results_full, file = "newscenario_results.csv")


# summary.fixed
sim_fixed <- data.frame()
for(i in 1:450){
  sim_fixed <- rbind(sim_fixed,
                     rbind(cbind(ResultList[[i]]$str_v$result$coefficients,
                                 model = "structured",
                                 sim = i,
                                 section = ResultList[[i]]$param$section),
                           #cbind(ResultList[[i]]$uns_v$result$coefficients,
                            #     model = "unstructured",
                             #    sim = i,
                           #section = ResultList[[i]]$param$section),
                           cbind(ResultList[[i]]$joi_v$result$coefficients,
                                 model = "joint",
                                 sim = i,
                                 section = ResultList[[i]]$param$section),
                           #cbind(ResultList[[i]]$unsbias_v$result$coefficients,
                            #     model = "unstr_bias",
                             #    sim = i,
                           #section = ResultList[[i]]$param$section),
                           cbind(ResultList[[i]]$joibias_v$result$coefficients,
                                 model = "joint_bias",
                                 sim = i,
                                 section = ResultList[[i]]$param$section),
                           cbind(ResultList[[i]]$cov_v$result$coefficients,
                                 model = "covariate",
                                 sim = i,
                                 section = ResultList[[i]]$param$section),
                           cbind(ResultList[[i]]$covbias_v$result$coefficients,
                                 model = "covariate_bias",
                                 sim = i,
                                 section = ResultList[[i]]$param$section),
                           cbind(ResultList[[i]]$cor_v_str$result$coefficients,
                                 model = "correlation",
                                 sim = i,
                                 section = ResultList[[i]]$param$section),
                           cbind(ResultList[[i]]$corbias_v_str$result$coefficients,
                                 model = "correlation_bias",
                                 sim = i,
                                 section = ResultList[[i]]$param$section))
  )
}

sim_fixed_unst <- data.frame()
for(i in seq(9, 450, 9)){
  sim_fixed_unst <- rbind(sim_fixed_unst,
                     rbind(
                       cbind(ResultList[[i]]$uns_v$result$coefficients,
                             model = "unstructured",
                             sim = i,
                             section = ResultList[[i]]$param$section),
                       cbind(ResultList[[i]]$unsbias_v$result$coefficients,
                             model = "unstr_bias",
                             sim = i,
                             section = ResultList[[i]]$param$section)
                       )
                     )
  }

sim_fixed_full <- rbind(sim_fixed, sim_fixed_unst)

write.csv(sim_fixed_full, file = "newscenario_fixedeff.csv")


# summary.hyperpar
sim_hyperpar <- data.frame()
for(i in 1:450){
  sim_hyperpar <- rbind(sim_hyperpar,
                        rbind(cbind(ResultList[[i]]$str_v$result$hyperparameters,
                                    model = "structured",
                                    sim = i,
                                    section = ResultList[[i]]$param$section),
                              #cbind(ResultList[[i]]$uns_v$result$hyperparameters,
                              #      model = "unstructured",
                              #      sim = i,
                              #      section = ResultList[[i]]$param$section),
                              cbind(ResultList[[i]]$joi_v$result$hyperparameters,
                                    model = "joint",
                                    sim = i,
                                    section = ResultList[[i]]$param$section),
                              #cbind(ResultList[[i]]$unsbias_v$result$hyperparameters,
                              #      model = "unstr_bias",
                              #      sim = i,
                              #      section = ResultList[[i]]$param$section),
                              cbind(ResultList[[i]]$joibias_v$result$hyperparameters,
                                    model = "joint_bias",
                                    sim = i,
                                    section = ResultList[[i]]$param$section),
                              cbind(ResultList[[i]]$cov_v$result$hyperparameters,
                                    model = "covariate",
                                    sim = i,
                                    section = ResultList[[i]]$param$section),
                              cbind(ResultList[[i]]$covbias_v$result$hyperparameters,
                                    model = "covariate_bias",
                                    sim = i,
                                    section = ResultList[[i]]$param$section),
                              cbind(ResultList[[i]]$cor_v_str$result$hyperparameters,
                                    model = "correlation",
                                    sim = i,
                                    section = ResultList[[i]]$param$section),
                              cbind(ResultList[[i]]$corbias_v_str$result$hyperparameters,
                                    model = "correlation_bias",
                                    sim = i,
                                    section = ResultList[[i]]$param$section))
  )
}

sim_hyperpar_unst <- data.frame()
for(i in seq(9, 450, 9)){
  sim_hyperpar_unst <- rbind(sim_hyperpar_unst,
                          rbind(
                            cbind(ResultList[[i]]$uns_v$result$hyperparameters,
                                  model = "unstructured",
                                  sim = i,
                                  section = ResultList[[i]]$param$section),
                            cbind(ResultList[[i]]$unsbias_v$result$hyperparameters,
                                  model = "unstr_bias",
                                  sim = i,
                                  section = ResultList[[i]]$param$section)
                          )
  )
}

sim_hyperpar_full <- rbind(sim_hyperpar, sim_hyperpar_unst)

write.csv(sim_hyperpar_full, file = "newscenario_hyperpar.csv")

##########################

# plot parameter estimates
library(data.table)
env_coef <- setDT(sim_fixed_full, keep.rownames = TRUE)[]
env_coef2 <- env_coef[env_coef$rn %like% "env", ]

#boxplot
ggplot(env_coef2, aes(y = mean, x = section, group = model, fill = model)) + 
  geom_boxplot() + 
  facet_wrap(. ~ section, scales = "free") +
  ggtitle("fitted Env coefficient ") +
  theme(axis.text.x = element_blank()) + 
  xlab("") + geom_hline(yintercept=1.2, linetype="dashed", color = "red") +
  scale_y_continuous(breaks = c(pretty(env_coef$mean), 1.2))
