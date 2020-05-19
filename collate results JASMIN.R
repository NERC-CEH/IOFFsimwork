##collate results - large sample

file.list <- list.files(path = "Outputs", pattern = "ResultList_largesample")
AllRes <- list()
for(i in 1:length(file.list)){
  load(paste0("Outputs/ResultList_largesample_repeat_", i,".Rdata"))
  AllRes[[i]] <- ResultList
}

ResultList <- AllRes

#results
sim_results <- data.frame()
for(i in 1:500){     # for each simulation
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

write.csv(sim_results, file = "sim_results_large.csv")


# summary.fixed
sim_fixed <- data.frame()
for(i in 1:500){
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
write.csv(sim_fixed, file = "sim_fixed_large.csv")

# summary.hyperpar
sim_hyperpar <- data.frame()
for(i in 1:500){
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
write.csv(sim_hyperpar, file = "sim_hyperpar_large.csv")


##collate results - default

file.list <- list.files(path = "Outputs", pattern = "ResultList_default")
AllRes <- list()
for(i in 1:length(file.list)){
  load(paste0("Outputs/ResultList_default_repeat_", i,".Rdata"))
  AllRes[[i]] <- ResultList
}

ResultList <- AllRes

#results
sim_results <- data.frame()
for(i in 1:500){     # for each simulation
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

write.csv(sim_results, file = "sim_results_default.csv")


# summary.fixed
sim_fixed <- data.frame()
for(i in 1:500){
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
write.csv(sim_fixed, file = "sim_fixed_default.csv")

# summary.hyperpar
sim_hyperpar <- data.frame()
for(i in 1:500){
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
write.csv(sim_hyperpar, file = "sim_hyperpar_default.csv")



##collate results - unbiased

file.list <- list.files(path = "Outputs", pattern = "ResultList_unbiased")
AllRes <- list()
for(i in 1:length(file.list)){
  load(paste0("Outputs/ResultList_unbiased_repeat_", i,".Rdata"))
  AllRes[[i]] <- ResultList
}

ResultList <- AllRes

#results
sim_results <- data.frame()
for(i in 1:500){     # for each simulation
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

write.csv(sim_results, file = "sim_results_unbiased.csv")


# summary.fixed
sim_fixed <- data.frame()
for(i in 1:500){
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
write.csv(sim_fixed, file = "sim_fixed_unbiased.csv")

# summary.hyperpar
sim_hyperpar <- data.frame()
for(i in 1:500){
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
write.csv(sim_hyperpar, file = "sim_hyperpar_unbiased.csv")


##collate results - large sample

file.list <- list.files(path = "Outputs", pattern = "ResultList_biased")
AllRes <- list()
for(i in 1:500){
  ResultList <- NA
  try(load(paste0("Outputs/ResultList_biased_repeat_", i,".Rdata")))
  if(length(ResultList) > 1){AllRes[[i]] <- ResultList} else {AllRes[[i]] <- NA}
}

ResultList <- AllRes

#results
sim_results <- data.frame()
for(i in 1:500){     # for each simulation
  if(is.na(ResultList[[i]])){next} else{
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
  
}

write.csv(sim_results, file = "sim_results_biased.csv")


# summary.fixed
sim_fixed <- data.frame()
for(i in 1:500){
  if(!is.na(ResultList[[i]])){
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
}
write.csv(sim_fixed, file = "sim_fixed_biased.csv")

# summary.hyperpar
sim_hyperpar <- data.frame()
for(i in 1:500){
  if(!is.na(ResultList[[i]])){
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
}
write.csv(sim_hyperpar, file = "sim_hyperpar_biased.csv")

