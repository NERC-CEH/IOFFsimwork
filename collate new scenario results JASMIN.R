##collate results new scenario JASMIN

file.list <- list.files(path = "Outputs", pattern = "ResultList_newscenario_")
AllRes <- list()
for(i in 1:500){
  if(paste0("ResultList_newscenario_", i,".Rdata")%in% file.list){
  load(paste0("Outputs/ResultList_newscenario_", i,".Rdata"))} else {ResultList <- NA}
  AllRes[[i]] <- ResultList
}

#second run
file.list2 <- list.files(path = "Outputs", pattern = "ResultList_newscenario2")
AllRes2 <- list()
for(i in 1:100){
  if(paste0("ResultList_newscenario2_", i,".Rdata")%in% file.list2){
    load(paste0("Outputs/ResultList_newscenario2_", i,".Rdata"))} else {ResultList <- NA}
  AllRes2[[i]] <- ResultList
}


ResultList <- c(AllRes, AllRes2)

#results sans unstructured and unstructuredbias
sim_results <- data.frame()
for(i in 1:600){      # for each run
  if(length(ResultList[[i]]) == 1){next} else{
  
  for (k in 1:9) { #for each structured dataset
    for(j in c(2,4,6,7,8,9,10,11,12)){ #for each model
    
    sim_results <- rbind(sim_results,
                         rbind(cbind(ResultList[[i]][[k]]$param,
                                     ResultList[[i]][[k]][[j]]$result$`Proto-table`[1],
                                     MAE = mean(abs(ResultList[[i]][[k]][[j]]$values$mean_predicted/mean(ResultList[[i]][[k]][[j]]$values$mean_predicted)-ResultList[[i]][[k]][[j]]$values$truth/mean(ResultList[[i]][[k]][[j]]$values$truth))),
                                     correlation = ResultList[[i]][[k]][[j]]$result$correlation,
                                     worst = paste(x=(ResultList[[i]][[k]][[j]]$result$Worst_grid_cells), collapse = ', '),
                                     best = paste(x=(ResultList[[i]][[k]][[j]]$result$Best_grid_cells), collapse = ', '),
                                     #waic = ResultList[[i]][[j]]$result$WAIC,
                                     tot_cpu = ResultList[[i]][[k]][[j]]$result$CPU[4],
                                     sim = i)))
    
  }
  }

  }
}



#results for unstructured and unstructuredbias
sim_results_unst <- data.frame()
for(i in 1:600){      # for each run
  if(length(ResultList[[i]]) == 1){next} else{
    
    for (k in 9) { #only the whole dataset
      for(j in c(3,5)){ #for the two unstructured models
        
        sim_results_unst <- rbind(sim_results_unst,
                             rbind(cbind(ResultList[[i]][[k]]$param,
                                         ResultList[[i]][[k]][[j]]$result$`Proto-table`[1],
                                         MAE = mean(abs(ResultList[[i]][[k]][[j]]$values$mean_predicted/mean(ResultList[[i]][[k]][[j]]$values$mean_predicted)-ResultList[[i]][[k]][[j]]$values$truth/mean(ResultList[[i]][[k]][[j]]$values$truth))),
                                         correlation = ResultList[[i]][[k]][[j]]$result$correlation,
                                         worst = paste(x=(ResultList[[i]][[k]][[j]]$result$Worst_grid_cells), collapse = ', '),
                                         best = paste(x=(ResultList[[i]][[k]][[j]]$result$Best_grid_cells), collapse = ', '),
                                         #waic = ResultList[[i]][[j]]$result$WAIC,
                                         tot_cpu = ResultList[[i]][[k]][[j]]$result$CPU[4],
                                         sim = i)))
        
      }
    }
    
  }
}





sim_results_full <- rbind(sim_results, sim_results_unst)

write.csv(sim_results_full, file = "sim_results_newscenario_rel.csv")


# summary.fixed
sim_fixed <- data.frame()
for(i in 1:600){
  if(length(ResultList[[i]]) == 1){next} else{
    for(k in 1:9){ # for each section
  sim_fixed <- rbind(sim_fixed,
                     rbind(cbind(coefficient = row.names(ResultList[[i]][[k]]$str_v$result$coefficients),
                                ResultList[[i]][[k]]$str_v$result$coefficients,
                                 model = "structured",
                                 sim = i,
                                 section = ResultList[[i]][[k]]$param$section),
                           #cbind(ResultList[[i]]$uns_v$result$coefficients,
                           #     model = "unstructured",
                           #    sim = i,
                           #section = ResultList[[i]]$param$section),
                           cbind(coefficient = row.names(ResultList[[i]][[k]]$joi_v$result$coefficients),ResultList[[i]][[k]]$joi_v$result$coefficients,
                                 model = "joint",
                                 sim = i,
                                 section = ResultList[[i]][[k]]$param$section),
                           #cbind(ResultList[[i]]$unsbias_v$result$coefficients,
                           #     model = "unstr_bias",
                           #    sim = i,
                           #section = ResultList[[i]]$param$section),
                           cbind(coefficient = row.names(ResultList[[i]][[k]]$joibias_v$result$coefficients),ResultList[[i]][[k]]$joibias_v$result$coefficients,
                                 model = "joint_bias",
                                 sim = i,
                                 section = ResultList[[i]][[k]]$param$section),
                           cbind(coefficient = row.names(ResultList[[i]][[k]]$cov_v$result$coefficients),ResultList[[i]][[k]]$cov_v$result$coefficients,
                                 model = "covariate",
                                 sim = i,
                                 section = ResultList[[i]][[k]]$param$section),
                           cbind(coefficient = row.names(ResultList[[i]][[k]]$covbias_v$result$coefficients),ResultList[[i]][[k]]$covbias_v$result$coefficients,
                                 model = "covariate_bias",
                                 sim = i,
                                 section = ResultList[[i]][[k]]$param$section),
                           cbind(coefficient = row.names(ResultList[[i]][[k]]$cor_v_str$result$coefficients),ResultList[[i]][[k]]$cor_v_str$result$coefficients,
                                 model = "correlation",
                                 sim = i,
                                 section = ResultList[[i]][[k]]$param$section),
                           cbind(coefficient = row.names(ResultList[[i]][[k]]$corbias_v_str$result$coefficients),ResultList[[i]][[k]]$corbias_v_str$result$coefficients,
                                 model = "correlation_bias",
                                 sim = i,
                                 section = ResultList[[i]][[k]]$param$section))
  )
    }
  }
}

sim_fixed_unst <- data.frame()
for(i in 1:600){
  if(length(ResultList[[i]]) == 1){next} else{
  for(k in 9){
  sim_fixed_unst <- rbind(sim_fixed_unst,
                          rbind(
                            cbind(coefficient = row.names(ResultList[[i]][[k]]$uns_v$result$coefficients),ResultList[[i]][[k]]$uns_v$result$coefficients,
                                  model = "unstructured",
                                  sim = i,
                                  section = ResultList[[i]][[k]]$param$section),
                            cbind(coefficient = row.names(ResultList[[i]][[k]]$unsbias_v$result$coefficients),ResultList[[i]][[k]]$unsbias_v$result$coefficients,
                                  model = "unstr_bias",
                                  sim = i,
                                  section = ResultList[[i]][[k]]$param$section)
                          )
  )
    }
  }
} 

sim_fixed_full <- rbind(sim_fixed, sim_fixed_unst)

write.csv(sim_fixed_full, file = "sim_fixedeff_newscenario.csv")


# summary.hyperpar
sim_hyperpar <- data.frame()
for(i in 1:600){
  if(length(ResultList[[i]]) == 1){next} else{
    for(k in 1:9){
    sim_hyperpar <- rbind(sim_hyperpar,
                        rbind(cbind(ResultList[[i]][[k]]$str_v$result$hyperparameters,
                                    model = "structured",
                                    sim = i,
                                    section = ResultList[[i]][[k]]$param$section),
                              #cbind(ResultList[[i]]$uns_v$result$hyperparameters,
                              #      model = "unstructured",
                              #      sim = i,
                              #      section = ResultList[[i]]$param$section),
                              cbind(ResultList[[i]][[k]]$joi_v$result$hyperparameters,
                                    model = "joint",
                                    sim = i,
                                    section = ResultList[[i]][[k]]$param$section),
                              #cbind(ResultList[[i]]$unsbias_v$result$hyperparameters,
                              #      model = "unstr_bias",
                              #      sim = i,
                              #      section = ResultList[[i]]$param$section),
                              cbind(ResultList[[i]][[k]]$joibias_v$result$hyperparameters,
                                    model = "joint_bias",
                                    sim = i,
                                    section = ResultList[[i]][[k]]$param$section),
                              cbind(ResultList[[i]][[k]]$cov_v$result$hyperparameters,
                                    model = "covariate",
                                    sim = i,
                                    section = ResultList[[i]][[k]]$param$section),
                              cbind(ResultList[[i]][[k]]$covbias_v$result$hyperparameters,
                                    model = "covariate_bias",
                                    sim = i,
                                    section = ResultList[[i]][[k]]$param$section),
                              cbind(ResultList[[i]][[k]]$cor_v_str$result$hyperparameters,
                                    model = "correlation",
                                    sim = i,
                                    section = ResultList[[i]][[k]]$param$section),
                              cbind(ResultList[[i]][[k]]$corbias_v_str$result$hyperparameters,
                                    model = "correlation_bias",
                                    sim = i,
                                    section = ResultList[[i]][[k]]$param$section))
    )
    }
  }
}

sim_hyperpar_unst <- data.frame()
for(i in 1:600){
  if(length(ResultList[[i]]) == 1){next} else{
  sim_hyperpar_unst <- rbind(sim_hyperpar_unst,
                             rbind(
                               cbind(ResultList[[i]][[9]]$uns_v$result$hyperparameters,
                                     model = "unstructured",
                                     sim = i,
                                     section = ResultList[[i]][[9]]$param$section),
                               cbind(ResultList[[i]][[9]]$unsbias_v$result$hyperparameters,
                                     model = "unstr_bias",
                                     sim = i,
                                     section = ResultList[[i]][[9]]$param$section)
                             )
  )
  }
}

sim_hyperpar_full <- rbind(sim_hyperpar, sim_hyperpar_unst)

write.csv(sim_hyperpar_full, file = "sim_hyperpar_newscenario.csv")

