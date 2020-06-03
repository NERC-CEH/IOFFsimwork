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
  for(j in c(9,11)){   # for each model absolute difference
    
    sim_results <- rbind(sim_results,
                         rbind(cbind(ResultList[[i]]$param,
                                     ResultList[[i]][[j]]$result$`Proto-table`,
                                     rho = ResultList[[i]][[j]]$result$hyperparameters[3,1],
                                     sim = i)))
    
  }
  
}


sim_results_cor <- sim_results[sim_results$Model == "correlation_str",]
sim_results_corbias <- sim_results[sim_results$Model == "correlationbias_str",]

plot(sim_results_cor$MAE ~ sim_results_cor$rho)

plot(sim_results_corbias$MAE ~ sim_results_corbias$rho, col = 2)

cor.test(sim_results_cor$MAE,sim_results_cor$rho)

cor.test(sim_results_corbias$MAE,sim_results_corbias$rho)

