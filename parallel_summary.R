#### Function to extract and summarise results from parallel runs
library(matrixStats)
library(stringr)
library(dplyr)

parallel_summary <- function(results, type=c("single", "joint")){
  # results is input as a list
  # each element of the list is the result of that simulation, which has 7 elements
  # every first element is absolute validation - every second is relative
  
  # should run in mapply so that it goes through the list
  
  # want to make a summary which will include: correlation, env estimate, and MAE
  
  if(type == "single"){
    if(nrow(results$coefficients) == 2){
      return(as.numeric(c(results$'Proto-table'[2], 
                                           results$correlation, 
                                           results$coefficients[2,1], 
                                           results$coefficients[2,2], 
                                           results$coefficients[2,3])))
    } else {
      return(as.numeric(c(results$'Proto-table'[2], 
                          results$correlation, 
                          results$coefficients[2,1], 
                          results$coefficients[2,2], 
                          results$coefficients[2,3],
                          results$coefficients[3,1])))
    }
  }
  
  # as joint has two intercepts, environmental covariate estimate is in a different place!
  if(type == "joint"){
    if(nrow(results$coefficients) == 3){
      return(as.numeric(c(results$'Proto-table'[2], 
                      results$correlation, 
                      results$coefficients[3,1], 
                      results$coefficients[3,2], 
                      results$coefficients[3,3])))
      }   else {
        return(as.numeric(c(results$'Proto-table'[2], 
                            results$correlation, 
                            results$coefficients[3,1], 
                            results$coefficients[3,2], 
                            results$coefficients[3,3],
                            results$coefficients[4,1])))
      }
  }

}

#### Wrapper function for parallel summary

summary_wrapper <- function(file_name,
                            summary = c("raw", "summary"),
                            n_tot, n_by){
  
  if(str_detect(file_name, "joint")){type <- "joint"}else{type <- "single"}
  
  load(file_name)
  results <- model_output
  
  if(summary == "raw"){
  output_raw <- mapply(parallel_summary, results[seq(1,n_tot,n_by)],
            MoreArgs = list(type = type), SIMPLIFY = T)
  return(output_raw)}
  
  if(summary == "summary"){
  output_mean <- round(rowMeans(mapply(parallel_summary, results[seq(1,n_tot,n_by)], 
                  MoreArgs = list(type = type), SIMPLIFY = T)),2)
  output_sd <- round(rowSds(mapply(parallel_summary, results[seq(1,n_tot,n_by)], 
                                             MoreArgs = list(type = type), SIMPLIFY = T)),2)
  PO_data <- mean(mapply(mean,results[seq(4,n_tot,n_by)]))
  PA_data <- mean(mapply(mean,results[seq(3,n_tot,n_by)]))
  
  return(data.frame(MAE = output_mean[1],
             MAE_sd = output_sd[1],
             Correlation = output_mean[2],
             Correlation_sd = output_sd[2],
             MeanENV = output_mean[3], 
             LowerENV = output_mean[4], 
             UpperENV = output_mean[5],
             num_PO = PO_data,
             num_PA = PA_data))}
}


#### Function to make plotting dataframes

summary_plot_function <- function(summary_raw, scenario, n_runs, type = c("summary", "CI")){
  # summary raw comes in as a list
  
  output_data <- data.frame(correlation = NA,
                            env = NA,
                            env_in_CI = NA,
                            model = NA, # extract name of model
                            scenario = NA, # extract numeric part 
                            mae = NA,
                            width = NA,
                            biascov = NA) 
  prop_CI <- NA
  
  for(i in 1:length(summary_raw)){
    for(j in 1:n_runs){
      #different for correlation scenario as scenario is logical not numeric
      if(scenario != "Correlation_"){output_data_temp <- data.frame(correlation = summary_raw[[i]][2,j],
                                env = summary_raw[[i]][3,j],
                                env_in_CI = between(1.2, summary_raw[[i]][4,j], summary_raw[[i]][5,j]),
                                model = gsub("[^a-zA-Z]", "",str_sub(names(summary_raw)[i],nchar(scenario)+1, -7)), # extract name of model
                                scenario = gsub("[^0-9]", "",str_sub(names(summary_raw)[i],nchar(scenario)+1, -7)), # extract numeric part
                                mae = summary_raw[[i]][1,j],
                                width = summary_raw[[i]][5,j] - summary_raw[[i]][4,j])
      if(nrow(summary_raw[[i]])>5) {biascov <- summary_raw[[i]][6,j]
      output_data_temp$biascov <- biascov} else {output_data_temp$biascov <- NA}
                                output_data <- rbind(output_data, output_data_temp)
      }else{output_data_temp <- data.frame(correlation = summary_raw[[i]][2,j],
                                           env = summary_raw[[i]][3,j],
                                           env_in_CI = between(1.2, summary_raw[[i]][4,j], summary_raw[[i]][5,j]),
                                           model = gsub("[^a-z]", "",str_sub(names(summary_raw)[i],nchar(scenario)+1, -7)), # extract name of model
                                           scenario = gsub("[^A-Z]", "",str_sub(names(summary_raw)[i],nchar(scenario)+1, -7)), # extract numeric part
                                           mae = summary_raw[[i]][1,j], 
                                           width = summary_raw[[i]][5,j] - summary_raw[[i]][4,j])
      if(output_data_temp$scenario != "TRUE"){output_data_temp$scenario <- "FALSE"}
      output_data <- rbind(output_data, output_data_temp)}}
    
    prop_CI_T <- length(which((output_data$env_in_CI[(((i-1)*n_runs)+1):(i*n_runs)])==TRUE))/n_runs
    prop_CI <- c(prop_CI, prop_CI_T)
  }

  if(type == "summary")return(output_data[-1,])else{return(prop_CI[-1])}
}

# Function to find the minimum y limits

y_limits <- function(plotting_data, variable_name){
  marker <- which(colnames(plotting_data)==variable_name)
  store <- matrix(NA, ncol=2, nrow=6)
  for(i in 1:6){
    temp <- plotting_data[plotting_data$model == 
                            unique(plotting_data$model)[i],
                          marker]
    # need the bound for lower 2.5%
    lower <- sort(temp)[length(temp)*0.025]
    # and upper
    upper <- sort(temp)[length(temp)*0.975]
    
    store[i,] <- c(lower,upper)
  }
  return(c(min(store[,1], na.rm=T), max(store[,2], na.rm=T)))
}
