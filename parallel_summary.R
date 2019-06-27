#### Function to extract and summarise results from parallel runs
library(matrixStats)
library(stringr)

parallel_summary <- function(results, type=c("single", "joint")){
  # results is input as a list
  # each element of the list is the result of that simulation, which has 7 elements
  # every first element is absolute validation - every second is relative
  
  # should run in mapply so that it goes through the list
  
  # want to make a summary which will include: correlation, env estimate, and MAE
  
  if(type == "single"){return(as.numeric(c(results$'Proto-table'[2], 
                                           results$correlation, 
                                           results$coefficients[2,1], 
                                           results$coefficients[2,2], 
                                           results$coefficients[2,3])))}
  
  # as joint has two intercepts, environmental covariate estimate is in a different place!
  if(type == "joint"){return(as.numeric(c(results$'Proto-table'[2], 
                      results$correlation, 
                      results$coefficients[3,1], 
                      results$coefficients[3,2], 
                      results$coefficients[3,3])))}

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
  return(data.frame(MAE = output_mean[1],
             MAE_sd = output_sd[1],
             Correlation = output_mean[2],
             Correlation_sd = output_sd[2],
             MeanENV = output_mean[3], 
             LowerENV = output_mean[4], 
             UpperENV = output_mean[5]))}
}


#### Function to make plotting dataframes

summary_plot_function <- function(summary_raw, scenario){
  # summary raw comes in as a list
  output_data <- data.frame(correlation = summary_raw[[1]][2,1],
                            env = summary_raw[[1]][3,1],
                            env_in_CI = 1.2%in%(summary_raw[[1]][4,1]:summary_raw[[1]][5,1]),
                            model = gsub("[^a-zA-Z]", "",str_sub(names(summary_raw)[1],nchar(scenario)+1, -7)), # extract name of model
                            scenario = gsub("[^0-9]", "",str_sub(names(summary_raw)[1],nchar(scenario)+1, -7))) # extract numeric part 
  
  for(i in 2:length(summary_raw)){
    output_data_temp <- data.frame(correlation = summary_raw[[i]][2,1],
                              env = summary_raw[[i]][3,1],
                              env_in_CI = 1.2%in%(summary_raw[[i]][4,1]:summary_raw[[i]][5,1]),
                              model = gsub("[^a-zA-Z]", "",str_sub(names(summary_raw)[i],nchar(scenario)+1, -7)), # extract name of model
                              scenario = gsub("[^0-9]", "",str_sub(names(summary_raw)[i],nchar(scenario)+1, -7))) # extract numeric part 
    output_data <- rbind(output_data, output_data_temp)
  }
  
  return(output_data)
}