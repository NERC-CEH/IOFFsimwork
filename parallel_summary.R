#### Function to extract and summarise results from parallel runs
load("unstructured_output_parallel.RData")

parallel_summary <- function(results){
  # results is input as a list
  # each element of the list is the result of that simulation, which has 7 elements
  # every first element is absolute validation - every second is relative
  
  # should run in mapply so that it goes through the list
  
  # want to make a summary which will include: correlation, env estimate, and MAE
  
  return(as.numeric(c(results$'Proto-table'[2], results$correlation, results$coefficients[2,1], results$coefficients[2,2], results$coefficients[2,3])))
  
}