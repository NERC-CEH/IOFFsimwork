# script to summarise outputs from multiple runs

source('parallel_summary.R')

load('unstructured_output_parallel.RData')

# split out absolute (odd) and relative (even) 

unstructured_summary_A <- rowMeans(mapply(parallel_summary, simulation_output_unstructured[seq(1,100,2)], SIMPLIFY = T))
unstructured_summary_A <- data.frame(MAE = unstructured_summary_A[1],
                                   Correlation = unstructured_summary_A[2], 
                                   MeanENV = unstructured_summary_A[3], 
                                   LowerENV = unstructured_summary_A[4], 
                                   UpperENV = unstructured_summary_A[5])

unstructured_summary_R <- rowMeans(mapply(parallel_summary, simulation_output_unstructured[seq(2,100,2)], SIMPLIFY = T))
unstructured_summary_R <- data.frame(MAE = unstructured_summary_R[1],
                                     Correlation = unstructured_summary_R[2], 
                                     MeanENV = unstructured_summary_R[3], 
                                     LowerENV = unstructured_summary_R[4], 
                                     UpperENV = unstructured_summary_R[5])

load('structured_output_parallel.RData')

structured_summary_A <- rowMeans(mapply(parallel_summary, simulation_output_structured[seq(1,100,2)], SIMPLIFY = T))
structured_summary_A <- data.frame(MAE = structured_summary_A[1],
                                   Correlation = structured_summary_A[2], 
                                   MeanENV = structured_summary_A[3], 
                                   LowerENV = structured_summary_A[4], 
                                   UpperENV = structured_summary_A[5])

structured_summary_R <- rowMeans(mapply(parallel_summary, simulation_output_structured[seq(2,100,2)], SIMPLIFY = T))
structured_summary_R <- data.frame(MAE = structured_summary_R[1],
                                 Correlation = structured_summary_R[2], 
                                 MeanENV = structured_summary_R[3], 
                                 LowerENV = structured_summary_R[4], 
                                 UpperENV = structured_summary_R[5])

load('joint_output_parallel.RData')

joint_summary_A <- rowMeans(mapply(parallel_summary, simulation_output_joint, SIMPLIFY = T))
joint_summary_A <- data.frame(MAE = joint_summary_A[1],
                                 Correlation = joint_summary_A[2], 
                                 MeanENV = joint_summary_A[3], 
                                 LowerENV = joint_summary_A[4], 
                                 UpperENV = joint_summary_A[5])

joint_summary_R <- rowMeans(mapply(parallel_summary, simulation_output_joint, SIMPLIFY = T))
joint_summary_R <- data.frame(MAE = joint_summary_R[1],
                            Correlation = joint_summary_R[2], 
                            MeanENV = joint_summary_R[3], 
                            LowerENV = joint_summary_R[4], 
                            UpperENV = joint_summary_R[5])


rbind(unstructured_summary_A, structured_summary_A, joint_summary_A)
rbind(unstructured_summary_R, structured_summary_R, joint_summary_R)