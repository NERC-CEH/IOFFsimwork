#' # Evaluation of first scenario run
#' 
#' # Try assigning nsamp to global env
#' 
#' ## Sample size of structured changes
#' 
#' This has so far been run with a different 'truth' for each model. I don't know if this is a problem or something we want to address.
#' 
#' The default parameters are as follows:
#' 
#' - simulation is conducted on a grid of 300*300  
#' - environmental covariate coefficient of 1.2
#' - scale parameter kappa for matern covariance of 0.05  
#' - variance parameter sigma2x of matern covariance of 2  
#' - mean log intensity of point process of -3  
#' - 150 structured samples
#' - probability of sampling strata rep(c(0.5, 0.3, 0.1, 0.05, 0.01),5) 
#' - qsize of 1
#' 
#' Then sample size of structured is changed to 100 and 50
#' 
#' ## Results
#' 
#' All evaluation relative at the moment.
#' 
#+ warning = FALSE, message = FALSE, error = FALSE, include = TRUE, echo = FALSE
#### Unstructured model
source('parallel_summary.R')
n_runs = 500
n_tot = n_runs*3
n_by = 3


load('unstructured_output_parallel.RData')

library(matrixStats)
library(RColorBrewer)
library(ggplot2)

unstructured_summary_R_raw <- mapply(parallel_summary, simulation_output_unstructured[seq(1,n_tot,n_by)],
                                     MoreArgs = list(type = "single"), SIMPLIFY = T)
unstructured_summary_R <- rowMeans(mapply(parallel_summary, simulation_output_unstructured[seq(1,n_tot,n_by)], 
                                          MoreArgs = list(type = "single"), SIMPLIFY = T))
unstructured_summary_R_sd <- rowSds(mapply(parallel_summary, simulation_output_unstructured[seq(1,n_tot,n_by)], 
                                          MoreArgs = list(type = "single"), SIMPLIFY = T))
unstructured_summary_R <- data.frame(MAE = unstructured_summary_R[1],
                                     MAE_sd = unstructured_summary_R_sd[1],
                                     Correlation = unstructured_summary_R[2],
                                     Correlation_sd = unstructured_summary_R_sd[2],
                                     MeanENV = unstructured_summary_R[3], 
                                     LowerENV = unstructured_summary_R[4], 
                                     UpperENV = unstructured_summary_R[5])

#' 
#' 
#+ warning = FALSE, message = FALSE, error = FALSE, include = TRUE, echo = FALSE
#### Structured model
load('structured_output_v_high_parallel.RData')


structured_summary_R_v_high_raw <- mapply(parallel_summary, simulation_output_structured_v_high[seq(1,n_tot,n_by)], 
                                        MoreArgs = list(type = "single"), SIMPLIFY = T)
structured_summary_R_v_high <- rowMeans(structured_summary_R_v_high_raw)
structured_summary_R_v_high_sd <- rowSds(mapply(parallel_summary, simulation_output_structured_v_high[seq(1,n_tot,n_by)], 
                                           MoreArgs = list(type = "single"), SIMPLIFY = T))
structured_summary_R_v_high <- data.frame(MAE = structured_summary_R_v_high[1],
                                          MAE_sd = structured_summary_R_v_high_sd[1],
                                          Correlation = structured_summary_R_v_high[2],
                                          Correlation_sd = structured_summary_R_v_high_sd[2], 
                                        MeanENV = structured_summary_R_v_high[3], 
                                        LowerENV = structured_summary_R_v_high[4], 
                                        UpperENV = structured_summary_R_v_high[5])

load('structured_output_high_parallel.RData')


structured_summary_R_high_raw <- mapply(parallel_summary, simulation_output_structured_high[seq(1,n_tot,n_by)], 
                                             MoreArgs = list(type = "single"), SIMPLIFY = T)
structured_summary_R_high <- rowMeans(structured_summary_R_high_raw)
structured_summary_R_high_sd <- rowSds(mapply(parallel_summary, simulation_output_structured_high[seq(1,n_tot,n_by)], 
                                                MoreArgs = list(type = "single"), SIMPLIFY = T))
structured_summary_R_high <- data.frame(MAE = structured_summary_R_high[1],
                                        MAE_sd = structured_summary_R_high_sd[1],
                                        Correlation = structured_summary_R_high[2],
                                        Correlation_sd = structured_summary_R_high_sd[2],
                                     MeanENV = structured_summary_R_high[3], 
                                     LowerENV = structured_summary_R_high[4], 
                                     UpperENV = structured_summary_R_high[5])

load('structured_output_mid_parallel.RData')


structured_summary_R_mid_raw <- mapply(parallel_summary, simulation_output_structured_mid[seq(1,n_tot,n_by)], 
                                        MoreArgs = list(type = "single"), SIMPLIFY = T)
structured_summary_R_mid <- rowMeans(structured_summary_R_mid_raw)
structured_summary_R_mid_sd <- rowSds(mapply(parallel_summary, simulation_output_structured_mid[seq(1,n_tot,n_by)], 
                                                MoreArgs = list(type = "single"), SIMPLIFY = T))
structured_summary_R_mid <- data.frame(MAE = structured_summary_R_mid[1],
                                       MAE_sd = structured_summary_R_mid_sd[1],
                                       Correlation = structured_summary_R_mid[2],
                                       Correlation_sd = structured_summary_R_mid_sd[2], 
                                        MeanENV = structured_summary_R_mid[3], 
                                        LowerENV = structured_summary_R_mid[4], 
                                        UpperENV = structured_summary_R_mid[5])

load('structured_output_low_parallel.RData')


structured_summary_R_low_raw <- mapply(parallel_summary, simulation_output_structured_low[seq(1,n_tot,n_by)], 
                                        MoreArgs = list(type = "single"), SIMPLIFY = T)
structured_summary_R_low <- rowMeans(structured_summary_R_low_raw)
structured_summary_R_low_sd <- rowSds(mapply(parallel_summary, simulation_output_structured_low[seq(1,n_tot,n_by)], 
                                                MoreArgs = list(type = "single"), SIMPLIFY = T))
structured_summary_R_low <- data.frame(MAE = structured_summary_R_low[1],
                                       MAE_sd = structured_summary_R_low_sd[1],
                                       Correlation = structured_summary_R_low[2],
                                       Correlation_sd = structured_summary_R_low_sd[2], 
                                       MeanENV = structured_summary_R_low[3], 
                                       LowerENV = structured_summary_R_low[4], 
                                       UpperENV = structured_summary_R_low[5])

load('structured_output_v_low_parallel.RData')


structured_summary_R_v_low_raw <- mapply(parallel_summary, simulation_output_structured_v_low[seq(1,n_tot,n_by)], 
                                       MoreArgs = list(type = "single"), SIMPLIFY = T)
structured_summary_R_v_low <- rowMeans(structured_summary_R_v_low_raw)
structured_summary_R_v_low_sd <- rowSds(mapply(parallel_summary, simulation_output_structured_v_low[seq(1,n_tot,n_by)], 
                                                MoreArgs = list(type = "single"), SIMPLIFY = T))
structured_summary_R_v_low <- data.frame(MAE = structured_summary_R_v_low[1],
                                         MAE_sd = structured_summary_R_v_low_sd[1],
                                         Correlation = structured_summary_R_v_low[2],
                                         Correlation_sd = structured_summary_R_v_low_sd[2],
                                       MeanENV = structured_summary_R_v_low[3], 
                                       v_lowerENV = structured_summary_R_v_low[4], 
                                       UpperENV = structured_summary_R_v_low[5])

load('joint_output_v_high_parallel.RData')


joint_summary_R_v_high_raw <- mapply(parallel_summary, simulation_output_joint_v_high[seq(1,n_tot,n_by)], 
                                   MoreArgs = list(type = "joint"), SIMPLIFY = T)
joint_summary_R_v_high <- rowMeans(joint_summary_R_v_high_raw)
joint_summary_R_v_high_sd <- rowSds(mapply(parallel_summary, simulation_output_joint_v_high[seq(1,n_tot,n_by)], 
                                                MoreArgs = list(type = "single"), SIMPLIFY = T))
joint_summary_R_v_high <- data.frame(MAE = joint_summary_R_v_high[1],
                                     MAE_sd = joint_summary_R_v_high_sd[1],
                                     Correlation = joint_summary_R_v_high[2],
                                     Correlation_sd = joint_summary_R_v_high_sd[2], 
                                   MeanENV = joint_summary_R_v_high[3], 
                                   v_higherENV = joint_summary_R_v_high[4], 
                                   UpperENV = joint_summary_R_v_high[5])

load('joint_output_high_parallel.RData')


joint_summary_R_high_raw <- mapply(parallel_summary, simulation_output_joint_high[seq(1,n_tot,n_by)], 
                                        MoreArgs = list(type = "joint"), SIMPLIFY = T)
joint_summary_R_high <- rowMeans(joint_summary_R_high_raw)
joint_summary_R_high_sd <- rowSds(mapply(parallel_summary, simulation_output_joint_high[seq(1,n_tot,n_by)], 
                                           MoreArgs = list(type = "single"), SIMPLIFY = T))
joint_summary_R_high <- data.frame(MAE = joint_summary_R_high[1],
                                   MAE_sd = joint_summary_R_high_sd[1],
                                   Correlation = joint_summary_R_high[2],
                                   Correlation_sd = joint_summary_R_high_sd[2],  
                                  MeanENV = joint_summary_R_high[3], 
                                  higherENV = joint_summary_R_high[4], 
                                  UpperENV = joint_summary_R_high[5])


load('joint_output_mid_parallel.RData')


joint_summary_R_mid_raw <- mapply(parallel_summary, simulation_output_joint_mid[seq(1,n_tot,n_by)], 
                                   MoreArgs = list(type = "joint"), SIMPLIFY = T)
joint_summary_R_mid <- rowMeans(joint_summary_R_mid_raw)
joint_summary_R_mid_sd <- rowSds(mapply(parallel_summary, simulation_output_joint_mid[seq(1,n_tot,n_by)], 
                                           MoreArgs = list(type = "single"), SIMPLIFY = T))
joint_summary_R_mid <- data.frame(MAE = joint_summary_R_mid[1],
                                  MAE_sd = joint_summary_R_mid_sd[1],
                                  Correlation = joint_summary_R_mid[2],
                                  Correlation_sd = joint_summary_R_mid_sd[2],  
                                  MeanENV = joint_summary_R_mid[3], 
                                  miderENV = joint_summary_R_mid[4], 
                                  UpperENV = joint_summary_R_mid[5])

load('joint_output_low_parallel.RData')


joint_summary_R_low_raw <- mapply(parallel_summary, simulation_output_joint_low[seq(1,n_tot,n_by)], 
                                   MoreArgs = list(type = "joint"), SIMPLIFY = T)
joint_summary_R_low <- rowMeans(joint_summary_R_low_raw)
joint_summary_R_low_sd <- rowSds(mapply(parallel_summary, simulation_output_joint_low[seq(1,n_tot,n_by)], 
                                           MoreArgs = list(type = "single"), SIMPLIFY = T))
joint_summary_R_low <- data.frame(MAE = joint_summary_R_low[1],
                                  MAE_sd = joint_summary_R_low_sd[1],
                                  Correlation = joint_summary_R_low[2],
                                  Correlation_sd = joint_summary_R_low_sd[2],  
                                       MeanENV = joint_summary_R_low[3], 
                                       LowerENV = joint_summary_R_low[4], 
                                       UpperENV = joint_summary_R_low[5])

load('joint_output_v_low_parallel.RData')


joint_summary_R_v_low_raw <- mapply(parallel_summary, simulation_output_joint_v_low[seq(1,n_tot,n_by)], 
                                  MoreArgs = list(type = "joint"), SIMPLIFY = T)
joint_summary_R_v_low <- rowMeans(joint_summary_R_v_low_raw)
joint_summary_R_v_low_sd <- rowSds(mapply(parallel_summary, simulation_output_joint_v_low[seq(1,n_tot,n_by)], 
                                           MoreArgs = list(type = "single"), SIMPLIFY = T))
joint_summary_R_v_low <- data.frame(MAE = joint_summary_R_v_low[1],
                                    MAE_sd = joint_summary_R_v_low_sd[1],
                                    Correlation = joint_summary_R_v_low[2],
                                    Correlation_sd = joint_summary_R_v_low_sd[2],  
                                  MeanENV = joint_summary_R_v_low[3], 
                                  v_lowerENV = joint_summary_R_v_low[4], 
                                  UpperENV = joint_summary_R_v_low[5])

load('jointcov_output_v_high_parallel.RData')


jointcov_summary_R_v_high_raw <- mapply(parallel_summary, simulation_output_jointcov_v_high[seq(1,n_tot,n_by)], 
                                      MoreArgs = list(type = "joint"), SIMPLIFY = T)
jointcov_summary_R_v_high <- rowMeans(jointcov_summary_R_v_high_raw)
jointcov_summary_R_v_high_sd <- rowSds(mapply(parallel_summary, simulation_output_jointcov_v_high[seq(1,n_tot,n_by)], 
                                           MoreArgs = list(type = "single"), SIMPLIFY = T))
jointcov_summary_R_v_high <- data.frame(MAE = jointcov_summary_R_v_high[1],
                                        MAE_sd = jointcov_summary_R_v_high_sd[1],
                                        Correlation = jointcov_summary_R_v_high[2],
                                        Correlation_sd = jointcov_summary_R_v_high_sd[2],  
                                      MeanENV = jointcov_summary_R_v_high[3], 
                                      v_higherENV = jointcov_summary_R_v_high[4], 
                                      UpperENV = jointcov_summary_R_v_high[5])

load('jointcov_output_high_parallel.RData')


jointcov_summary_R_high_raw <- mapply(parallel_summary, simulation_output_jointcov_high[seq(1,n_tot,n_by)], 
                                   MoreArgs = list(type = "joint"), SIMPLIFY = T)
jointcov_summary_R_high <- rowMeans(jointcov_summary_R_high_raw)
jointcov_summary_R_high_sd <- rowSds(mapply(parallel_summary, simulation_output_jointcov_high[seq(1,n_tot,n_by)], 
                                              MoreArgs = list(type = "single"), SIMPLIFY = T))
jointcov_summary_R_high <- data.frame(MAE = jointcov_summary_R_high[1],
                                      MAE_sd = jointcov_summary_R_high_sd[1],
                                      Correlation = jointcov_summary_R_high[2],
                                      Correlation_sd = jointcov_summary_R_high_sd[2], 
                                   MeanENV = jointcov_summary_R_high[3], 
                                   higherENV = jointcov_summary_R_high[4], 
                                   UpperENV = jointcov_summary_R_high[5])


load('jointcov_output_mid_parallel.RData')

jointcov_summary_R_mid_raw <- mapply(parallel_summary, simulation_output_jointcov_mid[seq(1,n_tot,n_by)], 
                                      MoreArgs = list(type = "joint"), SIMPLIFY = T)
jointcov_summary_R_mid <- rowMeans(jointcov_summary_R_mid_raw)
jointcov_summary_R_mid_sd <- rowSds(mapply(parallel_summary, simulation_output_jointcov_mid[seq(1,n_tot,n_by)], 
                                              MoreArgs = list(type = "single"), SIMPLIFY = T))
jointcov_summary_R_mid <- data.frame(MAE = jointcov_summary_R_mid[1],
                                     MAE_sd = jointcov_summary_R_mid_sd[1],
                                     Correlation = jointcov_summary_R_mid[2],
                                     Correlation_sd = jointcov_summary_R_mid_sd[2], 
                                  MeanENV = jointcov_summary_R_mid[3], 
                                  miderENV = jointcov_summary_R_mid[4], 
                                  UpperENV = jointcov_summary_R_mid[5])

load('jointcov_output_low_parallel.RData')


jointcov_summary_R_low_raw <- mapply(parallel_summary, simulation_output_jointcov_low[seq(1,n_tot,n_by)], 
                                      MoreArgs = list(type = "joint"), SIMPLIFY = T)
jointcov_summary_R_low <- rowMeans(jointcov_summary_R_low_raw)
jointcov_summary_R_low_sd <- rowSds(mapply(parallel_summary, simulation_output_jointcov_low[seq(1,n_tot,n_by)], 
                                              MoreArgs = list(type = "single"), SIMPLIFY = T))
jointcov_summary_R_low <- data.frame(MAE = jointcov_summary_R_low[1],
                                     MAE_sd = jointcov_summary_R_low_sd[1],
                                     Correlation = jointcov_summary_R_low[2],
                                     Correlation_sd = jointcov_summary_R_low_sd[2], 
                                  MeanENV = jointcov_summary_R_low[3], 
                                  LowerENV = jointcov_summary_R_low[4], 
                                  UpperENV = jointcov_summary_R_low[5])

load('jointcov_output_v_low_parallel.RData')


jointcov_summary_R_v_low_raw <- mapply(parallel_summary, simulation_output_jointcov_v_low[seq(1,n_tot,n_by)], 
                                     MoreArgs = list(type = "joint"), SIMPLIFY = T)
jointcov_summary_R_v_low <- rowMeans(jointcov_summary_R_v_low_raw)
jointcov_summary_R_v_low_sd <- rowSds(mapply(parallel_summary, simulation_output_jointcov_v_low[seq(1,n_tot,n_by)], 
                                              MoreArgs = list(type = "single"), SIMPLIFY = T))
jointcov_summary_R_v_low <- data.frame(MAE = jointcov_summary_R_v_low[1],
                                       MAE_sd = jointcov_summary_R_v_low_sd[1],
                                       Correlation = jointcov_summary_R_v_low[2],
                                       Correlation_sd = jointcov_summary_R_v_low_sd[2],
                                     MeanENV = jointcov_summary_R_v_low[3], 
                                     v_lowerENV = jointcov_summary_R_v_low[4], 
                                     UpperENV = jointcov_summary_R_v_low[5])

load('joint2_output_v_high_parallel.RData')

joint2_summary_R_v_high_raw <- mapply(parallel_summary, simulation_output_joint2_v_high[seq(1,n_tot,n_by)], 
                                       MoreArgs = list(type = "joint"), SIMPLIFY = T)
joint2_summary_R_v_high <- rowMeans(joint2_summary_R_v_high_raw)
joint2_summary_R_v_high_sd <- rowSds(mapply(parallel_summary, simulation_output_joint2_v_high[seq(1,n_tot,n_by)], 
                                              MoreArgs = list(type = "single"), SIMPLIFY = T))
joint2_summary_R_v_high<- data.frame(MAE = joint2_summary_R_v_high[1],
                                     MAE_sd = joint2_summary_R_v_high_sd[1],
                                     Correlation = joint2_summary_R_v_high[2],
                                     Correlation_sd = joint2_summary_R_v_high_sd[2], 
                                       MeanENV = joint2_summary_R_v_high[3], 
                                       v_higherENV = joint2_summary_R_v_high[4], 
                                       UpperENV = joint2_summary_R_v_high[5])

load('joint2_output_high_parallel.RData')

joint2_summary_R_high_raw <- mapply(parallel_summary, simulation_output_joint2_high[seq(1,n_tot,n_by)], 
                                      MoreArgs = list(type = "joint"), SIMPLIFY = T)
joint2_summary_R_high <- rowMeans(joint2_summary_R_high_raw)
joint2_summary_R_high_sd <- rowSds(mapply(parallel_summary, simulation_output_joint2_high[seq(1,n_tot,n_by)], 
                                            MoreArgs = list(type = "single"), SIMPLIFY = T))
joint2_summary_R_high<- data.frame(MAE = joint2_summary_R_high[1],
                                   MAE_sd = joint2_summary_R_high_sd[1],
                                   Correlation = joint2_summary_R_high[2],
                                   Correlation_sd = joint2_summary_R_high_sd[2],  
                                     MeanENV = joint2_summary_R_high[3], 
                                     v_higherENV = joint2_summary_R_high[4], 
                                     UpperENV = joint2_summary_R_high[5])

load('joint2_output_mid_parallel.RData')

joint2_summary_R_mid_raw <- mapply(parallel_summary, simulation_output_joint2_mid[seq(1,n_tot,n_by)], 
                                      MoreArgs = list(type = "joint"), SIMPLIFY = T)
joint2_summary_R_mid <- rowMeans(joint2_summary_R_mid_raw)
joint2_summary_R_mid_sd <- rowSds(mapply(parallel_summary, simulation_output_joint2_mid[seq(1,n_tot,n_by)], 
                                            MoreArgs = list(type = "single"), SIMPLIFY = T))
joint2_summary_R_mid<- data.frame(MAE = joint2_summary_R_mid[1],
                                  MAE_sd = joint2_summary_R_mid_sd[1],
                                  Correlation = joint2_summary_R_mid[2],
                                  Correlation_sd = joint2_summary_R_mid_sd[2], 
                                     MeanENV = joint2_summary_R_mid[3], 
                                     miderENV = joint2_summary_R_mid[4], 
                                     UpperENV = joint2_summary_R_mid[5])

load('joint2_output_low_parallel.RData')

joint2_summary_R_low_raw <- mapply(parallel_summary, simulation_output_joint2_low[seq(1,n_tot,n_by)], 
                                   MoreArgs = list(type = "joint"), SIMPLIFY = T)
joint2_summary_R_low <- rowMeans(joint2_summary_R_low_raw)
joint2_summary_R_low_sd <- rowSds(mapply(parallel_summary, simulation_output_joint2_low[seq(1,n_tot,n_by)], 
                                            MoreArgs = list(type = "single"), SIMPLIFY = T))
joint2_summary_R_low<- data.frame(MAE = joint2_summary_R_low[1],
                                  MAE_sd = joint2_summary_R_low_sd[1],
                                  Correlation = joint2_summary_R_low[2],
                                  Correlation_sd = joint2_summary_R_low_sd[2],  
                                  MeanENV = joint2_summary_R_low[3], 
                                  lowerENV = joint2_summary_R_low[4], 
                                  UpperENV = joint2_summary_R_low[5])

load('joint2_output_v_low_parallel.RData')

joint2_summary_R_v_low_raw <- mapply(parallel_summary, simulation_output_joint2_v_low[seq(1,n_tot,n_by)], 
                                   MoreArgs = list(type = "joint"), SIMPLIFY = T)
joint2_summary_R_v_low <- rowMeans(joint2_summary_R_v_low_raw)
joint2_summary_R_v_low_sd <- rowSds(mapply(parallel_summary, simulation_output_joint2_v_low[seq(1,n_tot,n_by)], 
                                            MoreArgs = list(type = "single"), SIMPLIFY = T))
joint2_summary_R_v_low<- data.frame(MAE = joint2_summary_R_v_low[1],
                                    MAE_sd = joint2_summary_R_v_low_sd[1],
                                    Correlation = joint2_summary_R_v_low[2],
                                    Correlation_sd = joint2_summary_R_v_low_sd[2],  
                                  MeanENV = joint2_summary_R_v_low[3], 
                                  v_lowerENV = joint2_summary_R_v_low[4], 
                                  UpperENV = joint2_summary_R_v_low[5])

                                
# summary table 

# make all column names the same
names(structured_summary_R_v_high) <- names(structured_summary_R_v_low) <- names(structured_summary_R_low) <- names(structured_summary_R_mid) <- names(structured_summary_R_high) <- names(unstructured_summary_R) 
names(joint_summary_R_v_high) <- names(joint_summary_R_v_low) <- names(joint_summary_R_low) <- names(joint_summary_R_mid) <- names(joint_summary_R_high) <- names(unstructured_summary_R) 
names(jointcov_summary_R_v_high) <- names(jointcov_summary_R_v_low) <- names(jointcov_summary_R_low) <- names(jointcov_summary_R_mid) <- names(jointcov_summary_R_high) <- names(unstructured_summary_R) 
names(joint2_summary_R_v_high) <- names(joint2_summary_R_v_low) <- names(joint2_summary_R_low) <- names(joint2_summary_R_mid) <- names(joint2_summary_R_high) <- names(unstructured_summary_R)

summary_table <- rbind(unstructured_summary_R, structured_summary_R_low, structured_summary_R_v_low,
                       structured_summary_R_mid, structured_summary_R_high, structured_summary_R_v_high, 
                       joint_summary_R_v_low,
                       joint_summary_R_low, joint_summary_R_mid, joint_summary_R_high, joint_summary_R_v_high,
                       jointcov_summary_R_v_low,
                       jointcov_summary_R_low, jointcov_summary_R_mid, jointcov_summary_R_high, jointcov_summary_R_v_high,
                       joint2_summary_R_v_low,
                       joint2_summary_R_low, joint2_summary_R_mid, joint2_summary_R_high, joint2_summary_R_v_high)

row.names(summary_table) <- c("unstructured", "structured_v_low", "structured_low", "structured_mid", "structured_high", "structured_v_high",
                              "joint_v_low", "joint_low", "joint_mid", "joint_high", "joint_v_high",
                              "jointcov_v_low", "jointcov_low", "jointcov_mid", "jointcov_high", "jointcov_v_high",
                              "joint2_v_low", "joint2_low", "joint2_mid", "joint2_high", "joint2_v_high")

round(summary_table,2)

write.csv(round(summary_table,2), "SummaryTable_samplesize.csv", row.names=T)

#' ## Box plots of everything
#' 
#' ### Structured model - correlation
#+ warning = FALSE, message = FALSE, error = FALSE, include = TRUE, echo = FALSE
# join all of the correlation estimates into a dataframe so can use ggplot
correlation_data <- data.frame(correlation = c(unstructured_summary_R_raw[2,],
                                               structured_summary_R_v_low_raw[2,],
                                               structured_summary_R_low_raw[2,],
                                               structured_summary_R_mid_raw[2,],
                                               structured_summary_R_high_raw[2,],
                                               structured_summary_R_v_high_raw[2,],
                                               joint_summary_R_v_low_raw[2,],
                                               joint_summary_R_low_raw[2,],
                                               joint_summary_R_mid_raw[2,],
                                               joint_summary_R_high_raw[2,],
                                               joint_summary_R_v_high_raw[2,],
                                               jointcov_summary_R_v_low_raw[2,],
                                               jointcov_summary_R_low_raw[2,],
                                               jointcov_summary_R_mid_raw[2,],
                                               jointcov_summary_R_high_raw[2,],
                                               jointcov_summary_R_v_high_raw[2,],
                                               joint2_summary_R_v_low_raw[2,],
                                               joint2_summary_R_low_raw[2,],
                                               joint2_summary_R_mid_raw[2,],
                                               joint2_summary_R_high_raw[2,],
                                               joint2_summary_R_v_high_raw[2,]),
                               group = c(rep("unstructured", n_runs),
                                         rep("structured_v_low", n_runs),
                                         rep("structured_low", n_runs),
                                         rep("structured_mid", n_runs),
                                         rep("structured_high", n_runs),
                                         rep("structured_v_high", n_runs),
                                         rep("joint_v_low", n_runs),
                                         rep("joint_low", n_runs),
                                         rep("joint_mid", n_runs),
                                         rep("joint_high", n_runs),
                                         rep("joint_v_high", n_runs),
                                         rep("jointcov_v_low", n_runs),
                                         rep("jointcov_low", n_runs),
                                         rep("jointcov_mid", n_runs),
                                         rep("jointcov_high", n_runs),
                                         rep("jointcov_v_high", n_runs),
                                         rep("joint2_v_low", n_runs),
                                         rep("joint2_low", n_runs),
                                         rep("joint2_mid", n_runs),
                                         rep("joint2_high", n_runs),
                                         rep("joint2_v_high", n_runs)),
                               group2 = c(rep("Unstructured \nonly", n_runs), 
                                          rep("Structured \nonly", n_runs*5),
                                          rep("Joint", n_runs*5),
                                          rep("Joint with \nbias \ncovariate", n_runs*5),
                                          rep("Joint with \nsecond \nspatial field", n_runs*5)))
# change levels of factor
correlation_data$group <- factor(correlation_data$group, level = c("unstructured",
                                                                   "structured_v_low",
                                                                   "structured_low", "structured_mid", 
                                                                   "structured_high", "structured_v_high",
                                                                   "joint_v_low",
                                                                   "joint_low", "joint_mid", 
                                                                   "joint_high", "joint_v_high",
                                                                   "jointcov_v_low",
                                                                   "jointcov_low", "jointcov_mid", 
                                                                   "jointcov_high", "jointcov_v_high",
                                                                   "joint2_v_low",
                                                                   "joint2_low", "joint2_mid", 
                                                                   "joint2_high", "joint2_v_high"))
correlation_data$group2 <- factor(correlation_data$group2, level = c("Unstructured \nonly", 
                                                                     "Structured \nonly", 
                                                                     "Joint",
                                                                    "Joint with \nbias \ncovariate",
                                                                    "Joint with \nsecond \nspatial field"))


# manual colours
manual_colours <- brewer.pal(5, "Paired")
Correlation <- ggplot(correlation_data, aes(group, correlation))+
  scale_fill_manual(values=manual_colours, name = "",
                    labels = c("Unstructured only", "Structured only", "Joint",
                               "Joint with \nbias \ncovariate", "Joint with \nsecond spatial field"))+
  geom_violin(aes(fill=as.factor(group2)), trim=FALSE)+
  geom_boxplot(width=0.1)+
  theme_classic()+
  theme(legend.position = "none")+
  xlab("Structured sample size")+
  ylab("Correlation between prediction and truth")+
  facet_wrap(~as.factor(group2), nrow=1, scales="free_x")+
  scale_x_discrete(labels=c("unstructured" = "", "structured_v_low" = "26",
                            "structured_low" = "50", "structured_mid" = "100", 
                            "structured_high" = "150", "structured_v_high" = "500",
                            "joint_v_low" = "26",
                            "joint_low" = "50", "joint_mid" = "100", 
                            "joint_high" = "150", "joint_v_high" = "500",
                            "jointcov_v_low" = "26",
                            "jointcov_low" = "50", "jointcov_mid" = "100", 
                            "jointcov_high" = "150", "jointcov_v_high" = "500",
                            "joint2_v_low" = "26",
                            "joint2_low" = "50", "joint2_mid" = "100", 
                            "joint2_high" = "150", "joint2_v_high" = "500"))

Correlation

ggsave(filename = "CorrelationPlot_samplesize.png", plot=last_plot(),
       width = 20, height = 10, units="cm", dpi=300)

#' ### Structured model - environment coefficient
#+ warning = FALSE, message = FALSE, error = FALSE, include = TRUE, echo = FALSE
environment_data <- data.frame(environment = c(unstructured_summary_R_raw[3,],
                                               structured_summary_R_v_low_raw[3,],
                                               structured_summary_R_low_raw[3,],
                                               structured_summary_R_mid_raw[3,],
                                               structured_summary_R_high_raw[3,],
                                               structured_summary_R_v_high_raw[3,],
                                               joint_summary_R_v_low_raw[3,],
                                               joint_summary_R_low_raw[3,],
                                               joint_summary_R_mid_raw[3,],
                                               joint_summary_R_high_raw[3,],
                                               joint_summary_R_v_high_raw[3,],
                                               jointcov_summary_R_v_low_raw[3,],
                                               jointcov_summary_R_low_raw[3,],
                                               jointcov_summary_R_mid_raw[3,],
                                               jointcov_summary_R_high_raw[3,],
                                               jointcov_summary_R_v_high_raw[3,],
                                               joint2_summary_R_v_low_raw[3,],
                                               joint2_summary_R_low_raw[3,],
                                               joint2_summary_R_mid_raw[3,],
                                               joint2_summary_R_high_raw[3,],
                                               joint2_summary_R_v_high_raw[3,]),
                               group = c(rep("unstructured", n_runs),
                                         rep("structured_v_low", n_runs),
                                         rep("structured_low", n_runs),
                                         rep("structured_mid", n_runs),
                                         rep("structured_high", n_runs),
                                         rep("structured_v_high", n_runs),
                                         rep("joint_v_low", n_runs),
                                         rep("joint_low", n_runs),
                                         rep("joint_mid", n_runs),
                                         rep("joint_high", n_runs),
                                         rep("joint_v_high", n_runs),
                                         rep("jointcov_v_low", n_runs),
                                         rep("jointcov_low", n_runs),
                                         rep("jointcov_mid", n_runs),
                                         rep("jointcov_high", n_runs),
                                         rep("jointcov_v_high", n_runs),
                                         rep("joint2_v_low", n_runs),
                                         rep("joint2_low", n_runs),
                                         rep("joint2_mid", n_runs),
                                         rep("joint2_high", n_runs),
                                         rep("joint2_v_high", n_runs)),
                               group2 = c(rep("Unstructured \nonly", n_runs), 
                                          rep("Structured \nonly", n_runs*5),
                                          rep("Joint", n_runs*5),
                                          rep("Joint with \nbias \ncovariate", n_runs*5),
                                          rep("Joint with \nsecond \nspatial field", n_runs*5)),
                               line = 0.3)
# change levels of factor
environment_data$group <- factor(environment_data$group, level = c("unstructured",
                                                                   "structured_v_low",
                                                                   "structured_low", "structured_mid", 
                                                                   "structured_high", "structured_v_high",
                                                                   "joint_v_low",
                                                                   "joint_low", "joint_mid", 
                                                                   "joint_high", "joint_v_high",
                                                                   "jointcov_v_low",
                                                                   "jointcov_low", "jointcov_mid", 
                                                                   "jointcov_high", "jointcov_v_high",
                                                                   "joint2_v_low",
                                                                   "joint2_low", "joint2_mid", 
                                                                   "joint2_high", "joint2_v_high"))
environment_data$group2 <- factor(environment_data$group2, level = c("Unstructured \nonly", 
                                                                     "Structured \nonly", 
                                                                     "Joint",
                                                                     "Joint with \nbias \ncovariate",
                                                                     "Joint with \nsecond \nspatial field"))

Environment <- ggplot(environment_data, aes(group, environment))+
  scale_fill_manual(values=manual_colours, name = "",
                    labels = c("Unstructured only", "Structured only", "Joint",
                               "Joint with \nbias covariate", "Joint with \nsecond spatial field"))+
  geom_violin(aes(fill=as.factor(group2)), trim=FALSE)+
  ylim(c(-10,50))+
  geom_boxplot(width=0.1)+
  theme_classic()+
  theme(legend.position = "none")+
  xlab("Structured sample size")+
  ylab("Mean estimate of environment coefficient")+
  facet_wrap(~as.factor(group2), nrow=1, scales="free_x")+
  scale_x_discrete(labels=c("unstructured" = "", "structured_v_low" = "26",
                            "structured_low" = "50", "structured_mid" = "100", 
                            "structured_high" = "150", "structured_v_high" = "500",
                            "joint_v_low" = "26",
                            "joint_low" = "50", "joint_mid" = "100", 
                            "joint_high" = "150", "joint_v_high" = "500",
                            "jointcov_v_low" = "26",
                            "jointcov_low" = "50", "jointcov_mid" = "100", 
                            "jointcov_high" = "150", "jointcov_v_high" = "500",
                            "joint2_v_low" = "26",
                            "joint2_low" = "50", "joint2_mid" = "100", 
                            "joint2_high" = "150", "joint2_v_high" = "500"))+
  geom_hline(aes(yintercept = line), color = "red")

Environment

ggsave(filename = "EnvironmentPlot_samplesize.png", plot=last_plot(),
       width = 20, height = 10, units="cm", dpi=300)
