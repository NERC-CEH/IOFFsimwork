#' # Evaluation of first scenario run
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
n_runs = 4
n_tot = n_runs*3
n_by = 3


load('unstructured_output_parallel.RData')

unstructured_summary_R_raw <- mapply(parallel_summary, simulation_output_unstructured[seq(1,n_tot,n_by)],
                                     MoreArgs = list(type = "single"), SIMPLIFY = T)
unstructured_summary_R <- rowMeans(mapply(parallel_summary, simulation_output_unstructured[seq(1,n_tot,n_by)], 
                                          MoreArgs = list(type = "single"), SIMPLIFY = T))
unstructured_summary_R <- data.frame(MAE = unstructured_summary_R[1],
                                     Correlation = unstructured_summary_R[2], 
                                     MeanENV = unstructured_summary_R[3], 
                                     LowerENV = unstructured_summary_R[4], 
                                     UpperENV = unstructured_summary_R[5])

#' 
#' 
#+ warning = FALSE, message = FALSE, error = FALSE, include = TRUE, echo = FALSE
#### Structured model
load('structured_output_v_high_parallel.RData')

structured_summary_R_v_high_raw <- mapply(parallel_summary, simulation_output_structured_v_high[seq(1,12,3)], 
                                        MoreArgs = list(type = "single"), SIMPLIFY = T)
structured_summary_R_v_high <- rowMeans(structured_summary_R_v_high_raw)
structured_summary_R_v_high <- data.frame(MAE = structured_summary_R_v_high[1],
                                        Correlation = structured_summary_R_v_high[2], 
                                        MeanENV = structured_summary_R_v_high[3], 
                                        LowerENV = structured_summary_R_v_high[4], 
                                        UpperENV = structured_summary_R_v_high[5])

load('structured_output_high_parallel.RData')

structured_summary_R_high_raw <- mapply(parallel_summary, simulation_output_structured_high[seq(1,n_tot,n_by)], 
                                             MoreArgs = list(type = "single"), SIMPLIFY = T)
structured_summary_R_high <- rowMeans(structured_summary_R_high_raw)
structured_summary_R_high <- data.frame(MAE = structured_summary_R_high[1],
                                     Correlation = structured_summary_R_high[2], 
                                     MeanENV = structured_summary_R_high[3], 
                                     LowerENV = structured_summary_R_high[4], 
                                     UpperENV = structured_summary_R_high[5])

load('structured_output_mid_parallel.RData')

structured_summary_R_mid_raw <- mapply(parallel_summary, simulation_output_structured_mid[seq(1,n_tot,n_by)], 
                                        MoreArgs = list(type = "single"), SIMPLIFY = T)
structured_summary_R_mid <- rowMeans(structured_summary_R_mid_raw)
structured_summary_R_mid <- data.frame(MAE = structured_summary_R_mid[1],
                                        Correlation = structured_summary_R_mid[2], 
                                        MeanENV = structured_summary_R_mid[3], 
                                        LowerENV = structured_summary_R_mid[4], 
                                        UpperENV = structured_summary_R_mid[5])

load('structured_output_low_parallel.RData')

structured_summary_R_low_raw <- mapply(parallel_summary, simulation_output_structured_low[seq(1,n_tot,n_by)], 
                                        MoreArgs = list(type = "single"), SIMPLIFY = T)
structured_summary_R_low <- rowMeans(structured_summary_R_low_raw)
structured_summary_R_low <- data.frame(MAE = structured_summary_R_low[1],
                                       Correlation = structured_summary_R_low[2], 
                                       MeanENV = structured_summary_R_low[3], 
                                       LowerENV = structured_summary_R_low[4], 
                                       UpperENV = structured_summary_R_low[5])

load('structured_output_v_low_parallel.RData')

structured_summary_R_v_low_raw <- mapply(parallel_summary, simulation_output_structured_v_low[seq(1,n_tot,n_by)], 
                                       MoreArgs = list(type = "single"), SIMPLIFY = T)
structured_summary_R_v_low <- rowMeans(structured_summary_R_v_low_raw)
structured_summary_R_v_low <- data.frame(MAE = structured_summary_R_v_low[1],
                                       Correlation = structured_summary_R_v_low[2], 
                                       MeanENV = structured_summary_R_v_low[3], 
                                       v_lowerENV = structured_summary_R_v_low[4], 
                                       UpperENV = structured_summary_R_v_low[5])

load('joint_output_v_high_parallel.RData')

joint_summary_R_v_high_raw <- mapply(parallel_summary, simulation_output_joint_v_high[seq(1,n_tot,n_by)], 
                                   MoreArgs = list(type = "joint"), SIMPLIFY = T)
joint_summary_R_v_high <- rowMeans(joint_summary_R_v_high_raw)
joint_summary_R_v_high <- data.frame(MAE = joint_summary_R_v_high[1],
                                   Correlation = joint_summary_R_v_high[2], 
                                   MeanENV = joint_summary_R_v_high[3], 
                                   v_higherENV = joint_summary_R_v_high[4], 
                                   UpperENV = joint_summary_R_v_high[5])

load('joint_output_high_parallel.RData')

joint_summary_R_high_raw <- mapply(parallel_summary, simulation_output_joint_high[seq(1,n_tot,n_by)], 
                                        MoreArgs = list(type = "joint"), SIMPLIFY = T)
joint_summary_R_high <- rowMeans(joint_summary_R_high_raw)
joint_summary_R_high <- data.frame(MAE = joint_summary_R_high[1],
                                  Correlation = joint_summary_R_high[2], 
                                  MeanENV = joint_summary_R_high[3], 
                                  higherENV = joint_summary_R_high[4], 
                                  UpperENV = joint_summary_R_high[5])


load('joint_output_mid_parallel.RData')

joint_summary_R_mid_raw <- mapply(parallel_summary, simulation_output_joint_mid[seq(1,n_tot,n_by)], 
                                   MoreArgs = list(type = "joint"), SIMPLIFY = T)
joint_summary_R_mid <- rowMeans(joint_summary_R_mid_raw)
joint_summary_R_mid <- data.frame(MAE = joint_summary_R_mid[1],
                                  Correlation = joint_summary_R_mid[2], 
                                  MeanENV = joint_summary_R_mid[3], 
                                  miderENV = joint_summary_R_mid[4], 
                                  UpperENV = joint_summary_R_mid[5])

load('joint_output_low_parallel.RData')

joint_summary_R_low_raw <- mapply(parallel_summary, simulation_output_joint_low[seq(1,n_tot,n_by)], 
                                   MoreArgs = list(type = "joint"), SIMPLIFY = T)
joint_summary_R_low <- rowMeans(joint_summary_R_low_raw)
joint_summary_R_low <- data.frame(MAE = joint_summary_R_low[1],
                                       Correlation = joint_summary_R_low[2], 
                                       MeanENV = joint_summary_R_low[3], 
                                       LowerENV = joint_summary_R_low[4], 
                                       UpperENV = joint_summary_R_low[5])

load('joint_output_v_low_parallel.RData')

joint_summary_R_v_low_raw <- mapply(parallel_summary, simulation_output_joint_v_low[seq(1,n_tot,n_by)], 
                                  MoreArgs = list(type = "joint"), SIMPLIFY = T)
joint_summary_R_v_low <- rowMeans(joint_summary_R_v_low_raw)
joint_summary_R_v_low <- data.frame(MAE = joint_summary_R_v_low[1],
                                  Correlation = joint_summary_R_v_low[2], 
                                  MeanENV = joint_summary_R_v_low[3], 
                                  v_lowerENV = joint_summary_R_v_low[4], 
                                  UpperENV = joint_summary_R_v_low[5])

load('jointcov_output_v_high_parallel.RData')

jointcov_summary_R_v_high_raw <- mapply(parallel_summary, simulation_output_jointcov_v_high[seq(1,n_tot,n_by)], 
                                      MoreArgs = list(type = "joint"), SIMPLIFY = T)
jointcov_summary_R_v_high <- rowMeans(jointcov_summary_R_v_high_raw)
jointcov_summary_R_v_high <- data.frame(MAE = jointcov_summary_R_v_high[1],
                                      Correlation = jointcov_summary_R_v_high[2], 
                                      MeanENV = jointcov_summary_R_v_high[3], 
                                      v_higherENV = jointcov_summary_R_v_high[4], 
                                      UpperENV = jointcov_summary_R_v_high[5])

load('jointcov_output_high_parallel.RData')

jointcov_summary_R_high_raw <- mapply(parallel_summary, simulation_output_jointcov_high[seq(1,n_tot,n_by)], 
                                   MoreArgs = list(type = "joint"), SIMPLIFY = T)
jointcov_summary_R_high <- rowMeans(jointcov_summary_R_high_raw)
jointcov_summary_R_high <- data.frame(MAE = jointcov_summary_R_high[1],
                                   Correlation = jointcov_summary_R_high[2], 
                                   MeanENV = jointcov_summary_R_high[3], 
                                   higherENV = jointcov_summary_R_high[4], 
                                   UpperENV = jointcov_summary_R_high[5])


load('jointcov_output_mid_parallel.RData')

jointcov_summary_R_mid_raw <- mapply(parallel_summary, simulation_output_jointcov_mid[seq(1,n_tot,n_by)], 
                                      MoreArgs = list(type = "joint"), SIMPLIFY = T)
jointcov_summary_R_mid <- rowMeans(jointcov_summary_R_mid_raw)
jointcov_summary_R_mid <- data.frame(MAE = jointcov_summary_R_mid[1],
                                  Correlation = jointcov_summary_R_mid[2], 
                                  MeanENV = jointcov_summary_R_mid[3], 
                                  miderENV = jointcov_summary_R_mid[4], 
                                  UpperENV = jointcov_summary_R_mid[5])

load('jointcov_output_low_parallel.RData')

jointcov_summary_R_low_raw <- mapply(parallel_summary, simulation_output_jointcov_low[seq(1,n_tot,n_by)], 
                                      MoreArgs = list(type = "joint"), SIMPLIFY = T)
jointcov_summary_R_low <- rowMeans(jointcov_summary_R_low_raw)
jointcov_summary_R_low <- data.frame(MAE = jointcov_summary_R_low[1],
                                  Correlation = jointcov_summary_R_low[2], 
                                  MeanENV = jointcov_summary_R_low[3], 
                                  LowerENV = jointcov_summary_R_low[4], 
                                  UpperENV = jointcov_summary_R_low[5])

load('jointcov_output_v_low_parallel.RData')

jointcov_summary_R_v_low_raw <- mapply(parallel_summary, simulation_output_jointcov_v_low[seq(1,n_tot,n_by)], 
                                     MoreArgs = list(type = "joint"), SIMPLIFY = T)
jointcov_summary_R_v_low <- rowMeans(jointcov_summary_R_v_low_raw)
jointcov_summary_R_v_low <- data.frame(MAE = jointcov_summary_R_v_low[1],
                                     Correlation = jointcov_summary_R_v_low[2], 
                                     MeanENV = jointcov_summary_R_v_low[3], 
                                     v_lowerENV = jointcov_summary_R_v_low[4], 
                                     UpperENV = jointcov_summary_R_v_low[5])

# summary table 

# make all column names the same
names(structured_summary_R_v_high) <- names(structured_summary_R_v_low) <- names(structured_summary_R_low) <- names(structured_summary_R_mid) <- names(structured_summary_R_high) <- names(unstructured_summary_R) 
names(joint_summary_R_v_high) <- names(joint_summary_R_v_low) <- names(joint_summary_R_low) <- names(joint_summary_R_mid) <- names(joint_summary_R_high) <- names(unstructured_summary_R) 
names(jointcov_summary_R_v_high) <- names(jointcov_summary_R_v_low) <- names(jointcov_summary_R_low) <- names(jointcov_summary_R_mid) <- names(jointcov_summary_R_high) <- names(unstructured_summary_R) 

summary_table <- rbind(unstructured_summary_R, structured_summary_R_low, structured_summary_R_v_low,
                       structured_summary_R_mid, structured_summary_R_high, structured_summary_R_v_high, 
                       joint_summary_R_v_low,
                       joint_summary_R_low, joint_summary_R_mid, joint_summary_R_high, joint_summary_R_v_high,
                       jointcov_summary_R_v_low,
                       jointcov_summary_R_low, jointcov_summary_R_mid, jointcov_summary_R_high, jointcov_summary_R_v_high)

row.names(summary_table) <- c("unstructured", "structured_v_low", "structured_low", "structured_mid", "structured_high", "structured_v_high",
                              "joint_v_low", "joint_low", "joint_mid", "joint_high", "joint_v_high",
                              "jointcov_low", "jointcov_v_low", "jointcov_mid", "jointcov_high", "jointcov_v_high")

summary_table

#load('joint2_output_high_parallel.RData')

#joint2_summary_R_high <- rowMeans(mapply(parallel_summary, simulation_output_joint2_high[seq(2,100,2)], MoreArgs = list(type = "single"), SIMPLIFY = T))
#joint2_summary_R_high <- data.frame(MAE = joint2_summary_R_high[1],
 #                                  Correlation = joint2_summary_R_high[2], 
  #                                 MeanENV = joint2_summary_R_high[3], 
  #                                 higherENV = joint2_summary_R_high[4], 
   #                                UpperENV = joint2_summary_R_high[5])

#joint2_summary_R_high

#load('joint2_output_mid_parallel.RData')

#joint2_summary_R_mid <- rowMeans(mapply(parallel_summary, simulation_output_joint2_mid[seq(2,100,2)], MoreArgs = list(type = "single"), SIMPLIFY = T))
#joint2_summary_R_mid <- data.frame(MAE = joint2_summary_R_mid[1],
 #                                 Correlation = joint2_summary_R_mid[2], 
  #                                MeanENV = joint2_summary_R_mid[3], 
   #                               miderENV = joint2_summary_R_mid[4], 
    #                              UpperENV = joint2_summary_R_mid[5])
#joint2_summary_R_mid

#load('joint2_output_low_parallel.RData')

#joint2_summary_R_low <- rowMeans(mapply(parallel_summary, simulation_output_joint2_low[seq(2,100,2)], MoreArgs = list(type = "single"), SIMPLIFY = T))
#joint2_summary_R_low <- data.frame(MAE = joint2_summary_R_low[1],
 #                                 Correlation = joint2_summary_R_low[2], 
  #                                MeanENV = joint2_summary_R_low[3], 
   #                               LowerENV = joint2_summary_R_low[4], 
    #                              UpperENV = joint2_summary_R_low[5])
#joint2_summary_R_low


#' ## Box plots of everything
#' 
#' ### Structured model - correlation
#+ warning = FALSE, message = FALSE, error = FALSE, include = TRUE, echo = FALSE
boxplot(unstructured_summary_R_raw[2,], ylab = "correlation", axes=F, main="Unstructured")
axis(1)
axis(2, las=1)

par(mfrow=c(1,5))
boxplot(structured_summary_R_v_low_raw[2,], ylab = "correlation", axes=F, main="v low")
axis(1)
axis(2, las=1)
boxplot(structured_summary_R_low_raw[2,], ylab = "", axes=F, main="low")
axis(1)
boxplot(structured_summary_R_mid_raw[2,], ylab = "", axes=F, main="mid")
axis(1)
boxplot(structured_summary_R_high_raw[2,], ylab = "", axes=F, main="high")
axis(1)
boxplot(structured_summary_R_v_high_raw[2,], ylab = "", axes=F, main="v high")
axis(1)
#' ### Joint model - correlation
#+ warning = FALSE, message = FALSE, error = FALSE, include = TRUE, echo = FALSE
par(mfrow=c(1,5))
boxplot(joint_summary_R_v_low_raw[2,], ylab = "correlation", axes=F, main="v low")
axis(1)
axis(2, las=1)
boxplot(joint_summary_R_low_raw[2,], ylab = "", axes=F, main="low")
axis(1)
boxplot(joint_summary_R_mid_raw[2,], ylab = "", axes=F, main="mid")
axis(1)
boxplot(joint_summary_R_high_raw[2,], ylab = "", axes=F, main="high")
axis(1)
boxplot(joint_summary_R_v_high_raw[2,], ylab = "", axes=F, main="v high")
axis(1)
#' ### Joint model + bias covariate - correlation
#+ warning = FALSE, message = FALSE, error = FALSE, include = TRUE, echo = FALSE
par(mfrow=c(1,5))
boxplot(jointcov_summary_R_v_low_raw[2,], ylab = "correlation", axes=F, main="v low")
axis(1)
axis(2, las=1)
boxplot(jointcov_summary_R_low_raw[2,], ylab = "", axes=F, main="low")
axis(1)
boxplot(jointcov_summary_R_mid_raw[2,], ylab = "", axes=F, main="mid")
axis(1)
boxplot(jointcov_summary_R_high_raw[2,], ylab = "", axes=F, main="high")
axis(1)
boxplot(jointcov_summary_R_v_high_raw[2,], ylab = "", axes=F, main="v high")
axis(1)
#' ### Structured model - environment coefficient
#+ warning = FALSE, message = FALSE, error = FALSE, include = TRUE, echo = FALSE
boxplot(unstructured_summary_R_raw[2,], ylab = "env", axes=F, main="Unstructured")
axis(1)
axis(2, las=1)

par(mfrow=c(1,5))
boxplot(structured_summary_R_v_low_raw[3,], ylab = "env", axes=F, main="v low")
axis(1)
axis(2, las=1)
boxplot(structured_summary_R_low_raw[3,], ylab = "", axes=F, main="low")
axis(1)
boxplot(structured_summary_R_mid_raw[3,], ylab = "", axes=F, main="mid")
axis(1)
boxplot(structured_summary_R_high_raw[3,], ylab = "", axes=F, main="high")
axis(1)
boxplot(structured_summary_R_v_high_raw[3,], ylab = "", axes=F, main="v high")
axis(1)
#' ### Joint model - environment coefficient
#+ warning = FALSE, message = FALSE, error = FALSE, include = TRUE, echo = FALSE
par(mfrow=c(1,5))
boxplot(joint_summary_R_v_low_raw[3,], ylab = "env", axes=F, main="v low")
axis(1)
axis(2, las=1)
boxplot(joint_summary_R_low_raw[3,], ylab = "", axes=F, main="low")
axis(1)
boxplot(joint_summary_R_mid_raw[3,], ylab = "", axes=F, main="mid")
axis(1)
boxplot(joint_summary_R_high_raw[3,], ylab = "", axes=F, main="high")
axis(1)
boxplot(joint_summary_R_v_high_raw[3,], ylab = "", axes=F, main="v high")
axis(1)
#' ### Joint model + bias covariate - environment coefficient
#+ warning = FALSE, message = FALSE, error = FALSE, include = TRUE, echo = FALSE
par(mfrow=c(1,5))
boxplot(jointcov_summary_R_v_low_raw[3,], ylab = "env", axes=F, main="v low")
axis(1)
axis(2, las=1)
boxplot(jointcov_summary_R_low_raw[3,], ylab = "", axes=F, main="low")
axis(1)
boxplot(jointcov_summary_R_mid_raw[3,], ylab = "", axes=F, main="mid")
axis(1)
boxplot(jointcov_summary_R_high_raw[3,], ylab = "", axes=F, main="high")
axis(1)
boxplot(jointcov_summary_R_v_high_raw[3,], ylab = "", axes=F, main="v high")
axis(1)

