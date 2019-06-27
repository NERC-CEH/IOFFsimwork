#' # Evaluation of the scenarios
#' 
#+ warning = FALSE, message = FALSE, error = FALSE, include = TRUE, echo = FALSE
# Packages
library(RColorBrewer)
library(ggplot2)
#' 
#' The truth is the same within each scenario.
#' 
#' The default parameters are as follows:
#' 
#' - simulation is conducted on a grid of 300*300  
#' - environmental covariate coefficient of 1.2
#' - scale parameter kappa for matern covariance of 0.05  
#' - variance parameter sigma2x of matern covariance of 2  
#' - mean log intensity of point process of -1  
#' - 150 structured samples
#' - probability of sampling strata rep(c(0.5, 0.3, 0.1, 0.05, 0.01),5) 
#' - qsize of 1
#' 
#' 
#' ## Structured sample size scenario
#' 
#' 
#+ warning = FALSE, message = FALSE, error = FALSE, include = TRUE, echo = FALSE

# set up code and parameters for summaries
source('parallel_summary.R')
n_runs = 500
n_by = 4
n_tot = n_runs*n_by

files <- list.files(path = ".", pattern = "Sample_size")

# create a summary of all runs of this scenario

summary_scenario_sample_size <- as.data.frame(t(mapply(summary_wrapper, files, 
                                       MoreArgs = list( 
                                       summary = "summary", n_tot,
                                       n_by), SIMPLIFY = T))) # transposed to look clearer

raw_scenario_sample_size <- mapply(summary_wrapper, files, 
                                   MoreArgs = list(summary = "raw", n_tot,
                                   n_by), SIMPLIFY = F)
                                
# summary table 

row.names(summary_scenario_sample_size) <- str_sub(row.names(summary_scenario_sample_size), 13, -7)

# add new column of the number of samples
summary_scenario_sample_size$Scenario <- unlist(as.numeric(str_sub(row.names(summary_scenario_sample_size), 11)))
summary_scenario_sample_size[,1:7] <- unlist(summary_scenario_sample_size[,1:7]) # need to unlist to save

write.csv(summary_scenario_sample_size, "SummaryTable_samplesize.csv", row.names=T)

#' ### Table
#' 
#+ warning = FALSE, message = FALSE, error = FALSE, include = TRUE, echo = FALSE
summary_scenario_sample_size

#' 
#' ### Figures
#+ warning = FALSE, message = FALSE, error = FALSE, include = TRUE, echo = FALSE
# join all of the correlation estimates into a dataframe so can use ggplot
# do this from the raw data

plotting_data <- summary_plot_function(raw_scenario_sample_size, scenario = "Sample_size_")
# relevel model column
plotting_data$model <- factor(plotting_data$model, level = c("unstructured",
                                                             "unstructuredcov",
                                                             "structured", 
                                                             "joint",
                                                             "jointcov", "joint2"))

# now plot
# set manual colours
manual_colours <- brewer.pal(5, "Paired")


Correlation <- ggplot(plotting_data, aes(model, correlation))+
  scale_fill_manual(values=manual_colours, name = "",
                    labels = c("Unstructured only", "Unstructured with \nbias \ncovariate",
                               "Structured only", "Joint",
                               "Joint with \nbias \ncovariate", "Joint with \nsecond spatial field"))+
  geom_violin(aes(fill=as.factor(model)), trim=FALSE)+
  geom_boxplot(width=0.1)+
  theme_classic()+
  theme(legend.position = "none")+
  xlab("Structured sample size")+
  ylab("Correlation between prediction and truth")+
  facet_wrap(~as.factor(scenario), nrow=1, scales="free_x")

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
