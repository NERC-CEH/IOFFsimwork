#' # Evaluation of the scenarios
#' 
#+ warning = FALSE, message = FALSE, error = FALSE, include = TRUE, echo = FALSE
# Packages
library(RColorBrewer)
library(ggplot2)
library(plyr)
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
#files <- files[-c(20:29)]

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
# need to remove the model name - can be tricky as different lengths
scenario_names <- unlist(row.names(summary_scenario_sample_size))
# model names need to be in set order so remove completely
model_names = c("unstructuredcov", "unstructured", "structured", "jointtwo", "jointcov", "joint")

# easiest in loop
for(i in 1:length(model_names)){
scenario_names <- str_replace(scenario_names, model_names[i], "")
}

summary_scenario_sample_size$Scenario <- as.numeric(scenario_names)
  
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

plotting_data <- summary_plot_function(raw_scenario_sample_size, scenario = "Sample_size_", n_runs, type="summary")
# relevel model column
plotting_data$model <- factor(plotting_data$model, level = c("unstructured",
                                                             "unstructuredcov",
                                                             "structured", 
                                                             "joint",
                                                             "jointcov", "jointtwo"))
plotting_data$model <- revalue(plotting_data$model, c("unstructured" = "Unstructured only",
                               "unstructuredcov" = "Unstructured with \nbias \ncovariate",
                               "structured" = "Structured only", 
                               "joint" = "Joint model",
                               "jointcov" = "Joint with \nbias \ncovariate", 
                               "jointtwo" = "Joint with \nsecond spatial \nfield"))
plotting_data$scenario <- as.numeric(plotting_data$scenario)

# now plot
# set manual colours
manual_colours <- c("blue", "darkblue", "orange", "grey30", "grey50", "grey80")


Correlation <- ggplot(plotting_data, aes(as.factor(scenario), correlation))+
  scale_fill_manual(values=manual_colours, name = "",
                    labels = c("Unstructured only", "Unstructured with \nbias \ncovariate",
                               "Structured only", "Joint",
                               "Joint with \nbias \ncovariate", "Joint with \nsecond spatial field"))+
  #geom_violin(aes(fill=as.factor(model)), trim=TRUE)+
  geom_boxplot(aes(fill=as.factor(model)), outlier.colour ='grey', outlier.size = 0.5)+
  theme_classic()+
  theme(legend.position = "none")+
  xlab("Structured sample size")+
  ylab("Correlation between prediction and truth")+
  facet_wrap(~as.factor(model), nrow=1, scales="free_x")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

Correlation

ggsave(filename = "CorrelationPlot_samplesize.png", plot=last_plot(),
       width = 20, height = 10, units="cm", dpi=300)

Environment <- ggplot(plotting_data, aes(as.factor(scenario), env))+
  scale_fill_manual(values=manual_colours, name = "",
                    labels = c("Unstructured only", "Unstructured with \nbias \ncovariate",
                               "Structured only", "Joint",
                               "Joint with \nbias \ncovariate", "Joint with \nsecond spatial field"))+
  #geom_violin(aes(fill=as.factor(model)), trim=TRUE)+
  geom_boxplot(aes(fill=as.factor(model)), outlier.colour ='grey', outlier.size = 0.5)+
  geom_hline(aes(yintercept = 1.2), linetype="dashed", color = "red")+
  theme_classic()+
  theme(legend.position = "none")+
  xlab("Structured sample size")+
  ylab("Environmental covariate estimate")+
  ylim(-5,5)+
  facet_wrap(~as.factor(model), nrow=1, scales="free_x")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

Environment

ggsave(filename = "EnvironmentPlot_samplesize.png", plot=last_plot(),
       width = 20, height = 10, units="cm", dpi=300)

# add an extra plot of width of credible intervals

Environment_CI <- ggplot(plotting_data, aes(as.factor(scenario), width))+
  scale_fill_manual(values=manual_colours, name = "",
                    labels = c("Unstructured only", "Unstructured with \nbias \ncovariate",
                               "Structured only", "Joint",
                               "Joint with \nbias \ncovariate", "Joint with \nsecond spatial field"))+
  #geom_violin(aes(fill=as.factor(model)), trim=TRUE)+
  geom_boxplot(aes(fill=as.factor(model)), outlier.colour ='grey', outlier.size = 0.5)+
  theme_classic()+
  theme(legend.position = "none")+
  xlab("Structured sample size")+
  ylab("Environmental covariate estimate")+
  ylim(0,15)+
  facet_wrap(~as.factor(model), nrow=1, scales="free_x")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

Environment_CI

ggsave(filename = "EnvironmentPlotCI_samplesize.png", plot=last_plot(),
       width = 20, height = 10, units="cm", dpi=300)


MAE <- ggplot(plotting_data, aes(as.factor(scenario), mae))+
  scale_fill_manual(values=manual_colours, name = "",
                    labels = c("Unstructured only", "Unstructured with \nbias \ncovariate",
                               "Structured only", "Joint",
                               "Joint with \nbias \ncovariate", "Joint with \nsecond spatial field"))+
  #geom_violin(aes(fill=as.factor(model)), trim=TRUE)+
  geom_boxplot(aes(fill=as.factor(model)), outlier.colour ='grey', outlier.size = 0.5)+
  theme_classic()+
  theme(legend.position = "none")+
  xlab("Structured sample size")+
  ylab("MAE")+
  ylim(0.25,1.5)+
  facet_wrap(~as.factor(model), nrow=1, scales="free_x")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

MAE

ggsave(filename = "MAEPlot_samplesize.png", plot=last_plot(),
       width = 20, height = 10, units="cm", dpi=300)

#' ## Table of proportion of env estimate in CI
#' 
#+ warning = FALSE, message = FALSE, error = FALSE, include = TRUE, echo = FALSE
# calculate the proportion of simulations where true environmental beta
# in credibility interval

prop_env_in_CI <- summary_plot_function(raw_scenario_sample_size, scenario = "Sample_size_", n_runs, type="CI")
cbind(row.names(summary_scenario_sample_size),prop_env_in_CI)

#' ## Correlation between bias and environment scenario
#' 
#' 
#+ warning = FALSE, message = FALSE, error = FALSE, include = TRUE, echo = FALSE

# set up code and parameters for summaries
source('parallel_summary.R')
n_runs = 500
n_by = 4
n_tot = n_runs*n_by

files <- list.files(path = ".", pattern = "Correlation_")
#files <- files[-3]

# create a summary of all runs of this scenario

summary_scenario_correlation <- as.data.frame(t(mapply(summary_wrapper, files, 
                                                       MoreArgs = list( 
                                                         summary = "summary", n_tot,
                                                         n_by), SIMPLIFY = T))) # transposed to look clearer

raw_scenario_correlation <- mapply(summary_wrapper, files, 
                                   MoreArgs = list(summary = "raw", n_tot,
                                                   n_by), SIMPLIFY = F)

# summary table 

row.names(summary_scenario_correlation) <- str_sub(row.names(summary_scenario_correlation), 13, -11)

# add new column of the number of samples
# need to remove the model name - can be tricky as different lengths
scenario_names <- unlist(row.names(summary_scenario_correlation))
# model names need to be in set order so remove completely
model_names = c("unstructuredcov", "unstructured", "structured", "jointtwo", "jointcov", "joint")

# do not need a scenario here as all TRUE just need one FALSE to compare
# take sample size = 150 scenario

summary_scenario_correlation <- rbind(summary_scenario_correlation,
                                      summary_scenario_sample_size[which(summary_scenario_sample_size$Scenario == 150),
                                                                   1:7])
summary_scenario_correlation$Scenario <- c(rep("TRUE", 6), rep("FALSE", 6))

summary_scenario_correlation[,1:7] <- unlist(summary_scenario_correlation[,1:7]) # need to unlist to save

write.csv(summary_scenario_correlation, "SummaryTable_correlation.csv", row.names=T)

#' ### Table
#' 
#+ warning = FALSE, message = FALSE, error = FALSE, include = TRUE, echo = FALSE
summary_scenario_correlation

#' 
#' ### Figures
#+ warning = FALSE, message = FALSE, error = FALSE, include = TRUE, echo = FALSE
# join all of the correlation estimates into a dataframe so can use ggplot
# do this from the raw data

# need to add the sample size n = 150 raw results to this

raw_scenario_correlation <- c(raw_scenario_correlation, raw_scenario_sample_size[c(2,11,21,30,31,41,42)])

plotting_data <- summary_plot_function(raw_scenario_correlation, scenario = "Correlation_", 
                                       n_runs, type="summary")
# relevel model column
plotting_data$model <- factor(plotting_data$model, level = c("unstructured",
                                                             "unstructuredcov",
                                                             "structured", 
                                                             "joint",
                                                             "jointcov", "jointtwo"))

# now plot
# set manual colours
manual_colours <- c("blue", "darkblue", "orange", "grey30", "grey50", "grey80")

Correlation <- ggplot(plotting_data, aes(as.factor(scenario), correlation))+
  scale_fill_manual(values=manual_colours, name = "",
                    labels = c("Unstructured only", "Unstructured with \nbias \ncovariate",
                               "Structured only", "Joint",
                               "Joint with \nbias \ncovariate", "Joint with \nsecond spatial field"))+
  geom_violin(aes(fill=as.factor(model)), trim=FALSE)+
  geom_boxplot(width=0.1)+
  theme_classic()+
  theme(legend.position = "none")+
  xlab("Correlation between environment and bias")+
  ylab("Correlation between prediction and truth")+
  facet_wrap(~as.factor(model), nrow=1, scales="free_x")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

Correlation

ggsave(filename = "CorrelationPlot_correlation.png", plot=last_plot(),
       width = 20, height = 10, units="cm", dpi=300)


Environment <- ggplot(plotting_data, aes(as.factor(scenario), env))+
  scale_fill_manual(values=manual_colours, name = "",
                    labels = c("Unstructured only", "Unstructured with \nbias \ncovariate",
                               "Structured only", "Joint",
                               "Joint with \nbias \ncovariate", "Joint with \nsecond spatial field"))+
  #geom_violin(aes(fill=as.factor(model)), trim=TRUE)+
  geom_boxplot(aes(fill=as.factor(model)), outlier.colour ='grey', outlier.size = 0.5)+
  theme_classic()+
  theme(legend.position = "none")+
  xlab("Correlation between environment and bias")+
  ylab("Environmental covariate estimate")+
  ylim(-10,15)+
  facet_wrap(~as.factor(model), nrow=1, scales="free_x")+
  geom_hline(aes(yintercept = 1.2), linetype="dashed", color = "red")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

Environment

ggsave(filename = "EnvironmentPlot_correlation.png", plot=last_plot(),
       width = 20, height = 10, units="cm", dpi=300)

Environment_CI <- ggplot(plotting_data, aes(as.factor(scenario), width))+
  scale_fill_manual(values=manual_colours, name = "",
                    labels = c("Unstructured only", "Unstructured with \nbias \ncovariate",
                               "Structured only", "Joint",
                               "Joint with \nbias \ncovariate", "Joint with \nsecond spatial field"))+
  #geom_violin(aes(fill=as.factor(model)), trim=TRUE)+
  geom_boxplot(aes(fill=as.factor(model)), outlier.colour ='grey', outlier.size = 0.5)+
  theme_classic()+
  theme(legend.position = "none")+
  xlab("Correlation between environment and bias")+
  ylab("Environmental covariate estimate")+
  ylim(0,20)+
  facet_wrap(~as.factor(model), nrow=1, scales="free_x")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

Environment_CI

ggsave(filename = "EnvironmentPlotCI_correlation.png", plot=last_plot(),
       width = 20, height = 10, units="cm", dpi=300)

MAE <- ggplot(plotting_data, aes(as.factor(scenario), mae))+
  scale_fill_manual(values=manual_colours, name = "",
                    labels = c("Unstructured only", "Unstructured with \nbias \ncovariate",
                               "Structured only", "Joint",
                               "Joint with \nbias \ncovariate", "Joint with \nsecond spatial field"))+
  geom_violin(aes(fill=as.factor(model)), trim=FALSE)+
  geom_boxplot(width=0.1)+
  theme_classic()+
  theme(legend.position = "none")+
  xlab("Correlation between environment and bias")+
  ylab("MAE")+
  ylim(0,2.5)+
  facet_wrap(~as.factor(model), nrow=1, scales="free_x")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

MAE

ggsave(filename = "MAEPlot_correlation.png", plot=last_plot(),
       width = 20, height = 10, units="cm", dpi=300)

#' ## Table of proportion of env estimate in CI
#' 
#+ warning = FALSE, message = FALSE, error = FALSE, include = TRUE, echo = FALSE
# calculate the proportion of simulations where true environmental beta
# in credibility interval

prop_env_in_CI <- summary_plot_function(raw_scenario_correlation, scenario = "Correlation_", n_runs, type="CI")
prop_env_in_CI

#' ## Bias scenario
#' 
#' 
#+ warning = FALSE, message = FALSE, error = FALSE, include = TRUE, echo = FALSE

# set up code and parameters for summaries
source('parallel_summary.R')
n_runs = 500
n_by = 4
n_tot = n_runs*n_by

files <- list.files(path = ".", pattern = "Bias_")
#files <- files[-c(1,2,8:21,22,23,29,30,36,37)]

# create a summary of all runs of this scenario

summary_scenario_bias <- as.data.frame(t(mapply(summary_wrapper, files, 
                                                       MoreArgs = list( 
                                                         summary = "summary", n_tot,
                                                         n_by), SIMPLIFY = T))) # transposed to look clearer

raw_scenario_bias <- mapply(summary_wrapper, files, 
                                   MoreArgs = list(summary = "raw", n_tot,
                                                   n_by), SIMPLIFY = F)

# summary table 

row.names(summary_scenario_bias) <- str_sub(row.names(summary_scenario_bias), 6, -7)

# add new column of the number of samples
# need to remove the model name - can be tricky as different lengths
scenario_names <- unlist(row.names(summary_scenario_bias))
# model names need to be in set order so remove completely
model_names = c("unstructuredcov", "unstructured", "structured", "jointtwo", "jointcov", "joint")

# easiest in loop
for(i in 1:length(model_names)){
  scenario_names <- str_replace(scenario_names, model_names[i], "")
}

summary_scenario_bias$Scenario <- as.numeric(scenario_names)

summary_scenario_bias[,1:7] <- unlist(summary_scenario_bias[,1:7]) # need to unlist to save

write.csv(summary_scenario_bias, "SummaryTable_bias.csv", row.names=T)

#' ### Table
#' 
#+ warning = FALSE, message = FALSE, error = FALSE, include = TRUE, echo = FALSE
summary_scenario_bias

#' 
#' ### Figures
#+ warning = FALSE, message = FALSE, error = FALSE, include = TRUE, echo = FALSE
# join all of the correlation estimates into a dataframe so can use ggplot
# do this from the raw data

plotting_data <- summary_plot_function(raw_scenario_bias, scenario = "Bias_", 
                                       n_runs, type="summary")
# relevel model column
plotting_data$model <- factor(plotting_data$model, level = c("unstructured",
                                                             "unstructuredcov",
                                                             "structured", 
                                                             "joint",
                                                             "jointcov", "jointtwo"))

# now plot
# set manual colours
manual_colours <- c("blue", "darkblue", "orange", "grey30", "grey50", "grey80")

Correlation <- ggplot(plotting_data, aes(as.factor(scenario), correlation))+
  scale_fill_manual(values=manual_colours, name = "",
                    labels = c("Unstructured only", "Unstructured with \nbias \ncovariate",
                               "Structured only", "Joint",
                               "Joint with \nbias \ncovariate", "Joint with \nsecond spatial field"))+
  #geom_violin(aes(fill=as.factor(model)), trim=FALSE)+
  geom_boxplot(aes(fill=as.factor(model)), outlier.colour ='grey', outlier.size = 0.5)+
  theme_classic()+
  theme(legend.position = "none")+
  xlab("Bias in unstructured data")+
  ylab("Correlation between prediction and truth")+
  facet_wrap(~as.factor(model), nrow=1, scales="free_x")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

Correlation

ggsave(filename = "CorrelationPlot_bias.png", plot=last_plot(),
       width = 20, height = 10, units="cm", dpi=300)


Environment <- ggplot(plotting_data, aes(as.factor(scenario), env))+
  scale_fill_manual(values=manual_colours, name = "",
                    labels = c("Unstructured only", "Unstructured with \nbias \ncovariate",
                               "Structured only", "Joint",
                               "Joint with \nbias \ncovariate", "Joint with \nsecond spatial field"))+
  #geom_violin(aes(fill=as.factor(model)), trim=TRUE)+
  geom_boxplot(aes(fill=as.factor(model)), outlier.colour ='grey', outlier.size = 0.5)+
  theme_classic()+
  theme(legend.position = "none")+
  xlab("Maximum detection probability in unstructured data")+
  ylab("Environmental covariate estimate")+
  geom_hline(aes(yintercept = 1.2), linetype="dashed", color = "red")+
  ylim(-5,10)+
  facet_wrap(~as.factor(model), nrow=1, scales="free_x")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

Environment

ggsave(filename = "EnvironmentPlot_bias.png", plot=last_plot(),
       width = 20, height = 10, units="cm", dpi=300)

Environment_CI <- ggplot(plotting_data, aes(as.factor(scenario), width))+
  scale_fill_manual(values=manual_colours, name = "",
                    labels = c("Unstructured only", "Unstructured with \nbias \ncovariate",
                               "Structured only", "Joint",
                               "Joint with \nbias \ncovariate", "Joint with \nsecond spatial field"))+
  #geom_violin(aes(fill=as.factor(model)), trim=TRUE)+
  geom_boxplot(aes(fill=as.factor(model)), outlier.colour ='grey', outlier.size = 0.5)+
  theme_classic()+
  theme(legend.position = "none")+
  xlab("Maximum detection probability in unstructured data")+
  ylab("Environmental covariate estimate")+
  ylim(0,15)+
  facet_wrap(~as.factor(model), nrow=1, scales="free_x")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

Environment_CI

ggsave(filename = "EnvironmentPlotCI_bias.png", plot=last_plot(),
       width = 20, height = 10, units="cm", dpi=300)


MAE <- ggplot(plotting_data, aes(as.factor(scenario), mae))+
  scale_fill_manual(values=manual_colours, name = "",
                    labels = c("Unstructured only", "Unstructured with \nbias \ncovariate",
                               "Structured only", "Joint",
                               "Joint with \nbias \ncovariate", "Joint with \nsecond spatial field"))+
  geom_violin(aes(fill=as.factor(model)), trim=FALSE)+
  geom_boxplot(width=0.1)+
  theme_classic()+
  theme(legend.position = "none")+
  xlab("Bias in unstructured data")+
  ylab("MAE")+
  ylim(0,5)+
  facet_wrap(~as.factor(model), nrow=1, scales="free_x")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

MAE

ggsave(filename = "MAEPlot_bias.png", plot=last_plot(),
       width = 20, height = 10, units="cm", dpi=300)

#' ## Table of proportion of env estimate in CI
#' 
#+ warning = FALSE, message = FALSE, error = FALSE, include = TRUE, echo = FALSE
# calculate the proportion of simulations where true environmental beta
# in credibility interval

prop_env_in_CI <- summary_plot_function(raw_scenario_bias, scenario = "Bias_", n_runs, type="CI")
prop_env_in_CI