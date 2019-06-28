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

plotting_data <- summary_plot_function(raw_scenario_sample_size, scenario = "Sample_size_", n_runs, type="summary")
# relevel model column
plotting_data$model <- factor(plotting_data$model, level = c("unstructured",
                                                             "unstructuredcov",
                                                             "structured", 
                                                             "joint",
                                                             "jointcov", "joint2"))
plotting_data$scenario <- as.numeric(plotting_data$scenario)

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


Environment <- ggplot(plotting_data, aes(model, env))+
  scale_fill_manual(values=manual_colours, name = "",
                    labels = c("Unstructured only", "Unstructured with \nbias \ncovariate",
                               "Structured only", "Joint",
                               "Joint with \nbias \ncovariate", "Joint with \nsecond spatial field"))+
  geom_violin(aes(fill=as.factor(model)), trim=FALSE)+
  geom_boxplot(width=0.1)+
  theme_classic()+
  theme(legend.position = "none")+
  xlab("Structured sample size")+
  ylab("Environmental covariate estimate")+
  facet_wrap(~as.factor(scenario), nrow=1, scales="free_x")

Environment

ggsave(filename = "EnvironmentPlot_samplesize.png", plot=last_plot(),
       width = 20, height = 10, units="cm", dpi=300)

#' ## Table of proportion of env estimate in CI
#' 
#+ warning = FALSE, message = FALSE, error = FALSE, include = TRUE, echo = FALSE
# calculate the proportion of simulations where true environmental beta
# in credibility interval

prop_env_in_CI <- summary_plot_function(raw_scenario_sample_size, scenario = "Sample_size_", n_runs, type="CI")
prop_env_in_CI
