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

files <- list.files(path = ".", pattern = "Rho")
files <- files[-c(1:2)]

# create a summary of all runs of this scenario

summary_scenario_rho <- as.data.frame(t(mapply(summary_wrapper, files, 
                                                       MoreArgs = list( 
                                                         summary = "summary", n_tot,
                                                         n_by), SIMPLIFY = T))) # transposed to look clearer

raw_scenario_rho <- mapply(summary_wrapper, files, 
                                   MoreArgs = list(summary = "raw", n_tot,
                                                   n_by), SIMPLIFY = F)

# summary table 

row.names(summary_scenario_rho) <- str_sub(row.names(summary_scenario_rho), 5, -7)

# add new column of the number of samples
# need to remove the model name - can be tricky as different lengths
scenario_names <- unlist(row.names(summary_scenario_rho))
# model names need to be in set order so remove completely
model_names = c("unstructuredcov", "unstructured", "structured", "jointtwo", "jointcov", "joint")

# easiest in loop
for(i in 1:length(model_names)){
  scenario_names <- str_replace(scenario_names, model_names[i], "")
}

summary_scenario_rho$Scenario <- as.numeric(scenario_names)

summary_scenario_rho[,1:9] <- unlist(summary_scenario_rho[,1:9]) # need to unlist to save

write.csv(summary_scenario_rho, "SummaryTable_rho.csv", row.names=T)

#' ### Table
#' 
#+ warning = FALSE, message = FALSE, error = FALSE, include = TRUE, echo = FALSE
summary_scenario_rho

#' 
#' ### Figures
#+ warning = FALSE, message = FALSE, error = FALSE, include = TRUE, echo = FALSE
# join all of the correlation estimates into a dataframe so can use ggplot
# do this from the raw data

plotting_data <- summary_plot_function(raw_scenario_rho, scenario = "rho_", n_runs, type="summary")
# relevel model column
plotting_data$model <- factor(plotting_data$model, level = c("structured",
                                                             "unstructured",
                                                             "joint",
                                                             "unstructuredcov",
                                                             "jointcov", "jointtwo"))
plotting_data$model <- revalue(plotting_data$model, c("unstructured" = "PO only (B)",
                                                      "unstructuredcov" = "PO with \nbias \ncovariate (D)",
                                                      "structured" = "PA only (A)", 
                                                      "joint" = "IDM (C)",
                                                      "jointcov" = "IDM with \nbias \ncovariate (E)", 
                                                      "jointtwo" = "IDM with \nsecond spatial \nfield (F)"))
plotting_data$scenario <- as.numeric(plotting_data$scenario)
plotting_data$scenario[which(plotting_data$scenario==9)] <- plotting_data$scenario[which(plotting_data$scenario==9)]/10 
plotting_data$scenario[which(plotting_data$scenario==95)] <- plotting_data$scenario[which(plotting_data$scenario==95)]/100 
plotting_data$scenario[which(plotting_data$scenario==99)] <- plotting_data$scenario[which(plotting_data$scenario==99)]/100 

# now plot
# set manual colours
manual_colours <- c("darkblue", "grey50")

# Plot at least 95% of the estimates for each scenario
y_correlation <- round(y_limits(plotting_data, "correlation"),2)

Correlation <- ggplot(plotting_data, aes(as.factor(scenario), correlation))+
  scale_fill_manual(values=manual_colours, name = "",
                    labels = c("Structured only", 
                               "Unstructured only", 
                               "Joint",
                               "Unstructured with \nbias \ncovariate",
                               "Joint with \nbias \ncovariate", 
                               "Joint with \nsecond spatial field"))+
  geom_boxplot(aes(fill=as.factor(model)), outlier.shape=NA)+
  theme_classic()+
  theme(legend.position = "none")+
  xlab("Rho (correlation between actual bias and covariate describing bias)")+
  ylab("Correlation between prediction and truth")+
  facet_wrap(~as.factor(model), nrow=1, scales="free_x")+
  theme(axis.text.x = element_text(angle = 0, hjust = 1))+
  ylim(c(y_correlation))

Correlation

ggsave(filename = "CorrelationPlot_rho.png", plot=last_plot(),
       width = 20, height = 10, units="cm", dpi=300)

y_env <- round(y_limits(plotting_data, "env"),2)

Environment <- ggplot(plotting_data, aes(as.factor(scenario), env))+
  scale_fill_manual(values=manual_colours, name = "",
                    labels = c("Structured only", 
                               "Unstructured only", 
                               "Joint",
                               "Unstructured with \nbias \ncovariate",
                               "Joint with \nbias \ncovariate", 
                               "Joint with \nsecond spatial field"))+
  geom_boxplot(aes(fill=as.factor(model)), outlier.shape=NA)+
  geom_hline(aes(yintercept = 2), linetype="dashed", color = "red")+
  theme_classic()+
  theme(legend.position = "none")+
  xlab("Rho (correlation between actual bias and covariate describing bias)")+
  ylab("Environmental covariate estimate")+
  ylim(c(0,6))+
  facet_wrap(~as.factor(model), nrow=1, scales="free_x")+
  theme(axis.text.x = element_text(angle = 0, hjust = 1))

Environment

ggsave(filename = "EnvironmentPlot_rho.png", plot=last_plot(),
       width = 20, height = 10, units="cm", dpi=300)

# add an extra plot of width of credible intervals
y_width <- round(y_limits(plotting_data, "width"),2)

Environment_CI <- ggplot(plotting_data, aes(as.factor(scenario), width))+
  scale_fill_manual(values=manual_colours, name = "",
                    labels = c("Structured only", 
                               "Unstructured only", 
                               "Joint",
                               "Unstructured with \nbias \ncovariate",
                               "Joint with \nbias \ncovariate", 
                               "Joint with \nsecond spatial field"))+
  geom_boxplot(aes(fill=as.factor(model)), outlier.shape=NA)+
  theme_classic()+
  theme(legend.position = "none")+
  xlab("Rho (correlation between actual bias and covariate describing bias)")+
  ylab("Width of credible interval for environmental covariate")+
  ylim(c(0,35))+
  facet_wrap(~as.factor(model), nrow=1, scales="free_x")+
  theme(axis.text.x = element_text(angle = 0, hjust = 1))

Environment_CI

ggsave(filename = "EnvironmentPlotCI_Rho.png", plot=last_plot(),
       width = 20, height = 10, units="cm", dpi=300)

y_mae <- round(y_limits(plotting_data, "mae"),2)

MAE <- ggplot(plotting_data, aes(as.factor(scenario), mae))+
  scale_fill_manual(values=manual_colours, name = "",
                    labels = c("Structured only", 
                               "Unstructured only", 
                               "Joint",
                               "Unstructured with \nbias \ncovariate",
                               "Joint with \nbias \ncovariate", 
                               "Joint with \nsecond spatial field"))+
  geom_boxplot(aes(fill=as.factor(model)), outlier.shape=NA)+
  theme_classic()+
  theme(legend.position = "none")+
  xlab("Rho (correlation between actual bias and covariate describing bias)")+
  ylab("MAE")+
  ylim(c(0,1.5))+
  facet_wrap(~as.factor(model), nrow=1, scales="free_x")+
  theme(axis.text.x = element_text(angle = 0, hjust = 1))

MAE

ggsave(filename = "MAEPlot_Rho.png", plot=last_plot(),
       width = 20, height = 10, units="cm", dpi=300)

#' ## Table of proportion of env estimate in CI
#' 
#+ warning = FALSE, message = FALSE, error = FALSE, include = TRUE, echo = FALSE
# calculate the proportion of simulations where true environmental beta
# in credibility interval

prop_env_in_CI <- summary_plot_function(raw_scenario_rho, scenario = "rho_", n_runs, type="CI")
cbind(row.names(summary_scenario_rho),prop_env_in_CI)
