
large <- read.csv("sim_results_large.csv")
default <- read.csv("sim_results_default.csv")
biased <- read.csv("sim_results_biased.csv")
unbiased <- read.csv("sim_results_unbiased.csv")

large$X <- "Large"
default$X <- "Default"
biased$X <- "Biased"
unbiased$X <- "Unbiased"

simres <- rbind(default, large, biased, unbiased)

#simres <- read.csv("sim_results-compiled.csv")

simres$Scenario <- factor(simres$X, levels = c("Default", "Large", "Biased", "Unbiased"))

simres <- simres[!simres$Model %in% c("correlation_uns", "correlationbias_uns"),]

simres$Model <- factor(simres$Model)


library(RColorBrewer)

corr_plot <- ggplot(simres, aes(Model, correlation)) + 
  geom_boxplot(aes(fill = Scenario), outlier.shape = NA) +
  scale_fill_brewer(palette = "Dark2")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
corr_plot

ggsave("correlation_part1.png", plot = corr_plot, device = "png", height = 100, width = 170, units = "mm")

MAE_plot <- ggplot(simres, aes(Model, MAE)) + 
  geom_boxplot(aes(fill = Scenario), outlier.shape = NA) +
  scale_fill_brewer(palette = "Dark2")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_y_continuous(limits = c(0, 6.2))
MAE_plot

ggsave("MAE_part1.png", plot = MAE_plot, device = "png", height = 100, width = 170, units = "mm")
