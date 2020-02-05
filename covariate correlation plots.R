
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

#png("correlation_scenarios.png", height = 300, width = 650)
boxplot(correlation ~ Scenario*Model, data = simres, col = c("green", "orange", "purple", "pink"))


