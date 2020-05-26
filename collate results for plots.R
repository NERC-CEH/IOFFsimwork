#compile sim results

sim_def <- read.csv("sim_results_default_rel.csv")
sim_large <- read.csv("sim_results_large_rel.csv")
sim_unbias <- read.csv("sim_results_unbiased_rel.csv")
sim_bias <- read.csv("sim_results_biased_rel.csv")


sim_def$Scenario <- "Default"
sim_large$Scenario <- "Large"
sim_unbias$Scenario <- "Unbiased"
sim_bias$Scenario <- "Biased"

sim_coll <- rbind(sim_def, sim_large, sim_unbias, sim_bias)

write.csv(sim_coll, "sim_results_collated_500_rel.csv")


##env covar

env_def <- read.csv("sim_fixed_default.csv")
env_def <- env_def[grep("env", env_def$X),]

env_large <- read.csv("sim_fixed_large.csv")
env_large <- env_large[grep("env", env_large$X),]

env_unbias <- read.csv("sim_fixed_unbiased.csv")
env_unbias <- env_unbias[grep("env", env_unbias$X),]

env_bias <- read.csv("sim_fixed_biased.csv")
env_bias <- env_bias[grep("env", env_bias$X),]

env_def$Scenario <- "Default"
env_large$Scenario <- "Large"
env_unbias$Scenario <- "Unbiased"
env_bias$Scenario <- "Biased"

env_coll <- rbind(env_def, env_large, env_unbias, env_bias)

write.csv(env_coll, "env_results_collated_500_rel.csv")



