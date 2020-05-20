##plots

sim_res <- read.csv("sim_results_collated_500.csv")

env_res <- read.csv("env_results_collated_500.csv")

#remove extreme MAE values
sim_res <- sim_res[sim_res$MAE < 7.5,]

library(ggplot2)

##bias cov present, MAE

sub1 <- sim_res[sim_res$Model %in% c("structured", "unstructuredcov", "jointcov", "covariatebias", "correlationbias_str") & sim_res$Scenario != "Large",]

p1 <- ggplot(sub1, aes(y = MAE, x = Model), group = Scenario) +
  geom_boxplot(aes(fill = Scenario), outlier.shape = NA) +
  #facet_wrap(. ~ section, scales = "free") +
  labs(fill = "Scenario", y = "MAE \n") +
  scale_x_discrete(labels=c("correlationbias_str" = "Correlation", "covariatebias" = "Covariate","jointcov" = "Joint likelihood", "structured" = "Structured only", "unstructuredcov" = "Unstructured only"))+
  ylim(0,6) +
  theme_classic()+
  theme(legend.position = "none") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(size = 10),
        legend.text = element_text(size = 10),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 10),
        axis.title.x = element_blank())


##bias cov absent, MAE

sub2 <- sim_res[sim_res$Model %in% c("structured", "unstructured", "joint", "covariate", "correlation_str")& sim_res$Scenario != "Large",]

p2 <- ggplot(sub2, aes(y = MAE, x = Model), group = Scenario) +
  geom_boxplot(aes(fill = Scenario), outlier.shape = NA) +
  #facet_wrap(. ~ section, scales = "free") +
  labs(fill = "Scenario", y = "MAE\n") +
  scale_x_discrete(labels=c("correlation_str" = "Correlation", "covariate" = "Covariate","joint" = "Joint likelihood", "structured" = "Structured only", "unstructured" = "Unstructured only"))+
  ylim(0,6) +
  theme_classic()+ 
  theme(legend.position = "top") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(size = 10),
        legend.text = element_text(size = 10),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 10),
        axis.title.x = element_blank())

##bias cov present, corr

p3 <- ggplot(sub1, aes(y = correlation, x = Model), group = Scenario) +
  geom_boxplot(aes(fill = Scenario), outlier.shape = NA) +
  #facet_wrap(. ~ section, scales = "free") +
  labs(fill = "Scenario", y = "Correlation") +
  scale_x_discrete(labels=c("correlationbias_str" = "Correlation", "covariatebias" = "Covariate","jointcov" = "Joint likelihood", "structured" = "Structured only", "unstructuredcov" = "Unstructured only"))+
  theme_classic()+
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(size = 10, angle = 45, vjust = 1, hjust = 1),
        axis.text.y = element_text(size = 10),
        legend.text = element_text(size = 10),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 10))


##bias cov absent, corr

p4 <- ggplot(sub2, aes(y = correlation, x = Model), group = Scenario) +
  geom_boxplot(aes(fill = Scenario), outlier.shape = NA) +
  #facet_wrap(. ~ section, scales = "free") +
  labs(fill = "Scenario", y = "Correlation") +
  scale_x_discrete(labels=c("correlation_str" = "Correlation", "covariate" = "Covariate","joint" = "Joint likelihood", "structured" = "Structured only", "unstructured" = "Unstructured only"))+
  theme_classic()+
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(size = 10, angle = 45, vjust = 1, hjust = 1),
        axis.text.y = element_text(size = 10),
        legend.text = element_text(size = 10),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 10))
  



get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}


leg <- get_legend(p2)

p2 <- p2 + theme(legend.position = "none")

library("gridExtra")
#p5 <- grid.arrange(p1,p2,p3,p4, ncol = 2, nrow = 2)
p5 <- grid.arrange(p1, p2, p3, p4, leg, ncol=2, nrow = 3, 
             layout_matrix = rbind(c(1,2),c(3,4), c(5,5)),
             widths = c(2.7, 2.7), heights = c(2, 3, 0.2))

ggsave("Figure 2.png", p5, device = "png", width = 15, height = 15, units = "cm")

ggplot(sim_res, aes(y = MAE, x = Model), group = Scenario) +
  geom_boxplot(aes(fill = Scenario), outlier.shape = NA) +
  #facet_wrap(. ~ section, scales = "free") +
  labs(fill = "Scenario", y = "MAE") +
  theme(legend. position = "none") + 
  theme(axis.text.x = element_text(size = 10, angle = 45, vjust = 1, hjust = 1),
        axis.text.y = element_text(size = 10),
        legend.text = element_text(size = 10),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 10))
