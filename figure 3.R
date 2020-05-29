sim_res <- read.csv("sim_results_newscenario.csv")

env_res <- read.csv("sim_fixedeff_newscenario.csv")
env_res <- env_res[grep("env", env_res$X),]

sim_res$section <- factor(sim_res$section, levels = c("left", "midleft", "right", "midright", "bottom", "midbottom", "top", "midtop", "whole"))

env_res$section <- factor(env_res$section, levels = c("left", "midleft", "right", "midright", "bottom", "midbottom", "top", "midtop", "whole"))

##bias cov present, MAE

sub1 <- sim_res[sim_res$Model %in% c("structured", "unstructuredcov", "jointcov", "covariatebias", "correlationbias_str") & sim_res$section %in% c("left", "right","top", "bottom", "whole"),]

p1 <- ggplot(sub1, aes(y = MAE, x = Model), group = section) +
  geom_boxplot(aes(fill = section), outlier.shape = NA) +
  #facet_wrap(. ~ section, scales = "free") +
  labs(fill = "Section", y = "MAE \n", tag = "a)") +
  scale_x_discrete(labels=c("correlationbias_str" = "Correlation", "covariatebias" = "Covariate","jointcov" = "Joint likelihood", "structured" = "Structured only", "unstructuredcov" = "Unstructured only"))+
  scale_fill_brewer(palette="Paired")+
  theme_classic()+
  ylim(0,11)+
  theme(legend.position = "bottom") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(size = 10),
        legend.text = element_text(size = 10),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 10),
        axis.title.x = element_blank(),
        plot.tag = element_text(size = 10))


p2 <- ggplot(sub1, aes(y = correlation, x = Model), group = section) +
  geom_boxplot(aes(fill = section), outlier.shape = NA) +
  #facet_wrap(. ~ section, scales = "free") +
  labs(fill = "Section", y = "Correlation", tag = "c)") +
  scale_x_discrete(labels=c("correlationbias_str" = "Correlation", "covariatebias" = "Covariate","jointcov" = "Joint likelihood", "structured" = "Structured only", "unstructuredcov" = "Unstructured only"))+
  scale_fill_brewer(palette="Paired")+
  theme_classic()+
  theme(legend.position = "none") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(size = 10),
        legend.text = element_text(size = 10),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 10),
        axis.title.x = element_blank(),
        plot.tag = element_text(size = 10))


##bias covariate absent 

sub2 <- sim_res[sim_res$Model %in% c("structured", "unstructured", "joint", "covariate", "correlation_str") & sim_res$section %in% c("left", "right","top", "bottom", "whole"),]

p3 <- ggplot(sub2, aes(y = MAE, x = Model), group = section) +
  geom_boxplot(aes(fill = section), outlier.shape = NA) +
  #facet_wrap(. ~ section, scales = "free") +
  labs(fill = "Section", y = "MAE \n", tag = "b)") +
  scale_x_discrete(labels=c("correlation_str" = "Correlation", "covariate" = "Covariate","joint" = "Joint likelihood", "structured" = "Structured only", "unstructured" = "Unstructured only"))+
  scale_fill_brewer(palette="Paired")+
  theme_classic()+
  ylim(0,11)+
  theme(legend.position = "none") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(size = 10),
        legend.text = element_text(size = 10),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 10),
        axis.title.x = element_blank(),
        plot.tag = element_text(size = 10))


p4 <- ggplot(sub2, aes(y = correlation, x = Model), group = section) +
  geom_boxplot(aes(fill = section), outlier.shape = NA) +
  #facet_wrap(. ~ section, scales = "free") +
  labs(fill = "Section", y = "Correlation", tag = "d)") +
  scale_x_discrete(labels=c("correlation_str" = "Correlation", "covariate" = "Covariate","joint" = "Joint likelihood", "structured" = "Structured only", "unstructured" = "Unstructured only"))+
  scale_fill_brewer(palette="Paired")+
  theme_classic()+
  theme(legend.position = "none") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(size = 10),
        legend.text = element_text(size = 10),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 10),
        axis.title.x = element_blank(),
        plot.tag = element_text(size = 10))


##bias cov present, env

sub1e <- env_res[env_res$model %in% c("structured", "unstr_bias", "joint_bias", "covariate_bias", "correlation_bias") & env_res$section %in% c("left", "right", "bottom", "top", "whole"),]

p5 <- ggplot(sub1e, aes(y = mean, x = model), group = section) +
  geom_boxplot(aes(fill = section), outlier.shape = NA) +
  #facet_wrap(. ~ section, scales = "free") +
  labs(fill = "Section", y = "Coefficient estimate", x = "Model", tag = "e)") +
  scale_x_discrete(labels=c("correlation_bias" = "Correlation", "covariate_bias" = "Covariate","joint_bias" = "Joint likelihood", "structured" = "PA only", "unstr_bias" = "PO only"))+
  theme_classic()+
  scale_fill_brewer(palette = "Paired")+
  ylim(-25,25)+
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(size = 10, angle = 45, vjust = 1, hjust = 1),
        axis.text.y = element_text(size = 10),
        legend.text = element_text(size = 10),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 10),
        plot.tag = element_text(size = 10))+
  geom_hline(yintercept = 1.2, linetype = "dashed")

##bias cov absent, env

sub2e <- env_res[env_res$model %in% c("structured", "unstructured", "joint", "covariate", "correlation")& env_res$section %in% c("left", "right", "bottom", "top", "whole"),]

p6 <- ggplot(sub2e, aes(y = mean, x = model), group = section) +
  geom_boxplot(aes(fill = section), outlier.shape = NA) +
  #facet_wrap(. ~ section, scales = "free") +
  labs(fill = "Section", y = "Coefficient estimate", x = "Model", tag = "f)") +
  scale_x_discrete(labels=c("correlation" = "Correlation", "covariate" = "Covariate","joint" = "Joint likelihood", "structured" = "PA only", "unstructured" = "PO only"))+
  scale_fill_brewer(palette = "Paired")+
  ylim(-25,25)+
  theme_classic()+
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(size = 10, angle = 45, vjust = 1, hjust = 1),
        axis.text.y = element_text(size = 10),
        legend.text = element_text(size = 10),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 10),
        plot.tag = element_text(size = 10))+
  geom_hline(yintercept = 1.2, linetype = "dashed")



get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}


leg <- get_legend(p1)

p1 <- p1 + theme(legend.position = "none")

library("gridExtra")

p7 <- grid.arrange(p1, p3, p2, p4, p5, p6, leg, ncol=2, nrow = 4, 
                   layout_matrix = rbind(c(1,2),c(3,4), c(5,6), c(7,7)),
                   widths = c(2.7, 2.7), heights = c(2, 2, 3.3, 0.2))

ggsave("Figure 3.png", p7, device = "png", width = 15, height = 18.5, units = "cm")


