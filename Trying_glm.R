# try glm

# take data from unstructured and add absences
# add same number as data at random locations - easiest to turn half into absence
pp3$presence[sample(1:234, 234/2, replace=F)] <- 0

Pres <- pp3$presence
X.des <- cbind(pp3$x, pp3$y, pp3$env)
colnames(X.des) <- c("x", "y", "env")

# Need to include ONLY environment in models

# infinite weight logistic
up.wt = (10^6)^(1 - Pres)
iwlr = glm(Pres ~ X.des[,3], family = binomial(), weights = up.wt)
summary(iwlr)

# downweighted poisson

p.wt = rep(1.e-6, length(Pres))
p.wt[Pres == 0] = (100*300)/sum(Pres == 0)
dwpr = glm(Pres/p.wt ~ X.des[,3], family = poisson(), weights = p.wt)
summary(dwpr)
