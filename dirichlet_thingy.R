library(tidyverse)
library(DirichletReg)
library(mvabund)
library(gllvm)
library(brms)

data("ArcticLake");View(ArcticLake)

comp <- mvabund(lapply(ArcticLake[,1:3]*1e3, as.integer))
depth <- as.matrix(ArcticLake$depth, DROP=F)

m=manyglm(comp~depth)
summary(m)
plot(m)
anova(m, p.uni = "adjusted")


comp <- as.matrix(ArcticLake[,1:3])
m=gllvm(ArcticLake[,1:3], depth, formula = ~ depth, 
        family =  binomial(link = "probit"),
        starting.val = 0)
summary(m)

# ArcticLake$sand = round(ArcticLake$sand,2)
# ArcticLake$silt = round(ArcticLake$silt,2)
# ArcticLake$clay = round(ArcticLake$clay,2)
# ArcticLake$sand = ArcticLake$sand + (1-rowSums(ArcticLake[,1:3]))/3
# ArcticLake$silt = ArcticLake$silt + (1-rowSums(ArcticLake[,1:3]))/3
ArcticLake$clay = ArcticLake$clay + (1-rowSums(ArcticLake[,1:3]))


ArcticLake$Y=cbind(ArcticLake$sand,
                   ArcticLake$silt,
                   ArcticLake$clay)
m = brm(Y~depth,
        data = ArcticLake,
        family = dirichlet(), cores = 4,
        backend = "cmdstanr")
summary(m)
plot(m)
plot(conditional_effects(m, categorical = T, effects = "depth"),
     plot = T, points = T)
library(report)
report(m)
check_model(m)
