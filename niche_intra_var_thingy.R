library(tidyverse)

library(mvtnorm)

sigma <- matrix(c(4,0,
                  0,4), ncol=2)
x <- rmvnorm(n=500, mean=c(1,2), sigma=sigma)
colMeans(x)
var(x)

x <- rmvnorm(n=1e6, mean=c(0,0), sigma=sigma, method="chol")
y <- rmvnorm(n=1e6, mean=c(0,0), sigma=sigma, method="chol")

n = 25
niches = data.frame(Species = rep(1:n, each = 1e3),
                    Trait.1 = NA,
                    Trait.2 = NA)
for (i in 1:n) {
  sigma <- matrix(c(runif(1,.1,.2),            0,
                    0           ,runif(1,.1,.2)),
                  ncol=2)
  niches[niches$Species == i,2:3] = rmvnorm(n=1e3,
                                            mean=c(runif(1,-1,1),
                                                   runif(1,-1,1)),
                                            sigma=sigma,
                                            method="chol")
}
ggplot(niches, aes(Trait.1,Trait.2, color = as.factor(Species)))+
  geom_point(alpha = .1)

distances = matrix(NA, nrow = n, ncol = n)


niches = vector(mode = "list", n)
for (i in 1:n) {
  
  sigma <- matrix(c(runif(1,1,10),             0,
                    0            ,runif(1,1,10)),
                  ncol=2)
  
  niches[[i]][[1]] = c(runif(1,-20,20),
                       runif(1,-20,20))
  niches[[i]][[2]] = sigma
}
distances = matrix(NA, nrow = n, ncol = n)
library(fpc)
for (i in 1:n) {
  for (j in 1:n) {
    distances[i,j] = 1/(1 + bhattacharyya.dist(niches[[i]][[1]],
                                               niches[[j]][[1]],
                                               niches[[i]][[2]],
                                               niches[[j]][[2]]))
  }
}

niches.r = data.frame(Species = rep(1:n, each = 1e3),
                      Trait.1 = NA,
                      Trait.2 = NA)

for (i in 1:n) {
  niches.r[niches.r$Species == i,2:3] = rmvnorm(n=1e3,
                                                mean=niches[[i]][[1]],
                                                sigma=diag(niches[[i]][[2]] - 1)*diag(2),
                                                method="chol")
}
ggplot(niches.r, aes(Trait.1,Trait.2, color = as.factor(Species)))+
  geom_point(alpha = .1)



colMeans(x)
var(x)

plot(x)
plot(y)

1/(1-dad::hellinger(x, y, check = T))















rlnormtrunc.intuitive = function(n, m, s, p=.9) {
  trnc <- EnvStats::rlnormTrunc(n,
                                meanlog = log(m^2 / sqrt(s^2 + m^2)),
                                sdlog = sqrt(log(1 + (s^2 / m^2))),
                                min = qlnorm((1-p)/2,
                                             meanlog = log(m^2 / sqrt(s^2 + m^2)),
                                             sdlog = sqrt(log(1 + (s^2 / m^2)))),
                                max = qlnorm(1-(1-p)/2,
                                             meanlog = log(m^2 / sqrt(s^2 + m^2)),
                                             sdlog = sqrt(log(1 + (s^2 / m^2)))))
  return(trnc)
}
plot(density(rlnormtrunc.intuitive(1e5, 10, 5, 1)))
