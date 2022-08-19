############################### random walk ####################################

# https://mikegruz.github.io/articles/2016-12/random-walks

require(ggplot2)

set.seed(123) # for reproducibility
n = 500 # length of random walk

rand.walk = data.frame(
  value = cumsum(sample(c(-1,1), size=n, replace=TRUE)),
  n = 1:n
)

ggplot(rand.walk, aes(x=n, y=value)) + geom_line() + theme_minimal()




n = 10000

d2.rand.walk = data.frame(
  x = cumsum(sample(c(-1,1), size=n, replace=TRUE)),
  y = cumsum(sample(c(-1,1), size=n, replace=TRUE))
)


ggplot(d2.rand.walk, aes(x=x, y=y)) + geom_path() + theme_minimal()


############################### other random walk ##############################

# https://fabiandablander.com/r/Spurious-Correlation.html


crw = data.frame(x=numeric(n),
                 y=numeric(n))

simulate_ar <- function(n, phi, sigma = .01) {
  x <- rep(0, n)
  y <- rep(0, n)
  
  for (t in seq(2, n)) {
    x[t] <- phi*x[t-1] + rnorm(1, 0, sigma)
    y[t] <- phi*y[t-1] + rnorm(1, 0, sigma)
  }
  
  c(x,y)
  
}

n <- 1000
set.seed(1)

rw1 <- simulate_ar(n, phi = 1)
rw2 <- simulate_ar(n, phi = 1)
rw3 <- simulate_ar(n, phi = 1)
ar <- simulate_ar(n, phi = .999)


crw[,"x"] = ar[1:1000]
crw[,"y"] = ar[1001:2000]
plot(crw$x, crw$y, type = "l")
