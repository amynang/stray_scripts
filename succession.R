library(tidyverse)

# regional pool
reg = data.frame(Species = paste0("sp", sprintf("%03d", 1:1000)),
                 axis1.from = NA,
                 axis1.to = NA,
                 axis2.from = NA,
                 axis2.to =NA)

# each species' niche is a random rectangle in the trait surface
for (i in 1:1000) {
  a = sample(1:100, 1)
  b = sample(a:100, 1)
  c = sample(1:100, 1)
  d = sample(c:100, 1)
  
  reg[i,2] = runif(1,a,b)
  reg[i,3] = runif(1,reg[i,2]+2,b)
  reg[i,4] = runif(1,c,d)
  reg[i,5] = runif(1,reg[i,4]+2,d)
}

# not sure why NaNs, I am just getting rid of them for now
reg = reg[complete.cases(reg),]

ggplot(reg) +
  geom_rect(aes(xmin=axis1.from,
                xmax=axis1.to,
                ymin=axis2.from,
                ymax=axis2.to,
                fill=Species, 
                alpha=.3), color = "black") +
  geom_rect(aes(xmin=25,
                xmax=50,
                ymin=25,
                ymax=50,
                fill=alpha("grey",0)), 
            color = "red",
            fill=alpha("grey",0),
            size = 1) +
  theme(legend.position = "none") #+
 
# for species to exist in a location theirniche must be at least partly covered 
# by it
loc = reg %>% filter(axis1.to >= 25 &
                       axis1.from <= 50 &
                       axis2.to >=25 &
                       axis2.from <=50)


ggplot(loc) +
  geom_rect(aes(xmin=axis1.from,
                xmax=axis1.to,
                ymin=axis2.from,
                ymax=axis2.to,
                fill=Species, 
                alpha=.3), color = "black") +
  geom_rect(aes(xmin=25,
                xmax=50,
                ymin=25,
                ymax=50,
                fill=alpha("grey",0)), 
            color = "red",
            fill=alpha("grey",0),
            size = 1) +
  theme(legend.position = "none")

# interactions of species are limited by their niche overlap inside a location
over = matrix(NA,nrow(loc),nrow(loc))
for (i in 1:nrow(loc)) {
  for (j in 1:nrow(loc)) {
    # first we get the intersect of each species with the location
    a = raster::intersect(raster::extent(as.numeric(loc[i, 2:5])),
                          raster::extent(25,75,25,75))
    b = raster::intersect(raster::extent(as.numeric(loc[j, 2:5])),
                          raster::extent(25,75,25,75))
    # then we get the intersect of the two species inside the lacation
    ext = raster::intersect(a,b)
    # now we get the proportion of species i niche that is intersected
    # the niche of species j
    over[i,j] = ifelse(length((ext[2] - ext[1])*(ext[4] - ext[3]))==0, 
                       0, 
                       (ext[2] - ext[1])*(ext[4] - ext[3]))/
                       ((a[2]-a[1])*(a[4]-a[3]))
  }
}
