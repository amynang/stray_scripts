land = matrix(0,9,9,
              dimnames=list(1:9,1:9))

land[1,1] = 1 ; land[9,9] = 2 ; land

for (i in 1:500) {
  # keep coordinates
  chi.1 = which(land==1, arr.ind = T)[1]
  psi.1 = which(land==1, arr.ind = T)[2]
  # remove previous position
  land[land == 1] = 0
  # make a random step
  new.chi.1 = chi.1 + sample(c(-1,0,1),1)
  while (new.chi.1 < 1 | new.chi.1 > 9) {
    new.chi.1 = chi.1 + sample(c(-1,0,1),1)
  }
  new.psi.1 = psi.1 + sample(c(-1,0,1),1)
  while (new.psi.1 < 1 | new.psi.1 > 9) {
    new.psi.1 = psi.1 + sample(c(-1,0,1),1)
  }
  
  # keep coordinates
  chi.2 = which(land==2, arr.ind = T)[1]
  psi.2 = which(land==2, arr.ind = T)[2]
  # remove previous position
  land[land == 2] = 0
  # make a random step
  new.chi.2 = chi.2 + sample(c(-1,0,1),1)
  while (new.chi.2 < 1 | new.chi.2 > 9) {
    new.chi.2 = chi.2 + sample(c(-1,0,1),1)
  }
  new.psi.2 = psi.2 + sample(c(-1,0,1),1)
  while (new.psi.2 < 1 | new.psi.2 > 9) {
    new.psi.2 = psi.2 + sample(c(-1,0,1),1)
  }
  
  land[new.chi.1, new.psi.1] = 1
  land[new.chi.2, new.psi.2] = 2
  
  if (new.chi.1 == new.chi.2 & 
      new.psi.1 == new.psi.2) {
    print("2 ate 1")
    heatmap(land, Colv = NA, Rowv = NA)
    }
  else {
    print(land)
    heatmap(land, Colv = NA, Rowv = NA)
    }
  
}
