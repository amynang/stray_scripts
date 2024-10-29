loreau_hector <- function(
    M_i, # vector of species yield in monoculture
    Y_oi # vector of species yield in mixture
) {
  N = length(M_i)       # number of species in the mixture
                        
  Y_o = sum(Y_oi)       # total observed yield of the mixture
                        
  dRY = Y_oi/M_i - 1/N  # deviation from expected relative yield of species i
                        
  M_bar = mean(M_i)     # average yield across the monocultures of constituent species
                        
  dRY_bar = mean(dRY)   # mean deviation
                        
  CE = N*dRY_bar*M_bar  # complementarity effect
  SE = N*cov(dRY,M_i)   # selection effect
  NE = CE + SE          # net effect
  c(NE,CE,SE)
}


loreau_hector(c(2,4,6,8),c(2,4,6,8)/4)
loreau_hector(c(2,4,6,8),c(1,4,6,9)/4)
loreau_hector(c(2,4,6,8),c(0,4,6,10)/4)
loreau_hector(c(2,4,6,8),c(2,4,6,9)/4)
loreau_hector(c(2,4,6,8),c(3,4,6,8)/4)
