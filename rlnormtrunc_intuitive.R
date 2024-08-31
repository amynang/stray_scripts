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
