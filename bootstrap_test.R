bootstrap.test <- function(series, n.replicates=10000, .ncpus=4){
  
  # Function used inside boot function
  bootStat <- function(dats, idx){
    d <- dats[idx]
    n = length(d)
    m <- mean(d)
    s <- sd(d)
    stat <- m / (s / sqrt(n))
    return(c(stat, s))
  }
  
  #
  Tstat <- mean(series) / ( sd(series) / sqrt(length(series)) )
  
  #
  boot.test <- boot(data = ( series - mean(series) ),
                    statistic=bootStat,
                    R=n.replicates,
                    sim="ordinary",
                    stype="i",
                    parallel="multicore",
                    ncpus=.ncpus)
  
  pvalue <- (1 + sum(boot.test$t[, 1] > Tstat) ) / (1 + n.replicates)
  return(pvalue)
}
