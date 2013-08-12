# Implements the bootstrap method for choosing
# the sample fraction in tail index estimation
# by Danielsson et. al, 2001
# Available e.g. here: http://ideas.repec.org/a/eee/jmvana/v76y2001i2p226-248.html

# Package boot must be loaded
# Package doMC must be loaded, and registerDoMC(cores=...) set. e.g.
# library(doMC)
# registerDoMC(cores=4)

# Important to note: data must be strictly positive!

kChoice <- function(dats, 
           n.samples=1000, 
           min.subsamp.prop=0.3,
           max.subsamp.prop=0.95,
           step.prop=0.05, 
           min.k.prop=0.05,
           max.k.prop=0.15,
           k.step=1,
           .ncpus=4){
  # n.samples is the number of bootstrap samples
  # min.subsamp.prop is the lower bound on the boostrap subsample
  # i.e. lower bound on n1 or n2, expressed as a proportion of e.g. n1
  # max.subsamp.prop is the upper bound
  # step.prop determines the step size between different n1's
  # min.k.prop and max.k.prop determines between which bounds k runs
  # .ncpus is the number of processors to use for parallel processing
  
  
  # Define functions to be used
  Mfunc <- function(x, thres)
    # Function for M on page 230 of Danielsson
  {
    if (length(x) < 1)
    {
      stop("x is empty")
    }
    if (any(is.na(x)))
    {
      stop("x contains NA")
    }
    if (any(x <= 0))
    {
      stop("x contains nonpositive values")
    }
    if (length(thres) != 1)
    {
      stop("thres contains multiple values")
    }
    if (is.na(thres))
    {
      stop("thres is NA")
    }
    return(sum( (log((x / thres)))^2 ) / length(x))
  }
  
  Gfunc <- function(x, thres)
    # Function for gamma* on page 229 of Danielsson
  {
    if (length(x) < 1)
    {
      stop("x is empty")
    }
    if (any(is.na(x)))
    {
      stop("x contains NA")
    }
    if (any(x <= 0))
    {
      stop("x contains nonpositive values")
    }
    if (length(thres) != 1)
    {
      stop("thres contains multiple values")
    }
    if (is.na(thres))
    {
      stop("thres is NA")
    }
    return(sum( (log((x / thres))) ) / length(x))
  }
  
  Qfunc <- function(M, G){
    # Function for expression in expectation for Q on page 230 in Danielsson
    return((M - 2 * G^2)^2)
  }
  
  bootfunc <- function(data, idx, N, K)
    # For a bootstrap sample, calculate
    # expression in expectation of Q,
    # for Q on page 230 in Danielsson
  {
    if (length(data) != length(idx))
    {
      stop("data not same length as idx")
    }
    if (length(N) > 1)
    {
      stop("length N > 1")
    }
    if (length(K) > 1)
    {
      stop("length K > 1")
    }
    if (is.na(N) | is.na(K))
    {
      print(c("(N,K)", N, K))
      stop("N or K is NA")
    }
    if (K > N)
    {
      print(c("(N,K)", N, K))
      stop("K > N")
    }
    sample <- sort( (data[idx])[1:N], decreasing=TRUE)
    x.thres <- sample[(K+1)]
    Mstar <- Mfunc(sample, x.thres)
    Gstar <- Gfunc(sample, x.thres)
    Qstar <- Qfunc(Mstar, Gstar)
    return(Qstar)
  }
  
  kOptfunc <- function(n1, k1, k2)
    # Given optimal values of n1, k1, k2,
    # calculate optimal value of k,
    # the number of observations in the tail
    # as per Corollary 7 page 232 of Danielsson
  {
    if (any(c(length(n1), length(k1), length(k2)) > 1))
    {
      print(c("(n1,k1,k2", n1, k1, k2))
      stop("n1,k1, or k2 has length > 1")
    }
    if (any(c(n1, k1, k2) < 1))
    {
      print(c("(n1,k1,k2", n1, k1, k2))
      stop("n1,k1, or k2 nonpositive")
    }
    a <- (k1)^2 / k2
    b <- (log(k1))^2 / (2 * log(n1) - log(k1))^2
    c <- (log(n1) - log(k1)) / log(n1)
    kOpt <- a * b^c 
    return(floor(kOpt))
  }
  
  # Define
  n <- length(dats) 
  min.n1 <- floor(min.subsamp.prop * n)
  max.n1 <- floor(max.subsamp.prop * n)
  n1.step <- floor(step.prop * n)
  n1s <- seq(from=min.n1, to=max.n1, by=n1.step)
  
  # Rvals stores R, n1, n2, k1, k2
  # Where k1, k2 are optimal values for n1 and n2 respectively
#   Rvals <- matrix(data=NA, nrow=length(n1s), ncol=5)
  
  Rvals <- foreach(j=1:length(n1s), .combine=rbind) %dopar% {
    n1 <- n1s[j]
    min.k1 <- floor(min.k.prop * n1)
    max.k1 <- floor(max.k.prop * n1)
    k1s <- seq(from=min.k1, to=max.k1, by=k.step)
    k1.vec <- rep(NA, times=length(k1s))
    Q1.vec <- k1.vec
    
    for (i in 1:length(k1s)){
      k1 <- k1s[i]
      boot1.dat <- boot(data = dats,
  	             bootfunc,
  	             R = n.samples,
  	             sim = "ordinary",
  	             stype = "i",
  	             N=n1, K=k1,
  	             parallel = "multicore",
  	             ncpus = .ncpus)
      Q1.est <- mean(boot1.dat$t)
      Q1.vec[i] <- Q1.est
    }
    k1.idx <- which.min(Q1.vec)
    k1star <- k1s[k1.idx]
    
    n2 <- floor( (n1^2 / n))
    min.k2 <- floor(min.k.prop * n2)
    max.k2 <- floor(max.k.prop * n2)
    k2s <- seq(from=min.k2, to=max.k2, by=k.step)
    k2.vec <- rep(NA, times=length(k2s))
    Q2.vec <- k2.vec
    
    for (i in 1:length(k2s)){
      k2 <- k2s[i]
      boot2.dat <- boot(data = dats,
  	             bootfunc,
  	             R = n.samples,
  	             sim = "ordinary",
  	             stype = "i",
  	             N=n2, K=k2,
  	             parallel = "multicore",
  	             ncpus = 4)
      Q2.est <- mean(boot2.dat$t)
      Q2.vec[i] <- Q2.est
    }
    k2.idx <- which.min(Q2.vec)
    k2star <- k2s[k2.idx]
  
    Rn1 <- (Q1.vec[k1.idx])^2 / Q2.vec[k2.idx]
    
    c(Rn1, n1, n2, k1, k2)
  }
  
  opt.idx <- which.min(Rvals[,1])
  n1.opt <- Rvals[opt.idx,2]
  n2.opt <- Rvals[opt.idx,3]
  k1.opt <- Rvals[opt.idx,4]
  k2.opt <- Rvals[opt.idx,5]
  k.opt <- kOptfunc(n1.opt, k1.opt, k2.opt)
  
  return(as.integer(k.opt))
}
