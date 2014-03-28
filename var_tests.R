lr.test <- function(hit.sequence, hit.probability){
  # Here, a "hit" refers to a VaR-break
  # and a "miss" to NOT a VaR-break
  # hit.probability is the probability (e.g. 0.05) of a VaR-break
  # under the null hypothesis, while
  # hit.proportion (below) refers to the sample proportion
  # of VaR-breaks
  
  # Function for counting a sequence of two particular values
  sameas <- function(series, next.value, previous.value){
    count <- 0
    for (i in 2:length(series)){
      if ((series[i] == next.value) & (series[(i-1)] == previous.value)){
        count <- count + 1
      }
    }
    return(count)
  }
  
  
  # Unconditional coverage testing: Christoffersen book, pages 185-186
  n.obs <- length(hit.sequence)
  n.ones <- sum(hit.sequence)
  n.zeros <- n.obs - n.ones
  
  miss.proportion <- n.zeros / n.obs
  hit.proportion <- n.ones / n.obs
  
  # Handle lack of misses
  if (n.zeros == 0){
    miss.term <- 0
  } else {
    miss.term <- n.zeros * log(miss.proportion)
  }
  
  # Handle lack of hits
  if (n.ones == 0){
     hit.term <- 0
  } else {
    hit.term <- n.ones * log(hit.proportion)
  }
  
  # To possibly avoid some numerical errors
  LR.uc <- -2*( (n.zeros * log(1-hit.probability) - hit.term) + 
                (n.ones * log(hit.probability) - miss.term))
  
  # Independence testing: Christoffersen book, pages 186-188
  t00 <- sameas(hit.sequence, next.value=FALSE, previous.value=FALSE)
  t01 <- sameas(hit.sequence, next.value=TRUE, previous.value=FALSE)
  t10 <- sameas(hit.sequence, next.value=FALSE, previous.value=TRUE)
  t11 <- sameas(hit.sequence, next.value=TRUE, previous.value=TRUE)
  
  
  pi00 <- t00 / (t00 + t01)
  pi01 <- t01 / (t00 + t01)
  pi10 <- t10 / (t10 + t11)
  pi11 <- t11 / (t10 + t11)
  
  # If no miss-miss sequences
  if (t00 == 0){
    t00.term <- 0
  } else {
    t00.term <- t00 * log(pi00)
  }
  # If no miss-hit sequences
  if (t01 == 0){
    t01.term <- 0
  } else {
    t01.term <- t01 * log(pi01)
  }
  # If no hit-miss sequences
  if (t10 == 0){
    t10.term <- 0
  } else {
    t10.term <- t10 * log(pi10)
  }
  # If no hit-hit sequences
  if (t11 == 0){
    t11.term <- 0
  } else {
    t11.term <- t11 * log(pi11)
  }
  
  LR.ind <- -2*(miss.term + hit.term - (t00.term + t01.term + t10.term + t11.term))
  
  return(c(LR.uc, LR.ind, (LR.uc + LR.ind)))
}

# Simulate LR statistics and calculate p-values as per Christoffersen book, page 186 and forward
mcsim.test <- function(hit.sequence, hit.probability, nsims=999){
  nobs <- length(hit.sequence)
  
  # Simulate Bernoulli(hit.probability) variables
  bern.sim <- matrix(data=rbinom(n=nobs*nsims, size=1, prob=hit.probability), nrow=nobs, ncol=nsims)
  
  # Calculate the 3 LR statistics for the simulated variables
  sim.lr <- apply(bern.sim, MARGIN=2, FUN=lr.test, hit.probability=hit.probability)
  
  # Calculate the 3 LR statistics for the sample hit.sequence
  samp.lr <- lr.test(hit.sequence, hit.probability)
  
  # Count the number of times the simulated LR statistics are larger
  # than the sample LR statistics
  pvals <- rowSums(sim.lr > samp.lr)
  
  # Return p-values
  pvals <- (1 + pvals) / (1 + nsims)
  return(pvals)
}
