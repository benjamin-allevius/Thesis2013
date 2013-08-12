# Load the test functions
source("./var_tests.R")
source("./bootstrap_test.R")
source("./v_tests.R")


# Use to extract correct probability for index in loop below
prob.idxs <- seq(from=1, to=2*nq*nmodels, by=(2*nmodels)) + 2*nmodels

# Matrices to store test results
pvals.var <- mat.or.vec(nr=3, nc=ncol(data.exres))
pvals.boot <- mat.or.vec(nr=1, nc=ncol(data.exres))
vals.vtest <- mat.or.vec(nr=3, nc=ncol(data.exres))

# Loop over each estimation series
for (j in 1:ncol(data.exres)){
  
  # Index in qs corresponding to current j
  for (k in prob.idxs){
    if (j < k){
      prob.idx <- which(k == prob.idxs)
      break
    }
  }
  
  ##############################################################################################
  ###  Define data to be used
  # Probability corresponding to current data
  current.prob <- qs[prob.idx]
  
  # Data used for bootstrap test
  current.exres <- data.exres[,j]
  current.exres <- current.exres[as.logical(data.break[,j])]
  current.exres <- current.exres[!is.na(current.exres)]
  
  
  current.diff <- data.diff[,j]
  current.diff <- current.diff[!is.na(current.diff)]
  
  # If any NA in current data, it better line up with NA in the breaks...
  current.break <- data.break[,j]
  current.break <- as.logical(current.break[!is.na(current.break)])
  ##############################################################################################
  
  ##############################################################################################
  ###  Run tests, store results
  # Run tests on VaR-breaks
  pvals.var[, j] <- mcsim.test(hit.sequence=current.break, hit.probability=(1-current.prob))
  
  # Run bootstrap test on exceedance residuals
  pvals.boot[j] <- bootstrap.test(current.exres)
  
  # Run V-tests
  vals.vtest[, j] <- v.tests(diff.series=current.diff, breaks=current.break, q=current.prob)
  ##############################################################################################
  
  print(j)
}
