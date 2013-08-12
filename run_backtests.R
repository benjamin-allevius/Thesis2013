setwd("~/Your/Path/Here")

# Load packages to be used
library(timeSeries)
library(fGarch)
library(rugarch)
library(ismev)
library(boot)
library(doParallel)
registerDoParallel(cores = 4)
# library(doMC)
# registerDoMC(cores=4)


# Load variables and helper functions
source("./variable_definitions.R")
source("./garch_specs.R")
source("./var_es.R")
source("./make_estimates.R")


# Note! Only run one data set at a time! I.e. length(dsets)==1


dset.name <- paste("./", dset, ".csv", sep="")
dataset <-read.csv(dset.name, header = TRUE)

dataset <- timeSeries::as.timeSeries(dataset, FinCenter = "GMT")

# If testing on subset of data
#   dataset <- dataset[1:1050,]

# Convert price series to loss series
dataset <- -returns(dataset, method = "continuous")

length.dataset <- length(dataset[, 1])

# Define matrices to store results
source(".storage_setup.R")

# Normalize entire dataset to have variance one
dataset.sd <- apply(dataset, 2, sd)
dataset <- (dataset / dataset.sd)

# Load "optimal" thresholds
load(paste("./Data/Done/", dset, "_thresholds.RData", sep=""))

# Index to extract threshold
loop.seq <- seq(from=1, to=(length.dataset - n), by=500) + 500

# Loop for estimation window
loop.time <- system.time({
  for (i in 1:(length.dataset - n)) {
    if ((i %% 100) == 0){
      print(i)
      cat(paste(Sys.time()),"\n"); flush(stdout())
    }
    
    data.window <- dataset[i:(n-1+i), 1]
    return.tomorrow <- as.numeric(dataset[(n+i), 1])
    
    # Extract correct threshold
    for (t.idx in loop.seq){
      if (i < t.idx){
        thres.idx <- which(t.idx == loop.seq)
        break
      }
    }
    
    
    # Produce estimates from each model
    estimates <- foreach(j=1:nmodels, .combine=rbind) %dopar% 
    {
      return.values <- tryCatch(make.estimates(data.series=data.window,
                                               model=models[[j]][1], 
                                               distribution=models[[j]][2],
                                               k=thresholds[thres.idx, j]),
                                error=function(e) base::rep(NA, 2*ndatatypes*nq),
                                silent=TRUE)
    }
    
    # Flatten estimates to a vector
    ests <- as.vector(estimates)
    
    # Extra error check
    if (length(ests) != (nmodels*2*ndatatypes*nq)){
      ests <- base::rep(NA, nmodels*2*ndatatypes*nq)
    }
    
    # Store data in format specified in variable_definitions.R
    data.var[i,] <- as.vector(rbind(ests[data.idxs[1,]], ests[data.idxs[2,]]))
    data.es[i,] <- as.vector(rbind(ests[data.idxs[3,]], ests[data.idxs[4,]]))
    data.break[i,] <- as.vector(rbind(ests[data.idxs[5,]], ests[data.idxs[6,]]))
    data.diff[i,] <- as.vector(rbind(ests[data.idxs[7,]], ests[data.idxs[8,]]))
    data.exres[i,] <- as.vector(rbind(ests[data.idxs[9,]], ests[data.idxs[10,]]))
  }
})[3]
print(loop.time)

# Save estimation results before running tests (in case of errors)
save.image(file=paste("./Data/Done/", dset, ".RData", sep=""))


# Run tests
source("./Code/Backtest/all_tests.R")

# Make tables of results
source("./Code/Backtest/post_processing.R")

# Save results again
save.image(file=paste("./Data/Done/", dset, ".RData", sep=""))
