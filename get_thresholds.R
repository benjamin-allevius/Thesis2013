setwd("~/your/path/here")

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
library(gdata)

# Load variables and helper functions
source("./variable_definitions.R")
source("./garch_specs.R")
source("./threshold_choice.R")
source("./pot_threshold.R")


# Note! Only run one data set at a time! I.e. length(dsets)==1

for (dset in dsets){
  dset.name <- paste("./", dset, ".csv", sep="")
  dataset <-read.csv(dset.name, header = TRUE)

  
  dataset <- timeSeries::as.timeSeries(dataset, FinCenter = "GMT")
  
  # If testing on subset of data
#   dataset <- dataset[1:1050,]
  
  # Convert price series to loss series
  dataset <- -returns(dataset, method = "continuous")
  
  length.dataset <- length(dataset[, 1])
  
  
  # Normalize entire dataset to have variance one
  dataset.sd <- apply(dataset, 2, sd)
  dataset <- (dataset / dataset.sd)
  
  loop.seq <- seq(from=1, to=(length.dataset - n), by=500)
  
  thresholds <- mat.or.vec(nr=length(loop.seq), nc=nmodels)
  
  t.counter <- 1
  
  # 
  ptime <- system.time({
    for (i in loop.seq) 
    {      
      data.window <- dataset[i:(n-1+i), 1]
      
      # Produce threshold for each model
      for (j in 1:nmodels){
        thresholds[t.counter, j] <- pot.threshold(model=models[[j]][1], distribution=models[[j]][2])
        print(paste("(", as.character(t.counter), as.character(j), "): ", 
                    as.character(thresholds[t.counter, j])))
      }
      cat(paste(Sys.time()),"\n"); flush(stdout())
      
      t.counter <- t.counter + 1
    }
  })[3]
  
  # Remove all variables except  threshold, ptime
  keep(thresholds, dset, ptime, sure=TRUE)
  
  # Save results
  save.image(file=paste("./", dset, "_thresholds",".RData", sep=""))
  rm(dset)
}
