n <- 1000  # window size
qs <- c(0.95, 0.975, 0.99, 0.995)  # chosen quantile
nq <- length(qs)
df <- 4  # degrees of freedom for t-distribution


# Model-distribution combinations to use
models <- list(c("garch", "norm"), 
               c("garch", "std"),
               c("gjr", "norm"), 
               c("gjr", "std"),
               c("cs", "norm"),
               c("cs", "std"))

nmodels <- length(models)


# name of data set to use (file should be called "dset.csv", e.g. "sp500.csv")
dset <- "example_data"

# Note: example_data.csv should not be used,
# but it shows the format expected
