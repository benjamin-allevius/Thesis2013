# Data format: 
#  First 2*nmodel columns contain estimates of each model-distribution combination
#  with estimate from the corresponding POT model next to it, for the
#  first probability in the vector qs. 
#  For example, column 1 of data.var will contain the qs[1] VaR estimate
#  for the first model-distribution combination 
#  (e.g. AR(1)-GARCH(1,1) with normally distributed innovations)
#  and column 2 will contain the qs[1] VaR estimate for the POT model
#  fitted to the residuals of this AR(1)-GARCH(1,1) model
#
#  The second 2*nmodel columns will contain corresponding results for
#  the second probability qs[2], and so on

# Matrices to store estimates
data.var <- mat.or.vec((length.dataset - n), (2*nq*nmodels))
data.es <- mat.or.vec((length.dataset - n), (2*nq*nmodels))
data.break <- mat.or.vec((length.dataset - n), (2*nq*nmodels))
data.diff <- mat.or.vec((length.dataset - n), (2*nq*nmodels))
data.exres <- mat.or.vec((length.dataset - n), (2*nq*nmodels))

# Number of variables we save == number of matrices above
ndatatypes <- 5  # var, es, var.break, es.diff, exres

# Indices used when storing data in estimation window
data.idxs <- t(matrix(data=1:(2*nq*ndatatypes*nmodels), ncol=(2*ndatatypes), nrow=(nmodels*nq)))