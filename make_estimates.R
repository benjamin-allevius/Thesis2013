make.estimates <- function(data.series, model, distribution, k)
{
  # k: the number of observations in the tail when estimating
  #    the POT model. The (k+1)th residual is used as a threshold
  
  #######################################################################
  ### Check that model and distribution are allowed
  valid.models <- c("garch", "gjr", "fam", "cs")
  valid.distributions <- c("norm", "std")
  if (!(model %in% valid.models))
  {
    stop("Invalid model chosen")
  }
  if (!(distribution %in% valid.distributions))
  {
    stop("Invalid conditional distribution chosen")
  }
  #######################################################################
  
  
  
  #######################################################################
  # Fit models and extract needed parameters (and residuals)
  
  ##########################################################
  ####### Use package fGarch for AR(1)-GARCH(1,1) model
  
  # fGarch: AR(1)-GARCH(1,1)
  if ((model == "garch"))
  {
    if (distribution == "norm")  # Normal innovations
    {
      fitted.model <- garchFit(formula = ~ arma(1, 0) + garch(1, 1), 
                               data = data.series, 
                               cond.dist = "norm", 
                               trace = FALSE)
    }
    
    if (distribution == "std")  # Student t innovations
    {
      fitted.model <- garchFit(formula = ~ arma(1, 0) + garch(1, 1), 
                               data = data.series, 
                               cond.dist = "std", 
                               shape = df,
                               include.shape = FALSE,
                               trace = FALSE)
    }
    
    # Produce forecasts of mean and standard deviation
    model.forecast <- fGarch::predict(object = fitted.model, n.ahead = 1)
    model.mean <- model.forecast$meanForecast
    model.sd <- model.forecast$standardDeviation
    
    # Get residuals (for EVT): standardize through (time dependent) fitted values
    # and standard deviations
    model.residuals <- fGarch::residuals(fitted.model, standardize=TRUE)
  } else {
    
    ##########################################################
    ####### Use package rugarch for other models
    
    # AR(1) - GJR-GARCH(1,1) model
    if (model == "gjr")
    {
      if (distribution == "norm")  # Normal innovations
      {
        fitted.model <- ugarchfit(spec=gjr.spec.norm, 
                                  data=data.series, 
                                  solver=slvr, 
                                  solver.control=slvr.ctrl)
      }
      if (distribution == "std")  # Student t innovations
      {
        fitted.model <- ugarchfit(spec=gjr.spec.std, 
                                  data=data.series, 
                                  solver=slvr, 
                                  solver.control=slvr.ctrl)
      }
      
    }
    # AR(1) - component-GARCH(1,1) model
    if (model == "cs")
    {
      if (distribution == "norm")  # Normal innovations
      {
        fitted.model <- ugarchfit(spec=cs.spec.norm, 
                                  data=data.series, 
                                  solver=slvr, 
                                  solver.control=slvr.ctrl)
      }
      if (distribution == "std")  # Student t innovations
      {
        fitted.model <- ugarchfit(spec=cs.spec.std, 
                                  data=data.series, 
                                  solver=slvr, 
                                  solver.control=slvr.ctrl)
      }
    }
    
    # Make forecasts of tomorrow's expected value and standard deviation 
    # for models from rugarch package
    model.forecast <- ugarchforecast(fitted.model, n.ahead=1)
    model.mean <- model.forecast@forecast$forecasts[[1]]$series
    model.sd <- model.forecast@forecast$forecasts[[1]]$sigma
    
    # Get residuals (for EVT): standardize through (time dependent) fitted values
    # and standard deviations
    model.residuals <- rugarch::residuals(fitted.model, standardize=TRUE)
  }
  
  #######################################################################
  # Now calculate VaR, ES, VaR-break, ES difference, and excess residuals
  
  ############################################################
  # Base model estimates
  
  if (distribution == "norm")
  {
    model.var <- var.normal(mean=model.mean, sd=model.sd, probs=qs)
    model.es <- es.normal(mean=model.mean, sd=model.sd, probs=qs)
  }
  
  if (distribution == "std")
  {
    model.var <- var.student(mean=model.mean, sd=model.sd, probs=qs, df=df)
    model.es <- es.student(mean=model.mean, sd=model.sd, probs=qs, df=df)
  }
  
  # VaR-break
  model.break <- (return.tomorrow > model.var)
  
  # Difference between actual loss and ES estimate
  model.diff <- (return.tomorrow - model.es)
  
  # Excess residuals (page 294, McNeil-Frey) 
  model.exres <- model.diff / model.sd
  
  
  ############################################################
  # Peak-Over-Threshold estimates
  
  # Determine threshold
  EVTmodel.threshold <- (sort(model.residuals, decreasing = TRUE))[(k+1)]
  
  # Fit GPD to residuals
  EVTmodel.fit <- gpd.fit(xdat = model.residuals, 
                          threshold = EVTmodel.threshold, 
                          npy = NULL, 
                          show = FALSE)
  
  # Extract scale and shape parameter estimates
  EVTmodel.scale <- EVTmodel.fit$mle[1]
  EVTmodel.shape <- EVTmodel.fit$mle[2]
  
  # Estimate the innovation quantile (VaR for the residuals)
  EVTmodel.zq <- var.gpd(threshold=EVTmodel.threshold,
                         scale=EVTmodel.scale,
                         shape=EVTmodel.shape,
                         probs=qs,
                         n=n, 
                         k=k)
  # Calculate VaR
  EVTmodel.var <- model.mean + model.sd * EVTmodel.zq
  
  # Calculate the Expected Shortfall
  EVTmodel.es <- model.mean + model.sd * es.gpd(var=EVTmodel.zq,
                                                threshold=EVTmodel.threshold,
                                                scale=EVTmodel.scale,
                                                shape=EVTmodel.shape)
  # VaR-break
  EVTmodel.break <- (return.tomorrow > EVTmodel.var)
  
  # Difference between actual loss and ES estimate
  EVTmodel.diff <- (return.tomorrow - EVTmodel.es)
  
  # Exceedance residuals (page 294, McNeil-Frey)
  # Note that these are created for all observations;
  # only those on dates of VaR-breaks should be used
  # in the bootstrap test
  EVTmodel.exres <- EVTmodel.diff / model.sd
  
  
  #######################################################################
  # Return results
  
  return(c(model.var, EVTmodel.var,
           model.es, EVTmodel.es,
           model.break, EVTmodel.break,
           model.diff, EVTmodel.diff,
           model.exres, EVTmodel.exres))
}
