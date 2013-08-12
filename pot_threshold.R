pot.threshold <- function(model, distribution)
{
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
                               data = data.window, 
                               cond.dist = "norm", 
                               trace = FALSE)
    }
    
    if (distribution == "std")  # Student t innovations
    {
      fitted.model <- garchFit(formula = ~ arma(1, 0) + garch(1, 1), 
                               data = data.window, 
                               cond.dist = "std", 
                               shape = df,
                               include.shape = FALSE,
                               trace = FALSE)
    }
    
    
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
                                  data=data.window, 
                                  solver=slvr, 
                                  solver.control=slvr.ctrl)
      }
      if (distribution == "std")  # Student t innovations
      {
        fitted.model <- ugarchfit(spec=gjr.spec.std, 
                                  data=data.window, 
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
                                  data=data.window, 
                                  solver=slvr, 
                                  solver.control=slvr.ctrl)
      }
      if (distribution == "std")  # Student t innovations
      {
        fitted.model <- ugarchfit(spec=cs.spec.std, 
                                  data=data.window, 
                                  solver=slvr, 
                                  solver.control=slvr.ctrl)
      }
    }
    
    
    # Get residuals (for EVT): standardize through (time dependent) fitted values
    # and standard deviations
    model.residuals <- rugarch::residuals(fitted.model, standardize=TRUE)
  }
  
  #######################################################################
  
  # Shift residuals to be all positive
  if (min(model.residuals) <= 0.00001){
    model.residuals <- model.residuals + sign(min(model.residuals)) * min(model.residuals) + 0.00001
  }
  
  k.optimal <- kChoice(model.residuals)
  return(k.optimal)
}
