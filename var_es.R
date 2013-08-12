
# VaR for a normal distribution
var.normal <- function(mean, sd, probs)
{
  var <- mean + sd * qnorm(p=probs)
  return(var)
}

# VaR for a Student t distribution
var.student <- function(mean, sd, probs, df)
{
  scaling.factor <- sqrt((df-2) / df)
  var <- mean + sd * (scaling.factor * qt(p = probs, df = df) )
  return(var)
}

# VaR for a Generalized Pareto Distribution (GDP)
var.gpd <- function(threshold, scale, shape, probs, n, k)
{
  var <- threshold + (scale / shape) * (( (n/k)*(1-probs) )^(-shape) - 1)
  return(var)
}

# ES for a normal distribution
es.normal <- function(mean, sd, probs)
{
  es <- mean + sd * (dnorm(x=qnorm(p=probs)) / (1-probs))
  return(es)
}

# ES for a Student t distribution
es.student <- function(mean, sd, probs, df)
{
  scaling.factor <- sqrt((df-2)/df)
  factor1 <- dt(x=qt(p=probs, df=df), df=df) / (1-probs)
  factor2 <- (df + (qt(p=probs, df=df))^2 ) / (df-1)
  es <- mean + sd * scaling.factor * factor1 * factor2
  return(es)
}

# ES for a GPD
es.gpd <- function(var, threshold, scale, shape)
{
  es <- var / (1-shape) + (scale - shape * threshold) / (1-shape)
  return(es)
}


## Some tests
# var.n <- var.normal(mean=1, sd=5, probs=c(0.95, 0.975, 0.99))
# var.t <- var.student(mean=1, sd=5, probs=c(0.95, 0.975, 0.99), df=4)
# var.g <- var.gpd(threshold=1.96, scale=1, shape=0.3, probs=c(0.95, 0.975, 0.99), n=1000, k=100)
# es.n <- es.normal(1,5, c(0.95, 0.975, 0.99))
# es.t <- es.student(1,5, c(0.95, 0.975, 0.99), 4)
# es.g <- es.gpd(var=var.g, threshold=1.96, scale=1, shape=0.3)