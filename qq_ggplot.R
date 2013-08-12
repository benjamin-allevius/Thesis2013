qqplot.norm <- function (vec) # argument: vector of numbers
{
  vec <- (vec - mean(vec)) / sd(vec)
  
  # following four lines from base R's qqline()
  y <- quantile(vec[!is.na(vec)], c(0.25, 0.75))
  x <- qnorm(c(0.25, 0.75))
  slope <- diff(y)/diff(x)
  int <- y[1L] - slope * x[1L]

  d <- data.frame(resids = vec)

  p <- ggplot(d, aes(sample = resids)) + 
        stat_qq(distribution=qnorm) + 
        geom_abline(slope = slope, intercept = int) +
        xlab("Theoretical Quantiles") + ylab("Sample Quantiles") +
        theme_bw()
  return(p)
}

qqplot.t <- function (vec, df=4) 
{
  # vec: vector of observations
  # df: degrees of freedom for t-distribution
  
  vec <- (vec - mean(vec)) / sd(vec)
  
  # following four lines from base R's qqline()
  y <- quantile(vec[!is.na(vec)], c(0.25, 0.75))
  x <- qt(c(0.25, 0.75), df=df)
  slope <- diff(y)/diff(x)
  int <- y[1L] - slope * x[1L]
  
  d <- data.frame(resids = vec)
  
  p <- ggplot(d, aes(sample = resids)) + 
         stat_qq(distribution=qt, dparams=list(df=df)) +
         geom_abline(slope = slope, intercept = int) +
         xlab("Theoretical Quantiles") + ylab("Sample Quantiles") + 
         theme_bw()
}