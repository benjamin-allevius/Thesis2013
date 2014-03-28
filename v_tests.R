v.tests <- function(diff.series, breaks, q){
  # diff.series: observations minus ES estimate; univariate
  # breaks: sequence of VaR-breaks (TRUE or FALSE) of same
  #         length as diff.series
  # q: a high probability, e.g. 0.95
  
  # Sample quantile for V2 statistic
  d.quantile <- quantile(x=diff.series, probs=q, type=4)
  
  v1 <- mean(diff.series[as.logical(breaks)])
  v2 <- sum(diff.series * (diff.series > d.quantile)) / sum((diff.series > d.quantile))
  v3 <- (abs(v1) + abs(v2)) / 2
  
  return(c(v1, v2, v3))
}
