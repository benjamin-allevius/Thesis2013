# Code adapted from: 
# "Creating an Autocorrelation Plot in ggplot2"
# Peter DeWitt
# file: files.meetup.com/1696476/ACFinGGPLOT2Presentation.pdf


qacf <- function(x, 
                 fun=c("acf","pacf"), 
                 transformation="level",
                 conf.level = 0.95, 
                 max.lag = 20, 
                 min.lag = 0,
                 title = "", 
                 legend = 1) 
{
  if ((fun != "acf") & (fun != "pacf"))
  {
    stop("fun needs to be either 'acf' or 'pacf'")
  }
  
  if ((transformation != "level") & (transformation != "square") & (transformation != "abs"))
  {
    stop("Wrong transformation. Allowed: 'level', 'square', 'abs'.")
  }
  
  # Apply transformation to data
  if (transformation == "square")
  {
    x <- x^2
  }
  
  if (transformation == "abs")
  {
    x <- abs(x)
  }
  
  
  
  typefun <- match.fun(fun)
  
  # Line for confidence interval
  ciline <- qnorm((1 - conf.level)/2)/sqrt(length(x))
  
  # ACF or PACF
  bacf <- typefun(x, plot = FALSE, lag.max = max.lag)
  cfdata <- with(bacf, data.frame(lag, acf))
  
  
  if (min.lag > 0) 
    {
      cfdata <- cfdata[-seq(1, min.lag), ]
    }
  
  
  significant <- (abs(cfdata[, 2]) > abs(ciline))^2
  cfdata <- cbind(cfdata, significant)
  ylabel <- "ACF"
  if (fun == "pacf")
  {
    ylabel <- "Partial ACF"
  }
  
  # "Stupid" way of making sure legend shows all factors
  # Cols of cfdata: lag, acf, significant
  lb <- length(cfdata$lag)
  cfdata[lb+1,] <- c(cfdata[lb,1]+1, 0, 0)
  cfdata[lb+2,] <- c(cfdata[lb,1]+2, 0, 1)
  
  q <- qplot(x=lag, y=acf, 
             data = cfdata, 
             geom = "bar", 
             stat = "identity",
             position = "identity", 
             ylab = ylabel, 
             xlab = "Lags",
             main = title,
             fill = factor(significant))
  q <- q + geom_hline(yintercept = -ciline, color = "blue", size = 0.5)
  q <- q + geom_hline(yintercept = ciline, color = "blue", size = 0.5)
  q <- q + geom_hline(yintercept = 0, color = "red", size = 0.3)
  q <- q + scale_fill_hue(name = paste("Significant at the\n", conf.level, "level"), 
                          breaks = 0:1, labels = c("False", "True"))
  q <- q + theme_bw() + xlim(cfdata[1,"lag"] - 0.5, cfdata[lb,"lag"]+0.5)
  
  if (legend == 0){
    q <- q + theme(legend.position = "none") 
  }
  return(q)
}

####
# dats <- rnorm(100)
# dplot <- qacf(x=dats, fun="pacf", transformation="level")