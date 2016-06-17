# Create CUSUM chart for known mean and standard deviation of process data
# User defines process data set in string or data frame (x)
# User defines process mean (mu)
# User defines reference value (K)
# User defines decision interval (H)
# User defines head start if necessary (HS) - default equals 0

cusum <- function(x, mu, K, H, HS) {
  library(ggplot2)
  n <- length(x)              # calculate the number of observations
  cip <- rep(HS, n)
  cplus <- for(i in 1:n){
      cip[i] <- max(0, x[i]-(mu+K) + cip[i-1])
  }
  cil <- rep(HS, n)
  clow <- for(i in 1:n){
    cil[i] <- max(0, (mu-K) - x[i] + cil[i-1])
  }
  cusumdata <- data.frame(x, cip, cil)
  cusumplot <- ggplot(cusumdata, aes(x=seq(1:n), y=data[,2:3])) +
    geom_point() +
    geom_hline(yintercept=cl) +
    geom_hline(yintercept=ucl, linetype="dashed", color = "red") +
    geom_hline(yintercept=lcl, linetype="dashed", color = "red") +
    labs(x="Sample",y="x's") +
    ggtitle("CUSUM Chart: Standards Known")
  cusumplot
}
