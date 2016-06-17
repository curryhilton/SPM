# Create s chart for known standard deviation of process data
# User defines process standard deviations data set in string or data frame (x)
# User defines process standard deviation (s)
# User defines sigma limits (k)

sk <- function(x, s, k) {
  library(ggplot2)
  data <- data.frame(x)            # create data frame from user process data input
  n <- length(data[,1])            # calculate the number of subgroups
  c4 <- (4*(n-1))/(4*n-3)          # calculate control chart constant c4
  cl <- c4*s                       # calculate centerline of the s chart
  ucl <- c4*s + k*s*sqrt(1-c4^2)   # calculate upper control chart limit for the s chart
  lcl <- c4*s - k*s*sqrt(1-c4^2)   # calculate lower control chart limit for the s chart

  splot <- ggplot(data, aes(x=seq(1:length(x)), y=data[,1])) +
    geom_point() +
    geom_hline(yintercept=cl) +
    geom_hline(yintercept=ucl, linetype="dashed", color = "red") +
    geom_hline(yintercept=lcl, linetype="dashed", color = "red") +
    labs(x="Sample",y="s") +
    ggtitle("s Chart: Standards Known")
  splot
}
