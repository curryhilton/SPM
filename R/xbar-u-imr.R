# Create individual x-bar chart for unknown mean and standard deviation of process data using moving range
# User defines process data set in string or data frame (x)
# User defines sigma limits (k)
# User defines number of lag periods for difference (i)

ximrr <- function(x, k, i) {
  library(ggplot2)
  data <- data.frame(x)              # create data frame from user process data input
  n <- length(data[,1])              # calculate the length of the variable
  mr <- rep(NA, n)                   # function for MR
  for(j in 2:n+1){
    mr[j] = abs(data[,j]-data[,j-i])
  }
  mbar <- mean(mr)                     # calculate the mean of MR
  d2 <- c(1.128, 1.693, 2.059, 2.326, 2.534,  # define d2 control chart parameters
          2.704, 2.847, 2.970, 3.078, 3.173,  # Montgomery's textbook
          3.258, 3.336, 3.407, 3.472, 3.532,
          3.588, 3.640, 3.689, 3.735, 3.778,
          3.819, 3.858, 3.895, 3.931)
  cl <- mean(data[,1])               # calculate the centerline for the IMR chart
  ucl <- cl + k*(mrbar/d2[n-1])      # calculate upper control chart limit for IMR chart
  lcl <- cl - k*(mrbar/d2[n-1])      # calculate lower control chart limit for IMR chart

  IMRplot <- ggplot(data, aes(x=seq(1:length(x)), y=data[,1])) +
    geom_point() +
    geom_hline(yintercept=cl) +
    geom_hline(yintercept=ucl, linetype="dashed", color = "red") +
    geom_hline(yintercept=lcl, linetype="dashed", color = "red") +
    labs(x="Sample",y="IMR") +
    ggtitle("IMR Chart: Standards Unknown")
  IMRplot
}
