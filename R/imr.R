# Create individual moving range chart for unknown mean and standard deviation of process data
# User defines process data set in string or data frame (x)
# User defines sigma limits (k)
# User defines number of lag periods for difference (i)

imr <- function(x, k, i) {
  library(ggplot2)
  data <- data.frame(x)              # create data frame from user process data input
  n <- length(data[,1])              # calculate the length of the variable
  mr <- rep(NA, n)                   # function for MR
  for(j in 2:n+1){
    mr[j] = abs(data[,j]-data[,j-i])
  }
  d2 <- c(1.128, 1.693, 2.059, 2.326, 2.534,  # define d2 control chart parameters
          2.704, 2.847, 2.970, 3.078, 3.173,  # Montgomery's textbook
          3.258, 3.336, 3.407, 3.472, 3.532,
          3.588, 3.640, 3.689, 3.735, 3.778,
          3.819, 3.858, 3.895, 3.931)
  d3 <- c(0.853, 0.888, 0.880, 0.864, 0.848,  # define d3 control chart parameters
          0.833, 0.820, 0.808, 0.797, 0.787,  # Montgomery's textbook
          0.778, 0.770, 0.763, 0.756, 0.750,
          0.744, 0.739, 0.734, 0.729, 0.724,
          0.720, 0.716, 0.712, 0.708)
  D3 = 1-k*(d3[n-1]/d2[n-1])         # calculate control chart constant D3
  D4 = 1+k*(d3[n-1]/d2[n-1])         # calculate control chart constant D4
  cl <- mean(mr)                     # calculate the mean of MR
  ucl <- cl*D4                       # calculate upper control chart limit for IMR chart
  lcl <- cl*D3                       # calculate lower control chart limit for IMR chart

  IMRplot <- ggplot(data, aes(x=seq(1:length(x)), y=data[,1])) +
    geom_point() +
    geom_hline(yintercept=cl) +
    geom_hline(yintercept=ucl, linetype="dashed", color = "red") +
    geom_hline(yintercept=lcl, linetype="dashed", color = "red") +
    labs(x="Sample",y="IMR") +
    ggtitle("IMR Chart: Standards Unknown")
  IMRplot
}
