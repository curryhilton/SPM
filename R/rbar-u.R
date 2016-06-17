# Create R-bar chart for unknown standard deviation of process data
# User defines process range data set in string or data frame (x)
# User defines sigma limits (k)

rbaru <- function(x, k) {
  library(ggplot2)
  data <- data.frame(x)           # create data frame from user process data input
  n <- length(data[,1])           # calculate the number of subgroups
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
  D3 = 1-k*(d3[n-1]/d2[n-1])      # calculate control chart constant D3
  D4 = 1+k*(d3[n-1]/d2[n-1])      # calculate control chart constant D4
  cl <- mean(data[,1])            # calculate centerline of R-bar chart
  ucl <- D4*cl                    # calculate upper control chart limit for R-bar chart
  lcl <- D3*cl                    # calculate lower control chart limit for R-bar chart

  rplot <- ggplot(data, aes(x=seq(1:length(x)), y=data[,1])) +
    geom_point() +
    geom_hline(yintercept=cl) +
    geom_hline(yintercept=ucl, linetype="dashed", color = "red") +
    geom_hline(yintercept=lcl, linetype="dashed", color = "red") +
    labs(x="Sample",y="R's") +
    ggtitle("R-Bar Chart: Standards Unknown")
  rplot
}
