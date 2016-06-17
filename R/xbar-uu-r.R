# Create x-bar chart for unknown mean and standard deviation (using R) of process data
# User defines process data set in string or data frame (x) which includes subgroup mean (1) and range (2)
# User defines sigma limits (k)

xbaruur <- function(x, k) {
  library(ggplot2)
  data <- data.frame(x)          # create data frame from user process data input
  rbar <- mean(data[,2])         # calculate r-bar based on ranges of subgroups
  n <- length(data[,1])          # calculate the number of subgroups
  d2 <- c(1.128, 1.693, 2.059, 2.326, 2.534,  # define d2 control chart parameters
          2.704, 2.847, 2.970, 3.078, 3.173,  # Montgomery's textbook
          3.258, 3.336, 3.407, 3.472, 3.532,
          3.588, 3.640, 3.689, 3.735, 3.778,
          3.819, 3.858, 3.895, 3.931)
  cl <- mean(data[,1])           # calculate centerline of x-bar chart based on mean of subgroups
  A2 <- k/(d2[n-1]*sqrt(n))      # calculate control chart parameter A2
  ucl <- cl + A2*rbar            # calculate upper control chart limit for x-bar chart
  lcl <- cl - A2*rbar            # calculate lower control chart limit for x-bar chart

  xplot <- ggplot(data, aes(x=seq(1:length(x)), y=data[,1])) +
    geom_point() +
    geom_hline(yintercept=cl) +
    geom_hline(yintercept=ucl, linetype="dashed", color = "red") +
    geom_hline(yintercept=lcl, linetype="dashed", color = "red") +
    labs(x="Sample",y="x's") +
    ggtitle("X-Bar Chart: Standards Unknown")
  xplot
}
