# Create np chart for known fraction of nonconforming of process data
# User defines process data set in string or data frame for the fraction of nonconforming (x)
# User defines known fraction of nonconforming (p)
# User defines the number of samples in subgroup (n)
# User defines sigma limits (k)

npu <- function(x, n, k) {
  library(ggplot2)
  data <- data.frame(x)              # create data frame from user process data input
  m <- length(data[,1])              # calculate the number of subgroups
  p <- mean(data[,1])                # calculate the mean of the fraction of nonconforming
  cl <- n*p                          # calculate n*p
  ucl <- cl + k*sqrt(cl*(1-cl))      # calculate upper control chart limit for np chart
  lcl <- cl - k*sqrt(cl*(1-cl))      # calculate lower control chart limit for np chart

  pplot <- ggplot(data, aes(x=seq(1:length(x)), y=data[,1])) +
    geom_point() +
    geom_hline(yintercept=cl) +
    geom_hline(yintercept=ucl, linetype="dashed", color = "red") +
    geom_hline(yintercept=lcl, linetype="dashed", color = "red") +
    labs(x="Sample",y="x's") +
    ggtitle("np Chart: Standards Unknown")
  pplot
}
