# Create c chart for known number of defects
# User defines process data set in string or data frame for the number of defects
# User defines sigma limits (k)

cu <- function(x, n, k) {
  library(ggplot2)
  data <- data.frame(x)              # create data frame from user process data input
  c <- mean(data[,1])                # calculate the mean of the number of defects
  cl <- c                            # calculate c
  ucl <- cl + k*sqrt(cl)             # calculate upper control chart limit for c chart
  lcl <- cl - k*sqrt(cl)             # calculate lower control chart limit for c chart

  cplot <- ggplot(data, aes(x=seq(1:length(x)), y=data[,1])) +
    geom_point() +
    geom_hline(yintercept=cl) +
    geom_hline(yintercept=ucl, linetype="dashed", color = "red") +
    geom_hline(yintercept=lcl, linetype="dashed", color = "red") +
    labs(x="Sample",y="x's") +
    ggtitle("c Chart: Standards Unknown")
  cplot
}
