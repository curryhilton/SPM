# Create u chart for unknown number of defects
# User defines process data set in string or data frame for the average number of defects (x)
# User defines the number of observations in subgroup (n)
# User defines sigma limits (k)

uu <- function(x, n, k) {
  library(ggplot2)
  data <- data.frame(x)              # create data frame from user process data input
  u <- mean(data[,1])                # calculate the mean of the number of defects
  cl <- u                            # calculate u
  ucl <- cl + k*sqrt(cl/n)           # calculate upper control chart limit for u chart
  lcl <- cl - k*sqrt(cl/n)           # calculate lower control chart limit for u chart

  uplot <- ggplot(data, aes(x=seq(1:length(x)), y=data[,1])) +
    geom_point() +
    geom_hline(yintercept=cl) +
    geom_hline(yintercept=ucl, linetype="dashed", color = "red") +
    geom_hline(yintercept=lcl, linetype="dashed", color = "red") +
    labs(x="Sample",y="x's") +
    ggtitle("u Chart: Standards Unknown")
  uplot
}
