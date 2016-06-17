# Create x-bar chart for known mean and standard deviation of process data
# User defines process data set in string or data frame (x)
# User defines process mean (mu) and process standard deviation (s)
# User defines sigma limits (k)

xbarkk <- function(x, mu, s, k) {
  library(ggplot2)
  data <- data.frame(x)          # create data frame from user process data input
  cl <- mu                       # define centerline of x-bar chart based on known process mean
  A <- k/(sqrt(length(x)))       # calculate constant for A control chart parameter
  ucl <- cl + A*s                # calculate upper control chart limit for x-bar chart
  lcl <- cl - A*s                # calculate lower control chart limit for x-bar chart

  xplot <- ggplot(data, aes(x=seq(1:length(x)), y=data[,1])) +
    geom_point() +
    geom_hline(yintercept=cl) +
    geom_hline(yintercept=ucl, linetype="dashed", color = "red") +
    geom_hline(yintercept=lcl, linetype="dashed", color = "red") +
    labs(x="Sample",y="x's") +
    ggtitle("X-Bar Chart: Standards Known")
  xplot
}
