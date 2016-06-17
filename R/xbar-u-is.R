# Create individual x-bar chart for unknown mean and standard deviation of process data using standard deviation
# User defines process data set in string or data frame (x)
# User defines sigma limits (k)
# User defines number of lag periods for difference (i)

xis <- function(x, k, i) {
  library(ggplot2)
  data <- data.frame(x)              # create data frame from user process data input
  n <- length(data[,1])              # calculate the length of the variable
  s <- sd(data[,1])                  # calculate the standard deviation of variable
  c4 <- (4*(n-1))/(4*n-3)            # calculate control chart constant c4
  cl <- mean(data[,1])               # calculate the centerline for the IMR chart
  ucl <- cl + k*(s/c4)               # calculate upper control chart limit for IMR chart
  lcl <- cl - k*(s/c4)               # calculate lower control chart limit for IMR chart

  IMRplot <- ggplot(data, aes(x=seq(1:length(x)), y=data[,1])) +
    geom_point() +
    geom_hline(yintercept=cl) +
    geom_hline(yintercept=ucl, linetype="dashed", color = "red") +
    geom_hline(yintercept=lcl, linetype="dashed", color = "red") +
    labs(x="Sample",y="IMR") +
    ggtitle("IMR Chart: Standards Unknown")
  IMRplot
}
