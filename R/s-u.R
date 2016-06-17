# Create s chart for unknown standard deviation of process data
# User defines process standard deviation data set in string or data frame (x)
# User defines sigma limits (k)

su <- function(x, k) {
  library(ggplot2)
  data <- data.frame(x)           # create data frame from user process data input
  n <- length(data[,1])           # calculate the length of the variable
  c4 <- (4*(n-1))/(4*n-3)         # calculate control chart constant c4
  B3 = 1-(k/c4)*(sqrt(1-c4^2))    # calculate control chart constant B3
  B4 = 1+(k/c4)*(sqrt(1-c4^2))    # calculate control chart constant B4
  cl <- mean(data[,1])            # calculate centerline of s chart
  ucl <- B4*cl                    # calculate upper control chart limit for s chart
  lcl <- B3*cl                    # calculate lower control chart limit for s chart

  splot <- ggplot(data, aes(x=seq(1:length(x)), y=data[,1])) +
    geom_point() +
    geom_hline(yintercept=cl) +
    geom_hline(yintercept=ucl, linetype="dashed", color = "red") +
    geom_hline(yintercept=lcl, linetype="dashed", color = "red") +
    labs(x="Sample",y="s's") +
    ggtitle("s Chart: Standards Unknown")
  splot
}
