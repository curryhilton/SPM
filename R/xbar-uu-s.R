# Create x-bar chart for unknown mean and standard deviation (using s) of process data
# User defines process data set in string or data frame (x) which includes subgroup mean (1) and standard deviation (2)
# User defines sigma limits (k)

xbaruus <- function(x, k) {
  library(ggplot2)
  data <- data.frame(x)              # create data frame from user process data input
  sbar <- mean(data[,2])             # calculate s-bar based on standard deviation of subgroups
  n <- length(data[,1])              # calculate the number of subgroups
  c4 <- (4*(n-1))/(4*n-3)            # calculate control chart constant c4
  cl <- mean(data[,1])               # calculate x-bar baed on mean of subgroups
  ucl <- cl + (k*sbar)/(c4*sqrt(n))  # calculate upper control chart limit for x-bar chart
  lcl <- cl - (k*sbar)/(c4*sqrt(n))  # calculate lower control chart limit for x-bar chart

  xplot <- ggplot(data, aes(x=seq(1:length(x)), y=data[,1])) +
    geom_point() +
    geom_hline(yintercept=cl) +
    geom_hline(yintercept=ucl, linetype="dashed", color = "red") +
    geom_hline(yintercept=lcl, linetype="dashed", color = "red") +
    labs(x="Sample",y="x's") +
    ggtitle("X-Bar Chart: Standards Unknown")
  xplot
}
