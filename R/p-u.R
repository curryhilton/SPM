# Create p chart for unknown fraction of nonconforming of process data
# User defines process data set in string or data frame for the fraction of nonconforming (x)
# User defines sigma limits (k)

pu <- function(x, p, k) {
  library(ggplot2)
  data <- data.frame(x)              # create data frame from user process data input
  n <- length(data[,1])              # calculate the number of subgroups
  cl <- p/n                          # calculate p
  ucl <- cl + k*sqrt((cl*(1-cl))/n)  # calculate upper control chart limit for p chart
  lcl <- cl - k*sqrt((cl*(1-cl))/n)  # calculate lower control chart limit for p chart

  pplot <- ggplot(data, aes(x=seq(1:length(x)), y=data[,1])) +
    geom_point() +
    geom_hline(yintercept=cl) +
    geom_hline(yintercept=ucl, linetype="dashed", color = "red") +
    geom_hline(yintercept=lcl, linetype="dashed", color = "red") +
    labs(x="Sample",y="x's") +
    ggtitle("p Chart: Standards Unknown")
  pplot
}
