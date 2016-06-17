# Create u chart for known number of defects
# User defines process data set in string or data frame for the number of defects
# User defines known average number of defects (u)
# User defines the number of observations in subgroup (n)
# User defines sigma limits (k)

uk <- function(x, u, n, k) {
  library(ggplot2)
  data <- data.frame(x)              # create data frame from user process data input
  cl <- u                            # define u
  ucl <- cl + k*sqrt(cl/n)           # calculate upper control chart limit for u chart
  lcl <- cl - k*sqrt(cl/n)           # calculate lower control chart limit for u chart

  uplot <- ggplot(data, aes(x=seq(1:length(x)), y=data[,1])) +
    geom_point() +
    geom_hline(yintercept=cl) +
    geom_hline(yintercept=ucl, linetype="dashed", color = "red") +
    geom_hline(yintercept=lcl, linetype="dashed", color = "red") +
    labs(x="Sample",y="x's") +
    ggtitle("u Chart: Standards Known")
  uplot
}
