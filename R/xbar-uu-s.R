#' X-bar chart for unknown mean and unknown standard deviation (s based) of process data
#' @param x is a data frame of values representing process data for subgroup mean (1) and standard deviation (2)
#' @param k is the sigma limits for the control chart
#' @param n is the number of observations in each subgroup
#' @return A X-bar control chart
#' @examples
#' xbaruus(warpbreaks$breaks, 3, 5)
#' @export
#' @import stats methods datasets ggplot2

xbaruus <- function(x, k, n) {
  library(ggplot2)
  data <- data.frame(x)                # create data frame from user process data input
  sbar <- mean(data[,2])               # calculate s-bar based on standard deviation of subgroups
  c4 <- (4*(n-1))/(4*n-3)              # calculate control chart constant c4
  cl <- mean(data[,1])                 # calculate x-bar baed on mean of subgroups
  ucl <- cl + (k*sbar)/(c4*sqrt(n))    # calculate upper control chart limit for x-bar chart
  lcl <- cl - (k*sbar)/(c4*sqrt(n))    # calculate lower control chart limit for x-bar chart

  plot <- ggplot(data, aes(x=seq(1:length(data[,1])), y=data[,1])) +
    geom_point(size=2, aes(color=data[,1]>ucl | data[,1]<lcl)) +
    scale_colour_manual(values=c("black", "red")) +
    guides(colour=FALSE) +
    geom_hline(yintercept=cl) +
    geom_hline(yintercept=ucl, linetype="dashed", color = "red") +
    geom_hline(yintercept=lcl, linetype="dashed", color = "red") +
    labs(x="Subgroup",y="X-Bar") +
    ggtitle("X-Bar Chart: Standards Unknown - s") +
    theme(plot.title = element_text(size = 16))
  plot
}
