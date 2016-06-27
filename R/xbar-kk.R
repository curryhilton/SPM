#' X-bar chart for known mean and standard deviation of process data
#' @param x is a vector of values representing process data
#' @param mu is the process mean
#' @param sigma is the process standard deviation
#' @param k is the sigma limits for the control chart
#' @return A X-bar control chart
#' @examples
#' xbarkk(warpbreaks$breaks, 28, 13, 3)
#' @export
#' @import stats methods datasets ggplot2

xbarkk <- function(x, mu, sigma, k) {
  data <- data.frame(x)          # create data frame from user process data input
  cl <- mu                       # define centerline of x-bar chart based on known process mean
  A <- k/(sqrt(length(x)))       # calculate constant for A control chart parameter
  ucl <- cl + A*sigma            # calculate upper control chart limit for x-bar chart
  lcl <- cl - A*sigma            # calculate lower control chart limit for x-bar chart

  xplot <- ggplot(data, aes(x=seq(1:length(x)), y=data[,1])) +
    geom_point(size=2, aes(color=x>ucl | x<lcl)) +
    scale_colour_manual(values=c("black", "red")) +
    guides(colour=FALSE) +
    geom_hline(yintercept=cl) +
    geom_hline(yintercept=ucl, linetype="dashed", color = "red") +
    geom_hline(yintercept=lcl, linetype="dashed", color = "red") +
    labs(x="Subgroup",y="X-Bar") +
    ggtitle("X-Bar Chart: Standards Known") +
    theme(plot.title = element_text(size = 16))
  xplot
}
