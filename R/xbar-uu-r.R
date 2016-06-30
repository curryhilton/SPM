#' X-bar chart for unknown mean and unknown standard deviation (range based) of process data
#' @param x is a data frame of values representing process data for subgroup mean (1) and range (2)
#' @param k is the sigma limits for the control chart
#' @param n is the number of observations in each subgroup
#' @return A X-bar control chart
#' @examples
#' xbaruur(warpbreaks$breaks, 3, 5)
#' @export
#' @import stats methods datasets ggplot2

xbaruur <- function(x, k, n) {
  data <- data.frame(x)                       # create data frame from user process data input
  rbar <- mean(data[,2])                      # calculate r-bar based on ranges of subgroups
  d2 <- c(1.128, 1.693, 2.059, 2.326, 2.534,  # define d2 control chart parameters
          2.704, 2.847, 2.970, 3.078, 3.173,  # Montgomery's textbook
          3.258, 3.336, 3.407, 3.472, 3.532,
          3.588, 3.640, 3.689, 3.735, 3.778,
          3.819, 3.858, 3.895, 3.931)
  cl <- mean(data[,1])                        # calculate centerline of x-bar chart based on mean of subgroups
  A2 <- k/(d2[n-1]*sqrt(n))                   # calculate control chart parameter A2
  ucl <- cl + A2*rbar                         # calculate upper control chart limit for x-bar chart
  lcl <- cl - A2*rbar                         # calculate lower control chart limit for x-bar chart

  plot <- ggplot(data, aes(x=seq(1:length(data[,1])), y=data[,1])) +
    geom_point(size=2, aes(color=data[,1]>ucl | data[,1]<lcl)) +
    scale_colour_manual(values=c("black", "red")) +
    guides(colour=FALSE) +
    geom_hline(yintercept=cl) +
    geom_hline(yintercept=ucl, linetype="dashed", color = "red") +
    geom_hline(yintercept=lcl, linetype="dashed", color = "red") +
    labs(x="Subgroup",y="X-Bar") +
    ggtitle("X-Bar Chart: Standards Unknown - R") +
    theme(plot.title = element_text(size = 16))
  plot
}
