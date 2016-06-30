#' R-bar chart for known standard deviation of process data
#' @param x is vector of values representing process data for subgroup ranges
#' @param sigma is the known process standard deviation
#' @param k is the sigma limits for the control chart
#' @param n is the number of observations in each subgroup
#' @return A R-bar control chart
#' @examples
#' rbark(warpbreaks$breaks, 13, 3, 5)
#' @export
#' @import stats methods datasets ggplot2

rbark <- function(x, sigma, k, n) {
  data <- data.frame(x)          # create data frame from user process data input
  d2 <- c(1.128, 1.693, 2.059, 2.326, 2.534,  # define d2 control chart parameters
          2.704, 2.847, 2.970, 3.078, 3.173,  # Montgomery's textbook
          3.258, 3.336, 3.407, 3.472, 3.532,
          3.588, 3.640, 3.689, 3.735, 3.778,
          3.819, 3.858, 3.895, 3.931)
  d3 <- c(0.853, 0.888, 0.880, 0.864, 0.848,  # define d3 control chart parameters
          0.833, 0.820, 0.808, 0.797, 0.787,  # Montgomery's textbook
          0.778, 0.770, 0.763, 0.756, 0.750,
          0.744, 0.739, 0.734, 0.729, 0.724,
          0.720, 0.716, 0.712, 0.708)
  D1 = d2[n-1] - k*d3[n-1]       # calculate control chart constant D1
  D2 = d2[n-1] + k*d3[n-1]       # calculate control chart constant D2
  cl <- d2[n-1]*sigma            # calculate centerline of R-bar chart
  ucl <- D2*sigma                # calculate upper control chart limit for R-bar chart
  lcl <- D1*sigma                # calculate lower control chart limit for R-bar chart

  plot <- ggplot(data, aes(x=seq(1:length(data[,1])), y=data[,1])) +
    geom_point(size=2, aes(color=data[,1]>ucl | data[,1]<lcl)) +
    scale_colour_manual(values=c("black", "red")) +
    guides(colour=FALSE) +
    geom_hline(yintercept=cl) +
    geom_hline(yintercept=ucl, linetype="dashed", color = "red") +
    geom_hline(yintercept=lcl, linetype="dashed", color = "red") +
    labs(x="Subgroup",y="X-Bar") +
    ggtitle("R-Bar Chart: Standards Known") +
    theme(plot.title = element_text(size = 16))
  plot
}
