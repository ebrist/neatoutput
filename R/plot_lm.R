
#' Plot Residuals from Linear Models
#'
#' @description produces a grid of three plots: 
#'  (1) residuals vs predicted values
#'  (2) normal qq plot
#'  (3) histogram of residuals with normal curve
#'
#' @param fit a linear model object produced by lm.
#' @param point_size point size for residuals vs fitted plot
#' @param line_color line color for residual guides
#' @param line_size line size for residual guides
#' @param binwidth binwidth for histogram
#' @param hist_fill fill color for histogram
#' @param hist_color line color for histogram
#' @param hist_alpha alpha (transparency) for histogram
#' @param hist_curve_color normal curve color for histogram
#' @param hist_curve_size normal curve size for histogram
#'
#' @return a gtable of residual plots
#' @export
#'
#' @examples
#' lm_fit <- lm(mpg ~ ., data = mtcars)
#' plot_lm(lm_fit)
#' 
plot_lm <- function(fit, point_size = 1, line_color = "black", line_size = 1,
                    binwidth = NULL, hist_fill = "white", hist_color = "black", 
                    hist_alpha = 1, hist_curve_color = "black", hist_curve_size = 1) {
  # extract values from fit
  fitted <- unname(fit$fitted.values)
  rstud <- unname(rstandard(fit)) 
  qq_x <- qqnorm(rstud, plot.it = F)[[1]]
  qq_y <- qqnorm(rstud, plot.it = F)[[2]]
  mean <- 0
  rstud_sd <- sd(rstud)
  n <- length(rstud)
  bw <- ifelse(is.null(binwidth), 2 * IQR(rstud) * n^(-1 / 3), binwidth)
  # fitted vs residuals
  g1 <- ggplot(data.frame(x = fitted, y = rstud), aes(x, y)) +
    geom_point(na.rm = TRUE, size = point_size) + 
    theme_bw() +
    labs(title = "Residuals vs Fitted Values",
         x = "Fitted Values", y = "Studentized Residuals") +
    annotate("segment", x = -Inf, xend = Inf, y = -3, yend = -3,
             color = line_color, linetype = "dashed", size = line_size) +
    annotate("segment", x = -Inf, xend = Inf, y = 3, yend = 3,
             color = line_color, linetype = "dashed", size = line_size) 
  # normal q-q
  g2 <- ggplot(data.frame(x = qq_x, y = qq_y), aes(x, y)) +
    geom_point(na.rm = TRUE, size = point_size) + 
    stat_qq_line(aes(sample = qq_y), size = 1) +
    theme_bw() +
    labs(title = "Normal Q-Q",
         x = "Theoretical Quantiles", y = "Studentized Residuals") +
    scale_x_continuous(breaks = seq(-3, 3, by = 1))
  # histogram of residuals
  x_min <- min(-3.5 * rstud_sd, min(rstud) - 0.1)
  x_max <- max(3.5 * rstud_sd, max(rstud) + 0.1)
  g3 <- ggplot(data.frame(x = rstud), aes(x = x)) +
    geom_histogram(binwidth = bw, fill = hist_fill, color = hist_color,
                   alpha = hist_alpha) +
    theme_bw() +
    stat_function(
      fun = function(x, mean, sd, n, bw) {
        dnorm(x = x, mean = mean, sd = sd) * n * bw
      }, args = c(mean = mean, sd = rstud_sd, n = n, bw = bw), 
      size = hist_curve_size, color = hist_curve_color) +
    labs(title = "Histogram of Residuals", 
         x = "Studentized Residuals", y = "Count") +
    xlim(x_min, x_max)
  # arrange in grid
  grid.arrange(g1, g2, g3, layout_matrix = rbind(c(1, 2, 3)))
}

