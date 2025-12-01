#' Bootstrap Confidence Interval
#'
#' @param iter Number of bootstrap samples.
#' @param x Numeric sample data.
#' @param fun Function name (as character) for the statistic.
#' @param alpha Significance level for CI.
#' @param cx Text size for labels.
#' @param ... Extra graphical options.
#'
#' @return List with bootstrap CI, statistic name, data, and resampled values.
#' @examples
#' set.seed(39)
#' x <- rnorm(25, 25, 10)
#' myboot2(10000, x, fun = "mean", alpha = 0.05, col = "purple")
#' @importFrom stats quantile
#' @importFrom graphics segments
#' @export
myboot2 <- function(iter = 10000, x, fun = "mean", alpha = 0.05, cx = 1.5, ...) {
  n <- length(x)
  y <- sample(x, n * iter, replace = TRUE)
  rs.mat <- matrix(y, nrow = n, ncol = iter, byrow = TRUE)
  xstat <- apply(rs.mat, 2, fun)
  ci <- quantile(xstat, c(alpha / 2, 1 - alpha / 2))
  para <- hist(xstat, freq = FALSE, las = 1,
               main = paste("Bootstrap CI\nalpha =", alpha, "iter =", iter), ...)
  mat <- matrix(x, nrow = length(x), ncol = 1, byrow = TRUE)
  pte <- apply(mat, 2, fun)
  abline(v = pte, lwd = 3, col = "black")
  segments(ci[1], 0, ci[2], 0, lwd = 4)
  text(ci[1], 0, paste("(", round(ci[1], 2), sep = ""), col = "red", cex = cx)
  text(ci[2], 0, paste(round(ci[2], 2), ")", sep = ""), col = "red", cex = cx)
  text(pte, max(para$density) / 2, round(pte, 2), cex = cx)
  invisible(list(ci = ci, fun = fun, x = x, xstat = xstat))
}
