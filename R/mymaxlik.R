#' Maximum likelihood by grid search
#'
#' @param lfun Log-likelihood function of the form lfun(x, param).
#' @param x Data vector.
#' @param param Grid of parameter values.
#' @param ... Additional graphical arguments passed to plot.
#'
#' @return A list with the MLE and log-likelihood value.
#'
#' @importFrom graphics plot abline points axis
#' @export
mymaxlik <- function(lfun, x, param, ...) {
  z <- outer(x, param, lfun)
  y_ll <- apply(z, 2, sum)
  plot(param, y_ll, type = "l", lwd = 2, col = "blue",
       ylab = "Log-likelihood", ...)
  i <- max(which(y_ll == max(y_ll)))
  abline(v = param[i], lwd = 2, col = "red")
  points(param[i], y_ll[i], pch = 19)
  axis(3, param[i], round(param[i], 3))
  list(i = i, param_hat = param[i], loglik = y_ll[i])
}
