#' #' @keywords internal
#' @importFrom stats runif dnorm
"_PACKAGE"
#'
#' myclt_mean
#'
#' Generates sample means from a Uniform(a, b) distribution to demonstrate
#' the Central Limit Theorem and overlays the theoretical normal curve.
#'
#' @param n Sample size.
#' @param iter Number of iterations.
#' @param a Lower bound of the Uniform distribution.
#' @param b Upper bound of the Uniform distribution.
#'

#' @return A numeric vector of simulated sample means (invisible).
#' @export
#'
#' @examples
#' sum(myclt_mean(n = 10, iter = 10000, a = 0, b = 5))
myclt_mean <- function(n, iter, a = 0, b = 5) {
  x <- NULL
  y <- runif(n * iter, a, b)
  data <- matrix(y, nrow = n, ncol = iter, byrow = TRUE)
  means <- apply(data, 2, mean)
  hist(means, freq = FALSE,
       main = paste("Histogram of sample means, n =", n),
       xlab = "Sample mean")
  curve(dnorm(x, mean = (a + b)/2, sd = (b - a) / sqrt(12 * n)),
        add = TRUE, col = "red", lwd = 2)
  invisible(means)
}
