#' myncurve
#'
#' Plots a normal curve and shades the area from -Inf to x = a.
#'
#' @param mu Mean of the normal distribution.
#' @param sigma Standard deviation of the normal distribution.
#' @param a The value of x up to which the area is shaded.
#'
#' @return A list with mu, sigma, and the probability P(X <= a).
#' @importFrom graphics curve polygon text
#' @importFrom stats dnorm pnorm
#' @export
#'
#' @examples
#' myncurve(mu = 10, sigma = 5, a = 6)
myncurve <- function(mu, sigma, a) {
  x <- NULL

  curve(dnorm(x, mean = mu, sd = sigma),
        xlim = c(mu - 4 * sigma, mu + 4 * sigma),
        ylab = "Density",
        main = paste("Normal(", mu, ",", sigma, "^2)", sep = ""),
        col = "blue", lwd = 2)

  xshade <- seq(mu - 4 * sigma, a, length = 1000)
  yshade <- dnorm(xshade, mean = mu, sd = sigma)

  polygon(c(mu - 4 * sigma, xshade, a),
          c(0, yshade, 0),
          col = "lightblue")

  prob <- pnorm(a, mean = mu, sd = sigma)
  prob <- round(prob, 4)

  text(x = a, y = max(yshade) * 0.8,
       labels = paste("P(X <=", a, ") =", prob),
       pos = 4, col = "black")

  return(list(mu = mu, sigma = sigma, prob = prob))
}
