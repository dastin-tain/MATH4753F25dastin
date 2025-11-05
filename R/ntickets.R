#' ntickets - Overbooking optimization
#'
#' Calculates the optimal number of tickets to sell for a flight with N seats,
#' given a show-up probability and an acceptable overbooking probability.
#' Produces discrete (Binomial) and continuous (Normal approximation) results and plots.
#'
#' @param N Number of seats for cheeks available on the flight.
#' @param gamma Probability that the flight will be overbooked
#' @param p Probability that a passenger shows up.
#'
#' @importFrom stats pbinom uniroot
#' @importFrom graphics lines abline
#'
#' @returns list:
#' \itemize{
#'   \item \code{nd} - tickets for discrete cheeks
#'   \item \code{nc} - tickets for continuous cheeks
#'   \item \code{N} - number of seats for cheeks
#'   \item \code{p} - decent human probability
#'   \item \code{gamma} - PAIN probability
#'   }
#'
#' @export
#'
#' @examples
#' ntickets(N = 400, gamma = 0.02, p = 0.95)
ntickets <- function(N, gamma, p) {

  n_range <- seq(N - 10, N + 20, by = 1)  # search window
  obj_discrete <- 1 - gamma - pbinom(N, size = n_range, prob = p)
  nd_index <- which.min(abs(obj_discrete))
  nd <- n_range[nd_index]


  f_cont <- function(n)
    1 - gamma - pnorm(N + 0.5, mean = n * p, sd = sqrt(n * p * (1 - p)))

  nc <- uniroot(f_cont, c(N - 10, N + 20))$root

  par(mfrow = c(2, 1))

  plot(n_range, abs(obj_discrete), pch = 19, col = "blue",
       main = paste0("Objective Vs n to find optimal tickets sold\n(",
                     nd, ") gamma= ", gamma, " N=", N, " discrete"),
       xlab = "n", ylab = "Objective")
  lines(n_range, abs(obj_discrete))
  abline(v = nd, col = "red", lwd = 3)
  abline(h = 0, col = "red", lwd = 3)

  n_cont <- seq(N - 10, N + 20, length.out = 200)
  obj_cont <- sapply(n_cont, f_cont)
  plot(n_cont, abs(obj_cont), type = "l",
       main = paste0("Objective Vs n to find optimal tickets sold\n(",
                     round(nc, 6), ") gamma= ", gamma, " N=", N, " continuous"),
       xlab = "n", ylab = "Objective")
  abline(v = nc, col = "blue", lwd = 3)
  abline(h = 0, col = "blue", lwd = 3)

  result <- list(nd = nd, nc = nc, N = N, p = p, gamma = gamma)
  print(result)
  return(result)
}
