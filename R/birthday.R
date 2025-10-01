#' Birthday Problem Probability
#'
#' Computes the probability that two or more people in a group of size `x`
#' share the same birthday, assuming 365 equally likely birthdays and no leap years.
#'
#' @param x Integer vector. The sample size(s), i.e., the number of people in the group.
#'
#' @returns A numeric vector of probabilities, corresponding to each input value of `x`.
#' @export
#'
#' @examples
#' birthday(20)
#' birthday(20:25)
birthday <- function(x) {
  1 - exp(lchoose(365, x) + lfactorial(x) - x * log(365))
}
