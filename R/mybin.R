#' Binomial Simulation Function
#'
#' Simulates repeated binomial experiments and produces a barplot of the distribution of successes.
#'
#' @param iter Number of iterations (simulated experiments).
#' @param n Number of trials per experiment.
#' @param p Probability of success on each trial.
#'
#' @return A table of relative frequencies of the number of successes across iterations.
#' @importFrom grDevices rainbow
#' @export
#'
#' @examples
#' mybin(iter=1000, n=10, p=0.7)
mybin <- function(iter, n, p){
  sam.mat = matrix(NA, nrow = n, ncol = iter, byrow = TRUE)
  succ=c()
  for(i in 1:iter){
    sam.mat[,i]=sample(c(1,0), n, replace=TRUE, prob=c(p,1-p))
    succ[i]=sum(sam.mat[,i])
  }
  succ.tab=table(factor(succ, levels=0:n))
  barplot(succ.tab/(iter), col=rainbow(n+1),
          main="Binomial simulation", xlab="Number of successes")
  succ.tab/iter
}
