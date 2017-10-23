#' Calculates the probability of an ordered logit model
#' @param  Y vector of dependent variables
#' @param  X vector of a single explanatory variable (usually a latent variable)
#' @param  lambda Scalar parameter. Multiplies X in the utility of the ordered logit
#' @param  tau Vector of parameters. Thresholds.
#' @return A vector of probabilities, one for each element of Y.
#' @export
cmc_ologit <- function(Y, X, lambda, tau){
   tau <- c(-Inf,tau,Inf)
   V <- lambda*X
   p <- 1/(1 + exp(V-tau[Y+1])) - 1/(1 + exp(V-tau[Y]))
   return(p)
}