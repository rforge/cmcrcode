#' Search for better starting values by taking random pints around the user-defined starting point
#' @param logLike log-likelihood function, must return a negative scalar
#' @return The best theta found
cmc_searchInitialTheta <- function(logLike){
  
  cat("Searching for starting values\n")
  # generate initial LL (using base starting values)
  theta_best <- theta_start
  current_LL=sum(logLike(theta_start))
  cat("Iteration 0 LL=",current_LL,"\n")
  # now iterate to try different values
  i=1
  while(i<(cmc_control$startingvaluesloop+1))
  {
    theta_test=lowerlimits+runif(length(lowerlimits))*(upperlimits-lowerlimits)
    LL_test=sum(logLike(theta_test))
    if(LL_test>current_LL)
    {
      current_LL= LL_test
      theta_best= theta_test
    }
    cat("Iteration",i,"LL=",LL_test," (best so far:",current_LL,")\n")
    i=i+1
  }
  
  return(theta_best)
  
}