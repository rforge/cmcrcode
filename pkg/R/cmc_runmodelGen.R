#' Estimate model using genetic algorithm rgenoud
#' @param globalEnv A reference to the global environment (.GlobalEnv)
#' @return Nothing. Model's results are loaded as a global variable.
#' @export
cmc_runmodelGen <- function(globalEnv){
  # Preparation
  if(sink.number()>0) sink()
  if(exists("model")) rm(model)
  starttime <<- Sys.time()
  
  # Loglike function from probabilities
  cmc_loglike <- function(theta){
    names(theta) <- rownames(theta_domain)
    L <- cmc_probabilities(theta, allalts=FALSE)
    if (cmc_control$replace0==2){
      count0estimation <<- sum(L==0)
      L = replace(L, L < 1e-323, 1e-323) # replace 0 if set by user
    }
    LL <- sum(log(L))
    return(LL)
  }
  
  # Search for better starting values
  if(cmc_control$startingvaluesloop>0) theta <- cmc_searchInitialTheta(cmc_loglike)
  
  # Prepare cluster
  if(cmc_control$nCores > 1){
    cl <<- makeCluster(cmc_control$nCores)
    cat('Cluster with',length(cl),'nodes created\n')
    clusterExport(cl, varlist=ls(globalEnv))
  }
  
  # Main estimation (no fixed parameters)
  cat("\n\n")
  cat("Starting main estimation\n")
  initial <<- round(theta,4)
  model <<- rgenoud::genoud(cmc_loglike, max=TRUE, nvars=length(theta),
                            starting.values= theta, Domains = theta_domain,
                            pop.size=1600, hessian=FALSE, BFGS=TRUE, #BFGSburnin=5,
                            max.generations=500, cluster=cl, print.level=2,
                            wait.generations=2)#, boundary.enforcement=1,
                            #solution.tolerance=0.001)#, gradient.check=FALSE)
  if(!exists('model')){
    cat('Error during estimation\n')
    stopCluster(cl)
    return()
  }
  
  # Calculation of covariance matrix
  cat("Computing covariance matrix (this may take a while)\n")
  cmc_loglike <- function(theta){
    L <- cmc_probabilities(theta, allalts=FALSE)
    if (cmc_control$replace0>0){
      count0covar <<- sum(L==0)
      L = replace(L, L < 1e-323, 1e-323) # replace 0 if set by user
    }
    LL <- log(L)
    return(LL)
  }
  sumLogLike <- function(k){
    i <- 0
    I <- 2+8*( k*(k+1)/2 )
    step <- ceiling(I/20)
    one <- ceiling(I/4); two <- ceiling(I/2); thr <- ceiling(3*I/4)
    
    function(theta){
      if(i==0) cat('0%')
      tmp <- sum( cmc_loglike(theta) )
      i <<- i+1
      if(i%%step == 0 & i!=one & i!=two & i!=thr) cat('.')
      if(i==one) cat('25%')
      if(i==two) cat('50%')
      if(i==thr) cat('75%')
      if(i==I) cat('100%\n')
      return(tmp)
    }
  }
  names(model$par) <<- rownames(theta_domain)
  model$hessian <<- numDeriv::hessian(func=sumLogLike(length(theta)),
                                      x=model$par)
  
  # Calculation of robust covariance matrix
  cat("Computing robust covariance matrix (this may also take a little while)\n")
  est    <<- model$par
  varcov <<- solve(-model$hessian)
  cmc_loglike <- function(theta) log(cmc_probabilities(theta))
  scores <- numDeriv::jacobian(cmc_loglike, x=model$par)
  #scores <- matrix(0, nrow=N, ncol=length(theta))
  #step <- ceiling(N/20); one <- ceiling(N/4); two <- ceiling(N/2); thr <- ceiling(3*N/4)
  #for(i in 1:N){
  #  if(i==0) cat('0%')
  #  cmc_loglike <- function(theta) log(cmc_probabilities(theta, allalts=FALSE)[i])
  #  scores[i, ] <- numDeriv::grad(cmc_loglike, x=model$par)
  #  if(i%%step == 0 & i!=one & i!=two & i!=thr) cat('.')
  #  if(i==one) cat('25%')
  #  if(i==two) cat('50%')
  #  if(i==thr) cat('75%')
  #  if(i==N) cat('100%\n')
  #}
  bread1 <- varcov
  #meat1  <- cov(scores)
  #gradient <- numDeriv::grad(function(theta) sum(cmc_loglike(theta)), x=model$par)
  meat1 <- matrix(0, nrow=length(model$gradients), ncol=length(model$gradients))
  for(i in 1:nrow(scores)) meat1 <- meat1 +
    ((scores[i,]-model$gradients)%o%(scores[i,]-model$gradients))/N
  meat1[is.na(meat1)]=0
  bread1[is.na(bread1)]=0
  robvarcov  <<- bread1 %*% meat1 %*% bread1
  se         <<- sqrt(diag(varcov))
  robse      <<- sqrt(diag(robvarcov))
  corrmat    <<- varcov/(se%*%t(se))
  robcorrmat <<- robvarcov/(robse%*%t(robse)) 
  
  # Closing
  if(exists('cl')) stopCluster(cl)
  endtime <<- Sys.time()
}