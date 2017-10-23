#' Runs the cmc_probablities function several times on different number of cores and draws, and reports how long it takes.
#' @param  nDrawsTry Vector of number of draws to try (ignored if cmc_control$mixing==0)
#' @param  nCoresTry Vector of number of cores to try (defaults to 1:maximum_number_of_cores_detected)
#' @param  nRep Scalar. Number of times cmc_probabilities is calculated (defaults to 30)
#' @return A matrix indicating the time taken to estimate nRep times the cmc_probabilities function.
#' @export
cmc_speedTest <- function(nDrawsTry=c(10,50,100), nCoresTry=NA, nRep=30){
  
  # Checks input
  if(cmc_control$mixing==1){
    if(anyNA(cmc_inter_draws) | anyNA(cmc_intra_draws)){
      stop('cmc_inter_draws and cmc_intra_draws must be defined\n', call.=FALSE)
    }
  } else nDrawsTry <- 1
  if(anyNA(nCoresTry)) nCoresTry <- 1:parallel::detectCores()
  stepPrint <- floor(nRep/5)
  nErrors <- 0
  
  # Stores original cmc_control in temporal variable
  cmc_control_original <- cmc_control
  if(cmc_control$mixing==1){
    cmc_inter_draws_tmp  <- cmc_inter_draws
    cmc_intra_draws_tmp  <- cmc_intra_draws
    drawsL_original      <- drawsL
  }
  
  # Loglikelihood function for multicore
  cmc_loglike <- function(theta){
    LL <- parallel::clusterCall(cl=cl, cmc_probabilities, theta=theta)
    LL <- unlist(LL) # turn list of vector into one vector
    LL <- log(LL)
    return(LL)
  }
  
  # Create the random thetas
  thetaL <- list()
  for(i in 1:nRep) thetaL[[i]] <- theta_start + 0.1*runif(length(theta_start))
  
  # Create list to store times and function to convert time to number of seconds
  timeTaken <- matrix(-1, nrow=length(nCoresTry), ncol=length(nDrawsTry))
  
  for(nCores in nCoresTry){
    for(nDraws in nDrawsTry){
      # Create draws
      if(cmc_control$mixing>0){
        if(cmc_inter_draws_tmp$nDraws>0) cmc_inter_draws_tmp$nDraws <- nDraws
        if(cmc_intra_draws_tmp$nDraws>0) cmc_intra_draws_tmp$nDraws <- nDraws
        capture.output( drawsL <<- cmc_makeDraws(cmc_inter_draws_tmp, cmc_intra_draws_tmp) )
        tmp <- dim(drawsL[[1]])[2]
        cat('nInterDraws=',ifelse(tmp==1,0,tmp),sep='')
        tmp <- dim(drawsL[[1]])[3]
        cat(' nIntraDraws=',ifelse(tmp==1,0,tmp),sep='')
      }
      
      # Create cluster
      cmc_control$nCores <<- nCores
      #cmc_control$nCores <- nCores
      capture.output(cmc_prepareCluster())
      cat(' nCores=', length(cl), sep='')
      
      # Warm up
      sum(cmc_loglike(theta_start))
      
      # Start timer
      starttime <- Sys.time()
      
      # Calculate cmc_loglike repeatedly
      nErrors <- 0
      cat(' ')
      for(i in 1:nRep){
        LL_test <- sum(cmc_loglike(thetaL[[i]]))
        if(!is.finite(LL_test)) nErrors <- nErrors + 1
        if(i%%stepPrint==0) cat('.')
      }; cat(' ')
      
      # Stops timer and saves time
      endtime <- Sys.time()
      nC <- which(nCoresTry==nCores)
      nD <- which(nDrawsTry==nDraws)
      timeTaken[nC,nD] <- as.numeric(difftime(endtime,starttime,units='secs'))
      cat(round(timeTaken[nC,nD],2),'sec','\n')
      
      # Closes cluster
      parallel::stopCluster(cl)
    }
  }
  
  # Name columns of matrix containing results
  colnames(timeTaken) <- paste('draws', nDrawsTry, sep='')
  rownames(timeTaken) <- paste('cores', nCoresTry, sep='')
  
  # Get the average time of execution per call to cmc_probabilities
  timeTaken <- timeTaken/nRep
  
  # plot results
  matplot(x=nCoresTry, y=timeTaken,
          type='o', lty=1, lwd=2, cex=1.5, fg='gray',
          lab=c(length(nCoresTry), length(nDrawsTry), 1),
          pch=1:length(nDrawsTry), col=1:length(nDrawsTry),
          xlab='Number of cores', ylab='Time (sec)') # plot
  legend("topright", legend = colnames(timeTaken),
         pch=1:length(nDrawsTry), col=1:length(nDrawsTry), lty=1,
         bg='transparent', bty='n', horiz=TRUE) # legend
  
  cmc_control <<- cmc_control_original
  if(cmc_control$mixing==1) drawsL <<- drawsL_original
  
  return(timeTaken)
}
