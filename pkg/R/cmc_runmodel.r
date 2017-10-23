#' Estimate model
#' @return Nothing. Model's results are loaded as a global variable.
#' @export
cmc_runmodel=function()
{
 if(sink.number()>0) sink()
 
 if(cmc_control$nCores>1) cmc_prepareCluster()
 
 if(exists("model")) rm(model)
 starttime<<-Sys.time()
 
 # Create loglike function from cmc_probability, this is for use in estimation, different version for covariance below
 if(cmc_control$nCores>1){
   cmc_loglike <- function(theta){
     L <- parallel::clusterCall(cl=cl, cmc_probabilities, theta=theta)
     L <- unlist(L) # turn list of vector into one vector
     if (cmc_control$replace0==2){
       count0estimation <<- sum(L==0)
       L = replace(L, L < 1e-323, 1e-323) # replace 0 if set by user 
     } 
     LL <- log(L)
     writeBeta(theta,sum(LL))
     return(LL)
   }
 } else {
   cmc_loglike <- function(theta){
     L <- cmc_probabilities(theta)
     if (cmc_control$replace0==2){
       count0estimation <<- sum(L==0)
       L = replace(L, L < 1e-323, 1e-323) # replace 0 if set by user
     }
     LL <- log(L)
     writeBeta(theta,sum(LL))
     return(LL)
   }
 }
 
 # starting values loop
 if(cmc_control$startingvaluesloop>0){
   writeBetaToFile <<- FALSE # Used to write beta.csv
   theta_start <<- cmc_searchInitialTheta(cmc_loglike)
 }
 
 # main estimation
 cat("\n\n")
 cat("Starting main estimation\n")
 initial<<-round(theta_start,4)
 writeBetaToFile <<- TRUE # Used to write beta.csv
 if(cmc_control$bhhh==1){
   model<<-maxLik::maxLik(cmc_loglike, start=theta_start, fixed=fixedparams,
                          method="bhhh", print.level=3, finalHessian=FALSE)
 } else {
   model<<-maxLik::maxLik(cmc_loglike, start=theta_start, fixed=fixedparams,
                          method="bfgs", print.level=3, finalHessian=FALSE)
 }
 
 cmc_loglike(lastFuncParam) # Writes last iter to beta.csv
 writeBetaToFile <<- FALSE # Used to write beta.csv
 if(exists("model")) {
   if(model$code==0) {
     cat("Computing covariance matrix (this may take a while)\n")
	 
	 # Create loglike function from cmc_probability, this is for use in covariance calculation, different version for estimation above
	 if(cmc_control$nCores>1){
	   cmc_loglike <- function(theta){
	     L <- parallel::clusterCall(cl=cl, cmc_probabilities, theta=theta)
	     L <- unlist(L) # turn list of vector into one vector
	     if (cmc_control$replace0>0){
	       count0covar <<- sum(L==0)
	       L = replace(L, L < 1e-323, 1e-323) # replace 0 if set by user
	     }
	     LL <- log(L)
	     #writeBeta(theta,sum(LL))
	     return(LL)
	   }
	 } else {
	   cmc_loglike <- function(theta){
	     L <- cmc_probabilities(theta)
	     if (cmc_control$replace0>0){
	       count0covar <<- sum(L==0)
	       L = replace(L, L < 1e-323, 1e-323) # replace 0 if set by user
	     }
	     LL <- log(L)
	     #writeBeta(theta,sum(LL))
	     return(LL)
	   }
	 }
	 
	 # Create closure to keep track of the number of times cmc_loglike is called
	 sumLogLike <- function(k){
       i <- 0
       I <- 2+8*( k*(k+1)/2 )
       step <- ceiling(I/20)
       one <- ceiling(I/4)
       two <- ceiling(I/2)
       thr <- ceiling(3*I/4)
       
       function(theta){
         if(i==0) cat('0%')
         tmp <- sum( cmc_loglike(theta) )
         i <<- i+1
         if(i==one) cat('25%') else {
           if(i==two) cat('50%') else {
             if(i==thr) cat('75%') else{
               if(i==I) cat('100%\n') else {
                 if(i%%step == 0) cat('.')
               }
             }
           }
         }
         return(tmp)
       }
    } 
	 
	 # Extract estimated values from the model
	 est<<-model$estimate
	 
	 # Estimation of the Hessian
	 model$hessian <<- numDeriv::hessian(func = sumLogLike(length(theta_start)),
                                         x = est)
										 
	 # Calculate regular and robust s.e.
     varcov <<- vcov(model)
     meat1  = sandwich::meat(model)
     bread1 = sandwich::bread(model)
     meat1[is.na(meat1)]   = 0
     bread1[is.na(bread1)] = 0
     robvarcov <<- sandwich::sandwich(model,bread1,meat1)
     se    <<- sqrt(diag(varcov))
     robse <<- sqrt(diag(robvarcov))
	 
	 # Set s.e. of fixed parameters to NA
     se[model$fixed]         = NA
     robse[model$fixed]      = NA
     varcov[model$fixed,]    = NA
     varcov[,model$fixed]    = NA
     robvarcov[model$fixed,] <<- NA
     robvarcov[,model$fixed] <<- NA
	 
	 # Calculate the covariance and correlation matrices
     corrmat    <<- varcov/(se%*%t(se))
     robcorrmat <<- robvarcov/(robse%*%t(robse)) 
     
   } else {
     cat("No covariance matrix to compute\n")
   }
 }
 # Closes clusters if using multicore
 if(exists('cl') & cmc_control$nCores>1) parallel::stopCluster(cl)
 
 endtime<<-Sys.time()
}
