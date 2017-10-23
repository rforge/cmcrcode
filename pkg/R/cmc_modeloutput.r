#' Prints to screen the output of a model previously estimated by runmodel()
#' @param  model The model estimated using runmodel()
#' @return A matrix of coefficients, s.d. and t-tests
#' @export
cmc_modeloutput=function(model)
{
  cmcRcode_version <- "2.0.4"
  nParams <- length(theta_start)
  
  trat_0=est/se
  trat_0[model$fixed]=NA
  robtrat_0=est/robse
  trat_1=(est-1)/se
  trat_1[model$fixed]=NA
  robtrat_1=(est-1)/robse
  robtrat_0[model$fixed]=NA
  robtrat_1[model$fixed]=NA
  finalLL=model$maxim
  iterations=model$iterations
  params=nParams-sum(model$fixed)
  est=round(est,4)
  se=round(se,4)
  trat_0=round(trat_0,2)
  trat_1=round(trat_1,2)
  robse=round(robse,4)
  robtrat_0=round(robtrat_0,2)
  robtrat_1=round(robtrat_1,2)
  if(cmc_control$includetrat1==1) output=t(rbind(est,se,trat_0,trat_1,robse,robtrat_0,robtrat_1))
  if(cmc_control$includetrat1==0) output=t(rbind(est,se,trat_0,robse,robtrat_0))
  
  varcovoutput    <<- varcov
  robvarcovoutput <<- robvarcov
  
  estforoutput    <<- cbind(est,se,robse,initial)
  
  cat("Model run using CMC choice modelling code for R, version",cmcRcode_version,"\n")
  cat("www.cmc.leeds.ac.uk","\n")
  cat("Model run at: ",paste(starttime),"\n\n")
  
  cat("Model name: ",paste(cmc_control$modelname),"\n\n")
  
  cat("Model diagnosis:",model$message,"\n\n")
  
  cat("Number of decision makers:",N,"\n")
  cat("Number of observations:",choicetasks,"\n\n")
  
  if(exists('LL0')) if(LL0<0) cat("LL(0): ",LL0,"\n")
  cat("LL(final): ",finalLL,"\n")
  if(cmc_control$hybrid==1) cat("LL(choice model): ",sum(LLchoice),"\n")
  cat("Estimated parameters: ",nParams-length(fixedparams),"\n\n")
  if(exists('LL0')) if(LL0<0) cat("Rho-sq (0): ",round(1-(finalLL/LL0),2),"\n")
  if(exists('LL0')) if(LL0<0) cat("Adj. rho-sq (0): ",round(1-((finalLL-nParams+length(fixedparams))/LL0),2),"\n")
  if(exists('LLC')) if(LLC<0) cat("Rho-sq (C): ",round(1-(finalLL/LLC),2),"\n")
  if(exists('LLC')) if(LLC<0) cat("Adj. rho-sq (C): ",round(1-((finalLL-nParams+length(fixedparams))/LLC),2),"\n")
  cat("AIC: ",round(-2*finalLL+2*(nParams-length(fixedparams)),2),"\n")
  cat("BIC: ",round(-2*finalLL+(nParams-length(fixedparams))*log(choicetasks),2),"\n\n")
  
  timeTaken <- as.numeric(difftime(endtime,starttime,units='secs'))
  tmpH <- floor(timeTaken/60^2)
  tmpM <- floor((timeTaken-tmpH*60^2)/60)
  tmpS <- round(timeTaken-tmpH*60^2-tmpM*60,2)
  timeTaken <- paste(formatC(tmpH,width=2,format='d',flag=0),
                     formatC(tmpM,width=2,format='d',flag=0),
                     tmpS,sep=':')
  
  cat("Time taken: ",timeTaken,"\n")
  cat("Iterations: ",readNIter(),"\n\n")

  if (cmc_control$replace0==1){
    cat("Replacement of zeros used in covariance calculation, affecting ",count0covar," individuals\n\n")
  }
  
  if (cmc_control$replace0==2){
    cat("Replacement of zeros used in final estimates, affecting ",count0estimation," individuals\n")
    cat("Replacement of zeros used in covariance calculation, affecting ",count0covar," individuals\n\n")
  }

  cat("Estimates:\n")
  print(output)
  
  varcov=signif(varcov,4)
  robvarcov<<-signif(robvarcov,4)
  
  # cat("\n\nCovariance matrix:\n")
  # print(varcov)
  
  # cat("\n\nCorrelation matrix:\n")
  # print(corrmat)
  
  cat("\n\nRobust covariance matrix:\n")
  print(robvarcov)
  
  cat("\n\nRobust Correlation matrix:\n")
  print(robcorrmat)
  
  if(cmc_control$mdcev==0) 
  {
    nObsPerIndiv <- as.vector(table(database$ID))
    if(cmc_control$mixing==1) nDraws <- dim(drawsL[[1]])[2]
    probs <- cmc_probabilities(est)
    
    if(cmc_control$mixing==0) P_per_task=probs^nObsPerIndiv
    if(cmc_control$mixing==1 & cmc_control$hybrid==1) P_per_task=exp(LLchoice)^(1/nObsPerIndiv*nDraws)
    if(cmc_control$mixing==1 & cmc_control$hybrid==0) P_per_task=probs^(1/nObsPerIndiv*nDraws)
    
    outliers=cbind(unique(database$ID),P_per_task)
    colnames(outliers)=c("ID","Av prob per choice")
    
    outliers=outliers[order(outliers[,2]),] 
    
    cat("\n\n20 worst outliers in terms of lowest average per choice prediction:\n")
    print(as.data.frame(outliers[(1:20),]), row.names=FALSE)
  }
  
  cat("\n\nChanges in parameter estimates from starting values:\n")
  x=(cbind(initial,est,round(est-initial,4)))
  colnames(x)[1]="initial"
  colnames(x)[3]="diff"
  print(x)
  
  return(output)
}
