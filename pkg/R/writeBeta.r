#' Writes the vector [beta,ll] to a file called modelname_iterations.csv
#' @param  beta vector of parameters to be written
#' @param  ll scalar representing the loglikelihood of the whole model
#' @return Nothing.
writeBeta <- function(beta,ll){
  
  if(exists('writeBetaToFile') & cmc_control$writeIterations>0)if(writeBetaToFile){
	  fileName <- paste(cmc_control$modelname,'iterations.csv',sep='_')
	  
	  if(exists('lastFuncParam') & file.exists(fileName)){
	  	if( prod(beta==lastFuncParam)==1 ){
	  		tmp <- matrix(c(beta,ll ),nrow=1)
	  		tryCatch( write.table(tmp,file=fileName, append=TRUE, sep=',', col.names=FALSE, row.names=FALSE),
	  			error=function(e) cat('Current iteration could not be writen to ',fileName,'.\n', sep='') )
	  	}
	  } else {
	  	tmp <- matrix(c(beta,ll ),nrow=1)
	  	colnames(tmp) <- c(names(beta),'logLike')
	  	tryCatch( write.table(tmp,file=fileName, sep=',', row.names=FALSE, append=FALSE),
	  		error=function(e) cat('Initial iteration could not be writen to ',fileName,'.\n', sep='') )
	  }
	  
  }
}
