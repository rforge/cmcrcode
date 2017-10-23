#' Reads the number of iterations from the file written with the parameters' values at each iteration
#' @return Number of iterations written in the file.
readNIter <- function(){
	fileName <- paste(cmc_control$modelname,'iterations.csv',sep='_')
	if(exists('model')) nIter <- model$iter else nIter <- -1
	tryCatch( iterDB <- read.csv(fileName, header=TRUE),
	          error=function(e){}, warning=function(e){})
	if(exists('iterDB')) nIter <- length(unique(iterDB[,ncol(iterDB)])) # counts unique LL
	return(nIter)
}