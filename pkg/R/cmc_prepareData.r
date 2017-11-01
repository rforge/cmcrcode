#' Orders data by respondent and prepares a data set to be used in model estimation
#' @param  idColName String indicating the name of the column containing respondents' ID.
#' @param  choiceColName String indicating the name of the column containing respondents' choice (level coded).
#' @param  alternatives Vector containing the codes (labels) used to register choices in the database, e.g.:c(1,2,3) or c('bus','train','plane'). If mdcev==1, then it must contain the column name of consumptions.
#' @param  availColName String indicating the name of the columns containing respondents' availability. The order must be the same than in the choice coding. If ommited, full availability is assumed.
#' @return Nothing. Data is loaded as a global variable.
#' @export
cmc_prepareData <- function(idColName, choiceColName, alternatives, availColName=NA){
  
  # Order data according to ID, and creates a variable called "ID"
  database <<- database[order(database[,idColName]),]
  database$ID <<- database[,idColName]
  
  # Create choice matrix
  nAlts <- length(alternatives)
  choice <- matrix(0, nrow=nrow(database), ncol=nAlts) # creates choice matrix
  if(cmc_control$mdcev==0){
	for(i in 1:nAlts) choice[which(database[,choiceColName]==alternatives[i]),i] <- 1
	choice_matrix <<- matrix(choice>0, nrow=nrow(choice), ncol=ncol(choice))
  } else {
	for(i in 1:nAlts) choice[,i] <- database[,alternatives[i]]
  }
  
  # Create availability matrix
  if(anyNA(availColName)){ # creates availability matrix and store in .GlobalEnv
    avail_matrix <<- 1 # availability of all alternatives equal to 1
  } else {
    avail_matrix <<- as.matrix(database[,availColName])
  }
   
  # determine number of individuals in the data and store in .GlobalEnv
  N <<- length(unique(database$ID))
  
  # determine number of choice tasks in the data and store in .GlobalEnv
  choicetasks <<- nrow(database)
  
  # calculate LL(0) and store in .GlobalEnv
  if(is.matrix(avail_matrix)){
    LL0 <<- sum(log(1/rowSums(avail_matrix)))
  } else LL0 <<- sum(nrow(database)*log(1/nAlts))
  
  
  # calculate LL(C) and store in .GlobalEnv
  if(is.matrix(avail_matrix)) tmp <- colSums(avail_matrix) else tmp <- nrow(database)
  LLC <<- sum(log(colSums(choice_matrix)/tmp)*colSums(choice_matrix))
}
