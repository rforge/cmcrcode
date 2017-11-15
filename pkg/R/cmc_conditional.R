cmc_conditional <- function(betaD, P, isIntra=FALSE){
  if(cmc_control$mixing==0){
    cat('Conditionals can only be calculated for mixing models')
    return(NA)
  }
  
  if(length(dim(P))<3){
    cat('P must contain the probability for each draw.\n')
    cat('Use cmc_probabilities(theta, probEachDraw=TRUE).\n')
    return(NA)
  }
  
  if(length(dim(betaD))<3){
    cat('betaD must contain the the value of the parameter\n')
    cat('for each draw. Please construct them in the same\n')
    cat('way you do in cmc_probabilities().\n')
    return(NA)
  }
  
  if(isIntra==TRUE){
    # Conditionals for parameters with intra-person variability
    denom = apply(P, MARGIN=1, sum) # nObs vector
    b = apply(betaD*P, MARGIN=1, sum)/denom # nObs vector, weighted avg of intra-draws
  } else {
    # Conditionals for parameters with intra-person variability
    # Average the intra-person variability for probabilities and parameters
    P = colSums(aperm(P, perm=c(3,1,2)))/dim(P)[3] # nObs x nDrawsInter
    b = colSums(aperm(betaD, perm=c(3,1,2)))/dim(betaD)[3] # nObs x nDrawsInter
    # If paneldata, then calculate the product of all rows from the same indiv
    if(cmc_control$paneldata==1){
      P = exp(rowsum(log(P), group=database$ID)) # nIndiv x nDrawsInter
      nObsIndiv <- as.vector(table(database$ID))
      b = rowsum(b, group=database$ID)/nObsIndiv # nIndiv x nDrawsInter
    }
    # Weigthed average of coefficient draws
    denom = rowSums(P)
    b = rowSums(b*P)/denom # nIndiv (or nObs) vector
  }
  
  return(b)
}