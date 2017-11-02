#' Creates a list of matrices containing draws. One matrix per dimension
#'
#' @param cmc_inter_draws A list of arguments describing the inter-person
#'   draws to create. The list must include the following elements:
#'   - nDraws Scalar. Number of draws per individual per dimension.
#'   - drawNames Vector of strings. Name of dimensions (i.e. random variables to draw for).
#'   - normalDraws Vector of strings. Name of dimensions to turn into normal draws.
#'   - drawType String. Can be 'halton', 'mlhs', or 'pmc'. Determines drawing method.
#'   
#' @param cmc_intra_draws A list of arguments describing the intra-person
#'   draws to create. The list must include the following elements:
#'   - nDraws Scalar. Number of draws per individual per dimension.
#'   - drawNames Vector of strings. Name of dimensions (i.e. random variables to draw for).
#'   - normalDraws Vector of strings. Name of dimensions to turn into normal draws.
#'   - drawType String. Can be 'halton', 'mlhs', or 'pmc'. Determines drawing method.
#' 
#' @return A list of arrays, one for each variable.
#'   Each array has dimensions nObs x nDrawsInter x nDrawsIntra
#' @export
cmc_makeDraws=function(cmc_inter_draws, cmc_intra_draws){
  
  # PREPARATIONS
  # Rearrange params in convenient way
  nObs       <- nrow(database)
  if(cmc_control$panel==1) idIndiv <- database$ID else idIndiv <- 1:nObs
  nIndiv <- length(unique(idIndiv))
  nDimInter <- length(cmc_inter_draws$drawsNames)
  nDimIntra <- length(cmc_intra_draws$drawsNames)
  nNorInter <- length(cmc_inter_draws$normalDraws)
  nNorIntra <- length(cmc_intra_draws$normalDraws)
  nDrawsInter  <- cmc_inter_draws$nDraws
  nDrawsIntra  <- cmc_intra_draws$nDraws
  # Set type of draw to lower case
  cmc_inter_draws$drawsType <- tolower(cmc_inter_draws$drawsType)
  cmc_intra_draws$drawsType <- tolower(cmc_intra_draws$drawsType)
  # Set nDraws=1 if inter or intra draws are set to 0
  if(nDrawsInter==0) nDrawsInter <- 1
  if(nDrawsIntra==0) nDrawsIntra <- 1
  # Create list of draws
  drawsList <- list()
  cat('Creating draws ')
  if(cmc_inter_draws$nDraws>0 & cmc_intra_draws$nDraws>0 &
     cmc_inter_draws$drawsType=='halton' & cmc_intra_draws$drawsType=='halton'){
    cmc_intra_draws$drawsType=='mlhs'
    cat('WARNING: Type of intra-person draws has been changed to "mlhs".\n')
    cat('         Using halton draws in both inter and intra draws can\n')
    cat('         introduce unwanted correlation.\n')
    warning("Intra-person draws' type changed to MLHS.", call.=FALSE)
  }
  
  # GENERATING INTER DRAWS
  if(nDimInter>0){
    # create draws and store in draws variable (a matrix)
    if(cmc_inter_draws$drawsType=='halton') draws <- randtoolbox::halton(nDrawsInter*nIndiv,nDimInter)
    if(cmc_inter_draws$drawsType=='mlhs') draws <- cmc_mlhs(nDrawsInter,nDimInter,nIndiv)
    if(cmc_inter_draws$drawsType=='pmc') draws <- matrix(runif(nIndiv*nDrawsInter*nDimInter),
                                                         nrow=nIndiv*nDrawsInter, ncol=nDimInter,
                                                         byrow=TRUE)
    draws=as.matrix(draws) # (nIndiv*nDrawsInter) x nDimInter
    colnames(draws) <- cmc_inter_draws$drawsNames
    # make normal
    for(i in 1:nNorInter) draws[,cmc_inter_draws$normalDraws[i]] <- qnorm(draws[,cmc_inter_draws$normalDraws[i]])
    # Change structure of the draws to a nObs x nDrawsInter x nDrawsIntra array
    obsPerIndiv <- as.vector(table(idIndiv))
    for(d1 in 1:nDimInter){
      # Change structure to a nObs x nDrawsInter matrix
      M <- matrix(0, nrow=nObs,ncol=nDrawsInter)
      row1 <- 1
      for(i in 1:N){
        row2 <- row1 + obsPerIndiv[i] - 1
        M[row1:row2,] <- matrix(draws[((i-1)*nDrawsInter+1):(i*nDrawsInter),d1],
                                nrow=row2-row1+1, ncol=nDrawsInter, byrow=TRUE)
        row1 <- row2 + 1
      }
      # Change structure to an nObs x nDrawsInter x nDrawsIntra cube
      for(d2 in 1:nDimInter){
        C <- array(0, dim=c(nObs, nDrawsInter, nDrawsIntra))
        for(j in 1:nDrawsIntra) C[,,j] <- M
      }
      # Store the draws in the list of draws
      drawsList[[cmc_inter_draws$drawsNames[d1]]] <- C
      cat('.')
    }
  }
  
  # GENERATING INTRA DRAWS
  if(nDimIntra>0){
    for(d2 in 1:nDimIntra){
      C <- array(0, dim=c(nObs, nDrawsInter, nDrawsIntra))
      for(n in 1:nObs){
        # Creation of intra draws (for one observation, for one random variable)
        if(cmc_intra_draws$drawsType=='halton') M = randtoolbox::halton(nDrawsIntra, nDrawsInter)
        if(cmc_intra_draws$drawsType=='mlhs')   M = cmc_mlhs(nDrawsIntra,nDrawsInter,1)
        if(cmc_intra_draws$drawsType=='pmc')    M = matrix(runif(nDrawsInter*nDrawsIntra),
                                                           nrow=nDrawsIntra, ncol=nDrawsInter,
                                                           byrow=TRUE)
        C[n,,] <- M
      }
      if(cmc_intra_draws$drawsNames[d2] %in% cmc_intra_draws$normalDraws) C <- qnorm(C)
      drawsList[[cmc_intra_draws$drawsNames[d2]]] <- C
      cat('.')
    }
  }
  
  # RETURN
  cat(' Done\n')
  if(cmc_control$panel==0 & cmc_inter_draws$nDraws>0){
    cat("WARNING: Inter-person draws are usually used for panel\n")
    cat("         data, yet cmc_control$panel is set to 0.\n\n")
    warning('Inter-person draws are used without a panel structure.', call.=FALSE)
  }
  return(drawsList)
}