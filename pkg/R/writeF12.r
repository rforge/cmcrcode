#' Writes an F12 file with the results of a model estimation.
#' @return Nothing.
writeF12 <- function(){
  K <- length(model$estimate)
  
  line1 <- cmc_control$modelname
  if(nchar(cmc_control$modelname)>79) line1 <- substr(cmc_control$modelname,1,79)
  line1 <- paste(line1, paste(rep(' ',79-nchar(line1)),sep='',collapse=''), '\n', sep='')
  
  line2 <- paste('Converted from R estimation',paste(rep(' ',29),collapse=''),
                 format(Sys.time(),"%H:%M:%S"), ' on ', format(Sys.time(),"%d %b %y"),
                 '\n',sep='')
  
  line3 <- 'END\n'
  
  line4 <- ''
  coeffNamesNoSpaces <- gsub(pattern=' ', replacement='', names(model$estimate))
  maxNameWidth <- max(10,max(nchar(coeffNamesNoSpaces)))
  for(i in 1:K){
    coeffName <- names(model$estimate)[i]
    if(model$fixed[coeffName]) constrained <- 'T' else constrained <- 'F'
    coeffName <- gsub(pattern=' ', replacement='', coeffName)
    #if(nchar(coeffName)>10) coeffName <- substr(coeffName,1,10)
    coeffName <- paste(coeffName,
                       paste(rep(' ',maxNameWidth-nchar(coeffName)),collapse=''),sep='')
    coeffVal <- model$estimate[i]
    if(is.na(coeffVal) | is.infinite(coeffVal) | is.nan(coeffVal)) coeffVal <- 0
    coeffSE  <- sqrt(robvarcov[i,i])
    if(is.na(coeffSE) | is.infinite(coeffSE) | is.nan(coeffSE)) coeffSE <- 0
    tmp <- paste('   0 ', coeffName, ' ', constrained, ' ',
                 sprintf("%20.12E", coeffVal), sprintf("%20.12E", coeffSE ), sep='')
    line4 <- paste(line4, tmp, '\n', sep='')
  }
  
  lineK4 <- '  -1\n'
  
  LL1 <- 0 # THIS SHOULD BE UPDATED
  lineK5 <- paste(format( choicetasks, width=8 ),
                  sprintf("%20.12E", LL1), sprintf("%20.12E", LL0),
                  sprintf("%20.12E", model$maximum), '\n', sep='')
  
  lineK6 <- paste(format(readNIter(), width=4), '   0',
                  format(Sys.time(),"%H:%M:%S"), ' on ', format(Sys.time(),"%d %b %y"),
                  '\n', sep='')
  
  corrM <- round(robcorrmat*10^5,0)
  corrV <- rep('',(K-1)*K/2)
  for(i in 2:K) for(j in 1:(i-1)){
    if(is.na(corrM[i,j]) | is.infinite(corrM[i,j]) | is.nan(corrM[i,j])) corrM[i,j] <- 0
    corrV[(i-2)*(i-1)/2+j] <- format(corrM[i,j], width=7,digits=0, scientific=FALSE)
  } 
  lineK7 <- ''
  for(i in 1:length(corrV)){
    lineK7 <- paste(lineK7, corrV[i], sep='')
    if(i%%10==0 | i==length(corrV)) lineK7 <- paste(lineK7, '\n', sep='')
  }
  
  fileConn <- file( paste(cmc_control$modelname,'.F12',sep='') )
  writeLines(paste(line1,line2,line3,line4,lineK4,lineK5,lineK6,lineK7,sep=''),fileConn)
  close(fileConn)
}

