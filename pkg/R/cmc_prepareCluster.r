#' Creates cluster and loads pieces of data in each cluster
#' @return Nothing (the cluster is loaded as a global variable)
cmc_prepareCluster <- function(){
  
  # Remove useless columns from data
  uselessCols <- c('task','choicetask')
  dataL <- database[,which( !(names(database) %in% uselessCols) )]
  
  # Calculate useful values
  nObs   <- nrow(database)
  nIndiv <- length(unique(database$ID))
  nAlt   <- ncol(choice_matrix)
  tmp    <- table(database$ID)
  nObsID <- as.vector(tmp)
  nameID <- names(tmp)
  rm(tmp)
  
  # Calculates the assignment of individuals' data to cores
  cat('Attempting to split data into',cmc_control$nCores,'pieces.\n',sep=' ')
  obj          <- ceiling(sum(nObsID)/cmc_control$nCores)
  counter      <- 0
  currentCore  <- 1
  assignedCore <- rep(0,nIndiv)
  for(n in 1:nIndiv){
    assignedCore[n] <- currentCore
    counter <- counter + nObsID[n]
    if(counter>=obj & currentCore<cmc_control$nCores){
      currentCore <- currentCore + 1
      counter <- 0
    }
  }
  coreID <- rep(0, nObs)
  for(n in 1:nIndiv) coreID[which(database$ID==nameID[n])] <- assignedCore[n]
  cmc_control$nCores <- max(coreID)
  coreLoad <- as.vector(table(coreID)) # number of observations per worker
  
  # Break data base in smaller pieces
  dataL <- split(dataL, f=coreID)
  
  # Break avail matrix in smaller pieces
  availL <- list()
  for(i in 1:cmc_control$nCores){
    if(is.matrix(avail_matrix)) availL[[i]] <- avail_matrix[which(coreID==i),] else availL[[i]] <- 1
  }
  
  # Break choice matrix in smaller pieces
  choiceL <- list()
  for(i in 1:cmc_control$nCores) choiceL[[i]] <- choice_matrix[which(coreID==i),]
  
  ## Break indiv vector in smaller pieces
  #indivID <- split(indivID, f=coreID)
  
  # Break draws in smaller pieces
  drawsLL <- list()
  if(cmc_control$mixing==0) for(i in 1:cmc_control$nCores) drawsLL[[i]] <- NA
  if(cmc_control$mixing==1){
    nDrawsInter <- dim(drawsL[[1]])[2]
    nDrawsIntra <- dim(drawsL[[1]])[3]
    for(i in 1:cmc_control$nCores){
      drawsLL[[i]] <- list()
      
      # Always store a cube, even if some of its dimensions are 1
      dimDraw <- c(coreLoad[i],
                   ifelse(nDrawsInter>0,nDrawsInter,1),
                   ifelse(nDrawsIntra>0,nDrawsIntra,1))
      for(d in 1:length(drawsL)){
        drawsLL[[i]][[d]] <- drawsL[[d]][which(coreID==i),,]
        if( length(dim(drawsLL[[i]][[d]]))!=3 ) drawsLL[[i]][[d]] <- array(drawsLL[[i]][[d]],
                                                                           dim=dimDraw)
      }
      
      ## If one of the dimensions is 1, store a matrix instead of a cube
      ## This is more robust than the previous line of code
      #for(d in 1:length(drawsL)){
      #  if(nDrawsInter>1  & nDrawsIntra==1) drawsLL[[i]][[d]] <- drawsL[[d]][which(coreID==i),,1]
      #  if(nDrawsInter==1 & nDrawsIntra>1 ) drawsLL[[i]][[d]] <- drawsL[[d]][which(coreID==i),1,]
      #  if(nDrawsInter>1  & nDrawsIntra>1 ) drawsLL[[i]][[d]] <- drawsL[[d]][which(coreID==i),,]
      #  # This is just to make sure the draws don't get lazy loaded
      #  drawsLL[[i]][[d]] <- drawsLL[[i]][[d]] + 1
      #  drawsLL[[i]][[d]] <- drawsLL[[i]][[d]] - 1
      #}
      
      names(drawsLL[[i]]) <- names(drawsL)
    }
  }
  
  # Consolidate data in one list
  DDACI <- list()
  for(i in 1:cmc_control$nCores) DDACI[[i]] <- list(db=dataL[[i]],
                                        dra=drawsLL[[i]],
                                        ava=availL[[i]],
                                        cho=choiceL[[i]])
  if(cmc_control$cml==1){
    for(i in 1:cmc_control$nCores){
      rows <- which(coreID==i)
      DDACI[[i]]$pa1 <- pair1[rows,]
      DDACI[[i]]$pa2 <- pair2[rows,]
      DDACI[[i]]$paA <- pairA[rows,,]
      DDACI[[i]]$paB <- pairB[rows,,]
      DDACI[[i]]$ya  <- yA[rows,]
      DDACI[[i]]$yb  <- yB[rows,]
    }
  }
  rm(dataL, drawsLL, availL, choiceL)
  cat('Data split into',cmc_control$nCores,'pieces.\n',sep=' ')
  #cat('Elements in DDACI[[1]]:',ls(DDACI[[1]]),'\n')
  
  # Creates clusters and loads required packages
  cat('Creating workers and loading required libraries in them.\n')
  cl <<- parallel::makeCluster(cmc_control$nCores)
  excludePackages<- c('parallel','cmcRcode')
  loadedPackages <- search()
  loadedPackages <- loadedPackages[grepl("^(package:)", loadedPackages)]
  loadedPackages <- substr(loadedPackages, start=9, stop=100)
  loadedPackages <- loadedPackages[!(loadedPackages %in% excludePackages)]
  if(length(loadedPackages)>0){
    parallel::clusterCall(cl=cl, function(lib) {
      if(Sys.info()["nodename"]=="ITS1-79") .libPaths( c( .libPaths(), 'M:/Rpack') )
      for(i in 1:length(lib)) library(lib[i],character.only=TRUE)
    }, lib=loadedPackages)
  }
  
  ## Preload data on each node
  cat('Pre-loading data pieces in workers.\n')
  #parLapply(cl=cl, X=DDACI,
  #          fun=function(DB){
  #            drawsL        <<- DB$dra
  #            database      <<- DB$db
  #            avail_matrix  <<- DB$ava
  #            choice_matrix <<- DB$cho})
  parallel::parLapply(cl=cl, X=DDACI,
                      fun=function(DB){
                        env <- globalenv()
                        assign('drawsL', DB$dra, envir=env)
                        assign('database', DB$db, envir=env)
                        assign('avail_matrix', DB$ava, envir=env)
                        assign('choice_matrix', DB$cho, envir=env)})
  if( ("package:cmcRcode" %in% search()) ) cmc_ologit <- cmcRcode::cmc_ologit
  parallel::clusterExport(cl=cl, varlist=c('cmc_probabilities','cmc_control','cmc_ologit') )
  if(cmc_control$cml==1){
    #parLapply(cl=cl, X=DDACI,
    #          fun=function(DB){
    #            pair1 <<- DB$pa1
    #            pair2 <<- DB$pa2
    #            pairA <<- DB$paA
    #            pairB <<- DB$paB
    #            yA    <<- DB$ya
    #            yB    <<- DB$yb})
    parallel::parLapply(cl=cl, X=DDACI,
                        fun=function(DB){
                          env <- globalenv()
                          assign('pair1', DB$pa1, envir=env)
                          assign('pair2', DB$pa2, envir=env)
                          assign('pairA', DB$paA, envir=env)
                          assign('pairB', DB$paB, envir=env)
                          assign('yA', DB$ya, envir=env)
                          assign('yB', DB$yb, envir=env)})
    parallel::clusterExport(cl=cl, varlist=c('cml') )
  }
  
  print(parallel::clusterEvalQ(cl=cl, ls()))
  #print(clusterEvalQ(cl=cl, head(dbPiece)))
}
