#' Validates the cmc_control list of estimation parameters. Sets default values for the ommited ones.
#' @return Nothing. The validated cmc_control list is stored as a global variable.
#' @export
cmc_validatecontrol=function(){
  check=0
  if(is.null(cmc_control$modelname)){
    cmc_control$modelname<<-"noname_model"
    cat("Default model name set ('noname_model')\n")
    check=check+1
  } 
  if(is.null(cmc_control$paneldata)){
    cmc_control$paneldata<<-1
    cat("Missing setting for paneldata, set to default of 1\n")
    check=check+1
  }   
  if(is.null(cmc_control$mixing)){
    cmc_control$mixing<<-0
    cat("Missing setting for mixing, set to default of 0\n")
    check=check+1
  }   
  if(is.null(cmc_control$mdcev)){
    cmc_control$mdcev<<-0
    cat("Missing setting for mdcev, set to default of 0\n")
    check=check+1
  }   
  if(is.null(cmc_control$hybrid)){
    cmc_control$hybrid<<-0
    cat("Missing setting for hybrid, set to default of 0\n")
    check=check+1
  }   
  if(is.null(cmc_control$nCores)){
    cmc_control$nCores<<-1
    cat("Missing setting for nCores, set to default of 1\n")
    check=check+1
  }   
  if(is.null(cmc_control$startingvaluesloop)){
    cmc_control$startingvaluesloop<<-0
    cat("Missing setting for startingvaluesloop, set to default of 0\n")
    check=check+1
  }   
  if(is.null(cmc_control$replace0)){
    cmc_control$replace0<<-0
    cat("Missing setting for replace0, set to default of 0\n")
    check=check+1
  }   
  if(is.null(cmc_control$saveestimates)){
    cmc_control$saveestimates<<-1
    cat("Missing setting for saveestimates, set to default of 1\n")
    check=check+1
  }   
  if(is.null(cmc_control$savecovar)){
    cmc_control$savecovar<<-1
    cat("Missing setting for savecovar, set to default of 1\n")
    check=check+1
  }   
  if(is.null(cmc_control$includetrat1)){
    cmc_control$includetrat1<<-0
    cat("Missing setting for includetrat1, set to default of 0\n")
    check=check+1
  }   
  if(is.null(cmc_control$writeIterations)){
    cmc_control$writeIterations<<-1
    cat("Missing setting for writeIterations, set to default of 0\n")
    check=check+1
  }   
  if(is.null(cmc_control$writeF12)){
    cmc_control$writeF12<<-0
    cat("Missing setting for writeF12, set to default of 0\n")
    check=check+1
  }
  if(is.null(cmc_control$geneticAlg)){
    cmc_control$geneticAlg<<-0
    cat("Missing setting for geneticAlg, set to default of 0\n")
    check=check+1
  }
  if(is.null(cmc_control$bhhh)){
    cmc_control$bhhh<<-0
    cat("Missing setting for bhhh, set to default of 0\n")
    check=check+1
  }
  if(is.null(cmc_control$cml)){
    cmc_control$cml<<-0
    cat("Missing setting for CML, set to default of 0\n")
    check=check+1
  }
  if (check==0) cat("All controls set correctly\n")
}
