#' Estimates model
#' @param  model Model estimated by runmodel() function
#' @return Nothing. Model's results are loaded as a global variable.
#' @export
cmc_saveoutput=function(model)
{
 sink(paste(cbind(cmc_control$modelname,"_output.txt"),collapse=""))
 cmc_modeloutput(model)
 sink()
 if (cmc_control$saveestimates==1) write.csv(estforoutput,paste(cbind(cmc_control$modelname,"_estimates.csv"),collapse=""))
 if (cmc_control$savecovar==1)
 {
  write.csv(varcovoutput,paste(cbind(cmc_control$modelname,"_covar.csv"),collapse=""))
  write.csv(robvarcovoutput,paste(cbind(cmc_control$modelname,"_robcovar.csv"),collapse=""))
 }
 if(cmc_control$writeF12>0) writeF12()
}
