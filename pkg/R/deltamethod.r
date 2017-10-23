#' Applies the delta method to calculate the standard error of transformations of parameters
#' @param  par1 String. Name of the first parameter.
#' @param  par2 String. Name of the second parameter.
#' @return Matrix indicating the value and s.e. of the addition, substraction and ratio of the two parameters.
#' @export
deltamethod=function(par1,par2)
{
  v1=est[par1]+est[par2]
  se1=sqrt(robvarcov[par1,par1]+robvarcov[par2,par2]+2*robvarcov[par1,par2])
  t1=round(v1/se1,2)
  #
  v2=est[par1]-est[par2]
  se2=sqrt(robvarcov[par1,par1]+robvarcov[par2,par2]-2*robvarcov[par1,par2])
  t2=round(v2/se2,2)
  #
  v3=est[par2]-est[par1]
  se3=se2
  t3=-t2
  #
  v4=est[par1]/est[par2]
  se4=sqrt(v4^2*(robvarcov[par1,par1]/(est[par1]^2)+robvarcov[par2,par2]/(est[par2]^2)-2*robvarcov[par1,par2]/(est[par1]*est[par2])))
  t4=round(v4/se4,2)
  #
  v5=est[par2]/est[par1]
  se5=sqrt(v5^2*(robvarcov[par1,par1]/(est[par1]^2)+robvarcov[par2,par2]/(est[par2]^2)-2*robvarcov[par1,par2]/(est[par1]*est[par2])))
  t5=round(v5/se5,2)
  #
  function_value=round(c(v1,v2,v3,v4,v5),4)
  function_se=round(c(se1,se2,se3,se4,se5),4)
  function_t=c(t1,t2,t3,t4,t5)
  #
  delta_output=cbind(function_value,function_se,function_t)
  rownames(delta_output)=c(paste(cbind(par1,"+",par2),collapse=""),paste(cbind(par1,"-",par2),collapse=""),paste(cbind(par2,"-",par1),collapse=""),paste(cbind(par1,"/",par2),collapse=""),paste(cbind(par2,"/",par1),collapse=""))
  #
  return(delta_output)
}
