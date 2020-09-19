#' @title Mack.Bias.Adj.Full
#' @description It calculates the bias adjustment to be applied to the residuals.
#' @param Pearson.Resid MackNet residuals
#' @param Constant.Param If this variable is set to 0, the bias adjustment suggested by England y Verrall (2006) in "Predictive Distributions of Outstanding Liabilities in General Insurance" is applied, this means that residuals are multiplied by N/(N-p). In case this variable is set to 1, the adjustment suggested by Mack (1993) in "Distribution-free calculation of the standard error of chain ladder reserve estimates" is applied, this means that residuals are multiplied by n(i)/(n(i)-1). Finally, if this variable is set to 2, no adjustment is applied.
#' @return Bias adjustment to be applied to the MackNet residuals.
#' @import keras
#' @import abind
#' @export
#'

Mack.Bias.Adj.Full=function(Pearson.Resid,Constant.Param){
  Dimension=dim(Pearson.Resid)[1]
  Obs=(Dimension*Dimension)-Dimension
  Parameters=Dimension:1
  if (Constant.Param==0){Bias.Adj=rep(Obs/(Obs-max(Parameters)),Dimension)}
  if (Constant.Param==1){Bias.Adj=rep(Dimension/(Dimension-1),Dimension)}
  if (Constant.Param==2){Bias.Adj=rep(1,Dimension)}
  return(Bias.Adj)
}
