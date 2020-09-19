#' @title Pearson.Bias.Adj.MACK.Full
#' @description It generates the bias adjusted residuals needed for the boostrap implementation of MackNet
#' @param Pearson.Resid Non-scaled residuals.
#' @param Constant.Param If this variable is set to 0, the bias adjustment suggested by England y Verrall (2006) in "Predictive Distributions of Outstanding Liabilities in General Insurance" is applied, this means that residuals are multiplied by N/(N-p). In case this variable is set to 1, the adjustment suggested by Mack (1993) in "Distribution-free calculation of the standard error of chain ladder reserve estimates" is applied, this means that residuals are multiplied by n(i)/(n(i)-1). Finally, if this variable is set to 2, no adjustment is applied.
#' @return Bias adjusted residuals of the MackNet model. These residuals are used within the boostrap procedure.
#' @import keras
#' @import abind
#' @export
#'

Pearson.Bias.Adj.MACK.Full=function(Pearson.Resid,Constant.Param){
  Dimension=dim(Pearson.Resid)[1]
  Pearson.B=matrix(0,Dimension,Dimension)
  Pearson.B[1:Dimension,1:Dimension]=t(t(Pearson.Resid[1:Dimension,1:Dimension])*sqrt(Mack.Bias.Adj.Full(Pearson.Resid,Constant.Param)))
  return(Pearson.B)
}
