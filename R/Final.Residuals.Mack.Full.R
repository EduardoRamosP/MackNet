#' @title Final.Residuals.Mack.Full
#' @description It computes the scaled bias and mean adjusted MackNet residuals.
#' @param Residuals Scaled bias adjusted residuals.
#' @param ZeroMean If this variable is set to 0, residuals are not scaled to have zero mean. By default they are adjusted.
#' @return Scaled bias and mean adjusted MackNet residuals
#' @import keras
#' @import abind
#' @export
#'

Final.Residuals.Mack.Full=function(Residuals,ZeroMean){
  dimension=dim(Residuals)[1];Output=0;t=0
  for (i in 1:dimension){
    for (k in 1:(dimension-1)){
      t=t+1
      Output[t]=Residuals[i,k]
    }
  }
  return(Output=Output-mean(Output)*ZeroMean)
}
