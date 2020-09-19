#' @title Pearson.Bias.MACK.Full
#' @description It computes the scaled bias adjusted MackNet residuals.
#' @param Pearson.Biasadj Bias adjusted residuals.
#' @param Scale.Param MackNet scale parameter
#' @return Scaled bias adjusted MackNet residuals
#' @import keras
#' @import abind
#' @export
#'

Pearson.Bias.MACK.Full=function(Pearson.Biasadj,Scale.Param){
  Dimension=dim(Pearson.Biasadj)[1]
  Pearson.B=matrix(0,Dimension,Dimension)
  Pearson.B[,1:(Dimension-1)]=t(t(Pearson.Biasadj[,1:(Dimension-1)])/Scale.Param)
  Pearson.B[is.na(Pearson.B)]=0;Pearson.B[Pearson.B==Inf | Pearson.B==-Inf]=0
  return(Pearson.B)
}
