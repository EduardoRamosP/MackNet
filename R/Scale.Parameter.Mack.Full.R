#' @title Scale.Parameter.Mack.Full
#' @description It computes the parameter to scale the residuals.
#' @param Pearson.Biasadj Bias adjusted residuals.
#' @return MackNet scale parameter
#' @import keras
#' @import abind
#' @export
#'

Scale.Parameter.Mack.Full=function(Pearson.Biasadj){
  Dimension=dim(Pearson.Biasadj)[2]
  Contribution.to.ScaleParam=Pearson.Biasadj^2
  Mack.Alpha=colSums(Contribution.to.ScaleParam)/(Dimension)
  Mack.Alpha=ifelse(Mack.Alpha==0,min(Mack.Alpha[Mack.Alpha!=0]),Mack.Alpha)
  return(sqrt(Mack.Alpha)[-Dimension])
}
