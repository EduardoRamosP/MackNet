#' @title Boot.Residuals.MACK
#' @description It resamples the MackNet residuals.
#' @param Incremental.T Original triangle used to fit the MackNet model
#' @param Residuals.Boot MackNet residuals.
#' @return Residuals randomly resampled.
#' @import keras
#' @import abind
#' @export
#'

Boot.Residuals.MACK=function(Incremental.T,Residuals.Boot){
  Dimension=dim(Incremental.T)[1]
  Boot.T=matrix(sample(Residuals.Boot,Dimension*Dimension,replace=T),Dimension,Dimension)
  Boot.T[lower.tri(Boot.T,diag=T)]=0
  Boot.T=rotate_counter_clockwise(Boot.T)
  return(Boot.T)
}
