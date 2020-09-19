#' @title Boot.Ratios.MACK
#' @description It generates the set of link ratios resampled by using the resampled set of MackNet residuals.
#' @param Cumulative.T Cumulative triangle.
#' @param Resid.Resampled MackNet residuals resampled.
#' @param Scale.Param MackNet scale parameter.
#' @param Dev.Factors.AI Predictive development factors obtained from the ensemble of RNNs.
#' @return Link ratios resampled randomly.
#' @import keras
#' @import abind
#' @export
#'

Boot.Ratios.MACK=function(Cumulative.T,Resid.Resampled,Scale.Param,Dev.Factors.AI){
  Scale.Param.Adap=c(Scale.Param,1);Dev.Factors.AI.Adap=c(Dev.Factors.AI,1)
  ResampledRatios=replace(t(t(Resid.Resampled*sqrt(t((Scale.Param.Adap^2)/t(abs(Cumulative.T)))))+Dev.Factors.AI.Adap),
                          is.infinite(t(t(Resid.Resampled*sqrt(t((Scale.Param.Adap^2)/t(abs(Cumulative.T)))))+Dev.Factors.AI.Adap)), 0)
  ResampledRatios.Rotated=rotate_clockwise(ResampledRatios)
  ResampledRatios.Rotated[lower.tri(ResampledRatios.Rotated,diag=T)]=0
  ResampledRatios.final=rotate_counter_clockwise(ResampledRatios.Rotated)
  return(ResampledRatios.final)
}
