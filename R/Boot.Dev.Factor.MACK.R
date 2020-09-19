#' @title Boot.Ratios.MACK
#' @description It generates the set of development factors resampled by using the resampled set of MackNet link ratios.
#' @param Cumulative.T Cumulative triangle.
#' @param Link.R Link ratios resampled randomly.
#' @return Development factors resampled randomly.
#' @import keras
#' @import abind
#' @export
#'

Boot.Dev.Factor.MACK=function(Cumulative.T,Link.R){
  Dimension=dim(Cumulative.T)[1]
  Cumulative.T.Rotated=rotate_clockwise(Cumulative.T)
  Cumulative.T.Rotated[lower.tri(Cumulative.T.Rotated,diag=T)]=0
  Cumulative.T.Final=rotate_counter_clockwise(Cumulative.T.Rotated)
  Dev.Ratios.Boot=t(colSums(Cumulative.T.Final*Link.R))/colSums(Cumulative.T.Final)
  return(Dev.Ratios.Boot[1:(Dimension-1)])
}
