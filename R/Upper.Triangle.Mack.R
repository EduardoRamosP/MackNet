#' @title Upper.Triangle.Mack
#' @description Only for testing purposes. It assumes that the only observed values are the ones included in the first development year.
#' @param Cumulative.T Cumulative triangle.
#' @param Boot.Ratios Development factors resampled randomly.
#' @return Resampled triangle assumin that the only observed values are the ones included in the first development year.
#' @import keras
#' @import abind
#' @export
#'

Upper.Triangle.Mack=function(Cumulative.T,Boot.Ratios){
  dimension=dim(Cumulative.T)[1]; Out=Cumulative.T
  for (i in 2:dimension){
    Out[,i]=Out[,i-1]*Boot.Ratios[,i-1]
  }
  return(Out)
}
