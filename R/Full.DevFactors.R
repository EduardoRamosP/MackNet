#' @title Full.DevFactors
#' @description Calculation of development factors taking into consideration both, all the observed and predicted data.
#' @param Cumulative.T Upper and lower triangle used for estimating the predictive development factors.
#' @return Full development factors
#' @import keras
#' @import abind
#' @export
#'

Full.DevFactors=function(Cumulative.T){
  dimension=dim(Cumulative.T)[1];out=0
  for (i in 1:(dimension-1)){
    out[i]=sum(Cumulative.T[,i+1])/sum(Cumulative.T[,i])
  }
  return(out)
}
