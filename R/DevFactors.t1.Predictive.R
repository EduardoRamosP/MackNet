#' @title DevFactors.t1.Predictive
#' @description Calculation of the predictive development factors, assuming that the payment forecasted in t is true.
#' @param Triangle.Cumulative Cumulative triangle
#' @return Predictive development factors assuming the payment forecasted in t is true.
#' @import keras
#' @import abind
#' @export
#'

DevFactors.t1.Predictive=function(Triangle.Cumulative){
  dimension=dim(Triangle.Cumulative)[1]
  Factors=0
  for (i in 2:(dimension-1)){
    Factors[i]=sum(Triangle.Cumulative[(dimension-i):dimension,i+1])/sum(Triangle.Cumulative[(dimension-i):dimension,i])
  }
  Factors=ifelse(is.na(Factors),0,Factors)
  return(ifelse(Factors>50,50,ifelse(Factors< -50,-50,Factors)))
}
