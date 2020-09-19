#' @title DevFactors.t.Predictive
#' @description Calculation of predictive development factors.
#' @param Triangle.Cumulative Upper and lower triangle used for estimating the predictive development factors.
#' @return Predictive development factors
#' @import keras
#' @import abind
#' @export
#'

DevFactors.t.Predictive=function(Triangle.Cumulative){
  dimension=dim(Triangle.Cumulative)[1]
  Factors=0
  for (i in 1:(dimension-1)){
    Factors[i]=sum(Triangle.Cumulative[(dimension-i+1):dimension,i+1])/sum(Triangle.Cumulative[(dimension-i+1):dimension,i])
  }
  Factors=ifelse(is.na(Factors),0,Factors)
  return(ifelse(Factors>50,50,ifelse(Factors< -50,-50,Factors)))
}
