#' @title Alpha_Net
#' @description It calculates the predictive alpha factors of the RNNs ensemble.
#' @param Net_Alpha Set of alpha factors predicted by each individual RNN.
#' @param Net_DevYear Index that allows to identify the development year of each alpha factor.
#' @param Cumulative.T Cumulative payments triangle.
#' @return Predictive alpha factors of the MackNet model obtained from the ensemble of RNNs.
#' @import keras
#' @import abind
#' @export
#'

Alpha_Net=function(Net_Alpha,Net_DevYear,Cumulative.T){
  out=0
  for (i in 1:(dim(Cumulative.T)[1]-1)){
    out[i]=mean(Net_Alpha[Net_DevYear==i/dim(Cumulative.T)[1]])
  }
  return(out)
}
