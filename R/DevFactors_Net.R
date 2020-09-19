#' @title DevFactors_Net
#' @description It calculates the predictive development factors of the RNNs ensemble.
#' @param RNN_DevFactors Set of development factors predicted by each individual RNN.
#' @param RNN_DevYear Index that allows to identify the development year of each development factor.
#' @param Cumulative.T Cumulative payments triangle.
#' @param OutliersControl Development factors below 0.975 are not allowed when this variable is set to 1.
#' @return Predictive development factors of the MackNet model obtained from the ensemble of RNNs.
#' @import keras
#' @import abind
#' @export
#'

DevFactors_Net=function(RNN_DevFactors,RNN_DevYear,Cumulative.T,OutliersControl){
  out=0;OutCL=DevFactors.t(Cumulative.T)
  for (i in 1:(dim(Cumulative.T)[1]-1)){
    out[i]=mean(RNN_DevFactors[RNN_DevYear==i/dim(Cumulative.T)[1]])
    if(OutliersControl==1){if(abs(out[i]-OutCL[i])>0.025 & abs(OutCL[i]-1)<0.075){out[i]=OutCL[i]}}
  }
  if(OutliersControl==1){out=ifelse(out<0.975,1,out)}
  return(out)
}
