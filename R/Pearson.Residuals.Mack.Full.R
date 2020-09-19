#' @title Pearson.Residuals.Mack.Full
#' @description It generates the residuals needed for the boostrap implementation of MackNet
#' @param Cumulative.T Data used for calculating the residuals.
#' @param Dev.Factors Development factors used for calculating the residuals
#' @return Residuals of the MackNet model. These residuals are used within the boostrap procedure.
#' @import keras
#' @import abind
#' @export
#'

Pearson.Residuals.Mack.Full=function(Cumulative.T,Dev.Factors){
  Dimension=dim(Cumulative.T)[2]
  Pearson.Resid.MACK=matrix(0,Dimension,Dimension)
  for (i in 1:(Dimension-1)){
    for (j in 1:(Dimension)){
      Pearson.Resid.MACK[j,i]=replace(sqrt(Cumulative.T[j,i])*(Cumulative.T[j,i+1]/Cumulative.T[j,i]-Dev.Factors[i]),
                                      is.na(sqrt(Cumulative.T[j,i])*(Cumulative.T[j,i+1]/Cumulative.T[j,i]-Dev.Factors[i])), 0)
      if (Pearson.Resid.MACK[j,i]==Inf) {Pearson.Resid.MACK[j,i]=0}
    }
  }
  return(Pearson.Resid.MACK)
}
