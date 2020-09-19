#' @title Residuals_Full
#' @description It generates the scaled bias-adjusted residuals and the parameters needed for the boostrap implementation of MackNet
#' @param Triangles Data used for calculating the residuals.
#' @param ExposureVector Exposure measure. Written premiums is an appropriate measure to scale cumulative payments and incurred cost.
#' @param MackBias If this variable is set to 0, the bias adjustment suggested by England y Verrall (2006) in "Predictive Distributions of Outstanding Liabilities in General Insurance" is applied, this means that residuals are multiplied by N/(N-p). In case this variable is set to 1, the adjustment suggested by Mack (1993) in "Distribution-free calculation of the standard error of chain ladder reserve estimates" is applied, this means that residuals are multiplied by n(i)/(n(i)-1). Finally, if this variable is set to 2, no adjustment is applied.
#' @param ZeroMean If this variable is set to 0, residuals are not scaled to have zero mean. By default they are adjusted.
#' @return The formula generates the following outputs: \itemize{
#' \item \code{DF} Predictive development factors of the MackNet model.
#' \item \code{Alpha} Predictive alpha by development year of the MackNet model.
#' \item \code{Residuals} Residuals of the MackNet model. These residuals are used within the boostrap procedure.
#' }
#' @import keras
#' @import abind
#' @export
#'

Residuals_Full=function(Triangles,ExposureVector,MackBias,ZeroMean){
  MeanForecasted=apply(Triangles,c(1,2),mean)*ExposureVector
  Pearson.Resid=Pearson.Residuals.Mack.Full(MeanForecasted,Full.DevFactors(MeanForecasted))  #Pearson residuals calculation
  Pearson.Biasadj.Mack=Pearson.Bias.Adj.MACK.Full(Pearson.Resid,MackBias)                    #Mack Bias Adj
  Scale.Param=Scale.Parameter.Mack.Full(Pearson.Biasadj.Mack)                                #Scale parameter^0.5 calculation
  Pearson.Biasadj=Pearson.Bias.MACK.Full(Pearson.Biasadj.Mack,Scale.Param)                   #Bias Adj
  return(list(Residuals=Final.Residuals.Mack.Full(Pearson.Biasadj,ZeroMean),
              Alpha=c(Scale.Param,rep(0,1)),
              DF=c(DevFactors.t.Predictive(MeanForecasted),rep(1,1))))
}
