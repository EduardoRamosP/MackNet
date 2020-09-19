#' @title MackNet_Parameters
#' @description It calculated the predictive MackNet parameters.
#' @param Triangles Full triangles predicted by each RNN included within the ensemble.
#' @param Error Test error of all the possible levels of weighted decay.
#' @param Exposure Exposure measure. Written premiums is an appropriate measure to scale cumulative payments and incurred cost.
#' @param MackBias If this variable is set to 0, the bias adjustment suggested by England y Verrall (2006) in "Predictive Distributions of Outstanding Liabilities in General Insurance" is applied, this means that residuals are multiplied by N/(N-p). In case this variable is set to 1, the adjustment suggested by Mack (1993) in "Distribution-free calculation of the standard error of chain ladder reserve estimates" is applied, this means that residuals are multiplied by n(i)/(n(i)-1). Finally, if this variable is set to 2, no adjustment is applied.
#' @param ZeroMean If this variable is set to 0, residuals are not scaled to have zero mean. By default they are adjusted.
#' @return The formula generates the following outputs: \itemize{
#' \item \code{Factors} Predictive development factors of the MackNet model.
#' \item \code{Alpha} Predictive alpha by development year of the MackNet model. This variable is named as sigma in "Mack-Net model: Blending Mack's model with Recurrent Neural Networks".
#' \item \code{RNN_Residuals} Residuals of the MackNet model. These residuals are used within the boostrap procedure.
#' \item \code{Error} Optimum level of weighted decay.
#' }
#' @import keras
#' @import abind
#' @export
#'

MackNet_Parameters=function(Triangles,Error,Exposure,MackBias,ZeroMean){
  ExposureVector=matrix(Exposure,length(Exposure),length(Exposure));Opt_Triangles=Triangles
  RNN_DevFactors=NULL;RNN_DevYear=NULL;RNN_Alpha=NULL;RNN_Residuals=0
  RNN_Alpha=Residuals_Full(Opt_Triangles,ExposureVector,MackBias,ZeroMean)$Alpha             #Alpha values are obtained
  RNN_DevFactors=Residuals_Full(Opt_Triangles,ExposureVector,MackBias,ZeroMean)$DF           #DF values are obtained
  RNN_DevYear=c(RNN_DevYear,(1:(dim(Opt_Triangles)[1]))/(dim(Opt_Triangles)[1]))             #Development years are obtained
  return(list(Factors=cbind(RNN_DevYear,RNN_DevFactors),Alpha=cbind(RNN_DevYear,RNN_Alpha),
              RNN_Residuals=Residuals_Full(Opt_Triangles,ExposureVector,MackBias,ZeroMean)$Residuals,
              Error=Error))
}
