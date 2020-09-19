#' @title MackNet_Incurred
#' @description It fits the incurred MackNet model. First, the ensemble of RNNs is fitted and the hyperparameters are optimized. Second, the predictive Mack parameters are computed taking into consideration the predictions made by the ensemble of RNNs. Third, bootstrapping is applied in order to produced a stochastic reserves distribution.
#' @param Cumulative.T Cumulative payments triangle.
#' @param Incurred.T Incurred cost triangle.
#' @param Exposure Exposure measure. Written premiums is an appropriate measure to scale cumulative payments and incurred cost.
#' @param EarlyStoppingPatience In case the error does not improve during the number of epochs defined by this variable, the training process stop and the weights are restored from the epoch with lower error. Default=300.
#' @param Epochs Maximum number of epochs.
#' @param MinimumEpochs Minimum number of epochs.
#' @param wd The optimization algorithm used is ADAM. This variable defines the weighted decay.
#' @param Learning Learning rate.
#' @param drop Dropout regularization.
#' @param Ensemble Number of RNNs included in the ensemble.
#' @param AR This variable allows to remove the autorregressive component from the MackNet model when it is set to 0.
#' @param MackBias If this variable is set to 0, the bias adjustment suggested by England y Verrall (2006) in "Predictive Distributions of Outstanding Liabilities in General Insurance" is applied, this means that residuals are multiplied by N/(N-p). In case this variable is set to 1, the adjustment suggested by Mack (1993) in "Distribution-free calculation of the standard error of chain ladder reserve estimates" is applied, this means that residuals are multiplied by n(i)/(n(i)-1). Finally, if this variable is set to 2, no adjustment is applied.
#' @param ZeroMean If this variable is set to 0, residuals are not scaled to have zero mean. By default they are adjusted.
#' @param Control Development factors below 0.975 are not allowed when this variable is set to 1.
#' @param Simulations Number of triangle samples to be produced by the MackNet model.
#' @param Output Linear or ReLU activation function for the output layer.
#' @return The formula generates the following outputs: \itemize{
#' \item \code{TestError} Test error.
#' \item \code{Residuals} MackNet residuals used during the bootrapping.
#' \item \code{Alpha} Predective alpha parameter per development year. This parameters have a strong impact on the variance of the MackNet model.
#' \item \code{DevFactors} Predective development factors per development year.
#' \item \code{SampledTriangles} It contains all the triangles sampled by the MackNet model.
#' \item \code{SampledReserves} It contains the total reserves sampled by the MackNet model.
#' \item \code{SampledReservesAY} It contains the reserves by accident year sampled by the MackNet model.
#' \item \code{SampledUltimate} It contains the total ultimates sampled by the MackNet model.
#' \item \code{SampledUltimateAY} It contains the ultimates by accident year sampled by the MackNet model.
#' \item \code{EnsembleTriangles} Triangles predicted by the ensemble of networks.
#' }
#' @import keras
#' @import abind
#' @importFrom stats ks.test predict quantile rgamma rnorm runif sd ecdf
#' @export

MackNet_Incurred=function(Cumulative.T,Incurred.T,Exposure,EarlyStoppingPatience=50,Epochs=1000,MinimumEpochs=700,
                          wd=0,Learning=0.01,drop=0.1,Ensemble=10,AR=1,MackBias=0,ZeroMean=1,Control=0,Simulations=10000,Output="relu"){
  #Early Stopping function is defined
  ES=callback_early_stopping(monitor = "val_loss", min_delta = 0.00005, patience = EarlyStoppingPatience,mode="min",restore_best_weights=TRUE)
  #Ensemble of RNNs is fitted and wd is optimied
  MackNet_1 = MackNet_Fit_Incurred(Cumulative.T,Incurred.T,Exposure,AR,Ensemble,wd,Learning,drop,Epochs,MinimumEpochs,ES,Output)
  #MackNet parameters are calculated
  MackNet_2 = MackNet_Parameters(MackNet_1$TrianglesBackup,MackNet_1$Error,Exposure,MackBias,ZeroMean)
  #MackNet bootstrap procedure
  MackNet_3 = MackNet_Boot_Incurred(Simulations,MackNet_2,Cumulative.T,Incurred.T,Control)
  return(list(TestError=MackNet_2$Error,Residuals=MackNet_2$RNN_Residuals,Alpha=MackNet_3$Alpha,
              DevFactors=MackNet_3$Boot_DevFactors,SampledTriangles=MackNet_3$TrianglesBackup,
              SampledReserves=MackNet_3$Reservest.D,SampledReservesAY=MackNet_3$Reservest.D.Vector,
              SampledUltimate=MackNet_3$Ultimatest.D,SampledUltimateAY=MackNet_3$Ultimatest.D.Vector,
              EnsembleTriangles=MackNet_1$TrianglesBackup))

}
