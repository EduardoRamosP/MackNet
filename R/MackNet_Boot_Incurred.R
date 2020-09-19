#' @title MackNet_Boot_Incurred
#' @description It produces the boostrapping to obtain a full reserves distribution with the MackNet model.
#' @param Simulations Number of samples produced by the MackNet model
#' @param Parameters List with all the MackNet parameters. Please refer to function MackNet_Parameters for further details
#' @param Cumulative.T Cumulative payments triangle.
#' @param Cumulative.T.Incurred Incurred cost triangle.
#' @param OutliersControl Development factors below 0.975 are not allowed when this variable is set to 1.
#' @return The formula generates the following outputs: \itemize{
#' \item \code{Residuals_Boot} Residuals used within the bootstrap procedure.
#' \item \code{Payments.D} It contains the total following year payments sampled by the MackNet model.
#' \item \code{Reservest.D} It contains the total reserves sampled by the MackNet model.
#' \item \code{Reservest_1.D} It contains the total reserves in t+1 sampled by the MackNet model.
#' \item \code{Ultimatest.D} It contains the ultimate cost sampled by the MackNet model.
#' \item \code{Payments.D.Vector} It contains the following year payments by accident year sampled by the MackNet model.
#' \item \code{Reservest.D.Vector} It contains the reserves by accident year sampled by the MackNet model.
#' \item \code{Reservest_1.D.Vector} It contains the reserves in t+1 by accident year sampled by the MackNet model.
#' \item \code{Ultimatest.D.Vector} It contains the ultimate cost by accident year sampled by the MackNet model.
#' \item \code{TrianglesBackup} This array contains all the triangles sampled by the MackNet model.
#' \item \code{UpperTrianglesBackup} Only for testing purposes. It assumes that the only observed value is the one included in the first development year.
#' \item \code{Boot_DevFactors} Predictive development factors sampled by the boostrap procedure.
#' \item \code{Development_Factors_MackNet} Predictive development factors of the MackNet model.
#' \item \code{Alpha} Predictive alpha by development year of the MackNet model. This variable is named as sigma in "Mack-Net model: Blending Mack's model with Recurrent Neural Networks".
#' }
#' @import keras
#' @import abind
#' @export
#'

MackNet_Boot_Incurred=function(Simulations,Parameters,Cumulative.T,Cumulative.T.Incurred,OutliersControl){
  #Smoth development factors are calculated
  Full_DF=DevFactors_Net_Incurred(Parameters$Factors[,2],Parameters$Factors[,1],Cumulative.T.Incurred,OutliersControl)
  #Smoth alpha values are calculated
  Full_Alpha=Alpha_Net(Parameters$Alpha[,2],Parameters$Alpha[,1],Cumulative.T.Incurred)
  #Residuals to be bootstrapped are obtained
  Residuals.Boot=Parameters$RNN_Residuals
  ###------------------------------Bootstrap simulation----------------------------------------###
  set.seed(33433)
  n=Simulations;Ultimatest.D=Ultimatest_1.D=Reservest.D=0;SANN.Backup=0
  Ultimatest.D.Vector=Reservest.D.Vector=Ultimatest_1.D.Vector=matrix(0,dim(Cumulative.T)[1],n)
  TrianglesBackup=UpperTrianglesBackup=array(0,dim=c(dim(Cumulative.T)[1],dim(Cumulative.T)[1],n))
  for (i in 1:n){
    Step1=Boot.Residuals.MACK(Cumulative.T,Residuals.Boot)                                                   #Residuals resampling
    Step2=Boot.Ratios.MACK(Cumulative.T.Incurred,Step1,Full_Alpha,Full_DF)                           #Link ratios resampled
    Step3=Boot.Dev.Factor.MACK(Cumulative.T.Incurred,Step2)                                                  #Dev factors resampled
    Step4=Cumulative.T.Resampled.MACK(Cumulative.T.Incurred,Step3,Full_Alpha,Residuals.Boot,"Resampled")     #Cumulative resampled with process error
    Step5=ChainLaddert.t_1(Step4,DevFactors.t1.Predictive(Step4))                   #Incremental resampled
    Ultimatest.D[i]=sum(Step4[,dim(Step4)[1]])                                      #Ultimate predicted in t
    Reservest.D[i]=Ultimatest.D[i]-LastDiagonal(Cumulative.T)                       #Reserve predicted in t
    Ultimatest_1.D[i]=sum(Step5[,dim(Step5)[1]])                                    #Ultimate predicted in t
    Ultimatest.D.Vector[,i]=Step4[,dim(Step4)[1]]                                   #Vector of Ultimate predicted in t
    Reservest.D.Vector[,i]=Ultimatest.D.Vector[,i]-LastDiagonal.Vector(Cumulative.T)#Reserve predicted in t
    Ultimatest_1.D.Vector[,i]=Step5[,dim(Step5)[1]]                                 #Vector of Ultimate predicted in t
    TrianglesBackup[,,i]=Full.Incremental(Step4);SANN.Backup=Step3/n+SANN.Backup
    UpperTrianglesBackup[,,i]=Upper.Triangle.Mack(Cumulative.T.Incurred,Step2)
  }
  return(list(Residuals_Boot=Residuals.Boot, Reservest.D=Reservest.D, Ultimatest.D=Ultimatest.D,
              Ultimatest_1.D=Ultimatest_1.D, Reservest.D.Vector=Reservest.D.Vector,
              Ultimatest.D.Vector=Ultimatest.D.Vector, Ultimatest_1.D.Vector=Ultimatest_1.D.Vector,
              TrianglesBackup=TrianglesBackup, UpperTrianglesBackup=UpperTrianglesBackup,
              Boot_DevFactors=SANN.Backup, Development_Factors_MackNet=Full_DF, Alpha=Full_Alpha))
}
