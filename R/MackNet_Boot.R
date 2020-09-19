#' @title MackNet_Boot
#' @description It produces the boostrapping to obtain a full reserves distribution with the MackNet model.
#' @param Simulations Number of samples produced by the MackNet model
#' @param Parameters List with all the MackNet parameters. Please refer to function MackNet_Parameters for further details
#' @param Cumulative.T Cumulative payments triangle.
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

MackNet_Boot=function(Simulations,Parameters,Cumulative.T,OutliersControl){
  Full_DF=DevFactors_Net(Parameters$Factors[,2],Parameters$Factors[,1],Cumulative.T,OutliersControl)
  Full_Alpha=Alpha_Net(Parameters$Alpha[,2],Parameters$Alpha[,1],Cumulative.T)
  #Residuals to be bootstrapped are obtained
  Residuals.Boot=Parameters$RNN_Residuals
  ###------------------------------Bootstrap simulation----------------------------------------###
  set.seed(33433)
  n=Simulations;Payments.D=Reservest.D=Reservest_1.D=Ultimatest.D=0;SANN.Backup=0
  Payments.D.Vector=Reservest.D.Vector=Reservest_1.D.Vector=Ultimatest.D.Vector=matrix(0,dim(Cumulative.T)[1],n)
  TrianglesBackup=UpperTrianglesBackup=array(0,dim=c(dim(Cumulative.T)[1],dim(Cumulative.T)[1],n))
  for (i in 1:n){
    Step1=Boot.Residuals.MACK(Cumulative.T,Residuals.Boot)
    Step2=Boot.Ratios.MACK(Cumulative.T,Step1,Full_Alpha,Full_DF)
    Step3=Boot.Dev.Factor.MACK(Cumulative.T,Step2)
    Step4=Cumulative.T.Resampled.MACK(Cumulative.T,Step3,Full_Alpha,Residuals.Boot,"Resampled")
    Step5=ChainLaddert.t_1(Step4,DevFactors.t1.Predictive(Step4))
    Step6=Full.Incremental(Step5)
    Payments.D[i]=Payment(Step6);Ultimatest.D[i]=sum(Step4[,dim(Step4)[1]])
    Reservest.D[i]=Reserve.t(Full.Incremental(Step4));Reservest_1.D[i]=Reserve.t_1(Step6)
    Payments.D.Vector[,i]=Payment.Vector(Step6)
    Reservest.D.Vector[,i]=Reserve.t.Vector(Full.Incremental(Step4))
    Reservest_1.D.Vector[,i]=Reserve.t_1.Vector(Step6)
    Ultimatest.D.Vector[,i]=Step4[,dim(Step4)[1]]
    TrianglesBackup[,,i]=Full.Incremental(Step4)
    UpperTrianglesBackup[,,i]=Upper.Triangle.Mack(Cumulative.T,Step2);SANN.Backup=Step3/n+SANN.Backup
  }
  return(list(Residuals_Boot=Residuals.Boot, Payments.D=Payments.D,
              Reservest.D=Reservest.D,Reservest_1.D=Reservest_1.D,Ultimatest.D=Ultimatest.D,
              Payments.D.Vector=Payments.D.Vector,Reservest.D.Vector=Reservest.D.Vector,
              Reservest_1.D.Vector=Reservest_1.D.Vector,Ultimatest.D.Vector=Ultimatest.D.Vector,
              TrianglesBackup=TrianglesBackup,UpperTrianglesBackup=UpperTrianglesBackup,
              Boot_DevFactors=SANN.Backup, Development_Factors_MackNet=Full_DF, Alpha=Full_Alpha))
}
