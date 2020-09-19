#' @title MainResults
#' @description This function collects the ultimate and reserve from the Schedule P Database paid triangles. Additionally, paid MackNet results are reported.
#' @param CompanyCode Company code as defined by the Schedule P database
#' @param CumulativeValidation Validation triangle.
#' @param Model Paid MackNet model.
#' @return It returns a matrix with following information: \itemize{
#' \item \code{Company Code} Company code of Schedule P Database.
#' \item \code{Obs. Reserve} Reserve observed within the Schedule P Database.
#' \item \code{Obs. Ultimate} Ultimate observed within the Schedule P Database.
#' \item \code{Est. Ultimate} Ultimate estimated by the paid MackNet model.
#' \item \code{SE Ultimate} Squared error of the ultimates estimated by the MackNet model.
#' \item \code{Percentile} It compares the distribution sampled by the paid MackNet model with the reserve actually observed. The percentile where this last value is located within the stochastic process is given.
#' }
#' @import keras
#' @import abind
#' @export
#'

MainResults=function(CompanyCode,CumulativeValidation,Model){
  Data1=CompanyCode
  Data2=Reserve.t(Full.Incremental(CumulativeValidation))
  Data3=mean(Model$SampledReserves)
  Data4=sum(CumulativeValidation[,dim(CumulativeValidation)[1]])
  Data5=mean(Model$SampledUltimate)
  Data6=(Data5-Data4)^2
  ECDF_R=ecdf(Model$SampledReserves);Data7=ECDF_R(Data2)
  Out=matrix(c(Data1,Data2,Data3,Data4,Data5,Data6,Data7),1,7)
  colnames(Out) = c("Company Code", "Obs. Reserve", "Est. Reserve", "Obs. Ultimate", "Est. Ultimate", "SE Ultimate", "Percentile")
  return(Out)
}
