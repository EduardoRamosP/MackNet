#' @title CL_Ultimate_Reference
#' @description It calculates the ultimate from CL to be included within the ensemble of RNNs
#' @param Cumulative.T Cumulative payments triangle.
#' @param Exposure Exposure measure.
#' @param PI_Ratio Cumulative scaled payments triangle divided between the incurred payments triangle
#' @return The formula generates the following outputs: \itemize{
#' \item \code{X} Explanatory variables for training the incurred cost MackNet model.
#' \item \code{Y} Response variable for training the incurred cost MackNet model.
#' }
#' @export
#'


CL_Ultimate_Reference=function(Cumulative.T,Exposure,PI_Ratio){
  dimension = dim(Cumulative.T)[1]
  CL_Results = Full.Incremental(ChainLaddert.t(Cumulative.T, DevFactors.t(Cumulative.T))/matrix(Exposure, dimension, dimension))
  DY = t(matrix(seq(from = 1/dimension, to = 1, length.out = dimension), dimension, dimension))
  CL_Results_Tail=cbind(CL_Results[,-1],rep(0,dimension))
  DY_Tail=cbind(DY[,-1],rep(1,dimension))
  return(list(X = array(c(CL_Results_Tail[, c(-1, -dimension)], PI_Ratio[, -c(1,2)], DY_Tail[, c(-1, -dimension)]),
                        dim = c(dimension, dimension - 2, 3)), Y = matrix(CL_Results_Tail[,dimension])))
}
