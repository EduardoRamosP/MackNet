#' @title BA.Build.DB
#' @description It creates the database for fitting the ensemble of RNNs.
#' @param Incremental.T Incremental triangle.
#' @param PI_Ratio Payments between incurred ratio.
#' @return Database for fitting the MackNet model. It includes the autorregresive component.
#' @import keras
#' @import abind
#' @export
#'

BA.Build.DB=function(Incremental.T,PI_Ratio){
  dimension=dim(Incremental.T)[1];Count=1;Out=Out1=Out2=Out3=matrix(0,sum(1:dimension),dimension)
  if (Incremental.T[dimension,1]>max(Incremental.T[1:(dimension-1),1])) {Reference=1} else {Reference=0}
  for (i in 1:dimension){
    for (j in 1:(dimension-i+1)){
      Out[Count,]=c(rep(Reference,dimension-i),Incremental.T[j,1:i])
      Out1[Count,]=c(rep(0,dimension-i),PI_Ratio[j,1:i])
      Out2[Count,]=c(rep(0,dimension-j),seq(from=1/dimension,to=j/dimension,length.out = j))
      Out3[Count,]=c(rep(0,dimension-i),seq(from=1/dimension,to=i/dimension,length.out = i))
      Count=Count+1
    }
  }
  # return(array(c(Out[,2:dimension], Out1[,2:dimension], Out2[,2:dimension], Out3[,2:dimension]),
  #              dim=c(dim(Out[,2:dimension])[1], dim(Out[,2:dimension])[2], 4)))
  return(array(c(Out[,2:dimension], Out1[,2:dimension], Out3[,2:dimension]),
               dim=c(dim(Out[,2:dimension])[1], dim(Out[,2:dimension])[2], 3)))
}
