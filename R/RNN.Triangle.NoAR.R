#' @title RNN.Triangle.NoAR
#' @description It generates the triangle predicted by an individual RNN. The autorregressive component of the MackNet model is not taken into consideration.
#' @param CumulativeScaled.T Cumulative payments triangle divided between the exposure measure.
#' @param PI_Ratio Observed cumulative payments between incurred cost by development year.
#' @param model Keras object containing the structure and optimization algorithm of the RNN.
#' @return Triangle predicted by an individual RNN. The autorregressive component of the MackNet model is not taken into consideration.
#' @import keras
#' @import abind
#' @export
#'

RNN.Triangle.NoAR=function(CumulativeScaled.T,PI_Ratio,model){
  dimension=dim(CumulativeScaled.T)[1]
  if (CumulativeScaled.T[dimension,1]>max(CumulativeScaled.T[1:(dimension-1),1])) {Reference=1} else {Reference=0}
  for (i in 2:dimension){
    for (j in (dimension-i+2):dimension){
      if (j==dimension){Pred=CumulativeScaled.T[i,]}else{Pred=c(rep(Reference,dimension-j),CumulativeScaled.T[i,1:j])}
      if (j==dimension){Pred2=PI_Ratio[i,]}else{Pred2=c(rep(0,dimension-j),PI_Ratio[i,1:j])}
      Pred=Pred[2:(dimension-1)];Pred2=Pred2[2:(dimension-1)]
      Pred3=c(rep(0,dimension-i),seq(from=1/dimension,to=i/dimension,length.out = i))[2:(dimension-1)]
      Pred4=c(rep(0,dimension-j),seq(from=1/dimension,to=j/dimension,length.out = j))[2:(dimension-1)]
      # Pred.X=array(c(Pred2, Pred3, Pred4), dim=c(1,length(Pred2),3))
      Pred.X=array(c(Pred2, Pred4), dim=c(1,length(Pred2),2))
      CumulativeScaled.T[i,j]=(model %>% predict(Pred.X))
    }
  }
  return(CumulativeScaled.T)
}
