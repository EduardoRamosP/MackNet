#' @title Individual.backtest
#' @description It computes the percentile of the observed data vs the distribution generated for every AY/DY combination
#' @param TrianglesBackup Triangles sampled by the model.
#' @param Validation.T Upper and lower triangle. Needed to validate the model.
#' @return Percentile of the observed data vs the distribution generated for every AY/DY combination
#' @import keras
#' @import abind
#' @export
#'

#It computes the percentile of the observed data vs the distribution generated for every AY/DY combination
Individual.backtest=function(TrianglesBackup,Validation.T){
  dimension=dim(Validation.T)[1];Percentile.Test=0;l=1
  for (i in 2:dimension){
    for (k in dimension:(dimension-i+2)){
      Dist=ecdf(TrianglesBackup[i,k,])
      Percentile.Test[l]=Dist(Validation.T[i,k])
      l=l+1
    }
  }
  return(Percentile.Test)
}
