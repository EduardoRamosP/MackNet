#' @title Test.index
#' @description It creates the index to identify the last diagonal, which is used as test set.
#' @param Triangle incurred cost or cumulative payments triangle.
#' @return Index to identify the observations located in the last diagonal.
#' @import keras
#' @import abind
#' @export
#'

Test.index=function(Triangle){
  Location=0;dimension=dim(Triangle)[1]
  for(i in 1:(dimension)){
    if (i==1){Location[i]=dimension-i+1}
    if (i>1) {Location[i]=dimension-i+1+Location[i-1]}
  }
  return(Location)
}
