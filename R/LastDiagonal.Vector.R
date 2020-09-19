#' @title LastDiagonal.Vector
#' @description It obtains a vector with all the values of the last diagonal of the triangle.
#' @param Triangle.Incremental Triangle
#' @return Vector with all the values of the last diagonal of the triangle.
#' @export
#'

LastDiagonal.Vector=function(Triangle.Incremental){
  dimension=dim(Triangle.Incremental)[1];Reserve=c(rep(0,dimension))
  for (i in 1:(dimension)){
    Reserve[i]=sum(Triangle.Incremental[(i),(dimension-i+1)])
  }
  return(c(Reserve))
}
