#' @title LastDiagonal
#' @description It obtains the value of the last diagonal of the triangle.
#' @param Triangle.Incremental Triangle
#' @return Aggregated value of the last diagonal of the triangle.
#' @export
#'

LastDiagonal=function(Triangle.Incremental){
  dimension=dim(Triangle.Incremental)[1];Reserve=0
  for (i in 1:(dimension)){
    Reserve=Reserve+sum(Triangle.Incremental[(i),(dimension-i+1)])
  }
  return(Reserve)
}
