#' @title Reserve.t
#' @description It calculates the reserve.
#' @param Triangle.Incremental Upper and lower incremental triangle.
#' @return Reserve evaluated in t.
#' @import keras
#' @import abind
#' @export
#'

Reserve.t=function(Triangle.Incremental){
  dimension=dim(Triangle.Incremental)[1];Reserve=0
  for (i in 1:(dimension-1)){
    Reserve=Reserve+sum(Triangle.Incremental[(i+1),(dimension-i+1):dimension])
  }
  return(Reserve)
}


#' @title Reserve.t_1
#' @description It calculates the reserve in t+1.
#' @param Triangle.Incremental Upper and lower incremental triangle.
#' @return Reserve evaluated in t+1.
#' @import keras
#' @import abind
#' @export
#'

Reserve.t_1=function(Triangle.Incremental){
  dimension=dim(Triangle.Incremental)[1];Reserve=0
  for (i in 1:(dimension-2)){
    Reserve=Reserve+sum(Triangle.Incremental[(i+2),(dimension-i+1):dimension])
  }
  return(Reserve)
}


#' @title Payment
#' @description It calculates the payments in t.
#' @param Triangle.Incremental Upper and lower incremental triangle.
#' @return Payments in t.
#' @import keras
#' @import abind
#' @export
#'

Payment=function(Triangle.Incremental){
  dimension=dim(Triangle.Incremental)[1];Reserve=0
  for (i in 1:(dimension-1)){
    Reserve=Reserve+sum(Triangle.Incremental[(i+1),(dimension-i+1)])
  }
  return(Reserve)
}


#' @title Reserve.t.Vector
#' @description It calculates the reserve by accident year.
#' @param Triangle.Incremental Upper and lower incremental triangle.
#' @return Reserve by accident year evaluated in t.
#' @import keras
#' @import abind
#' @export
#'

Reserve.t.Vector=function(Triangle.Incremental){
  dimension=dim(Triangle.Incremental)[1];Reserve=c(rep(0,dimension-1))
  for (i in 1:(dimension-1)){
    Reserve[i]=sum(Triangle.Incremental[(i+1),(dimension-i+1):dimension])
  }
  return(c(0,Reserve))
}


#' @title Reserve.t_1.Vector
#' @description It calculates the reserve in t+1 by accident year.
#' @param Triangle.Incremental Upper and lower incremental triangle.
#' @return Reserve by accident year evaluated in t+1.
#' @import keras
#' @import abind
#' @export
#'

Reserve.t_1.Vector=function(Triangle.Incremental){
  dimension=dim(Triangle.Incremental)[1];Reserve=c(rep(0,dimension-2))
  for (i in 1:(dimension-2)){
    Reserve[i]=sum(Triangle.Incremental[(i+2),(dimension-i+1):dimension])
  }
  return(c(0,0,Reserve))
}


#' @title Payment.Vector
#' @description It calculates the payments by accident year in t.
#' @param Triangle.Incremental Upper and lower incremental triangle.
#' @return Payments by accident year in t.
#' @import keras
#' @import abind
#' @export
#'

Payment.Vector=function(Triangle.Incremental){
  dimension=dim(Triangle.Incremental)[1];Reserve=c(rep(0,dimension-1))
  for (i in 1:(dimension-1)){
    Reserve[i]=sum(Triangle.Incremental[(i+1),(dimension-i+1)])
  }
  return(c(0,Reserve))
}


#' @title ChainLaddert.t_1
#' @description It applies the Chain Ladder technique assuming that payments in t are true.
#' @param Triangle.Cumulative Cumulative triangle including the payments forecasted in t.
#' @param Development.Factors Development factors used within the Chain Ladder technique.
#' @return Full triangle obtained after the Chain Ladder methodology assuming payments in t are true.
#' @import keras
#' @import abind
#' @export
#'

ChainLaddert.t_1=function(Triangle.Cumulative,Development.Factors){
  dimension=dim(Triangle.Cumulative)[1]
  ChainLadder.Triangle=Triangle.Cumulative
  for (i in 1:(dimension-2)){
    x=ChainLadder.Triangle[,i+1]*Development.Factors[i+1]
    ChainLadder.Triangle[(dimension-i+1):dimension,i+2]=x[(dimension-i+1):dimension]
  }
  return(ChainLadder.Triangle)
}


#' @title ChainLaddert.t
#' @description It applies the Chain Ladder technique assuming that payments in t are true.
#' @param Triangle.Cumulative Cumulative triangle
#' @param Development.Factors Development factors used within the Chain Ladder technique.
#' @return Full triangle obtained after the Chain Ladder methodology.
#' @import keras
#' @import abind
#' @export
#'

ChainLaddert.t=function(Triangle.Cumulative,Development.Factors){
  dimension=dim(Triangle.Cumulative)[1]
  ChainLadder.Triangle=Triangle.Cumulative
  for (i in 1:(dimension-1)){
    x=ChainLadder.Triangle[,i]*Development.Factors[i]
    ChainLadder.Triangle[(dimension-i+1):dimension,i+1]=x[(dimension-i+1):dimension]
  }
  return(ChainLadder.Triangle)
}


#' @title DevFactors.t
#' @description It calculates the development factors derived from the observed information (Upper triangle).
#' @param Triangle.Cumulative Cumulative upper triangle.
#' @return Development factors.
#' @import keras
#' @import abind
#' @export
#'

DevFactors.t=function(Triangle.Cumulative){
  dimension=dim(Triangle.Cumulative)[1]
  Factors=0
  for (i in 1:(dimension-1)){
    Factors[i]=sum(Triangle.Cumulative[1:(dimension-i),i+1])/sum(Triangle.Cumulative[1:(dimension-i),i])
  }
  return(ifelse(Factors>50,50,ifelse(Factors< -50,-50,Factors)))
}


#' @title Full.Cumulative
#' @description It generates a full cumulative triangle taking into consideration the incremental data.
#' @param Triangle.Incremental Incremental upper and lower triangles.
#' @return Cumulative upper and lower triangles.
#' @import keras
#' @import abind
#' @export
#'

Full.Cumulative=function(Triangle.Incremental){
  dimension=dim(Triangle.Incremental)[1]
  Cumulative.Triangle=matrix(0,dimension,dimension)
  for (i in 1:dimension){
    if(i==1) {
      Cumulative=Triangle.Incremental[,i]
      Cumulative.Triangle[,i]=Cumulative
    }
    if (i>1){
      Cumulative.Triangle[,i]=Triangle.Incremental[,i]+Cumulative.Triangle[,i-1]
    }
  }
  return(Cumulative.Triangle)
}


#' @title Full.Incremental
#' @description It generates a full incremental triangle taking into consideration the cumulative data.
#' @param Triangle.Cumulative Cumulative upper and lower triangles.
#' @return Incremental upper and lower triangles.
#' @import keras
#' @import abind
#' @export
#'

Full.Incremental=function(Triangle.Cumulative){
  dimension=dim(Triangle.Cumulative)[1]
  Incremental.Triangle=matrix(0,dimension,dimension)
  for (i in 1:dimension){
    if(i==1) {
      Incremental=Triangle.Cumulative[,i]
      Incremental.Triangle[,i]=Incremental
    }
    if (i>1){
      Incremental.Triangle[,i]=Triangle.Cumulative[,i]-Triangle.Cumulative[,i-1]
    }
  }
  return(Incremental.Triangle)
}


#' @title rotate_clockwise
#' @description It rotates a matrix (Clockwise).
#' @param x Matrix to be rotated
#' @return Rotated matrix (Clockwise).
#' @import keras
#' @import abind
#' @export
#'

rotate_clockwise         <- function(x) { t(     apply(x, 2, rev))}


#' @title rotate_counter_clockwise
#' @description It rotates a matrix (Counter clockwise).
#' @param x Matrix to be rotated
#' @return Rotated matrix (Counter clockwise).
#' @import keras
#' @import abind
#' @export
#'

rotate_counter_clockwise <- function(x) { apply(     t(x),2, rev)}


#' @title SetDiag.0
#' @description Diagonal of a matrix is set to 0.
#' @param Triangle Matrix to be modified.
#' @return Original matrix with the diagonal set to 0.
#' @import keras
#' @import abind
#' @export
#'

SetDiag.0=function(Triangle){
  a=rotate_clockwise(Triangle);diag(a)=0;a=rotate_counter_clockwise(a)
  return(a)
}


#' @title Set.LowTriangle.0
#' @description Lower triangle of a matrix is set to 0.
#' @param Triangle Matrix to be modified.
#' @return Original matrix with lower triangle set to 0.
#' @import keras
#' @import abind
#' @export
#'

Set.LowTriangle.0=function(Triangle){
  Triangle=rotate_clockwise(Triangle)
  Triangle[lower.tri(Triangle,diag=T)]=0
  Triangle=rotate_counter_clockwise(Triangle)
  return(Triangle)

}


#' @title Set.LowTriangle.0.KeepDiag
#' @description Lower triangle of a matrix is set to 0. Diagonal is kept.
#' @param Triangle Matrix to be modified.
#' @return Original matrix with lower triangle set to 0. Diagonal is kept.
#' @import keras
#' @import abind
#' @export
#'

Set.LowTriangle.0.KeepDiag=function(Triangle){
  Triangle=rotate_clockwise(Triangle)
  Triangle[lower.tri(Triangle,diag=F)]=0
  Triangle=rotate_counter_clockwise(Triangle)
  return(Triangle)
}

#' @title Triangle.Cumulative.
#' @description Cumulative from incremental triangle.
#' @param Triangle.Incremental Incremental triangle.
#' @return Cumulative triangle.
#' @import keras
#' @import abind
#' @export
#'

Triangle.Cumulative=function(Triangle.Incremental){
  dimension=dim(Triangle.Incremental)[1]
  Cumulative.Triangle=matrix(0,dimension,dimension)
  for (i in 1:dimension){
    if(i==1) {
      Cumulative=Triangle.Incremental[,i]
      Cumulative.Triangle[,i]=Cumulative
    }
    if (i>1){
      Cumulative=Triangle.Incremental[1:(dimension-i+1),i]+Cumulative.Triangle[1:(dimension-i+1),i-1]
      Cumulative.Triangle[,i]=as.numeric(c(Cumulative,rep(0,i-1)))
    }
  }
  return(Cumulative.Triangle)
}

#' @title Triangle.Incremental.
#' @description Incremental from cumulative triangle.
#' @param Triangle.Cumulative Cumulative triangle
#' @return Incremental triangle.
#' @import keras
#' @import abind
#' @export
#'

Triangle.Incremental=function(Triangle.Cumulative){
  dimension=dim(Triangle.Cumulative)[1]
  Incremental.Triangle=matrix(0,dimension,dimension)
  for (i in 1:dimension){
    if(i==1) {
      Incremental=Triangle.Cumulative[,i]
      Incremental.Triangle[,i]=Incremental
    }
    if (i>1){
      Incremental=Triangle.Cumulative[1:(dimension-i+1),i]-Triangle.Cumulative[1:(dimension-i+1),i-1]
      Incremental.Triangle[,i]=as.numeric(c(Incremental,rep(0,i-1)))
    }
  }
  return(Incremental.Triangle)
}
