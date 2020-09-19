#' @title Cumulative.T.Resampled.MACK
#' @description It adds the process variance to the lower triangle.
#' @param Cumulative.T Cumulative triangle.
#' @param Dev.Fact.Boot Predictive development factors resampled randomly.
#' @param Scale.Param Predictive scale parameter estimated by the MackNet model.
#' @param Residuals.Boot Residuals resampled in order to include the process variance to the lower triangle
#' @param Method If this variable is set to "Resampled", the process variance is fully based on the bootstrap method and, thus, the model is distribution-free. The option to rely on "Normal" and "Gamma" distribution to generate the process variance is included.
#' @return Resampled triangle including the process variance.
#' @import keras
#' @import abind
#' @export
#'

Cumulative.T.Resampled.MACK=function(Cumulative.T,Dev.Fact.Boot,Scale.Param,Residuals.Boot,Method){
  if (Method=="Resampled"){
    Dimension=dim(Cumulative.T)[1];Cumulative.T.Resampled=Cumulative.T
    for (i in 2:Dimension){
      for (j in (Dimension-i+2):Dimension){
        Cumulative.T.Resampled[i,j]=Cumulative.T.Resampled[i,j-1]*Dev.Fact.Boot[j-1]+Scale.Param[j-1]*sqrt(abs(Cumulative.T.Resampled[i,j-1]))*sample(Residuals.Boot,1)
      }
    }
  }
  if (Method=="Normal"){
    Dimension=dim(Cumulative.T)[1];Cumulative.T.Resampled=Cumulative.T
    for (i in 2:Dimension){
      for (j in (Dimension-i+2):Dimension){
        Cumulative.T.Resampled[i,j]=rnorm(1,Cumulative.T.Resampled[i,j-1]*Dev.Fact.Boot[j-1],Scale.Param[j-1]*sqrt(abs(Cumulative.T.Resampled[i,j-1])))
      }
    }
  }
  if (Method=="Gamma"){
    Dimension=dim(Cumulative.T)[1];Cumulative.T.Resampled=Cumulative.T
    for (i in 2:Dimension){
      for (j in (Dimension-i+2):Dimension){
        if(Cumulative.T.Resampled[i,j-1]>0){
          Cumulative.T.Resampled[i,j]=rgamma(1,Cumulative.T.Resampled[i,j-1]*((Dev.Fact.Boot[j-1]/Scale.Param[j-1])^2),1/((Scale.Param[j-1]^2)/Dev.Fact.Boot[j-1]))
        }else{
          Cumulative.T.Resampled[i,j]=rnorm(1,Cumulative.T.Resampled[i,j-1]*Dev.Fact.Boot[j-1],Scale.Param[j-1]*sqrt(abs(Cumulative.T.Resampled[i,j-1])))
        }
      }
    }
  }
  return(Cumulative.T.Resampled)
}
