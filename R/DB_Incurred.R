#' @title DB_Incurred
#' @description It creates the database required to fit the incurred cost MackNet model.
#' @param ScaledCumulative.T Cumulative payments triangle divided between the exposure measure.
#' @param ScaledIncurred.T Incurred cost triangle divided between the exposure measure.
#' @return The formula generates the following outputs: \itemize{
#' \item \code{train.x} Explanatory variables for training the incurred cost MackNet model.
#' \item \code{test.x} Explanatory variables for testing the incurred cost MackNet model.
#' \item \code{train.y} Response variable for training the incurred cost MackNet model.
#' \item \code{test.y} Response variable for training the incurred cost MackNet model.
#' }
#' @export
#'

DB_Incurred=function(ScaledCumulative.T,ScaledIncurred.T){
  #Payments between incurred ratio is created
  PI_Ratio=matrix(colSums(ScaledCumulative.T)/colSums(ScaledIncurred.T), dim(ScaledCumulative.T)[1], dim(ScaledCumulative.T)[1], byrow = T)
  #Real Cumulative triangle is adapted to the required format to RNN
  DB=BA.Build.DB(Triangle.Incremental(ScaledIncurred.T), PI_Ratio)
  Index=Test.index(ScaledCumulative.T)                                #Last diagonal Index is created to separate test from train
  train=DB[-Index,,];test=DB[Index,,]                                 #Train and test data are separated
  train.x=train[,-ncol(train),];test.x=test[,-ncol(train),]           #Explicative variables
  train.y=train[,ncol(train),1];test.y=test[,ncol(train),1]           #Response Variable
  return(list(train.x=train.x,test.x=test.x,train.y=matrix(train.y),test.y=matrix(test.y)))
}
