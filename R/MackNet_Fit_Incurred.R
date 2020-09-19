#' @title MackNet_Fit_Incurred
#' @description This function fits the ensemble of RNNs required for the incurred cost MackNet model. The optimum weigthed decay is obtained by selecting the configuration that minimizes the test error.
#' @param Cumulative.T Cumulative payments triangle.
#' @param Incurred.T Incurred cost triangle.
#' @param Exposure Exposure measure. Written premiums is an appropriate measure to scale cumulative payments and incurred cost.
#' @param Epochs Maximum number of epochs.
#' @param MinimumEpochs Minimum number of epochs.
#' @param wd The optimization algorithm used is ADAM. This variable defines the weighted decay value.
#' @param Learning Learning rate.
#' @param drop Dropout regularization.
#' @param Ensemble Number of RNNs included in the ensemble.
#' @param AR This variable allows to remove the autorregressive component from the MackNet model when it is set to 0.
#' @param ES Early Stopping object defined under the keras framework.
#' @param Output Linear or ReLU activation function for the output layer.
#' @return The formula generates the following outputs: \itemize{
#' \item \code{TrianglesBackup} Full triangles predicted by each RNN included within the ensemble.
#' \item \code{Error} Test error.
#' }
#' @import keras
#' @import abind
#' @export
#'

MackNet_Fit_Incurred=function(Cumulative.T,Incurred.T,Exposure,AR,Ensemble,wd,Learning,drop,Epochs,MinimumEpochs,ES,Output){
  dimension=dim(Cumulative.T)[1]                            #Triangle dimension
  Exposure_Matrix=matrix(Exposure,dimension,dimension)      #Exposure for each position
  CumulativeScaled.T=Cumulative.T/Exposure_Matrix           #Scaled cumulative payments
  IncurredScaled.T=Incurred.T/Exposure_Matrix               #Scaled incurred cost
  RNN_DB=DB_Incurred(CumulativeScaled.T,IncurredScaled.T)   #Data for fitting RNNs
  train_x=RNN_DB$train.x;train_y=RNN_DB$train.y             #Train dataset is generated
  test_x=RNN_DB$test.x;test_y=RNN_DB$test.y                 #Test dataset is generated
  PI_Ratio=matrix(colSums(CumulativeScaled.T)/colSums(IncurredScaled.T),dimension,dimension,byrow = T)
  train_x=abind(train_x,CL_Ultimate_Reference(Incurred.T,Exposure,PI_Ratio)$X,along=1)
  train_y=rbind(train_y,CL_Ultimate_Reference(Incurred.T,Exposure,PI_Ratio)$Y)
  if (AR==0){train_x=train_x[,,-1];test_x=test_x[,,-1]}     #Autorregresive component is removed is AR=0
  batch=dim(train_x)[1]                                     #Batch size is equal to the observations of the DB
  #Matrix to save the triangles sampled
  TrianglesBackup=array(0,dim=c(dimension,dimension,Ensemble));Error=0;e=1;iter=1
  #Ensemble of RNNs is fitted for the different wd defined within the grid seach
  while (e<=Ensemble){
    #Model is defined and fitted
    if (Output=="relu"){model=RNN_Keras(dim(train_x)[2], dim(train_x)[3], Learning, wd, drop)} else {model=RNN_Keras_Linear(dim(train_x)[2], dim(train_x)[3], Learning, wd, drop)}
    model %>% fit(train_x, train_y, validation_data=list(test_x, test_y), epochs=MinimumEpochs, batch_size=batch, verbose=0)
    model %>% fit(train_x, train_y, validation_data=list(test_x, test_y), epochs=(Epochs-MinimumEpochs), batch_size=batch,verbose=0,callbacks=ES)
    #Triangle is predicted
    if (AR==1){TrianglesBackup[,,e]=Full.Cumulative(RNN.Triangle(Triangle.Incremental(IncurredScaled.T),PI_Ratio,model))}
    if (AR==0){TrianglesBackup[,,e]=Full.Cumulative(RNN.Triangle.NoAR(Triangle.Incremental(IncurredScaled.T),PI_Ratio,model))}
    #Check if the RNN is vanishing gradients or not
    if (TrianglesBackup[dimension,1,e]==TrianglesBackup[dimension,dimension,e]) {TrianglesBackup[,,e]=0} else {e=e+1; Error=Error+mean(((model %>% predict(test_x))-test_y)^2)/Ensemble}
    #Keras graph is reset
    model=NULL;K <- backend();K$clear_session(); iter=iter+1
    if ((iter>(Ensemble*2)) & (e<=Ensemble)){TrianglesBackup[,,e:Ensemble]=ChainLaddert.t(IncurredScaled.T,DevFactors.t(IncurredScaled.T)); e=Ensemble+1; print("ChainLadder Partially Assumptions Taken")}
  }
  return(list(TrianglesBackup=TrianglesBackup,Error=Error))
}
