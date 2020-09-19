#' @title RNN_Keras
#' @description It creates the keras object to fit each individual RNN.
#' @param Shape1 Length of the input matrix containing the explanatory variables
#' @param Shape2 Number of previous observation of the input matrix containing the explanatory variables
#' @param Learning Learning rate of the RNN.
#' @param wd Weighted decay used within the ADAM optimization algorithm.
#' @param drop Level of dropout regularization.
#' @return Keras object containing the structure and optimization algorithm of the RNN.
#' @import keras
#' @import abind
#' @export
#'

RNN_Keras=function(Shape1, Shape2, Learning, wd, drop){
  Input <- layer_input(shape = c(Shape1,Shape2), dtype = 'float32')
  Encoder <- Input %>% layer_lstm(units = 16, recurrent_dropout=drop, name="Encoder") %>%
    layer_dense(units = 8, activation = 'relu', name = 'EncoderII') %>%
    layer_dropout(drop)
  Decoder_GRU <- Encoder %>% layer_repeat_vector(Shape1) %>%
    layer_lstm(units = 8, name="Decoder_GRU", recurrent_dropout=drop, return_sequences = FALSE) %>%
    layer_dense(units = 8, activation = 'relu', name = 'Decoder_GRUII') %>%
    layer_dropout(drop)
  Decoder_FC <- Encoder %>% layer_dense(units = 4, activation = 'relu', name = 'Decoder_FC') %>%
    layer_dropout(drop) %>%
    layer_dense(units = 8, activation = 'relu', name = 'Decoder_FCII') %>%
    layer_dropout(drop)
  Output <- layer_concatenate(c(Decoder_GRU, Decoder_FC, Encoder)) %>%
    layer_dense(units = 4, activation = 'relu', name="DenseOutput") %>%
    layer_dropout(drop)%>%
    layer_dense(units = 1, activation = 'relu', name = 'Output')
  model <- keras_model(inputs = c(Input), outputs = c(Output))
  Opt=optimizer_adam(lr=Learning,beta_1=0.9, beta_2=0.999, epsilon=NULL, decay=wd, amsgrad=FALSE)
  return(model %>% compile(optimizer = Opt, loss='mean_squared_error'))
}
