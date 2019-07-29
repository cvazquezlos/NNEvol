library(keras)

evaluation <- function(individual, train_mode, I, O) {
  layers <- head(strsplit(individual$architeture, "/")[[1]], -1)
  hidden_layers <- numeric(0)
  i <- 0
  for (layer in layers) {
    if (i != 0) {
      hidden_layers[i] <- nchjar(layer)
    }
    i <- i + 1
  }
  model <- keras::keras_model_sequential()
  model %>% keras::layer_dense(units = hidden_layers[1], input_shape = c(I),
                        activation = "relu")
  for (layer in tail(hidden_layers, 1)) {
    model %>% keras::layer_dense(units = layer, activation = "relu")
  }
  model %>% keras::layer_dense(units = O, activation = "softmax")
  model %>% keras::compile(optimizer = "adam", loss = "categorical_crossentropy",
                           metrics = c("accuracy"))

}
