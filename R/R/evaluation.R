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
  model %>% layer_dense(units = hidden_layers[1], input_shape = c(I))
}
