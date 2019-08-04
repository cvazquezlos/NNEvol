library(keras)

evaluation <- function(individual, data, train_mode, input, output) {
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
  model %>% keras::layer_dense(activation = "relu",
                               input_shape = c(input),
                               kernel_initializer = "normal",
                               units = hidden_layers[1])

  for (layer in tail(hidden_layers, 1)) {
    model %>% keras::layer_dense(units = layer, activation = "relu")
  }

  if (train_mode == 0) { # Multi-class.
    model %>% keras::layer_dense(activation = "softmax",
                                 kernel_initializer = "normal",
                                 units = output)
    model %>% keras::compile(optimizer = "adam",
                             loss = "categorical_crossentropy",
                             metrics = c("accuracy"))
  } else { # Regression
    model %>% keras::layer_dense(kernel_initializer = "normal",
                                 units = 1)
    model %>% keras::compile(optimizer = "adam",
                             loss = "mean_squared_error",
                             metrics = c("accuracy"))
  }

  history <- model %>% keras::fit(rbind(data[1], data[2]),
                                  rbind(data[4], data[5]),
                                  validation_split = 0.25)
}
