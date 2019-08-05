library(keras)

evaluation <- function(individual, data, train_mode, epochs, batch_size) {
  layers <- head(strsplit(individual$architeture, "/")[[1]], -1)

  layers.hidden <- numeric(0)
  i <- 0
  for (layer in layers) {
    if (i != 0) {
      layers.hidden[i] <- nchjar(layer)
    }
    i <- i + 1
  }

  model <- keras::keras_model_sequential()
  model %>% keras::layer_dense(activation = "relu",
                               input_shape = c(input),
                               kernel_initializer = "normal",
                               units = layers.hidden[1])

  for (layer in tail(layers.hidden, 1)) {
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

  history <- model %>% keras::fit(rbind(data[0], data[1]),
                                  rbind(data[3], data[4]), callbacks = list(
                                  keras::callback_early_stopping(mode = "auto",
                                  monitor = "val_loss",
                                  patience = epochs * 0.02,
                                  verbose = 0)
                                  ),
                                  validation_split = 0.25, verbose = 0)
  score <- model %>% keras::evaluate(data[0], data[3])
  individual$evaluated <- TRUE
  individual$loss <- score["loss"][[1]]
  individual$metric <- score["acc"][[1]]
  return(individual)
}
