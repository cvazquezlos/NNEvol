#' Evaluation operator function
#'
#' @param individual individual which is evaluated
#' @param data data for solving both regression and classification problem
#' @param input number of inputs within the dataset
#' @param output number of outputs within the dataset
#' @param train_mode mode of the training. 0: multi-class, 1: regression
#' @param epochs number of epochs
#' @param batch_size size of the batch
#' @param seed seed
#'
#' @return Dataframe which contains the evaluated individual
evaluation <- function(individual, data, input, output, train_mode, epochs,
                       batch_size, seed) {
  architecture <- individual$architecture
  layers <- utils::head(strsplit(architecture, "/")[[1]], -1)
  layers_hidden <- numeric(0)
  i <- 0
  for (layer in layers) {
    if (i != 0) {
      layers_hidden[i] <- nchar(layer)
    }
    i <- i + 1
  }
  model <- keras::keras_model_sequential()
  model %>% keras::layer_dense(input,
                               activation = "relu",
                               input_shape = c(input),
                               kernel_initializer = keras::
                                 initializer_glorot_uniform(seed))

  for (layer in layers_hidden) {
    model %>% keras::layer_dense(units = layer, activation = "relu")
  }

  if (train_mode == 0) { # Multi-class.
    model %>% keras::layer_dense(output,
                                 activation = "softmax",
                                 kernel_initializer = keras::
                                   initializer_glorot_uniform(seed))
    model %>% keras::compile(optimizer = "adam",
                             loss = "categorical_crossentropy",
                             metrics = c("accuracy"))
  } else { # Regression
    model %>% keras::layer_dense(1,
                                 kernel_initializer = keras::
                                   initializer_glorot_uniform(seed))
    model %>% keras::compile(optimizer = "adam",
                             loss = "mean_squared_error",
                             metrics = c("accuracy"))
  }
  history <- model %>% keras::fit(rbind(data[[1]], data[[2]]),
                                  rbind(data[[4]], data[[5]]),
                                  batch_size = batch_size, callbacks = list(
                                  keras::callback_early_stopping(mode = "auto",
                                  monitor = "val_loss",
                                  patience = epochs * 0.15,
                                  verbose = 0)
                                  ),
                                  epochs = epochs,
                                  validation_split = 0.25, verbose = 0)
  score <- model %>% keras::evaluate(data[[1]], data[[4]])
  individual$evaluated <- TRUE
  individual$loss <- score["loss"][[1]]
  individual$metric <- score["acc"][[1]]
  return(individual)
}
