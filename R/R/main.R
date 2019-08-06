library(gramEvol)
library(keras)
source("R/initialization.R")
source("R/evaluation.R")
source("R/selection.R")
source("R/crossover.R")
source("R/replacement.R")

get_best_nn <- function(data, input, output, train = 0.70, validation = 0.20,
                        test = 0.10, train_mode = "regression",
                        train_final_nn = FALSE, seed = 123, population.size = 30,
                        generation.number = 30) {
  n <- nrow(data)
  data.shuffled <- as.data.frame(data[sample(n), ])
  colnames(data.shuffled) <- gsub("[^a-zA-Z]*", "", colnames(data.shuffled))
  validation <- train + validation
  data.train <- data.shuffled[1:round(train * n), ]
  data.validation <- data.shuffled[(round(train * n) + 1):round(validation *n), ]
  data.test <- data.shuffled[(round(validation * n) + 1):n, ]
  columns.input <- head(colnames(data.shuffled), input)
  columns.output <- tail(colnames(data.shuffled), output)
  data <- list(train[, columns.input] %>% as.matrix(),
               validation[, columns.input] %>% as.matrix(),
               test[, columns.input] %>% as.matrix(),
               train[, columns.output] %>% as.matrix(),
               validation[, columns.output] %>% as.matrix(),
               test[, columns.output] %>% as.matrix())
  if (n < 5000) {
    epochs <- (input * output) + 1
  } else {
    epochs <- round((input * output * 20) / n)
  }
  children.number <- round(generation.number * 0.20)
  if (children.number %% 2 == 0) {
    term <- 2
  } else {
    term <- 1
  }
  initialization.result <- initialization(population.size, input, output, 1,
                                          seed)
  population <- initialization.result$population
  id <- initialization.result$id
  for (individual in 1:nrow(population)) {
    population[individual, ] <- evaluation(population[individial, ],
                                           data, input, output, train_mode,
                                           epochs, 32)
  }
  for (generation in c(1:generation.number)) {
    keras::k_clear_session()
    matting.pool <- selection(population, children.number)
    children <- data.frame(id = integer(), architecture = character(),
                           evaluated = logical(), loss = numeric(),
                           metric = numeric(), stringsAsFactors = FALSE)
    for (i in c(1:round(children.number / term))) {
      j <- (i * 2) - 1
      children <- rbind(children, crossover(matting.pool[j, ],
                                            matting.pool[j + 1, ],
                                            input, output, id))
      children[j, ] <- evaluation(children[j, ], data, input, output,
                                  train_mode, epochs, 32)
      children[j + 1, ] <- evaluation(children[j + 1, ], data, input, output,
                                      train_mode, epochs, 32)
      id <- id + 2
    }
    population <- replacement(rbind(population, children), population.size)
  }
  ordered.population <- population[order(unlist(population$loss)), ]
  individual.best <- ordered.population[1, ]
  if (train_final_nn) {
    if (n < 200) {
      epochs <- 100
    } else {
      epochs <- round(n / 3) - (input * output)
    }
    individual.best <- evaluation(individual.best, data, input, output,
                                  train_mode, epochs, 32)
    return(list("architecture" = individual.best$architecture,
                "loss"= individual.best$loss, "metric" = individual.best$metric))
  }
  return(list("architecture" = individual.best$architecture))
}
