#' Calculates the best neural network architecture that fixes a given problem
#'
#' @param data entry dataset
#' @param input number of inputs within the dataset
#' @param output number of outputs within the dataset
#' @param train proportion of data that is used to train each individual
#' @param validation proportion of data that is used to validate each individual
#' @param test proportion of data that is used to evaluate the final individual
#'   if option train_final_nn is TRUE
#' @param train_mode mode of the training. 0: multi-class, 1: regression
#' @param train_final_nn if the final individual is evaluated or not
#' @param seed seed
#' @param population_size size of the population
#' @param generation_number number of generations
#'
#' @importFrom magrittr "%>%"
#' @export
#'
#' @return List with both architecture and final loss
#'
#' @examples {
#'   data <- data.frame(X1 = c(1:10), X2 = c(11:20), X3 = c(1:10), Y1 = c(0:9),
#'                      stringsAsFactors = FALSE)
#'   get_best_nn(data, 3, 1, train_mode = 1, population_size = 5,
#'               generation_number = 2)
#' }
#'
get_best_nn <- function(data, input, output, train = 0.70, validation = 0.20,
                        test = 0.10, train_mode = 0, train_final_nn = FALSE,
                        seed = 123, population_size = 30, generation_number = 30) {
  set.seed(seed)
  n <- nrow(data)
  data_shuffled <- as.data.frame(data[sample(n), ])
  colnames(data_shuffled) <- gsub("[^a-zA-Z]*", "", colnames(data_shuffled))
  validation <- train + validation
  data_train <- data_shuffled[1:round(train * n), ]
  data_validation <- data_shuffled[(round(train * n) + 1):round(validation *n), ]
  data_test <- data_shuffled[(round(validation * n) + 1):n, ]
  columns.input <- utils::head(colnames(data_shuffled), input)
  columns.output <- utils::tail(colnames(data_shuffled), output)
  data <- list(data_train[, columns.input] %>% as.matrix(),
               data_validation[, columns.input] %>% as.matrix(),
               data_test[, columns.input] %>% as.matrix(),
               data_train[, columns.output] %>% as.matrix(),
               data_validation[, columns.output] %>% as.matrix(),
               data_test[, columns.output] %>% as.matrix())
  if (n < 5000) {
    epochs <- (input * output) + 1
  } else {
    epochs <- round((input * output * 20) / n)
  }
  children_number <- round(population_size * 0.20)
  if (children_number <= 1) {
    children_number <- 2
  }
  if (children_number %% 2 == 0) {
    term <- 2
  } else {
    term <- 1
  }
  initialization_result <- initialization(population_size, input, output, 1,
                                          seed)
  population <- initialization_result$population
  id <- initialization_result$id
  for (i in 1:nrow(population)) {
    population[i, ] <- evaluation(population[i, ],
                                  data, input, output, train_mode,
                                  epochs, 32, seed)
  }
  keras::use_session_with_seed(seed)
  for (generation in c(1:generation_number)) {
    keras::k_clear_session()
    matting_pool <- selection(population, children_number, seed)
    children <- data.frame(id = integer(), architecture = character(),
                           evaluated = logical(), loss = numeric(),
                           metric = numeric(), stringsAsFactors = FALSE)
    for (i in c(1:round(children_number / term))) {
      j <- (i * 2) - 1
      children <- rbind(children, crossover(matting_pool[j, ],
                                            matting_pool[j + 1, ],
                                            input, output, id))
      children[j, ] <- evaluation(children[j, ], data, input, output,
                                  train_mode, epochs, 32, seed)
      children[j + 1, ] <- evaluation(children[j + 1, ], data, input, output,
                                      train_mode, epochs, 32, seed)
      id <- id + 2
    }
    population <- replacement(rbind(population, children), population_size)
  }
  ordered.population <- population[order(unlist(population$loss)), ]
  print(ordered.population)
  individual_best <- ordered.population[1, ]
  if (train_final_nn) {
    if (n < 200) {
      epochs <- 100
    } else {
      epochs <- round(n / 3) - (input * output)
    }
    individual_best <- evaluation(individual_best, data, input, output,
                                  train_mode, epochs, 32, seed)
    return(list("architecture" = individual_best$architecture,
                "loss"= individual_best$loss, "metric" = individual_best$metric))
  }
  return(list("architecture" = individual_best$architecture))
}
