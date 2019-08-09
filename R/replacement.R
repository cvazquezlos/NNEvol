#' Replacement operator function
#'
#' @param population population with both base population and generated childs
#' @param population_size size of the population
#'
#' @return Dataframe where only the best first population_size individuals are
#'   chosen
#'
#' @example
#' population <- data.frame(id = rep(NA, 3),
#'                          architecture = rep(NA, 3),
#'                          evaluated = rep(NA, 3),
#'                          loss = rep(NA, 3),
#'                          metric = rep(NA, 3),
#'                          stringsAsFactors = FALSE)
#' population[1, ] <- c(id = 1,
#'                      architecture = "nnn/nn/nnn/n/nnnn",
#'                      evaluated = TRUE,
#'                      loss = 0.34275,
#'                      metric = 0.67329)
#' population[2, ] <- c(id = 2,
#'                      architecture = "nnn/nn/n/nnnn",
#'                      evaluated = TRUE,
#'                      loss = 0.40167,
#'                      metric = 0.54719)
#' population[3, ] <- c(id = 3,
#'                      architecture = "nnn/nn/nnnn",
#'                      evaluated = TRUE,
#'                      loss = 0.20927,
#'                      metric = 0.74127)
#' replacement(population, 2)
replacement <- function(population, population_size) {
  ordered_population <- population[order(unlist(population$loss)), ]
  return(ordered_population[c(1:population_size), ])
}
