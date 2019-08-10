#' Replacement operator function
#'
#' @param population population with both base population and generated childs
#' @param population_size size of the population
#'
#' @keywords internal
#' @noRd
#'
#' @return Dataframe where only the best first population_size individuals are
#'   chosen
replacement <- function(population, population_size) {
  ordered_population <- population[order(unlist(population$loss)), ]
  return(ordered_population[c(1:population_size), ])
}
