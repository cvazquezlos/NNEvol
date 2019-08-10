#' Selection operator function
#'
#' @param population population of individuals
#' @param children_number number of children
#'
#' @keywords internal
#' @noRd
#'
#' @return Dataframe with children_number rows, each one of a child
#'
selection <- function(population, children_number) {
  matting_pool <- data.frame()
  if (children_number %% 2 == 0) {
    term <- 2
  } else {
    term <- 1
  }
  for (j in c(1:round(children_number / term))) {
    parents <- population[sample(nrow(population), children_number), ]
    ordered_parents <- parents[order(unlist(parents$loss)), ]
    matting_pool <- rbind(matting_pool, utils::head(ordered_parents, term))
  }
  return(matting_pool)
}
