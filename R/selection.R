#' Selection operator function
#'
#' @param population population of individuals
#' @param children_number number of children
#'
#' @return Dataframe with children_number rows, each one of a child
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
#' selection(population, 1)
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
