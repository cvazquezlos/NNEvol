#' Initialization operator function
#'
#' @param population_size size of the population
#' @param input number of inputs within the dataset
#' @param output number of outputs within the dataset
#' @param id_start last id non-used value
#' @param seed seed
#'
#' @keywords internal
#' @noRd
#'
#' @return List with both individual and id.
#'
initialization <- function(population_size, input, output, id_start, seed) {
  set.seed(seed)
  GRAMMAR <- list(
    S = gramEvol::gsrule("<a><h>/<z>"),
    a = gramEvol::grule(input),
    z = gramEvol::grule(output),
    h = gramEvol::gsrule("<h><h>", "<h><h>", "/<n>"),
    n = gramEvol::gsrule("n<n>", "n<n>", "n<n>", "n")
  )
  clean <- function(expression, layers_input, layers_output) {
    expression <- gsub("\"", "", toString(expression[[1]]))
    expression <- gsub("input", layers_input, expression)
    expression <- gsub("output", layers_output, expression)
    return(as.character(expression))
  }

  layers_input <- paste(unlist(rep("n", input)), collapse = "")
  layers_output <- paste(unlist(rep("n", output)), collapse = "")

  population_architectures <- gramEvol::GrammarRandomExpression(
    gramEvol::CreateGrammar(GRAMMAR), population_size
  )
  population_individuals <- data.frame(id = rep(NA, population_size),
                                       architecture = rep(NA, population_size),
                                       evaluated = rep(NA, population_size),
                                       loss = rep(NA, population_size),
                                       metric = rep(NA, population_size),
                                       stringsAsFactors = FALSE)

  for (i in c(1:length(population_architectures))) {
    population_individuals[i, ] <- c(id_start, clean(population_architectures[i],
                                     layers_input, layers_output), FALSE, NA, NA)
    id_start <- id_start + 1
  }
  return(list("population" = population_individuals, "id" = id_start))
}
