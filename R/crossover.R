#' Crossover operator function
#'
#' @param parent1 first parent that participates in the crossover process
#' @param parent2 second parent that participates in the crossover process
#' @param input number of inputs within the dataset
#' @param output number of outputs within the dataset
#' @param id_start last id non-used value
#'
#' @keywords internal
#' @noRd
#'
#' @return Dataframe which contains two rows, one for each child
#'
crossover <- function(parent1, parent2, input, output, id_start) {
  add_layers <- function(layers_hidden, input, output) {
    return(paste(strrep("n", input), "/", paste0(layers_hidden, collapse = "/"),
                 "/", strrep("n", output), sep = ""))
  }

  parent1_architecture <- parent1$architecture
  parent2_architecture <- parent2$architecture
  parent1_parts <- utils::tail(utils::head(strsplit(parent1_architecture,
                                             split = "/")[[1]], -1), -1)
  parent2_parts <- utils::tail(utils::head(strsplit(parent2_architecture,
                                             split = "/")[[1]], -1), -1)
  selected_parent1_part <- sample(parent1_parts, 1)
  selected_parent2_part <- sample(parent2_parts, 1)
  parent1_parts <- lapply(parent1_parts, function(x) {
    if (x == selected_parent1_part) {
      selected_parent2_part
    } else {
      x
    }
  })
  parent2_parts <- lapply(parent2_parts, function(x) {
    if (x == selected_parent2_part) {
      selected_parent1_part
    } else {
      x
    }
  })
  children <- data.frame(id = rep(NA, 2),
                         architecture = rep(NA, 2),
                         evaluated = rep(NA, 2),
                         loss = rep(NA, 2),
                         metric = rep(NA, 2),
                         stringsAsFactors = FALSE)
  children[1, ] <- c(id_start, add_layers(parent1_parts, input, output),
                     FALSE, NA, NA)
  children[2, ] <- c(id_start + 1, add_layers(parent2_parts, input, output),
                     FALSE, NA, NA)
  return(children)
}
