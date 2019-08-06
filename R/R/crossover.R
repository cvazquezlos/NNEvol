crossover <- function(parent1, parent2, input, output, id.start) {
  parent1.parts <- tail(head(strsplit(parent1$architecture, split = "/"), -1),
                        -1)
  parent2.parts <- tail(head(strsplit(parent2$architecture, split = "/"), -1),
                        -1)
  selected.parent1.part <- sample(parent1.parts, 1)
  selected.parent2.part <- sample(parent2.parts, 1)
  parent1.parts <- lapply(parent1.parts, function(x) {
    if (x == selected.parent1.part) {
      selected.parent2.part
    } else {
      x
    }
  })
  parent2.parts <- lapply(parent2.parts, function(x) {
    if (x == selected.parent2.part) {
      selected.parent1.part
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
  children[1, ] <- c(id.start, add_layers(parent1.parts, input, output),
                     FALSE, NA, NA)
  children[2, ] <- c(id.start + 1, add_layers(parent2.parts, input, output),
                     FALSE, NA, NA)
  return(children)
}

add_layers <- function(layers.hidden, input, output) {
  return(paste(strrep("n", input), "/", paste0(layers.hidden, collapse = "/"),
               "/", strrep("n", output), sep = ""))
}
