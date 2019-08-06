replacement <- function(population, population.size) {
  ordered.population <- population[order(unlist(population$loss)), ]
  return(ordered.population[c(1:population.size), ])
}
