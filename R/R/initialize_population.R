library(gramEvol)

GRAMMAR <- list(
  S = gsrule("<a><h>/<z>"),
  a = grule("nnnn"),
  z = grule("nnn"),
  h = gsrule("<h><h>", "<h><h>", "/<n>"),
  n = gsrule("n<n>", "n<n>", "n<n>", "n")
)

initialization <- function(population.size, id.start, seed) {
  set.seed(seed)

  population.architectures <- GrammarRandomExpression(CreateGrammar(GRAMMAR),
                                                      population.size)
  population.individuals <- data.frame(id = rep(NA, population.size),
                                       architecture = rep(NA, population.size),
                                       evaluated = rep(NA, population.size),
                                       loss = rep(NA, population.size),
                                       metric = rep(NA, population.size),
                                       saved_model = rep(NA, population.size),
                                       stringsAsFactors = FALSE)

  for (i in c(1:length(population.architectures))) {
    population.individuals[i, ] <- c(id.start, population.architectures[i],
                                     FALSE, NA, NA, NA)
    id.start <- id.start + 1
  }
  population.individuals <- clean_expressions(population.individuals)
  return(list("population" = population.individuals, "id" = id.start))
}

clean_expressions <- function(population.individuals) {
  by(population.individuals, 1:nrow(population.individuals), function(row) {
    print(row$architecture)
    row$architecture = toString(gsub("\"", "", toString(row$architecture)))
    print("---------------------")
    print(row$architecture)
    print(":::::::")
  })
  return(population.individuals)
}
