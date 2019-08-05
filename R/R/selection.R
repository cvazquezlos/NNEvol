selection <- function(population, child.number) {
  matting.pool <- data.frame()
  for (j in c(1:(child.number / 2))) {
    parents <- population[sample(nrow(population), child.number / 2), ]
    ordered.parents <- parents[order(unlist(parents$loss)), ]
    matting.pool <- rbind(matting.pool, head(ordered.parents, 2))
  }
  return(matting.pool)
}
