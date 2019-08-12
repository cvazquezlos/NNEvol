context("Testing NNEvol")

test_that("Testing function get_best_nn returns a correct value", {
  set.seed(123)
  data <- data.frame(X1 = c(1:10), X2 = c(11:20), X3 = c(1:10), Y1 = c(0:9), stringsAsFactors = FALSE)
  res <- get_best_nn(data, 3, 1, train_mode = 1, population_size = 5, generation_number = 2, seed = 123)
  expect_equal(res$architecture, "nnn/nn/n")
})
