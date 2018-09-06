context("Test whether fastpca works.")

test_that("PCA works", {
  x <- matrix(data=rnorm(100), ncol=10, nrow=10)
  expect_silent(fastpca(x, k=4))
  expect_warning(fastpca(x, k=10),
                 "all singular values are requested")
  expect_error(fastpca(x, k=30))
})
