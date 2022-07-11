context("data skeleton")

test_that("Properties are contained", {
  skeleton = expect_silent(constructDataSkeleton(iris))
  expect_equal(unique(iris$Species), unique(skeleton$Species))

  sk2 = expect_silent(constructDataSkeleton(iris, n = 100))
  expect_equal(nrow(sk2), 100)
})
