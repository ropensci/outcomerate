context("test-params.R")

x1 <- c(
  "I", "P", "I", "NC", "UH", "I", "R", "UO",
  "I", "O", "P", "I", "R", "O", "P"
)

test_that("Rates not requiring 'e' should work without it", {
  expect_equal(outcomerate(x1, e = NA, rate = "LOC1"), c(LOC1 = 13 / 15))
})

test_that("utcomerate should not work if 'e' is not numeric", {
  expect_error(outcomerate(x1, e = "none"))
})

test_that("utcomerate should not work if 'e' is not on interval [0, 1]", {
  expect_error(outcomerate(x1, e = -4))
  expect_error(outcomerate(x1, e = 1.5))
})

test_that("outcomerate should not work if 'e' is not a scalar", {
  expect_error(outcomerate(x1, e = c(0.3, 0.5)))
})

test_that("outcomerate should fail if foreign dispositions are used", {
  expect_error(outcomerate(c("I", NA_character_), e = .4))
  expect_error(outcomerate(c("I", "ABC"), e = .4))
  expect_error(outcomerate(c(I = 4, ABC = 3), e = .4))
})

test_that("outcomerate should fail if weight is different length than x", {
  expect_error(outcomerate(x1, weight = rnorm(5), e = .4))
})
