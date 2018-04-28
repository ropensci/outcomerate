context("test-rate.R")


# vector with all possible classifications
x1 <- c(
  "I", "P", "I", "NC", "UH", "I", "R", "UO",
  "I", "O", "P", "I", "R", "O", "P"
)

# expected rates if e = 0.5
res1 <- c(
  RR1 = 1 / 3, RR2 = 8 / 15, RR3 = 5 / 14, RR4 = 4 / 7,
  RR5 = 5 / 13, RR6 = 8 / 13, COOP1 = 5 / 12, COOP2 = 2 / 3,
  COOP3 = 1 / 2, COOP4 = 4 / 5, REF1 = 2 / 15, REF2 = 1 / 7,
  REF3 = 2 / 13, CON1 = 4 / 5, CON2 = 6 / 7, CON3 = 12 / 13,
  LOC1 = 13 / 15, LOC2 = 39 / 42
)

# vectors with missing classifications
x2 <- c("P", "NC", "UH", "R", "UO")

test_that("Outcome rates produce known expected results", {
  expect_equal(outcomerate(x1, rate = "RR2"), res1["RR2"])
})


test_that("Weighted rates produce known expected results", {
  # test with a scalar weight
  expect_equal(outcomerate(x1, weight = 3, e = 0.5), res1)

  # test with unequal weights
  w1 <- c(
    3.2, 0.4, 4.9, 3.8, 1.6, 3.3, 1.3, 2.2, 3.5,
    3.6, 0.1, 1.6, 1.1, 4.3, 2.3
  )
  expect_equal(outcomerate(x1, weight = w1, rate = "RR2"),
    expected = c(RR2 = 19.3 / 37.2)
  )

  # '0' weights
  expect_warning(outcomerate(x1, e = 0.5, weight = 0), regexp = "zero")
})

test_that("Rates requiring 'e' must stop if is not provided", {
  r <- c("RR3", "RR4", "REF2", "CON2", "LOC2")
  expect_error(outcomerate(x1, rate = r),
    regexp = "parameter e must be provided"
  )

  expect_equal(outcomerate(x1, e = NA, rate = "LOC1"), res1["LOC1"])
})

test_that("Should return numerator and denominator if asked", {
  res <- structure(c(12, 15), .Dim = 1:2, .Dimnames = structure(list(
    rate = "CON1", side = c("NUM", "DEN")
  ), .Names = c("rate", "side")))
  expect_equal(outcomerate(x1, rate = "CON1", return_nd = TRUE),
    expected = res
  )
})

test_that("Scrambling input vector should not alter the output", {
  # character input
  r1 <- outcomerate(x1, e = 0.3)
  r2 <- outcomerate(sample(x1), e = 0.3)
  expect_equal(r1, r2)

  # numeric input
  freq <- c(I = 3, P = 4, NC = 8, R = 7, O = 11, UO = 3, UH = 1)
  r3 <- outcomerate(freq, e = 0.3)
  r4 <- outcomerate(sample(freq), e = 0.3)
  expect_equal(r3, r4)
})

test_that("Low-count outcome cases should return 0's and NaNs", {
  res <- c(
    RR1 = 0, RR2 = 0, RR3 = 0, RR4 = 0, RR5 = NaN,
    RR6 = NaN, COOP1 = NaN, COOP2 = NaN, COOP3 = NaN, COOP4 = NaN,
    REF1 = 0, REF2 = 0, REF3 = NaN, CON1 = 0, CON2 = 0, CON3 = NaN,
    LOC1 = 0, LOC2 = 0
  )
  expect_equal(outcomerate("UO", e = 0.3), res)
})


test_that("outcomerate should work on a table object", {
  tab <- table(x1)
  expect_equal(outcomerate(tab, e = 0.5), res1)
})

test_that("outcomerate should work on factors", {

  # set-up factors
  levs <- c("I", "P", "R", "NC", "O", "UH", "UO")
  f1 <- as.factor(x1)
  f2 <- factor(x1, levels = levs)
  f3 <- factor(x1, levels = rev(levs), ordered = TRUE)

  expect_equal(outcomerate(f1, e = 0.5), res1)
  expect_equal(outcomerate(f2, e = 0.5), res1)
  expect_equal(outcomerate(f3, e = 0.5), res1)
})
