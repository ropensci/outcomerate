test_that("aggregate disposition counts require unique names", {
  duplicate_unknown <- stats::setNames(c(2, 0, 10), c("I", "UH", "UH"))
  duplicate_ineligible <- stats::setNames(c(2, 1, 3), c("I", "NE", "NE"))

  expect_error(
    outcomerate(duplicate_unknown, e = 0.5, rate = "RR3"),
    "Disposition names in 'x' must be unique"
  )
  expect_error(
    outcomerate(duplicate_unknown, rate = "RR3"),
    "Disposition names in 'x' must be unique"
  )
  expect_error(
    eligibility_rate(duplicate_ineligible),
    "Disposition names in 'x' must be unique"
  )
  expect_error(
    outcomerate(as.table(duplicate_unknown), e = 0.5, rate = "RR3"),
    "Disposition names in 'x' must be unique"
  )
  expect_error(
    eligibility_rate(as.table(duplicate_ineligible)),
    "Disposition names in 'x' must be unique"
  )
})

test_that("aggregate inputs reject observation-level weights", {
  outcome_counts <- c(I = 1, UH = 1)
  eligibility_counts <- c(I = 1, NE = 1)
  message <- "can only be supplied.*individual dispositions"

  for (x in list(outcome_counts, as.table(outcome_counts))) {
    expect_error(
      outcomerate(x, rate = "RR1", weight = c(1, 1)),
      message
    )
  }

  for (x in list(eligibility_counts, as.table(eligibility_counts))) {
    expect_error(
      eligibility_rate(x, weight = c(1, 1)),
      message
    )
  }
})

test_that("observation-level weighting retains weighted output names", {
  dispositions <- c("I", "UH")

  expect_identical(
    outcomerate(dispositions, rate = "RR1", weight = c(2, 3)),
    c(RR1w = 2 / 5)
  )
  expect_identical(
    eligibility_rate(c("I", "NE"), weight = c(2, 3)),
    c(ELR = 2 / 5)
  )
})

test_that("eligibility rates accept aggregate tables without weights", {
  counts <- as.table(c(I = 2, NE = 3))

  expect_identical(eligibility_rate(counts), c(ELR = 2 / 5))
})
