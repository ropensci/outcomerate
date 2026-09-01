test_that("category-specific e values are applied to unknown dispositions", {
  counts <- c(
    I = 10, P = 2, R = 3, NC = 4, O = 1,
    UH = 5, UR = 7, UO = 6, NE = 8
  )
  e_by_class <- c(UO = 0.1, UH = 0.2, UR = 0.8)
  estimated_eligible <- 20 + 0.2 * 5 + 0.8 * 7 + 0.1 * 6

  expected <- c(
    RR3 = 10 / estimated_eligible,
    RR4 = 12 / estimated_eligible,
    REF2 = 3 / estimated_eligible,
    CON2 = 16 / estimated_eligible,
    LOC2 = 20 / estimated_eligible
  )

  expect_equal(
    outcomerate(counts, e = e_by_class, rate = names(expected)),
    expected
  )
  expect_equal(
    outcomerate(counts, e = e_by_class, rate = c("RR1", "RR3")),
    c(RR1 = 10 / 38, RR3 = 10 / estimated_eligible)
  )

  nd <- outcomerate(counts, e = e_by_class, rate = "RR3", return_nd = TRUE)
  expect_equal(unname(nd["RR3", ]), c(10, estimated_eligible))
})

test_that("scalar e remains backward compatible", {
  counts <- c(I = 5, P = 2, R = 1, NC = 3, O = 1, UH = 4, UR = 2, UO = 3)
  scalar <- outcomerate(counts, e = 0.37)

  expect_identical(
    scalar,
    outcomerate(counts, e = c(UH = 0.37, UR = 0.37, UO = 0.37))
  )
  expect_identical(scalar, outcomerate(counts, e = c(ELR = 0.37)))
})

test_that("zero-count unknown categories may be omitted from e", {
  counts <- c(I = 5, P = 1, R = 2, UH = 4, UR = 3, UO = 0)
  e_partial <- c(UR = 0.8, UH = 0.25)
  denominator <- 8 + 0.25 * 4 + 0.8 * 3

  expect_equal(
    outcomerate(counts, e = e_partial, rate = "RR3"),
    c(RR3 = 5 / denominator)
  )

  # Supplying a reusable value for an absent category is also valid.
  expect_equal(
    outcomerate(counts, e = c(e_partial, UO = 0.6), rate = "RR3"),
    c(RR3 = 5 / denominator)
  )

  # A zero-weight category also has a zero aggregate contribution.
  dispositions <- c("I", "UH", "UR", "UO")
  weights <- c(1, 2, 3, 0)
  expect_equal(
    outcomerate(
      dispositions,
      e = e_partial,
      weight = weights,
      rate = "RR3"
    ),
    c(RR3w = 1 / (1 + 0.25 * 2 + 0.8 * 3))
  )
})

test_that("e is unnecessary when all unknown contributions are zero", {
  counts <- c(I = 4, P = 1, R = 2, NC = 3, O = 1)

  expect_equal(
    outcomerate(counts, rate = c("RR3", "RR4", "REF2", "CON2")),
    c(RR3 = 4 / 11, RR4 = 5 / 11, REF2 = 2 / 11, CON2 = 8 / 11)
  )
})

test_that("zero-weight unknown observations do not require e", {
  expect_identical(
    outcomerate(
      c("I", "UR"),
      weight = c(2, 0),
      rate = "RR3"
    ),
    c(RR3w = 1)
  )
})

test_that("category-specific e works across input and weighting methods", {
  dispositions <- c("I", "UH", "UR", "UO")
  e_by_class <- c(UH = 0.2, UR = 0.5, UO = 0.8)
  unweighted_denominator <- 1 + 0.2 + 0.5 + 0.8

  expect_equal(
    outcomerate(dispositions, e = e_by_class, rate = "RR3"),
    c(RR3 = 1 / unweighted_denominator)
  )
  expect_equal(
    outcomerate(factor(dispositions), e = e_by_class, rate = "RR3"),
    c(RR3 = 1 / unweighted_denominator)
  )
  expect_equal(
    outcomerate(table(dispositions), e = e_by_class, rate = "RR3"),
    c(RR3 = 1 / unweighted_denominator)
  )

  weights <- c(2, 3, 4, 5)
  weighted_denominator <- 2 + 0.2 * 3 + 0.5 * 4 + 0.8 * 5
  expect_equal(
    outcomerate(
      dispositions,
      e = e_by_class,
      weight = weights,
      rate = "RR3"
    ),
    c(RR3w = 2 / weighted_denominator)
  )
})

test_that("category-specific e is validated", {
  counts <- c(I = 2, UH = 1, UR = 1, UO = 1)

  expect_error(
    outcomerate(counts, e = c(0.2, 0.3), rate = "RR3"),
    "must be named"
  )
  expect_error(
    outcomerate(
      counts,
      e = stats::setNames(c(0.2, 0.3), c("UH", "UH")),
      rate = "RR3"
    ),
    "must be unique"
  )
  expect_error(
    outcomerate(counts, e = c(UH = 0.2, other = 0.3), rate = "RR3"),
    "drawn from"
  )
  expect_error(
    outcomerate(counts, e = c(UH = 0.2, UR = 0.3), rate = "RR3"),
    "missing estimates.*UO"
  )
  expect_error(
    outcomerate(
      counts,
      e = stats::setNames(c(0.2, 0.3), c("UH", "")),
      rate = "RR3"
    ),
    "must be named"
  )
  expect_error(
    outcomerate(
      counts,
      e = stats::setNames(c(0.2, 0.3), c("UH", NA_character_)),
      rate = "RR3"
    ),
    "must be named"
  )
  expect_error(
    outcomerate(counts, e = c(UH = 0.2, UR = NA), rate = "RR3"),
    "finite, non-missing"
  )
  expect_error(
    outcomerate(counts, e = c(UH = 0.2, UR = Inf), rate = "RR3"),
    "finite, non-missing"
  )
  expect_error(
    outcomerate(counts, e = c(UH = 0.2, UR = 1.1), rate = "RR3"),
    "interval"
  )
  expect_error(
    outcomerate(counts, e = numeric(), rate = "RR3"),
    "at least one"
  )
})

test_that("UR independently requires an eligibility estimate", {
  counts <- c(I = 2, UR = 1)

  expect_error(
    outcomerate(counts, rate = "RR3"),
    "require the parameter 'e'"
  )
  expect_error(
    outcomerate(
      counts,
      e = c(UH = 0.2, UO = 0.3),
      rate = "RR3"
    ),
    "missing estimates.*UR"
  )
})

test_that("weighted e returns weighted numerator and denominator", {
  nd <- outcomerate(
    c("I", "UH", "UR"),
    e = c(UH = 0.25, UR = 0.5),
    weight = c(2, 4, 6),
    rate = "RR3",
    return_nd = TRUE
  )

  expect_identical(rownames(nd), "RR3w")
  expect_identical(unname(nd["RR3w", ]), c(2, 6))
})

test_that("aggregate counts used with e are valid", {
  expect_error(
    outcomerate(c(I = 2, UH = 1, UR = 1, UO = -2), e = c(UH = 0.2, UR = 0.3),
                rate = "RR3"),
    "counts must be non-negative"
  )
  expect_error(
    outcomerate(c(I = 2, UH = 1, UR = 1, UO = Inf), e = c(UH = 0.2, UR = 0.3),
                rate = "RR3"),
    "counts must be finite"
  )
  expect_error(
    outcomerate(c(I = 2, UH = 1, UR = 1, UO = NA_real_),
                e = c(UH = 0.2, UR = 0.3), rate = "RR3"),
    "counts must not contain NA"
  )
})

test_that("e is ignored when requested rates do not use it", {
  counts <- c(I = 2, P = 1, R = 1, UH = 2, UR = 3, UO = 4)
  expected <- outcomerate(counts, rate = c("RR1", "RR5"))

  expect_equal(
    outcomerate(
      counts,
      e = c(unnamed = NA_real_, invalid = Inf),
      rate = c("RR1", "RR5")
    ),
    expected
  )
})
