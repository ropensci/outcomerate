test_that("UR has the expected unknown-case coefficients", {
  fmat <- getFromNamespace("fmat", "outcomerate")
  direct_rates <- c("RR1", "RR2", "REF1", "CON1", "LOC1")
  estimated_rates <- c("RR3", "RR4", "REF2", "CON2", "LOC2")
  unaffected_rates <- setdiff(
    dimnames(fmat)$rate,
    c(direct_rates, estimated_rates)
  )

  expect_true(all(c("UR", "eUR") %in% dimnames(fmat)$outcome))
  expect_identical(
    unname(fmat["UR", direct_rates, "DEN"]),
    rep(1L, length(direct_rates))
  )
  expect_identical(
    unname(fmat["eUR", estimated_rates, "DEN"]),
    rep(1L, length(estimated_rates))
  )
  expect_true(all(fmat[c("UR", "eUR"), , "NUM"] == 0L))
  expect_true(all(fmat["UR", c(estimated_rates, unaffected_rates), "DEN"] == 0L))
  expect_true(all(fmat["eUR", c(direct_rates, unaffected_rates), "DEN"] == 0L))
})

test_that("UR is included in outcome-rate formulas", {
  counts <- c(
    I = 10, P = 2, R = 3, NC = 4, O = 1,
    UH = 5, UR = 7, UO = 6, NE = 8
  )
  known_eligible <- 20
  unknown <- 18
  estimated_eligible <- known_eligible + 0.4 * unknown

  expected <- c(
    RR1 = 10 / (known_eligible + unknown),
    RR2 = 12 / (known_eligible + unknown),
    RR3 = 10 / estimated_eligible,
    RR4 = 12 / estimated_eligible,
    RR5 = 10 / known_eligible,
    RR6 = 12 / known_eligible,
    COOP1 = 10 / 16,
    COOP2 = 12 / 16,
    COOP3 = 10 / 15,
    COOP4 = 12 / 15,
    REF1 = 3 / (known_eligible + unknown),
    REF2 = 3 / estimated_eligible,
    REF3 = 3 / known_eligible,
    CON1 = 16 / (known_eligible + unknown),
    CON2 = 16 / estimated_eligible,
    CON3 = 16 / known_eligible,
    LOC1 = known_eligible / (known_eligible + unknown),
    LOC2 = known_eligible / estimated_eligible
  )

  expect_equal(outcomerate(counts, e = 0.4), expected)
})

test_that("UR works for character, factor, table, and weighted inputs", {
  dispositions <- c("I", "UR", "NE")

  expect_equal(outcomerate(dispositions, rate = "RR1"), c(RR1 = 1 / 2))
  expect_equal(
    outcomerate(factor(dispositions), rate = "RR1"),
    c(RR1 = 1 / 2)
  )
  expect_equal(
    outcomerate(table(dispositions), rate = "RR1"),
    c(RR1 = 1 / 2)
  )
  expect_equal(
    outcomerate(
      c("I", "UR"),
      e = 0.5,
      weight = c(2, 3),
      rate = c("RR1", "RR3")
    ),
    c(RR1w = 2 / 5, RR3w = 2 / 3.5)
  )
})

test_that("UR is accepted by the proportional-allocation eligibility helper", {
  counts <- c(I = 4, P = 1, R = 2, NC = 1, O = 2, UR = 100, NE = 5)

  expect_equal(eligibility_rate(counts), c(ELR = 10 / 15))
  expect_equal(
    eligibility_rate(c("I", "I", "UR", "NE")),
    c(ELR = 2 / 3)
  )
})

test_that("legacy dispositions retain their prior results", {
  legacy <- c(I = 5, P = 3, R = 2, NC = 1, O = 2, UH = 1, UO = 1, NE = 1)
  rates <- c("RR1", "RR3", "COOP1", "REF2", "CON1", "LOC2")
  expected <- c(
    RR1 = 5 / 15,
    RR3 = 5 / 14,
    COOP1 = 5 / 12,
    REF2 = 2 / 14,
    CON1 = 12 / 15,
    LOC2 = 13 / 14
  )

  expect_equal(outcomerate(legacy, e = 0.5, rate = rates), expected)
  expect_identical(
    outcomerate(c(legacy, UR = 0), e = 0.5),
    outcomerate(legacy, e = 0.5)
  )

  # In the 9th edition, code 3.20 was included in UO. With scalar e, moving
  # those cases to the 10th-edition UR aggregate is numerically identical.
  legacy_3_20 <- c(I = 5, P = 1, R = 2, NC = 1, O = 1, UO = 4, NE = 2)
  recoded_3_20 <- c(legacy_3_20[names(legacy_3_20) != "UO"], UR = 4, UO = 0)
  expect_identical(
    outcomerate(legacy_3_20, e = 0.37),
    outcomerate(recoded_3_20, e = 0.37)
  )
})

test_that("generic U is not an alias for population-standard UR", {
  expect_error(outcomerate("U", rate = "RR1"), "not valid")
  expect_error(eligibility_rate("U"), "not valid")
})
