test_that("amino_acids() returns 20 amino acids", {
  the_twenty <-
    c(
      "Ser",
      "Arg",
      "Leu",
      "Pro",
      "Thr",
      "Ala",
      "Val",
      "Gly",
      "Ile",
      "Phe",
      "Tyr",
      "Cys",
      "His",
      "Gln",
      "Asn",
      "Lys",
      "Asp",
      "Glu",
      "Met",
      "Trp"
    )

  expect_identical(amino_acids(), the_twenty)
})
