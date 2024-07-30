test_that("amino_acid_pairs() defaults to all 20 by 20 combinations", {
  expect_snapshot(print(amino_acid_pairs(), n = Inf))
})

test_that("amino_acid_pairs() validates amino acids (three letter codes)", {
  expect_error(amino_acid_pairs(x = "S", y = "Thr"))
  expect_error(amino_acid_pairs(x = "Ser", y = "T"))
  expect_error(amino_acid_pairs(x = "S", y = "T"))
  expect_no_error(amino_acid_pairs(x = "Ser", y = "Thr"))
})

test_that("amino_acid_pairs() removes self-self amino acid combinations", {

  with_self <- amino_acid_pairs(x = "Arg", y = c("Arg", "Leu"))
  without_self <- amino_acid_pairs(x = "Arg", y = c("Arg", "Leu"), keep_self = FALSE)

  with_self_expected <- tibble::tibble(x = c("Arg", "Arg"), y = c("Arg", "Leu"))
  without_self_expected <- tibble::tibble(x = "Arg", y = "Leu")

  expect_identical(object = with_self, expected = with_self_expected)
  expect_identical(object = without_self, expected = without_self_expected)
})

test_that("amino_acid_pairs() removes duplicate amino acid combinations", {

  with_duplicates <- amino_acid_pairs(x = c("Arg", "Arg"), y = c("Arg", "Leu"))
  without_duplicates <- amino_acid_pairs(x = c("Arg", "Arg"), y = c("Arg", "Leu"), keep_duplicates = FALSE)

  with_duplicates_expected <- tibble::tibble(x = rep("Arg", 4L), y = rep(c("Arg", "Leu"), 2L))
  without_duplicates_expected <- tibble::tibble(x = c("Arg", "Arg"), y = c("Arg", "Leu"))

  expect_identical(object = with_duplicates, expected = with_duplicates_expected)
  expect_identical(object = without_duplicates, expected = without_duplicates_expected)
})

test_that("amino_acid_pairs() removes reversed amino acid combinations", {

  with_reverses <- amino_acid_pairs(x = c("Arg", "Leu"), y = c("Leu", "Arg"))
  without_reverses <- amino_acid_pairs(x = c("Arg", "Leu"), y = c("Leu", "Arg"), keep_reverses = FALSE)

  with_reverses_expected <- tibble::tibble(x = rep(c("Arg", "Leu"), each = 2L), y = rep(c("Leu", "Arg"), 2L))
  without_reverses_expected <- tibble::tibble(x = c("Arg", "Arg", "Leu"), y = c("Leu", "Arg", "Leu"))

  expect_identical(object = with_reverses, expected = with_reverses_expected)
  expect_identical(object = without_reverses, expected = without_reverses_expected)
})
