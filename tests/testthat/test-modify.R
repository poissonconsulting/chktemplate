test_that("chk modifiers work for fish exploit template", {
  ### chk_to_type
  template <- demo_template_fish_exploit$outing
  expect_identical(
    chkrow_to_type(template[c(1, 2, 10, 16)]),
    c("integer", "character", "numeric")
  )

  ### chk_to_missing
  int <- c(1L, 3L, NA)
  num <- c(1, 2, NA)
  char <- c("a")
  char_na <- c("", NA)

  expect_identical(chk_to_missing(int), "yes")
  expect_identical(chk_to_missing(char), "no")
  expect_identical(chk_to_missing(char_na), "yes")

  ### chk_to_example integer
  one_na <- c(1L, NA)
  one <- c(1L)
  two_na <- c(1L, 5L, NA)
  two <- c(1L, 5L)
  three_na <- c(1L, 2L, 5L, NA)
  three <- c(1L, 2L, 5L)

  expect_true(chk_to_example(one_na) %in% one_na)
  expect_true(chk_to_example(one) == one)
  expect_true(chk_to_example(two_na) %in% c(1:5, NA))
  expect_true(chk_to_example(two) %in% 1:5)
  expect_true(chk_to_example(three_na) %in% three_na)
  expect_true(chk_to_example(three) %in% three)

  ### chk_to_example char
  one_na <- c("a", NA)
  one <- c("a")
  two_na <- c("a", "b", NA)
  two <- c("a", "b")
  three_na <- c("a", "b", "d", NA)
  three <- c("a", "b", "d")

  expect_true(chk_to_example(one_na) %in% one_na)
  expect_true(chk_to_example(one) == one)
  expect_true(chk_to_example(two_na) %in% two_na)
  expect_true(chk_to_example(two) %in% two)
  expect_true(chk_to_example(three_na) %in% three_na)
  expect_true(chk_to_example(three) %in% three)

  # numeric
  one_na <- c(1, NA)
  one <- c(1)
  two_na <- c(1, 4.5, NA)
  two <- c(1, 4.5)
  three_na <- c(1, 4, 5, NA)
  three <- c(1, 4, 5, 6)

  expect_true(chk_to_example(one_na) %in% one_na)
  expect_true(chk_to_example(one) == one)
  x <- chk_to_example(two_na)
  expect_true((x >= 1 & x <= 4.5) | is.na(x))
  x <- chk_to_example(two)
  expect_true(x %in% x >= 1 & x <= 4.5)
  expect_true(chk_to_example(three_na) %in% three_na)
  expect_true(chk_to_example(three) %in% three)

  ### test chk_to_constraing
  expect_identical(chk_to_constraint(c(1)), "any number")
  expect_identical(chk_to_constraint(c(1L)), "any integer")
  expect_identical(chk_to_constraint(c("a")), "any word(s)")

  expect_identical(chk_to_constraint(c("a", "b")), "one of 'a' or 'b'")
  expect_identical(chk_to_constraint(c(1, 2, 3, 4)), "one of 1, 2, 3 or 4")
  expect_identical(chk_to_constraint(c(1, 2, NA)), "number between 1 and 2")
  expect_identical(chk_to_constraint(c(10L, 1L)), "integer between 1 and 10")

  ### test lgl_to_yesno
  expect_identical(lgls_to_yesno(c(TRUE, FALSE, NA)), c("yes", "no", "no"))
})
