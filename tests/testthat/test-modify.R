test_that("chk modifiers work", {
  ### chk_to_type
  int <- c(1L, 3L, NA)
  num <- c(1, 2, NA)
  char <- c("a", NA)

  expect_identical(chk_to_type(int), "integer")
  expect_identical(chk_to_type(num), "numeric")
  expect_identical(chk_to_type(char), "character")

})
