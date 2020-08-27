test_that("template modification works", {
  x <- data.frame(name = "chk", a = "'GA'", b = "c(1, 2)")
  expr <- row_expr(x[x$name == "chk",])
  expect_identical(expr, list("GA", c(1, 2)))

  template <- demo_template$outing
  expr <- chkrow_to_expression(template)
  expect_identical(names(expr), names(template)[-1])
  expect_identical(expr[[1]], c(0L, 1000L))

  x <- template_human(template)
  expect_identical(x$name, c("description", "example", "constraint", "missing_allowed", "unique"))
  expect_identical(colnames(x), colnames(template))

})
