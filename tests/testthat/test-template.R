test_that("template modification works", {
  x <- data.frame(name = "chk", a = "'GA'", b = "c(1, 2)")
  expr <- row_expr(x[x$name == "chk",])
  expect_identical(expr, list("GA", c(1, 2)))

  template <- readxl::read_excel(system.file("extdata/demo_template.xlsx", package = "chktemplate"))

  x <- template_modify(template)
  expect_identical(x$name, c("description", "example", "constraint", "missing_allowed", "unique"))
  expect_identical(colnames(x), colnames(template))

})
