test_that("template modification works", {
  x <- data.frame(a = "'GA'", b = "c(1, 2)") %>%
    set_rownames("chk")
  expr <- row_expr(x["chk",])
  expect_identical(expr, list("GA", c(1, 2)))

  x <- template_modify(template)
  expect_identical(row.names(x), c("example", "description", "constraint", "unique", "missing_allowed"))
  expect_identical(colnames(x), colnames(template))

})
