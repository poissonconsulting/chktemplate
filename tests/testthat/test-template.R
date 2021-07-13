test_that("template modification works", {
  x <- data.frame(name = "chk", a = "'GA'", b = "c(1, 2)")
  expr <- row_expr(x[x$name == "chk", ])
  expect_identical(expr, list("GA", c(1, 2)))

  template <- demo_template$outing
  expr <- chkrow_to_expression(template)
  expect_identical(names(expr), names(template)[-1])
  expect_identical(expr[[1]], c(0L, 1000L))

  x <- template_human(template)
  expect_identical(x$name, c(
    "description", "example", "constraint",
    "missing_allowed", "unique"
  ))
  expect_identical(colnames(x), colnames(template))
})

test_that("test template", {

  template <- shinyupload2::read_template(
    system.file(
      "extdata/template_test.xlsx",
      package = "chktemplate"
    )
  )

  expect_warning(chktemplate::template_human(template[[1]]), regexp = NA)

  human_temp <- chktemplate::template_human(template[[1]])

  expect_equal(
    human_temp$Year[human_temp$name == "constraint"],
    "integer between 2021 and 2041"
  )

  expect_equal(
    human_temp$Month[human_temp$name == "constraint"],
    "integer between 1 and 12"
  )

  expect_equal(
    human_temp$Day[human_temp$name == "constraint"],
    "integer between 1 and 31"
  )

  expect_equal(
    human_temp$start_hour[human_temp$name == "constraint"],
    "integer between 0 and 23"
  )

  expect_equal(
    human_temp$start_minute[human_temp$name == "constraint"],
    "integer between 0 and 59"
  )

  expect_equal(
    human_temp$start_second[human_temp$name == "constraint"],
    "integer between 0 and 59"
  )

})
