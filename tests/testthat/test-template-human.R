test_that("template modification works fish exploit", {
  x <- data.frame(name = "chk", a = "'GA'", b = "c(1, 2)")
  expr <- row_expr(x[x$name == "chk", ])
  expect_identical(expr, list("GA", c(1, 2)))

  template <- demo_template_fish_exploit$outing
  expr <- chkrow_to_expression(template)
  expect_identical(names(expr), names(template)[-1])
  expect_identical(expr[[1]], c(0L, 100L))

  x <- template_human(template)
  expect_identical(
    x$name,
    c(
      "description", "example", "constraint", "missing allowed", "primary key",
      "unique", "joins through"
    )
  )
  expect_identical(colnames(x), colnames(template))
})

test_that("test template", {
  template <- readxl::read_excel(
    system.file(
      "extdata/template_test.xlsx",
      package = "chktemplate"
    )
  )

  expect_warning(chktemplate::template_human(template), regexp = NA)

  human_temp <- chktemplate::template_human(template)

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

test_that("chk modifiers work for data entry template", {
  ### chk_to_type
  template <- demo_template_tag

  expect_identical(
    chkrow_to_type(template[c(1, 2, 3, 5)]),
    c("character", "integer", "character")
  )

  expect_identical(template$name, c("label", "field", "chk", "description"))

  expect_identical(
    as.vector(
      unlist(
        template[1, ]
      )
    ),
    c("label", "Date", "Tag Number 1", "Tag Number 2", "Harvested", "Comments")
  )

  expect_identical(
    template$harvested[4],
    "Whether the fish was harvested"
  )
})

test_that("template modification works count with more then one join", {
  template <- demo_template_count$count
  expr <- chkrow_to_expression(template)
  expect_identical(names(expr), names(template)[-1])
  expect_identical(expr[[1]], c(2000L, 2099L))

  x <- template_human(template)
  expect_identical(
    x$name,
    c(
      "description", "example", "constraint", "missing allowed", "primary key",
      "unique", "joins through", "joins through"
    )
  )
  expect_identical(colnames(x), colnames(template))
})