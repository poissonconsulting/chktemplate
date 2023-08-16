test_that("template modification works fish exploit", {
  x <- data.frame(name = "chk", a = "'GA'", b = "c(1, 2)")
  expr <- row_expr(x[x$name == "chk", ])
  expect_identical(expr, list("GA", c(1, 2)))

  template <- demo_template_fish_exploit$outing
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
  template <- demo_template_data_entry

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
    "Was the fish harvested? Answers include yes or no."
  )
})

test_that("chk modifiers work for logger template", {
  template <- demo_template_logger
  expect_identical(colnames(template), c("Sites", "X", "Y"))
  expect_identical(template$Sites[1], "Demo Site")
  expect_equal(template$X[1], -132.282245)
  expect_equal(template$Y[1], 53.352985)
})


test_that("chk modifiers work for points template", {
  template <- demo_template_points_historic
  expect_identical(
    colnames(template),
    c("Longitude", "Latitude", "Year", "Other_Columns")
  )
  expect_equal(template$Longitude[1], -116.9465769)
  expect_equal(template$Latitude[1], 50.24804302)
  expect_equal(template$Year[1], 2020)
})

test_that("chk modifiers work for lines template", {
  template <- demo_template_lines_historic
  expect_identical(
    colnames(template),
    c(
      "Longitude_Start", "Longitude_End", "Latitude_Start", "Latitude_End",
      "Year", "Other_Columns"
    )
  )
  expect_equal(template$Longitude_Start[1], -117.232895)
  expect_equal(template$Longitude_End[1], -117.2388384)
  expect_equal(template$Latitude_Start[1], 50.48322481)
  expect_equal(template$Latitude_End[1], 50.48507439)
  expect_equal(template$Year[1], 2019)
})
