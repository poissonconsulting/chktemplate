test_that("Pass when single table and all good values are passed", {
  outing <- data.frame(
    outing_id = c(1L, 2L, 3L),
    year = c(2010, 2010, 2010),
    month = c(07, 07, 07),
    day = c(15, 16, 17),
    hour_start = c(9L, 11L, 8L),
    minute_start = c(0, 0, 0),
    guide = c("JT", "JT", "JT"),
    rod_count = c(2, 3, 2),
    comment = c(NA_character_, NA_character_, NA_character_)
  )

  data <- check_data_format(
    outing = outing,
    template = demo_template_fish_exploit,
    complete = FALSE
  )

  expect_type(data, "list")
  expect_s3_class(data$outing, "data.frame")

  expect_type(data$outing$year, "integer")
  expect_type(data$outing$guide, "character")
  expect_type(data$outing$rod_count, "double")
  expect_identical(data$outing$day, c(15L, 16L, 17L))
})

test_that("Errors when year beyond range is supplied", {
  outing <- data.frame(
    outing_id = c(1L, 2L, 3L),
    year = c(2010, 2010, 3000),
    month = c(07, 07, 07),
    day = c(15, 16, 17),
    hour_start = c(9L, 11L, 8L),
    minute_start = c(0, 0, 0),
    guide = c("JT", "JT", "JT"),
    rod_count = c(2, 3, 2),
    comment = c(NA_character_, NA_character_, NA_character_)
  )

  expect_error(
    check_data_format(
      outing = outing,
      template = demo_template_fish_exploit,
      complete = FALSE
    ),
    regexp = "`outing\\$year` must have values between 2000 and 2099."
  )
})

test_that("Errors when character supplied to numeric column", {
  outing <- data.frame(
    outing_id = c(1L, 2L, 3L),
    year = c(2010, 2010, 2010),
    month = c("seven", "July", 7),
    day = c(15, 16, 17),
    hour_start = c(9L, 11L, 8L),
    minute_start = c(0, 0, 0),
    guide = c("JT", "JT", "JT"),
    rod_count = c(2, 3, 2),
    comment = c(NA_character_, NA_character_, NA_character_)
  )

  expect_error(
    check_data_format(
      outing = outing,
      template = demo_template_fish_exploit,
      complete = FALSE
    ),
    regexp = "The following values in column 'month' should be a integer\\: 'seven' and 'July'\\."
  )
})
