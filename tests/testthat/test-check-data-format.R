test_that("Pass when single table and all good values are passed", {
  outing <- data.frame(
    outing_id = c(1L, 2L, 3L),
    site_name = c("Pretty Bay", "Pretty Bay", "Pretty Bay"),
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

test_that("Error when only 1 sheet is supplied in template of 3 sheets when
          complete is set to TRUE", {
  outing <- data.frame(
    outing_id = c(1L, 2L, 3L),
    site_name = c("Pretty Bay", "Pretty Bay", "Pretty Bay"),
    year = c(2010, 2010, 2010),
    month = c(7, 7, 7),
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
      complete = TRUE
    ),
    regexp = "The `complete = TRUE` argument was provided but not all data sets were
        supplied. Either change `complete = FALSE` or supply all the data in the
        `...` argument."
  )
})

test_that("Errors when year beyond range is supplied", {
  outing <- data.frame(
    outing_id = c(1L, 2L, 3L),
    site_name = c("Pretty Bay", "Pretty Bay", "Pretty Bay"),
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

test_that("Errors when year under range is supplied", {
  outing <- data.frame(
    outing_id = c(1L, 2L, 3L),
    site_name = c("Pretty Bay", "Pretty Bay", "Pretty Bay"),
    year = c(2010, 2010, 1000),
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
    site_name = c("Pretty Bay", "Pretty Bay", "Pretty Bay"),
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

test_that("Errors when a column name is not the same as the template", {
  outing <- data.frame(
    outing_id2 = c(1L, 2L, 3L),
    site_name = c("Pretty Bay", "Pretty Bay", "Pretty Bay"),
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
    regexp = "Column names in data must include 'outing_id'\\."
  )
})

test_that("Errors when a missing value supplied to column it is not allowed", {
  outing <- data.frame(
    outing_id = c(1L, 2L, NA_integer_),
    site_name = c("Pretty Bay", "Pretty Bay", "Pretty Bay"),
    year = c(2010, 2010, 2010),
    month = c(7, 7, 7),
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
    regexp = "`outing\\$outing_id` must not have any missing values\\."
  )
})

test_that("Errors when a missing value supplied to year column", {
  outing <- data.frame(
    outing_id = c(1L, 2L, 3L),
    site_name = c("Pretty Bay", "Pretty Bay", "Pretty Bay"),
    year = c(2010, 2010, NA_real_),
    month = c(7, 7, 7),
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
    regexp = "`outing\\$year` must not have any missing values."
  )
})

test_that("No error if extra columns are supplied and extra columna present in
          output", {
  outing <- data.frame(
    outing_id = c(1L, 2L, 3L),
    site_name = c("Pretty Bay", "Pretty Bay", "Pretty Bay"),
    year = c(2010, 2010, 2010),
    month = c(7, 7, 7),
    day = c(15, 16, 17),
    hour_start = c(9L, 11L, 8L),
    minute_start = c(0, 0, 0),
    guide = c("JT", "JT", "JT"),
    rod_count = c(2, 3, 2),
    comment = c(NA_character_, NA_character_, NA_character_),
    extra = c("this", "is", "info")
  )

  data <- check_data_format(
      outing = outing,
      template = demo_template_fish_exploit,
      complete = FALSE
  )

  expect_type(data, "list")
  expect_s3_class(data$outing, "data.frame")
  expect_identical(colnames(outing), colnames(data$outing))
})

test_that("Error if name of data frame doesn't match template", {
  outing <- data.frame(
    outing_id = c(1L, 2L, 3L),
    site_name = c("Pretty Bay", "Pretty Bay", "Pretty Bay"),
    year = c(2010, 2010, 2010),
    month = c(7, 7, 7),
    day = c(15, 16, 17),
    hour_start = c(9L, 11L, 8L),
    minute_start = c(0, 0, 0),
    guide = c("JT", "JT", "JT"),
    rod_count = c(2, 3, 2),
    comment = c(NA_character_, NA_character_, NA_character_),
    extra = c("this", "is", "info")
  )

  expect_error(
    check_data_format(
      visit = outing,
      template = demo_template_fish_exploit,
      complete = FALSE
    ),
    regexp = "The names of the data supplied in the `...` argument do not match the
      template names."
  )
})

test_that("Passes if data frame object has different name but list name matches
          template", {
  visit <- data.frame(
    outing_id = c(1L, 2L, 3L),
    site_name = c("Pretty Bay", "Pretty Bay", "Pretty Bay"),
    year = c(2010, 2010, 2010),
    month = c(7, 7, 7),
    day = c(15, 16, 17),
    hour_start = c(9L, 11L, 8L),
    minute_start = c(0, 0, 0),
    guide = c("JT", "JT", "JT"),
    rod_count = c(2, 3, 2),
    comment = c(NA_character_, NA_character_, NA_character_),
    extra = c("this", "is", "info")
  )

  data <- check_data_format(
    outing = visit,
    template = demo_template_fish_exploit,
    complete = FALSE
  )

  expect_type(data, "list")
  expect_s3_class(data$outing, "data.frame")
})

test_that("Errors when column set to unique is not unique", {
  outing <- data.frame(
    outing_id = c(1L, 2L, 3L),
    site_name = c("Pretty Bay", "Pretty Bay", "Pretty Bay"),
    year = c(2010, 2010, 2010),
    month = c(7, 7, 7),
    day = c(15, 16, 15),
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
    regexp = "Column 'day' of data must be unique."
  )
})

test_that("Errors when column set to pkey is not unique", {
  outing <- data.frame(
    outing_id = c(1L, 2L, 1L),
    site_name = c("Pretty Bay", "Pretty Bay", "Pretty Bay"),
    year = c(2010, 2010, 2010),
    month = c(7, 7, 7),
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
    regexp = "Columns 'outing_id' and 'guide' in outing must be a unique key."
  )
})

test_that("Passes when 2 datasets supplied", {
  outing <- data.frame(
    outing_id = c(1L, 2L, 3L),
    site_name = c("Pretty Bay", "Pretty Bay", "Pretty Bay"),
    year = c(2010, 2010, 2010),
    month = c(7, 7, 7),
    day = c(15, 16, 17),
    hour_start = c(9L, 11L, 8L),
    minute_start = c(0, 0, 0),
    guide = c("JT", "JT", "JT"),
    rod_count = c(2, 3, 2),
    comment = c(NA_character_, NA_character_, NA_character_)
  )

  capture <- data.frame(
    outing_id = c(1L, 2L, 3L),
    guide = c("JT", "JT", "JT"),
    hour = c(7L, 8L, 7L),
    minute = c(0L, 30L, 45L),
    easting = c(1031941, 1031971, 1031944),
    northing = c(892421, 892451, 892429),
    species = c("BT", "CT", "CT"),
    forklength_mm = c(100, 700, 300),
    weight_kg = c(0.5, 10, 4),
    tbartag_number1 = c(78, 91, 82),
    tbartag_number2 = c(14, 18, 21),
    released = c("yes", "no", "no")

  )

  data <- check_data_format(
    outing = outing,
    capture = capture,
    template = demo_template_fish_exploit,
    complete = FALSE
  )

  expect_type(data, "list")
  expect_s3_class(data$outing, "data.frame")
  expect_identical(length(data), 2L)

  expect_type(data$outing$year, "integer")
  expect_type(data$outing$guide, "character")
  expect_type(data$outing$rod_count, "double")
  expect_identical(data$outing$day, c(15L, 16L, 17L))

  expect_type(data$capture$hour, "integer")
  expect_type(data$capture$released, "character")
  expect_type(data$capture$weight_kg, "double")
  expect_identical(data$capture$tbartag_number1, c(78L, 91L, 82L))
})

test_that("Errors when complete set to TRUE when 2 of 3 sheets provided", {
  outing <- data.frame(
    outing_id = c(1L, 2L, 3L),
    site_name = c("Pretty Bay", "Pretty Bay", "Pretty Bay"),
    year = c(2010, 2010, 2010),
    month = c(7, 7, 7),
    day = c(15, 16, 17),
    hour_start = c(9L, 11L, 8L),
    minute_start = c(0, 0, 0),
    guide = c("JT", "JT", "JT"),
    rod_count = c(2, 3, 2),
    comment = c(NA_character_, NA_character_, NA_character_)
  )

  capture <- data.frame(
    outing_id = c(1L, 2L, 3L),
    guide = c("JT", "JT", "JT"),
    hour = c(7L, 8L, 7L),
    minute = c(0L, 30L, 45L),
    easting = c(1031941, 1031971, 1031944),
    northing = c(892421, 892451, 892429),
    species = c("BT", "CT", "CT"),
    forklength_mm = c(100, 700, 300),
    weight_kg = c(0.5, 10, 4),
    tbartag_number1 = c(78, 91, 82),
    tbartag_number2 = c(14, 18, 21),
    released = c("yes", "no", "no")
  )

  expect_error(
    check_data_format(
      outing = outing,
      capture = capture,
      template = demo_template_fish_exploit,
      complete = TRUE
    ),
    regexp = "The `complete = TRUE` argument was provided but not all data sets were
        supplied. Either change `complete = FALSE` or supply all the data in the
        `...` argument."
  )
})

test_that("Passes when 3 data sheets are passed and complete is FALSE", {
  outing <- data.frame(
    outing_id = c(1L, 2L, 3L),
    site_name = c("Pretty Bay", "Pretty Bay", "Pretty Bay"),
    year = c(2010, 2010, 2010),
    month = c(7, 7, 7),
    day = c(15, 16, 17),
    hour_start = c(9L, 11L, 8L),
    minute_start = c(0, 0, 0),
    guide = c("JT", "JT", "JT"),
    rod_count = c(2, 3, 2),
    comment = c(NA_character_, NA_character_, NA_character_)
  )

  capture <- data.frame(
    outing_id = c(1L, 2L, 3L),
    guide = c("JT", "JT", "JT"),
    hour = c(7L, 8L, 7L),
    minute = c(0L, 30L, 45L),
    easting = c(1031941, 1031971, 1031944),
    northing = c(892421, 892451, 892429),
    species = c("BT", "CT", "CT"),
    forklength_mm = c(100, 700, 300),
    weight_kg = c(0.5, 10, 4),
    tbartag_number1 = c(78, 91, 82),
    tbartag_number2 = c(14, 18, 21),
    released = c("yes", "no", "no")
  )

  recapture <- data.frame(
    year = c(2009, 2009),
    month = c(10, 10),
    day = c(14, 15),
    angler = c("Dave John", "John Smith"),
    contact = c("250-637-9999", "250-557-1414"),
    tbartag_number1 = c(92, 57),
    tbartag_number2 = c(10, 12)
  )

  data <- check_data_format(
    outing = outing,
    capture = capture,
    recapture = recapture,
    template = demo_template_fish_exploit,
    complete = FALSE
  )

  expect_type(data, "list")
  expect_s3_class(data$outing, "data.frame")
  expect_identical(length(data), 3L)
})

test_that("Passes with all 4 data sheets and complete is TRUE", {
  site <- data.frame(
    site_name = c("Pretty Bay", "Ugly Bay", "Green Bay")
  )

  outing <- data.frame(
    outing_id = c(1L, 2L, 3L),
    site_name = c("Pretty Bay", "Pretty Bay", "Pretty Bay"),
    year = c(2010, 2010, 2010),
    month = c(7, 7, 7),
    day = c(15, 16, 17),
    hour_start = c(9L, 11L, 8L),
    minute_start = c(0, 0, 0),
    guide = c("JT", "JT", "JT"),
    rod_count = c(2, 3, 2),
    comment = c(NA_character_, NA_character_, NA_character_)
  )

  capture <- data.frame(
    outing_id = c(1L, 2L, 3L),
    guide = c("JT", "JT", "JT"),
    hour = c(7L, 8L, 7L),
    minute = c(0L, 30L, 45L),
    easting = c(1031941, 1031971, 1031944),
    northing = c(892421, 892451, 892429),
    species = c("BT", "CT", "CT"),
    forklength_mm = c(100, 700, 300),
    weight_kg = c(0.5, 10, 4),
    tbartag_number1 = c(78, 91, 82),
    tbartag_number2 = c(14, 18, 21),
    released = c("yes", "no", "no")
  )

  recapture <- data.frame(
    year = c(2009, 2009),
    month = c(10, 10),
    day = c(14, 15),
    angler = c("Dave John", "John Smith"),
    contact = c("250-637-9999", "250-557-1414"),
    tbartag_number1 = c(92, 57),
    tbartag_number2 = c(10, 12)
  )

  data <- check_data_format(
    site = site,
    outing = outing,
    capture = capture,
    recapture = recapture,
    template = demo_template_fish_exploit,
    complete = TRUE
  )

  expect_type(data, "list")
  expect_s3_class(data$outing, "data.frame")
  expect_identical(length(data), 4L)
})

test_that(
  paste(
    "Error when all data sheets given, complete is TRUE, join values are bad",
    "in capture table outing id"), {
  site <- data.frame(
    site_name = c("Pretty Bay", "Ugly Bay", "Green Bay")
  )

  outing <- data.frame(
    outing_id = c(1L, 2L, 3L),
    site_name = c("Pretty Bay", "Pretty Bay", "Pretty Bay"),
    year = c(2010, 2010, 2010),
    month = c(7, 7, 7),
    day = c(15, 16, 17),
    hour_start = c(9L, 11L, 8L),
    minute_start = c(0, 0, 0),
    guide = c("JT", "JT", "JT"),
    rod_count = c(2, 3, 2),
    comment = c(NA_character_, NA_character_, NA_character_)
  )

  capture <- data.frame(
    outing_id = c(1L, 5L, 7L),
    guide = c("JT", "JT", "JY"),
    hour = c(7L, 8L, 7L),
    minute = c(0L, 30L, 45L),
    easting = c(1031941, 1031971, 1031944),
    northing = c(892421, 892451, 892429),
    species = c("BT", "CT", "CT"),
    forklength_mm = c(100, 700, 300),
    weight_kg = c(0.5, 10, 4),
    tbartag_number1 = c(78, 91, 82),
    tbartag_number2 = c(14, 18, 21),
    released = c("yes", "no", "no")
  )

  recapture <- data.frame(
    year = c(2009, 2009),
    month = c(10, 10),
    day = c(14, 15),
    angler = c("Dave John", "John Smith"),
    contact = c("250-637-9999", "250-557-1414"),
    tbartag_number1 = c(92, 57),
    tbartag_number2 = c(10, 12)
  )

  expect_error(
    check_data_format(
      site = site,
      outing = outing,
      capture = capture,
      recapture = recapture,
      template = demo_template_fish_exploit,
      complete = TRUE
    ),
    regexp = paste(
      "All 'outing_id', 'guide' values in the capture table must be in the",
      "outing table. The following rows\\(s\\) in the capture table are",
      "causing the issue\\: 2, 3."
    )
  )
})

test_that(
  paste(
    "Error all data sheets given, complete is TRUE, join values are bad in",
    "capture table guide column"
  ), {
  site <- data.frame(
    site_name = c("Pretty Bay", "Ugly Bay", "Green Bay")
  )

  outing <- data.frame(
    outing_id = c(1L, 2L, 3L),
    site_name = c("Pretty Bay", "Pretty Bay", "Pretty Bay"),
    year = c(2010, 2010, 2010),
    month = c(7, 7, 7),
    day = c(15, 16, 17),
    hour_start = c(9L, 11L, 8L),
    minute_start = c(0, 0, 0),
    guide = c("JT", "JT", "JT"),
    rod_count = c(2, 3, 2),
    comment = c(NA_character_, NA_character_, NA_character_)
  )

  capture <- data.frame(
    outing_id = c(1L, 2L, 3L),
    guide = c("JT", "JT", "JY"),
    hour = c(7L, 8L, 7L),
    minute = c(0L, 30L, 45L),
    easting = c(1031941, 1031971, 1031944),
    northing = c(892421, 892451, 892429),
    species = c("BT", "CT", "CT"),
    forklength_mm = c(100, 700, 300),
    weight_kg = c(0.5, 10, 4),
    tbartag_number1 = c(78, 91, 82),
    tbartag_number2 = c(14, 18, 21),
    released = c("yes", "no", "no")
  )

  recapture <- data.frame(
    year = c(2009, 2009),
    month = c(10, 10),
    day = c(14, 15),
    angler = c("Dave John", "John Smith"),
    contact = c("250-637-9999", "250-557-1414"),
    tbartag_number1 = c(92, 57),
    tbartag_number2 = c(10, 12)
  )

  expect_error(
    check_data_format(
      site = site,
      outing = outing,
      capture = capture,
      recapture = recapture,
      template = demo_template_fish_exploit,
      complete = TRUE
    ),
    regexp = paste0(
      "All 'outing_id', 'guide' values in the capture table must be in the ",
      "outing table. The following rows\\(s\\) in the capture table are ",
      "causing the issue\\: 3."
    )
  )
})

test_that("Error all data sheets given, complete is TRUE, join values are bad
          in site table", {
  site <- data.frame(
    site_name = c("Pretty Bay", "Ugly Bay", "Green Bay")
  )

  outing <- data.frame(
    outing_id = c(1L, 2L, 3L),
    site_name = c("Pretty Bay", "Pretty bay", "Pretty Bay"),
    year = c(2010, 2010, 2010),
    month = c(7, 7, 7),
    day = c(15, 16, 17),
    hour_start = c(9L, 11L, 8L),
    minute_start = c(0, 0, 0),
    guide = c("JT", "JT", "JT"),
    rod_count = c(2, 3, 2),
    comment = c(NA_character_, NA_character_, NA_character_)
  )

  capture <- data.frame(
    outing_id = c(1L, 5L, 7L),
    guide = c("JT", "JT", "JT"),
    hour = c(7L, 8L, 7L),
    minute = c(0L, 30L, 45L),
    easting = c(1031941, 1031971, 1031944),
    northing = c(892421, 892451, 892429),
    species = c("BT", "CT", "CT"),
    forklength_mm = c(100, 700, 300),
    weight_kg = c(0.5, 10, 4),
    tbartag_number1 = c(78, 91, 82),
    tbartag_number2 = c(14, 18, 21),
    released = c("yes", "no", "no")
  )

  recapture <- data.frame(
    year = c(2009, 2009),
    month = c(10, 10),
    day = c(14, 15),
    angler = c("Dave John", "John Smith"),
    contact = c("250-637-9999", "250-557-1414"),
    tbartag_number1 = c(92, 57),
    tbartag_number2 = c(10, 12)
  )

  expect_error(
    check_data_format(
      site = site,
      outing = outing,
      capture = capture,
      recapture = recapture,
      template = demo_template_fish_exploit,
      complete = TRUE
    ),
    regexp = paste(
      "All 'site_name' values in the outing table must be in the site table.",
      "The following rows\\(s\\) in the outing table are causing the",
      "issue\\: 2."
    )
  )
})
