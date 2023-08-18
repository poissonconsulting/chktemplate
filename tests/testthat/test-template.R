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

test_that("Count internal template is same as extdata file", {
  path <- system.file(
    "extdata/demo-template-count.xlsx",
    package = "chktemplate"
  )
  sheets <- readxl::excel_sheets(path)
  template_excel <- lapply(
    sheets,
    function(y) readxl::read_excel(path, y)
  )
  names(template_excel) <- sheets

  expect_identical(chktemplate::demo_template_count, template_excel)
  expect_type(chktemplate::demo_template_count, "list")
})

test_that("Fish exploit internal template is same as extdata file", {
  path <- system.file(
    "extdata/demo-template-fish-exploit.xlsx",
    package = "chktemplate"
  )
  sheets <- readxl::excel_sheets(path)
  template_excel <- lapply(
    sheets,
    function(y) readxl::read_excel(path, y)
  )
  names(template_excel) <- sheets

  expect_identical(chktemplate::demo_template_fish_exploit, template_excel)
  expect_type(chktemplate::demo_template_fish_exploit, "list")
})

test_that("Tag internal template is same as extdata file", {
  path <- system.file(
    "extdata/demo-template-tag.csv",
    package = "chktemplate"
  )
  expect_warning(template_excel <- read.csv(path), regex = "incomplete final line")

  expect_identical(chktemplate::demo_template_tag, template_excel)
  expect_type(chktemplate::demo_template_tag, "list")
  expect_s3_class(chktemplate::demo_template_tag, "data.frame")
})
