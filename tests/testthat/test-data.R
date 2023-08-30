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
