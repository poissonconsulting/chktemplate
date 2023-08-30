## code to prepare `data` dataset goes here

path <- file.path("testdata", "test-template-ex-1.csv")
test_template_1 <- read.csv(path)

path <- file.path("testdata", "test-template-ex-2.xlsx")
test_template_2 <- readxl::read_excel(path)

path <- file.path("testdata", "test-template-ex-3.xlsx")
sheets <- readxl::excel_sheets(path)
test_template_3 <- lapply(
  sheets,
  function(y) readxl::read_excel(path, y)
)
names(test_template_3) <- sheets

path <- file.path("testdata", "test-template-ex-4.xlsx")
sheets <- readxl::excel_sheets(path)
test_template_4 <- lapply(
  sheets,
  function(y) readxl::read_excel(path, y)
)
names(test_template_4) <- sheets

usethis::use_data(
  test_template_1, test_template_2, test_template_3, test_template_4,
  overwrite = TRUE,
  internal = TRUE
)
