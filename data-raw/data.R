## code to prepare `data` dataset goes here

path <- system.file(
  "extdata/demo-template-fish-exploit.xlsx",
  package = "chktemplate"
)
sheets <- readxl::excel_sheets(path)
demo_template_fish_exploit <- lapply(
  sheets,
  function(y) readxl::read_excel(path, y)
)
names(demo_template_fish_exploit) <- sheets
usethis::use_data(demo_template_fish_exploit, overwrite = TRUE)


path <- system.file(
  "extdata/demo-template-tag.csv",
  package = "chktemplate"
)
demo_template_tag <- read.csv(path)
usethis::use_data(demo_template_tag, overwrite = TRUE)
