#### Saving RDA files from extdata ####
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
  "extdata/demo-template-data-entry.csv",
  package = "chktemplate"
)
demo_template_data_entry <- read.csv(path)
usethis::use_data(demo_template_data_entry, overwrite = TRUE)
