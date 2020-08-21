path <- system.file("extdata/demo_template.xlsx", package = "chktemplate")
sheets <- readxl::excel_sheets(path)
demo_template <- lapply(sheets, function(y) readxl::read_excel(path, y))
names(demo_template) <- sheets

usethis::use_data(demo_template, overwrite = TRUE)
