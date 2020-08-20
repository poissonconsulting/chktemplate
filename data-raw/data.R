template <- as.data.frame(readxl::read_excel(system.file("extdata/demo_template.xlsx", package = "chktemplate")))
row.names(template) <- template$outing
template$outing <- NULL

usethis::use_data(template, overwrite = TRUE, internal = TRUE)
