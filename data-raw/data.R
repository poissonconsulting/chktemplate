# template <- as.data.frame(readxl::read_excel(system.file("extdata/demo_template.xlsx", package = "chktemplate")))
template <- as.data.frame(readxl::read_excel("~/Code/chilliwack-exploit-upload/templates/capture_template2.xlsx"))
row.names(template) <- template$outing
template$outing <- NULL

usethis::use_data(template, overwrite = TRUE, internal = TRUE)
