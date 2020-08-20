check_template <- function(template){
  chk_data(template)
  chk_superset(names(template), "name")
  x <- template$name
  chk_superset(template$name, c("description", "unique", "chk"))
}

check_template_chkrow <- function(template){
  chk_data(template)
  chk_superset(names(template), "name")
  x <- template$name
  chk_identical(x, "chk")
}
