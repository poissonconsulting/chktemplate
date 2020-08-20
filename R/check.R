check_template <- function(template){
  chk_data(template)
  x <- row.names(template)
  chk_superset(x, c("description", "unique", "chk"))
}
