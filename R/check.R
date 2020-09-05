#' Check that template is valid.
#'
#' @param x A data.frame of the template.
#' @return An invisible data.frame.
#' @export
check_template <- function(x) {
  chk_data(x)
  chk_superset(names(x), "name")
  chk_superset(x$name, c("description", "pkey", "chk"), x_name = "template column names")
  invisible(x)
}
