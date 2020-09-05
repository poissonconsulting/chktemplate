#' Convert chk row to character vector of types
#'
#' @param x A data.frame with chk values for each column.
#' @return A character string of the types.
#' @export
chkrow_to_type <- function(x) {
  x <- row_expr(x[x$name == "chk", ])
  sapply(x, class)
}

#' Convert chk row to list of expressions
#'
#' @param x A data.frame with chk values for each column.
#' Data.frame must have a column called 'name' and a value of 'chk'
#' within that column corresponding to the row of chk values.
#' @return A named list of the chk values by column.
#' @export
chkrow_to_expression <- function(x) {
  check_template(x)
  x <- x[x$name == "chk", ]
  chkvals <- row_expr(x)
  names(chkvals) <- names(x)[-1]
  chkvals
}
