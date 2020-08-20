#' Convert chk row to character vector of types
#'
#' @param x A data.frame with chk values for each column.
#' @return A character string of the types.
#' @export
chkrow_to_type <- function(x){
  x <- row_expr(x[x$name == "chk",])
  sapply(x, class)
}

#' Convert chk row to list of expressions
#'
#' @param x A data.frame with chk values for each column. Data.frame must have a column called 'name' and a value of 'chk' within that column corresponding to the row of chk values.
#' @return A named list of the chk values by column.
#' @export
chkrow_to_expression <- function(x){
  check_template(x)
  x <- x[x$name == "chk",]
  chkvals <- row_expr(x)
  names(chkvals) <- names(x)[-1]
  chkvals
}

chk_to_missing <- function(x){
  if(any(is.na(x)))
    return("yes")
  "no"
}

chk_to_example <- function(x){
  y <- sort(rm_na(x))
  # if 2 values excluding NA return random within range
  if(length(y) == 2){
    if(class(x) == "integer")
      return(sample(y[1]:y[2], 1))
    if(class(x) == "numeric")
      return(round(runif(1, y[1], y[2]), 1))
  }
  # all other cases
  sample(x, 1)
}

chk_to_constraint <- function(x){
  y <- sort(rm_na(x))
  type <- class(y)
  type <- switch(type,
                 "numeric" = "number",
                 "character" = "word", type)
  # if 2 values excluding NA return random within range
  if(length(y) == 1)
    return(any_p(type))
  if(length(y) == 2){
    if(type %in% c("integer", "number"))
      return(between_p(type, y))
  }
  # all other cases
  oneof_p(y)
}

lgl_to_yesno <- function(x){
  if(x == TRUE & !is.na(x))
    return("yes")
  "no"
}

lgls_to_yesno <- function(x){
  sapply(x, lgl_to_yesno)
}




