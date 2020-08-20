#' Convert chk to type
#'
#' @param x A vector of the check values (e.g. c(1, 2, NA)).
#' @return A character string of the type.
#' @export
chk_to_type <- function(x){
  class(x)
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




