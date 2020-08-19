### all chk_to_ functions accept a check vector, e.g. c(0L, 23L, NA)
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


