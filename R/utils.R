rm_na <- function(x){
  x[!is.na(x)]
}

any_p <- function(type){
  p("any", type)
}

between_p <- function(type, y){
  p(type, "between", err::cc_and(y))
}

oneof_p <- function(y){
  p("one of", err::cc_or(y))
}

add_row <- function(x, y){
  x[nrow(x) + 1,] <- y
  x
}

row_char <- function(x){
  as.character(as.vector(x))[-1]
}

parse_eval <- function(x) eval(parse(text = x))

row_expr <- function(x){
  sapply(row_char(x), parse_eval, USE.NAMES = FALSE, simplify = TRUE)
}

p <- function(...){
  paste(...)
}
