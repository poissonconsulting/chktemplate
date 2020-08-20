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

set_class <- function(x, class) {
  class(x) <- class
  x
}

set_names <- function(x, names) {
  names(x) <- names
  x
}

set_rownames <- function(x, names) {
  row.names(x) <- names
  x
}

row_char <- function(x){
  as.character(as.vector(x))
}

parse_eval <- function(x) eval(parse(text = x))

row_expr <- function(x){
  sapply(row_char(x), parse_eval, USE.NAMES = FALSE, simplify = TRUE)
}

sys_time <- function() {
  time <- Sys.time()
  attr(time, "tzone") <- "UTC"
  time
}

is_try_error <- function(x) inherits(x, "try-error")

# from https://recology.info/2018/10/limiting-dependencies/
remove_nulls <- function(x) Filter(Negate(is.null), x)

# from https://recology.info/2018/10/limiting-dependencies/
str_extract <- function(x, y) regmatches(x, regexpr(y, x))

str_extract_all <- function(x, y) regmatches(x, gregexpr(y, x))

# from https://recology.info/2018/10/limiting-dependencies/
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

last <- function(x) x[length(x)]

p <- function(...){
  paste(...)
}

p0 <- function(...){
  paste0(...)
}

