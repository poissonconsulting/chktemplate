#' Modify template
#'
#' Modify template to human-readable format.
#'
#' @param template A data.frame of the template with row names including 'description', 'chk', 'unique' and optionally, 'example'.
#' @return The modified data.frame.
#' @export
template_human <- function(template){
  check_template(template)

  x <- as.data.frame(template[template$name == "description",])
  chk_vals <- chkrow_to_expression(template)

  # examples
  example <- sapply(chk_vals, chk_to_example)
  if("example" %in% template$name){
    replace <- row_char(template[template$name == "example",])
    replace_i <- which(!is.na(replace))
    for(i in replace_i){
      example[i] <- replace[i]
    }
  }
  x <- add_row(x, c("example", example))

  # constraints
  x <- add_row(x, c("constraint", sapply(chk_vals, chk_to_constraint)))

  # missing
  x <- add_row(x, c("missing_allowed", sapply(chk_vals, chk_to_missing)))

  # unique
  if("unique" %in% template$name){
    x <- add_row(x, c("unique", lgls_to_yesno(row_char(template[template$name == "unique",]))))
  }

  tibble::as_tibble(x)
}
