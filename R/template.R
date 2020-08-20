#' Modify template
#'
#' Modify template to human-readable format.
#'
#' @param template A data.frame of the template with row names including 'description', 'chk', 'unique' and optionally, 'example'.
#' @return The modified data.frame.
#' @export
template_modify <- function(template){
  check_template(template)
  x <- template[0,]

  chk_vals <- row_expr(template["chk", ])

  # examples
  example <- sapply(chk_vals, chk_to_example)
  if("example" %in% row.names(template)){
    replace <- row_char(template["example",])
    replace_i <- which(!is.na(replace))
    for(i in replace_i){
      example[i] <- replace[i]
    }
  }
  template["example",] <- example

  # constraints
  template["constraint",] <- sapply(chk_vals, chk_to_constraint)

  # missing
  template["missing_allowed",] <- sapply(chk_vals, chk_to_missing)

  # unique
  template["unique",] <- lgls_to_yesno(row_char(template["unique",]))

  template[c("example", "description", "constraint", "unique", "missing_allowed"),]

}
