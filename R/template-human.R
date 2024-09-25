#' Create Human Readable Template
#'
#' Convert code written template to a human readable version.
#'
#' @param template A data.frame of the template. with row names including
#'   'description', 'chk', 'unique' and optionally, 'example'.
#' @export
#' @return The modified data.frame.
#' @details The template must contain a column called 'name'. The name column
#'   includes rows named 'description', 'chk', 'unique' and optionally,
#'   'example', 'unique' and 'join'.
#' @examples
#' template_human(demo_template_count$visit)
#' template_human(demo_template_fish_exploit$capture)
template_human <- function(template) {
  check_template(template)

  x <- as.data.frame(template[template$name == "description", ])
  chk_vals <- chkrow_to_expression(template)

  # examples
  example <- sapply(chk_vals, chk_to_example)
  if ("example" %in% template$name) {
    replace <- row_char(template[template$name == "example", ])
    replace_i <- which(!is.na(replace))
    for (i in replace_i) {
      example[i] <- replace[i]
    }
  }
  x <- add_row(x, c("example", example))

  # constraints
  x <- add_row(x, c("constraint", sapply(chk_vals, chk_to_constraint)))

  # missing
  x <- add_row(x, c("missing allowed", sapply(chk_vals, chk_to_missing)))

  # pkey
  if ("pkey" %in% template$name) {
    lgls <- lgls_to_yesno(row_char(template[template$name == "pkey", ]))
    x <- add_row(x, c("primary key", lgls))
  }

  # unique
  if ("unique" %in% template$name) {
    lgls <- lgls_to_yesno(row_char(template[template$name == "unique", ]))
    x <- add_row(x, c("unique", lgls))
  }

  # joins
  join_list <- template$name[grepl("join", template$name)]
  for (i in join_list) {
    vals <- row_char(template[template$name == i, ])
    x <- add_row(x, c("joins to", vals))
  }

  tibble::as_tibble(x)
}
