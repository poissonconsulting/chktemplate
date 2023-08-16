#' Check Data follow Required Format as per the Template
#'
#' Check the types, ranges, sets of values, primary keys, missing values, unique
#' and join requirements of the template against the data to ensure the data
#' follows the format outlined in the template.
#'
#' @param ... A list of named data frames of the data
#' @param template A list of named data frames that make up the template
#' @param joins NULL I think this can be removed...
#' @param complete A logical indicating if all tables present in the template
#'   need to be supplied in the ... argument. If TRUE all tables need to be
#'   provided in ... argument, default is set to FALSE. This must be set to TRUE
#'   to check joins between the tables.
#'
#' @details The template argument should contain all the sheets in the template
#' while the ... argument can be which ever set of data tables you want to check
#' against the template. This means either one, several or all the data tables
#' can be checked against the template.
#'
#' If complete is set to TRUE then all the data tables need to be supplied and
#' the function can check joins between the tables.
#'
#' @return A list of named data frames of the data
#' @export
#'
#' @examples
#' \dontrun{
#' check_data_format(
#'   outing = outing,
#'   template = demo_template_fish_exploit,
#'   complete = FALSE
#' )
#'
#' check_data_format(
#'   outing = outing,
#'   capture = capture,
#'   recapture = recapture,
#'   template = demo_template_fish_exploit,
#'   complete = TRUE
#' )
#' }
check_data_format <- function(..., template, joins = NULL, complete = FALSE) {
  ### TODO not sure if the join argument needs to be specified...
  chk::chk_used(...)
  chk::chk_list(template)
  chk::chk_lgl(complete)

  data <- list(...)
  template_names <- names(template)

  # remove the ones that were provided
  data[vapply(data, is.null, FUN.VALUE = TRUE)] <- NULL
  data_names <- names(data)

  if (!any(data_names %in% template_names)) {
    chk::abort_chk(
      "The names of the data supplied in the `...` argument do not match the
      template names"
    )
  }

  # check individual data sets
  data <- check_column_types(data, template)
  data <- check_template_ranges(data, template)

  # if complete = TRUE then must have all names represented
  # if complete = TRUE then check joins
  if (complete) {
    if (!all(template_names %in% data_names)) {
      chk::abort_chk(
        "The `complete = TRUE` argument was provided but not all data sets were
        supplied. Either change `complete = FALSE` or supply all the data in the
        `...` argument"
      )
    }
    # check the joins
    check_template_join(data, template)
  }
  data
}





check_column_types <- function(data, template) {
  for (i in names(data)) {
    data[[i]] <- check_types(data[[i]], template[[i]])
  }
  data
}

check_types <- function(data, template) {
  x <- template[-1]
  chk::chk_superset(names(data), names(x), x_name = "column names in data")
  extra <- setdiff(names(data), names(x))
  data <- data[c(names(x), extra)]

  type_vals <- chktemplate::chkrow_to_type(template[template$name == "chk", ])

  integer_cols <- names(data)[which(type_vals == "integer")]
  numeric_cols <- names(data)[which(type_vals == "numeric")]
  char_cols <- names(data)[which(type_vals == "character")]

  for (i in integer_cols) {
    data[[i]] <- safe_as_integer(data[[i]], i)
  }

  for (i in numeric_cols) {
    data[[i]] <- safe_as_numeric(data[[i]], i)
  }

  for (i in char_cols) {
    data[[i]] <- as.character(data[[i]])
  }

  data
}

safe_as_integer <- function(x, name) {
  bad <- unique(x[!is.na(x) & suppressWarnings(is.na(as.integer(x)))])
  if (length(bad) > 0) {
    chk::abort_chk(paste0(
      "The following values in column '", name,
      "' should be a integer: ", chk::cc(bad, " and ")
    ))
  }
  as.integer(x)
}

safe_as_numeric <- function(x, name) {
  bad <- unique(x[!is.na(x) & suppressWarnings(is.na(as.numeric(x)))])
  if (length(bad) > 0) {
    chk::abort_chk(paste0(
      "The following values in column '", name,
      "' should be a number: ", chk::cc(bad, " and ")
    ))
  }
  as.numeric(x)
}

check_template_values <- function(data, template, name) {
  x <- template[-1]
  chk_values <- chktemplate::chkrow_to_expression(template)
  key_values <- which(as.logical(as.vector(
    template[template$name == "pkey", ][-1]
  )))
  key_values <- names(x)[key_values]
  chk::check_data(data, values = chk_values, key = key_values, x_name = name)
  if ("unique" %in% template$name) {
    unique_values <- which(as.logical(as.vector(
      template[template$name == "unique", ][-1]
    )))
    unique_values <- names(x)[unique_values]
    if (length(unique_values)) {
      for (i in unique_values) {
        chk::chk_unique(data[[i]], x_name = p0("column '", i, "' of data"))
      }
    }
  }
  data
}

check_template_ranges <- function(data, template) {
  for (i in names(data)) {
    data[[i]] <- check_template_values(data[[i]], template[[i]], i)
  }
  data
}

check_template_join <- function(data, template) {
  for (i in names(template)) {
    if ("join" %in% template[[i]]$name) {

      x <- template[[i]][-1]

      join_by <- which(!is.na(as.vector(
        template[[i]][template[[i]]$name == "join", ][-1]
      )))
      join_by <- names(x)[join_by]
      if (length(join_by) == 0) {
        next()
      }

      # get the table name for the x table
      tbl_x <- unique(template[[i]][[join_by]][template[[i]]$name == "join"])

      # error if more then 1 table is listed
      if (length(tbl_x) < 1) {
        stop("Only 1 table can be joined")
      }

      l <- list(list(
        tbl_y = i,
        tbl_x = tbl_x,
        by = join_by
      ))

      names(l) <- i

      joins <- c(joins, l)

      if (!chk::vld_join(
        data[[joins[[i]]$tbl_y]], data[[joins[[i]]$tbl_x]], by = c(joins[[i]]$by)
      )) {

        no_match <- unique(
          data[[joins[[i]]$tbl_y]][!data[[joins[[i]]$tbl_y]][[joins[[i]]$by]] %in%
                                     data[[joins[[i]]$tbl_x]][[joins[[i]]$by]],][[joins[[i]]$by]]
        )

        chk::abort_chk(
          "All ", joins[[i]]$by, " values in the ", joins[[i]]$tbl_y,
          " table must be in the ", joins[[i]]$tbl_x, " table. The following value(s)
    are not in the ", joins[[i]]$tbl_x, " table:", chk::cc(no_match)
        )
      }
    }
  }
}
