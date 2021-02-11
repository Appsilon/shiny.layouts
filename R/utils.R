#' Split arguments to positional and named
#'
#' @param ... arguments to split
#'
#' @return
#' A list with two named elements:
#' * `positional`, a list of the positional arguments,
#' * `named`, a list of the named arguments.
#'
#' @md
split_args <- function(...) {
  args <- list(...)
  if (is.null(names(args))) {
    is_named <- logical(length(args))
  } else {
    is_named <- nzchar(names(args))
  }
  return(list(positional = args[!is_named], named = args[is_named]))
}


#' Extracts numeric value from string
#' @param value Value to be converted to numeric
#' @return Numeric value
get_numeric <- function(value) as.numeric(gsub("([0-9]+).*$", "\\1", value))
