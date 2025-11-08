#' Trim file at marker
#' 
#' Reads a file and trims content after a specified marker line
#' 
#' @param file File path to read
#' @param marker Marker string to search for
#' @return Lines up to and including the marker
#' @keywords internal
trim_file_at_marker <- function(file, marker = "## end of function") {
  # read file lines
  lines <- readLines(file)
  idx <- grep(marker, lines, fixed = TRUE)
  if (length(idx) > 0) {
    lines <- lines[seq_len(idx[1])]
  }
  return(lines)
}

#' Get prediction function based on GUIDE prediction type
#' 
#' @param guide_pred_type Type indicator ('a' or 'b')
#' @return Appropriate prediction function
#' @keywords internal
get_pred_func <- function(guide_pred_type) {
  if (guide_pred_type == "a") {
    make_prediction_tree_a
  } else if (guide_pred_type == "b") {
    make_prediction_tree_b
  } else {
    stop("Unknown guide_pred_type")
  }
}

#' Null-coalescing operator
#' 
#' @param x First value
#' @param y Default value if x is NULL
#' @return x if not NULL, otherwise y
#' @keywords internal
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

#' Type mapping for complexity modes
#' 
#' @keywords internal
type_map <- c(
  constant_exhaustive = "a",
  constant_quantiles  = "a",
  poly1               = "b",
  poly2               = "b",
  stepwise            = "a"
)
