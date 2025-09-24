#' Print a custom model
#'
#' @param x A "my_model" object.
#' @param ... Additional arguments.
#' @export
print.my_model <- function(x, ...) {
  cat("Custom model of type:", x$type, "\n")
  print(x$fit)
}
