`%||%` <- function(a, b) if (!is.null(a)) a else b

trim_file_at_marker <- function(file, marker = "## end of function") {
  # read file lines
  lines <- readLines(file)
  idx <- grep(marker, lines, fixed = TRUE)
  if (length(idx) > 0) {
    lines <- lines[seq_len(idx[1])]
  }
  return(lines)
}

get_pred_func <- function(guide_pred_type) {
  if (guide_pred_type == "a") {
    make_prediction_tree_a
  } else if (guide_pred_type == "b") {
    make_prediction_tree_b
  } else {
    stop("Unknown guide_pred_type")
  }
}

run_guide_command <- function(guide_path) {
  # Use system2 which gives more control
  result <- system2(
    guide_path,
    stdin = "data.in",
    stdout = TRUE,
    stderr = FALSE
  )
  # Check if command failed (non-zero exit code)
  if (!is.null(attr(result, "status")) && attr(result, "status") != 0) {
    # Re-run with stderr visible to debug
    system2(guide_path, stdin = "data.in", stdout = TRUE, stderr = TRUE)
    stop("GUIDE execution failed with error code")
  }
}

TYPE_MAP <- c(
  constant_exhaustive = "a",
  constant_quantiles  = "a",
  poly1               = "b",
  poly2               = "b",
  stepwise            = "a"
)

PRINT_ITERATIONS <- 10
