#'
#' @param x A data frame of predictors.
#' @param y A vector of outcomes.
#' @param guide_path Path to GUIDE executable.
#' @param config_path Path to configuration files.
#' @param run_folder Folder for temporary GUIDE files (default: tempdir()).
#' @param type Either "regression" or "binary_classification".
#' @param complexity Model complexity setting.
#' @param eta Learning rate (default: 0.1).
#' @param max_split_levels Maximum number of split levels (default: 4).
#' @param min_node_size Minimum node sample size (default: 4).
#' @param iterations Number of boosting iterations (default: 100).
#' @param bag_fraction Fraction of data for bagging (default: 1.0).
#' @param bag_seed Random seed for bagging (default: NULL).
#' @param val_x Validation data frame (default: NULL).
#' @param val_y Validation target vector (default: NULL).
#' @param early_stop_rounds Number of rounds for early stopping (default: NULL).
#' @param fit_pred_exact Whether to fit predictions exactly (default: TRUE).
#'
#' @return An object of class "guide_gb".
#'
guide_gb <- function(x, y, guide_path, config_path, run_folder=NULL,
                     type = c("regression", "binary_classification"),
                     complexity = c("constant_exhaustive", "constant_quantiles",
                                    "poly1", "poly2", "stepwise"),
                     eta = 0.1,
                     max_split_levels = 4,
                     min_node_size = 4,
                     iterations = 100,
                     bag_fraction = 1.0,
                     bag_seed = NULL,
                     val_x = NULL, val_y = NULL,
                     early_stop_rounds = NULL,
                     fit_pred_exact = TRUE) {
  type <- match.arg(type)
  complexity <- match.arg(complexity)
  # this is only required if fit_pred_exact is TRUE
  guide_pred_type <- TYPE_MAP[[complexity]]
  # validate that bag_fraction is more than 0 and less than or equal to 1
  if (bag_fraction <= 0 || bag_fraction > 1) {
    stop("bag_fraction must be in the range (0, 1].")
  }
  bagging <- ifelse(bag_fraction < 1.0, TRUE, FALSE)
  # if bagging is FALSE and bag_seed is provided, warn user
  if (!bagging && !is.null(bag_seed)) {
    warning("bag_seed is provided but bagging is disabled (bag_fraction = 1.0). The bag_seed will be ignored.")
  }
  # if type is classification, validate that y contains only 0 and 1
  if (type == "binary_classification") {
    if (!all(y %in% c(0, 1))) {
      stop("For binary_classification, y must contain only 0 and 1 values.")
    }
  }
  # check if validation set is provided, raise error if only one of val_x and val_y is provided
  has_watchlist <- !is.null(val_x) && !is.null(val_y)
  has_early_stop <- !is.null(early_stop_rounds)
  if (xor(is.null(val_x), is.null(val_y))) {
    stop("Both val_x and val_y must be provided for validation set.")
  }
  # TODO: next time implement auto splitting of training and validation set
  # if early_stop_round is provided, validate that watchlist is provided
  if (!is.null(early_stop_rounds) && !has_watchlist) {
    stop("Early_stop_rounds is provided, val_x and val_y must be provided.")
  }
  # Validate validation set structure if provided
  if (has_watchlist) {
    # Check same number of columns
    if (ncol(val_x) != ncol(x)) {
      stop("val_x must have the same number of columns as x.")
    }
    # Check same column names
    if (!all(colnames(val_x) == colnames(x))) {
      stop("val_x must have the same column names as x in the same order.")
    }
    # Check val_y length matches val_x rows
    if (length(val_y) != nrow(val_x)) {
      stop("Length of val_y must match number of rows in val_x.")
    }
    # For binary classification, check val_y contains only 0 and 1
    if (type == "binary_classification" && !all(val_y %in% c(0, 1))) {
      stop("For binary_classification, val_y must contain only 0 and 1 values.")
    }
    if (nrow(val_x) == 0) {
      stop("Validation set (val_x) cannot be empty.")
    }
  }
  # Validate early_stop_rounds value
  if (has_early_stop) {
    if (!is.numeric(early_stop_rounds) || length(early_stop_rounds) != 1) {
      stop("early_stop_rounds must be a single numeric value.")
    }
    if (early_stop_rounds < 1) {
      stop("early_stop_rounds must be at least 1.")
    }
    if (early_stop_rounds >= iterations) {
      warning("early_stop_rounds is >= iterations.")
    }
  }
  # raise error that fit_pred_exact = FALSE is only when bagging is FALSE
  if (!fit_pred_exact && bagging) {
    stop("fit_pred_exact = FALSE is only supported when bagging is FALSE (bag_fraction = 1.0).")
  }
  # validate that guide_path exists
  if (!file.exists(guide_path)) {
    stop("guide_path does not exist.")
  }
  # validate that run_folder exists, if not, create a temporary folder
  if (is.null(run_folder)) {
    run_folder <- tempdir()
  }
  if (!dir.exists(run_folder)) {
    stop("run_folder does not exist.")
  }
  # only read .in file contained within the config_path
  in_file <- paste0("data_", complexity, ".in")
  in_file_path <- file.path(config_path, in_file)
  if (!file.exists(in_file_path)) {
    stop(paste("Configuration .in file does not exist at", in_file_path))
  }
  in_file_lines <- readLines(in_file_path)
  # replace max split levels and min node size with user input
  in_file_lines[grep("\\(max. no. split levels\\)", in_file_lines)] <-
    paste0("        ", max_split_levels, "   (max. no. split levels)")
  in_file_lines[grep("\\(min. node sample size\\)", in_file_lines)] <-
    paste0("        ", min_node_size, "   (min. node sample size)")
  # write modified .in file to run_folder and save in attributes
  writeLines(in_file_lines, con = file.path(run_folder, "data.in"))
  in_file_concat <- paste(in_file_lines, collapse = "\n")

  # copy DSC file in config_path to run_folder
  dsc_file_src <- file.path(config_path, "data.DSC")
  if (!file.exists(dsc_file_src)) {
    stop(paste("DSC file does not exist at", dsc_file_src))
  }
  dsc_lines <- dsc_clean(readLines(dsc_file_src))
  # if (bagging || has_watchlist) dsc_lines <- dsc_add_weight(dsc_lines)
  # write modified DSC file to run_folder
  writeLines(dsc_lines, con = file.path(run_folder, "data.DSC"))
  dsc_vars <- dsc_get_variables(dsc_lines)

  # keep track of missing values for subsequent processing for predictions
  ## only relevant for stepwise complexities
  missing_num_vars <- dsc_count_missing_values(x, dsc_vars)

  # if fit_pred_exact is TRUE, then need to pass in guide_pred_type,
  ## only relevant for regression as classifier calculates logodds using nodes
  if (type == "regression") {
    fit <- fit_regression(x = x, y = y,
                          guide_path = guide_path,
                          run_folder = run_folder,
                          eta = eta,
                          iterations = iterations,
                          bagging = bagging,
                          bag_fraction = bag_fraction,
                          complexity = complexity, # only for regression
                          bag_seed = bag_seed,
                          val_x = val_x, val_y = val_y,
                          early_stop_rounds = early_stop_rounds,
                          has_early_stop = has_early_stop,
                          has_watchlist = has_watchlist,
                          fit_pred_exact = fit_pred_exact,
                          guide_pred_type = guide_pred_type, # only for regression
                          missing_num_vars = missing_num_vars # only for regression
      )
  }
  if (type == "binary_classification") {
    fit <- fit_binary_classifier(x = x, y = y,
                                 guide_path = guide_path,
                                 run_folder = run_folder,
                                 eta = eta,
                                 iterations = iterations,
                                 bagging = bagging,
                                 bag_fraction = bag_fraction,
                                 complexity = complexity,
                                 bag_seed = bag_seed,
                                 val_x = val_x, val_y = val_y,
                                 early_stop_rounds = early_stop_rounds,
                                 has_early_stop = has_early_stop,
                                 has_watchlist = has_watchlist,
                                 fit_pred_exact = fit_pred_exact,
                                 guide_pred_type = guide_pred_type,
                                 missing_num_vars = missing_num_vars)
  }
  # Add attributes
  structure(
    list(
      fit = fit,
      guide_path = guide_path,
      config_path = config_path,
      in_file = in_file_concat,
      dsc_vars = dsc_vars,
      missing_num_vars = missing_num_vars,
      guide_pred_type = guide_pred_type,
      eta = eta,
      iterations = iterations,
      complexity = complexity,
      type = type,
      call = match.call(),             # store the call
      nobs = nrow(x),                  # number of observations
      predictors = colnames(x)         # predictor names
    ),
    class = "guide_gb"
  )
}
