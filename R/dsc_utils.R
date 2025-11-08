#' Automatically creates GUIDE DSC files from a dataframe
#' 
#' Defaults to standard out if output is "", or else outputs to the filepath.
#' Standardises the output file:
#'   - input file = data.csv
#'   - missing values = NA
#'   - data would start at row 2
#'   - begin constructing DSC file by guessing data types
#'
#' @param df Data frame to create DSC for
#' @param output Output file path (empty string for stdout)
#' @param input_file Input CSV file name
#' @param missing_values Missing value indicator
#' @param data_start Row number where data starts
#' @return DSC string
#' @export
create_dsc <- function(df, output = "", input_file = 'data.csv',
                       missing_values = 'NA', data_start = 2) {
  dsc_string <- paste(input_file, missing_values, data_start, sep = "\n")
  cols <- c()
  for (i in 1:ncol(df)) {
    class_ <- class(df[, i])
    if (class_ == 'numeric') id = 'n'
    if (class_ == 'factor') id = 'c'
    cols[i] <- paste(i, colnames(df)[i], id)
  }
  dsc_string <- paste(dsc_string, "\n", paste(cols, collapse = "\n"), "\n", sep = "")
  cat(dsc_string, file = output)
  return(dsc_string)
}

#' Clean DSC file lines
#' 
#' @param dsc_lines Vector of DSC file lines
#' @return Cleaned DSC lines
#' @keywords internal
dsc_clean <- function(dsc_lines) {
  dsc_lines <- trimws(dsc_lines)
  dsc_lines[dsc_lines != ""]
}

#' Add weight variable to DSC
#' 
#' @param dsc_lines Vector of DSC file lines
#' @param weight_var Name of weight variable
#' @return DSC lines with weight variable added
#' @keywords internal
dsc_add_weight <- function(dsc_lines, weight_var = "istrain") {
  # get idx of last variable
  last_line <- dsc_lines[length(dsc_lines)]
  last_var_idx <- as.integer(unlist(strsplit(last_line, "\\s+"))[1])
  dsc_lines <- c(dsc_lines, paste(last_var_idx + 1, weight_var, "w"))
  dsc_lines
}

#' Extract variables from DSC file
#' 
#' @param dsc_lines Vector of DSC file lines
#' @return List of variables with their types
#' @keywords internal
dsc_get_variables <- function(dsc_lines) {
  variables <- list()
  for (i in seq_along(dsc_lines)) {
    # first 3 rows relate to missing value handling and rows to skip
    if (i <= 3) next
    # split by spaces and update list
    parts <- unlist(strsplit(dsc_lines[i], "\\s+"))
    var_name <- parts[2]
    var_type <- parts[3]
    variables[[var_name]] <- var_type
  }
  variables[["missing_indicator"]] <- dsc_lines[2]
  variables
}

#' Count missing values in numerical variables
#' 
#' @param x Data frame
#' @param dsc_vars DSC variable list
#' @return Vector of variable names with missing values
#' @keywords internal
count_missing_values <- function(x, dsc_vars) {
  missing_vars <- c()
  for (col in colnames(x)) {
    if (any(is.na(x[[col]])) && dsc_vars[[col]] == "n") {
      missing_vars <- c(missing_vars, col)
    }
  }
  missing_vars
}
