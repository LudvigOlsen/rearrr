

#   __________________ #< beae0116fffd26a3055dd7425c5928ad ># __________________
#   Mutators                                                                ####


#' Wrapper for running mutator methods
#'
#' @param data \code{data.frame} or \code{vector}.
#' @param col Column to mutate values of. Must be specified when \code{`data`} is a \code{data.frame}.
#' @param new_name Name of the new column with the mutated values. If \code{NULL}, the original \code{`col`} column is mutated directly.
#' @param mutate_fn Mutator to apply.
#' @param check_fn Function with checks post-preparation of \code{`data`} and \code{`col`}.
#'  Should not return anything.
#' @param force_df Whether to return a \code{data.frame} when \code{`data`} is a \code{vector}.
#' @param allowed_types Allowed types of the \code{`col(s)`} colums. The type restrictions do not apply to
#'  columns not mentioned in the \code{`col(s)`} argument.
#' @param ... Named arguments for the \code{`mutate_fn`}.
#' @keywords internal
#' @return
#'  The mutated \code{data.frame} (\code{tibble}) / \code{vector}.
mutator <- function(data,
                    mutate_fn,
                    check_fn,
                    col = NULL,
                    new_name = NULL,
                    force_df = FALSE,
                    allowed_types = c("numeric", "factor"),
                    ...) {

  # Prepare 'data' and 'col'
  # Includes a set of checks
  prepped <- prepare_input_data(data = data, cols = col, new_name = new_name)
  data <- prepped[["data"]]
  col <- prepped[["cols"]]
  new_name = prepped[["new_name"]]
  was_vector <- prepped[["was_vector"]]

  if (isTRUE(prepped[["use_index"]])){
    stop("When 'data' is a data frame, 'col' must be specified.")
  }

  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_data_frame(data, min.rows = 1, add = assert_collection)
  checkmate::assert_string(col, min.chars = 1, null.ok = FALSE, add = assert_collection)
  checkmate::assert_string(new_name, min.chars = 1, null.ok = FALSE, add = assert_collection)
  checkmate::assert_function(mutate_fn, add = assert_collection)
  checkmate::assert_function(check_fn, null.ok = TRUE, add = assert_collection)
  checkmate::reportAssertions(assert_collection)
  checkmate::assert_data_frame(data[[col]], types = allowed_types,
                               .var.name = ifelse(isTRUE(was_vector), "'data' as vector", "'col' column"),
                               add = assert_collection)
  checkmate::reportAssertions(assert_collection)
  # Extra checks
  # This is for checks we want to perform after preparing 'data' and 'col'
  if (!is.null(check_fn))
    check_fn(data = data, col = col, ...)
  # End of argument checks ####

  # Apply mutator method
  data <-
    run_by_group(
      data = data,
      fn = mutate_fn,
      col = col,
      new_name = new_name,
      ...
    )

  # Clean up output
  data <-
    prepare_output_data(
      data = data,
      cols = col,
      use_index = FALSE,
      to_vector = was_vector && !force_df
    )

  data

}


##  .................. #< fe0bd7a2242b745b16ba66e9d280aab4 ># ..................
##  Multicolumn mutator                                                     ####


#' Wrapper for running multicolumn mutator methods
#'
#' @param cols Columns to mutate values of. Must be specified when \code{`data`} is a \code{data.frame}.
#' @param check_fn Function with checks post-preparation of \code{`data`} and \code{`cols`}.
#'  Should not return anything.
#' @param suffix Suffix to add to the names of the generated columns.
#'
#'  Use an empty string (i.e. \code{""}) to overwrite the original columns.
#' @param keep_original Whether to keep the original columns. (Logical)
#'
#'  Some columns may have been overwritten, in which case only the newest versions are returned.
#' @param min_dims Minimum number of dimensions (cols) after preparations. When \code{`data`} is a \code{vector}
#'  setting \code{`min_dims`} to \code{2} will use both the index and the values as columns.
#' @inheritParams mutator
#' @keywords internal
#' @return
#'  The mutated \code{data.frame} (\code{tibble}).
multi_mutator <- function(
  data,
  mutate_fn,
  check_fn,
  cols = NULL,
  suffix = "_mutated",
  force_df = TRUE,
  allowed_types = c("numeric", "factor"),
  min_dims = 1,
  keep_original = TRUE,
  ...) {


  # Prepare 'data' and 'col'
  # Includes a set of checks
  prepped <- prepare_input_data(data = data, cols = cols, min_dims = min_dims)
  data <- prepped[["data"]]
  cols <- prepped[["cols"]]
  was_vector <- prepped[["was_vector"]]

  if (isTRUE(prepped[["use_index"]])){
    stop("When 'data' is a data frame, 'cols' must be specified.")
  }

  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_data_frame(data, min.rows = 1, add = assert_collection)
  checkmate::assert_character(cols, any.missing = FALSE, min.len = 1, min.chars = 1,
                              null.ok = FALSE, add = assert_collection)
  checkmate::assert_function(mutate_fn, add = assert_collection)
  checkmate::assert_function(check_fn, null.ok = TRUE, add = assert_collection)
  checkmate::assert_string(suffix, add = assert_collection)
  checkmate::assert_flag(force_df, add = assert_collection)
  checkmate::assert_flag(keep_original, add = assert_collection)
  checkmate::reportAssertions(assert_collection)
  checkmate::assert_data_frame(data[,cols, drop=FALSE], types = allowed_types,
                               .var.name = ifelse(isTRUE(was_vector), "'data' as vector", "'cols' columns"),
                               add = assert_collection)
  checkmate::reportAssertions(assert_collection)
  # Extra checks
  # This is for checks we want to perform after preparing 'data' and 'col'
  if (!is.null(check_fn))
    check_fn(data = data, cols = cols, ...)
  # End of argument checks ####

  # Find columns to remove post-run if keep_original is FALSE
  exclude_cols <- NULL
  if (!isTRUE(keep_original)){
    group_columns <- colnames(dplyr::group_keys(data))
    non_group_columns <- setdiff(colnames(data), group_columns)
    exclude_cols <- non_group_columns
    if (suffix == ""){
      # The cols will be overwritten
      # so we shouldn't exclude them
      exclude_cols <- setdiff(exclude_cols, cols)
    }
  }

  # Apply mutator method
  data <-
    run_by_group(
      data = data,
      fn = mutate_fn,
      cols = cols,
      suffix = suffix,
      ...
    )

  # Clean up output
  data <-
    prepare_output_data(
      data = data,
      cols = cols,
      use_index = FALSE,
      to_vector = was_vector && !force_df,
      exclude_cols = exclude_cols
    )

  data

}



