

#   __________________ #< beae0116fffd26a3055dd7425c5928ad ># __________________
#   Mutators                                                                ####


#' Wrapper for running mutator methods
#'
#' @param data \code{data.frame} or \code{vector}.
#' @param col Column to mutate values of. Must be specified when \code{data} is a \code{data frame}.
#' @param new_name Name of the new column with the mutated values. If \code{NULL}, the original \code{col} column is mutated directly.
#' @param mutate_fn Mutator to apply.
#' @param check_fn Function with checks post-preparation of \code{data} and \code{col}.
#'  Should not return anything.
#' @param force_df Whether to return a \code{data.frame} when \code{data} is a \code{vector}.
#' @param ... Named arguments for the \code{mutate_fn}.
#' @keywords internal
#' @return
#'  The mutated \code{data frame} / \code{vector}.
mutator <- function(data,
                    mutate_fn,
                    check_fn,
                    col = NULL,
                    new_name = NULL,
                    force_df = FALSE,
                    ...) {

  # Prepare 'data' and 'col'
  # Includes a set of checks
  prepped <- prepare_input_data(data = data, col = col, new_name = new_name)
  data <- prepped[["data"]]
  col <- prepped[["col"]]
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
      col = col,
      use_index = FALSE,
      to_vector = was_vector && !force_df
    )

  data

}


##  .................. #< fe0bd7a2242b745b16ba66e9d280aab4 ># ..................
##  Multicolumn mutator                                                     ####


#' Wrapper for running multicolumn mutator methods
#'
#' @param cols Columns to mutate values of. Must be specified when \code{data} is a \code{data frame}.
#' @param check_fn Function with checks post-preparation of \code{data} and \code{cols}.
#'  Should not return anything.
#' @param suffix Suffix to add to the names of the generated columns.
#' @inheritParams mutator
#' @keywords internal
#' @return
#'  The mutated \code{data.frame}.
multi_mutator <- function(data,
                    mutate_fn,
                    check_fn,
                    cols = NULL,
                    suffix = "_mutated",
                    force_df = TRUE,
                    keep_original = TRUE,
                    ...) {

  # Prepare 'data' and 'col'
  # Includes a set of checks
  prepped <- prepare_input_data(data = data, col = cols)
  data <- prepped[["data"]]
  cols <- prepped[["col"]]
  was_vector <- prepped[["was_vector"]]

  if (isTRUE(prepped[["use_index"]])){
    stop("When 'data' is a data frame, 'cols' must be specified.")
  }

  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_data_frame(data, min.rows = 1, add = assert_collection)
  checkmate::assert_character(cols, any.missing = FALSE, min.len = 1,
                              null.ok = FALSE, add = assert_collection)
  checkmate::assert_function(mutate_fn, add = assert_collection)
  checkmate::assert_function(check_fn, null.ok = TRUE, add = assert_collection)
  checkmate::assert_string(suffix, min.chars = 1, add = assert_collection)
  checkmate::assert_flag(force_df, add = assert_collection)
  checkmate::assert_flag(keep_original, add = assert_collection)
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
      col = cols,
      use_index = FALSE,
      to_vector = was_vector && !force_df,
      exclude_cols = exclude_cols
    )

  data

}



