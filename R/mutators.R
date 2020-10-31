

#   __________________ #< beae0116fffd26a3055dd7425c5928ad ># __________________
#   Mutators                                                                ####


##  .................. #< fe0bd7a2242b745b16ba66e9d280aab4 ># ..................
##  Multicolumn mutator                                                     ####


#' Wrapper for running multi-column mutator methods
#'
#' @param cols Columns to mutate values of. Must be specified when \code{`data`} is a \code{data.frame}.
#' @param suffix Suffix to add to the names of the generated columns.
#'
#'  Use an empty string (i.e. \code{""}) to overwrite the original columns.
#' @param keep_original Whether to keep the original columns. (Logical)
#'
#'  Some columns may have been overwritten, in which case only the newest versions are returned.
#' @param allow_missing Whether to allow missing values (\code{NA}s). (Logical)
#' @param altered_col Additional column that is mutated but is not
#'  mentioned in \code{`cols`}.
#' @param mutate_fn Mutator to apply.
#' @param ... Named arguments for the \code{`mutate_fn`}.
#' @inheritParams rearrr_fn_
#' @keywords internal
#' @return The mutated \code{data.frame} (\code{tibble}).
multi_mutator_ <- function(data,
                           mutate_fn,
                           check_fn,
                           cols = NULL,
                           suffix = "_mutated",
                           overwrite = FALSE,
                           force_df = TRUE,
                           allowed_types = c("numeric", "factor"),
                           allow_missing = FALSE,
                           min_dims = 1,
                           altered_col = NULL,
                           keep_original = TRUE,
                           origin_fn = NULL, # For docs inheritance
                           ...) {
  # Prepare 'data' and 'col'
  # Includes a set of checks
  prepped <- prepare_input_data_(
    data = data,
    cols = cols,
    min_dims = min_dims,
    allow_missing = allow_missing
  )
  data <- prepped[["data"]]
  cols <- prepped[["cols"]]
  was_vector <- prepped[["was_vector"]]

  if (isTRUE(prepped[["use_index"]])) {
    stop("When 'data' is a data.frame, 'cols' must be specified.")
  }

  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_data_frame(data, min.rows = 1, add = assert_collection)
  checkmate::assert_character(
    cols,
    any.missing = FALSE,
    min.len = 1,
    min.chars = 1,
    unique = TRUE,
    null.ok = FALSE,
    add = assert_collection
  )
  checkmate::assert_function(mutate_fn, add = assert_collection)
  checkmate::assert_function(check_fn, null.ok = TRUE, add = assert_collection)
  checkmate::assert_function(origin_fn, null.ok = TRUE, add = assert_collection)
  checkmate::assert_string(suffix, add = assert_collection)
  checkmate::assert_flag(overwrite, add = assert_collection)
  checkmate::assert_flag(force_df, add = assert_collection)
  checkmate::assert_flag(keep_original, add = assert_collection)
  checkmate::reportAssertions(assert_collection)
  checkmate::assert_data_frame(
    data[, cols, drop = FALSE],
    types = allowed_types,
    .var.name = ifelse(isTRUE(was_vector), "'data' as vector", "'cols' columns"),
    add = assert_collection
  )
  checkmate::reportAssertions(assert_collection)

  if (isTRUE(was_vector)){
    overwrite <- TRUE
  }

  # Extra checks
  # This is for checks we want to perform after preparing 'data' and 'col'
  if (!is.null(check_fn)) {
    check_fn(data = data, cols = cols, ...)
  }
  # End of argument checks ####

  # Store grp_keys
  grp_keys <- dplyr::group_keys(data)

  # Find columns to remove post-run if keep_original is FALSE
  exclude_cols <- NULL
  if (!isTRUE(keep_original)) {
    group_columns <- colnames(grp_keys)
    non_group_columns <- setdiff(colnames(data), group_columns)
    exclude_cols <- non_group_columns
    if (suffix == "" || isTRUE(was_vector)) {
      # The cols will be overwritten
      # so we shouldn't exclude them
      # If we changed a column not in 'cols' (e.g. 'dim_col' in 'dim_values()')
      # we make sure it is kept as well
      exclude_cols <- setdiff(exclude_cols, c(cols, altered_col))
    }
  }

  # Apply mutator method
  data <-
    run_by_group(
      data = data,
      fn = mutate_fn,
      cols = cols,
      suffix = suffix,
      overwrite = overwrite,
      origin_fn = origin_fn,
      ...
    )

  # Clean up output
  data <-
    prepare_output_data_(
      data = data,
      cols = cols,
      use_index = FALSE,
      to_vector = was_vector && !force_df,
      exclude_cols = exclude_cols,
      group_keys = grp_keys
    )

  data
}
