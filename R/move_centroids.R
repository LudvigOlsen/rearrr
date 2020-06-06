

#   __________________ #< be61338bfda81f321c3aeddf7eb9841c ># __________________
#   Move centroids                                                          ####


# @family clustering functions
# TODO Consider its purpose (does it fulfill that currently? Or should we allow e.g. one 'to' per group?)
# TODO Document and export
move_centroid <- function(data,
                          cols = NULL,
                          to = 0,
                          to_fn = NULL,
                          suffix = "_moved",
                          keep_original = TRUE,
                          change_col_name = ".moved") {

  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_string(change_col_name, null.ok = TRUE, add = assert_collection)
  checkmate::assert_numeric(to,
                            min.len = 1,
                            any.missing = FALSE,
                            add = assert_collection)
  checkmate::assert_function(to_fn, null.ok = TRUE, add = assert_collection)
  checkmate::reportAssertions(assert_collection)
  # End of argument checks ####

  # Mutate with each multiplier
  multi_mutator(
    data = data,
    mutate_fn = move_centroid_mutator_method,
    check_fn = NULL,
    cols = cols,
    suffix = suffix,
    force_df = TRUE,
    keep_original = keep_original,
    to= to,
    to_fn = to_fn,
    change_col_name = change_col_name
  )

}


move_centroid_mutator_method <- function(data,
                                         cols,
                                         to = 0,
                                         to_fn = NULL,
                                         suffix = "",
                                         change_col_name = ".moved") {

  if (length(to) %ni% c(1, length(cols))) {
    stop("the 'to' must have length 1 or same length as 'cols'.")
  }

  # Convert columns to list of vectors
  dim_vectors <- as.list(data[, cols, drop = FALSE])

  # Find current centroid if specified
  old_centroid <- tryCatch(
    do.call(centroid, dim_vectors),
    error = function(e) {
      stop(paste0("failed to apply 'centroid': ", e))
    }
  )

  # Find to if specified
  if (!is.null(to_fn)) {
    to <- tryCatch(
      do.call(to_fn, dim_vectors),
      error = function(e) {
        stop(paste0("failed to apply 'to_fn': ", e))
      }
    )
    to_check_msg <- "output of 'to_fn'"
  } else {
    to_check_msg <- "'to'"
  }

  if (length(to) %ni% c(1, num_dims)) {
    stop(
      paste0(
        to_check_msg,
        " must have either length 1 or same",
        " length as 'cols' (",
        num_dims,
        ") but had length ",
        length(to),
        "."
      )
    )
  }
  if (!is.numeric(to)) {
    stop(paste0(to_check_msg, " was not numeric."))
  }

  # Move centroid
  to_move <- to - old_centroid
  dim_vectors <-
    purrr::map2(.x = dim_vectors, .y = to_move, .f = ~ {
      .x + .y
    })

  # Convert to data frame
  names(dim_vectors) <- paste0(names(dim_vectors), suffix)
  expanded_data <-
    data.frame(dim_vectors, stringsAsFactors = FALSE)

  # If overwriting columns, delete in 'data' first
  col_intersection <-
    intersect(colnames(expanded_data), colnames(data))
  if (length(col_intersection) > 0) {
    data <- data[, colnames(data) %ni% col_intersection, drop = FALSE]
  }

  # Add to original dataframe
  data <- dplyr::bind_cols(data, expanded_data)

  # Add info columns
  if (!is.null(change_col_name)){
    data[[change_col_name]] <- list(to_move)
  }


  data

}
