

#   __________________ #< be61338bfda81f321c3aeddf7eb9841c ># __________________
#   Move centroids                                                          ####


# @family clustering functions
# TODO Consider its purpose (does it fulfill that currently? Or should we allow e.g. one 'to' per group?)
# TODO Document and export
move_centroid <- function(data,
                          cols = NULL,
                          to = NULL,
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
    null.ok = TRUE,
    add = assert_collection
  )
  checkmate::assert_function(to_fn, null.ok = TRUE, add = assert_collection)
  checkmate::reportAssertions(assert_collection)
  # End of argument checks ####

  # Mutate with each multiplier
  multi_mutator_(
    data = data,
    mutate_fn = move_centroid_mutator_method_,
    check_fn = NULL,
    cols = cols,
    suffix = suffix,
    force_df = TRUE,
    keep_original = keep_original,
    to = to,
    to_fn = to_fn,
    change_col_name = change_col_name
  )
}


move_centroid_mutator_method_ <- function(data,
                                          grp_id,
                                          cols,
                                          to,
                                          to_fn,
                                          suffix,
                                          change_col_name,
                                          ...) {
  if (length(to) %ni% c(1, length(cols))) {
    stop("the 'to' must have length 1 or same length as 'cols'.")
  }

  # Convert columns to list of vectors
  dim_vectors <- as.list(data[, cols, drop = FALSE])

  # Find current centroid if specified
  old_centroid <- apply_coordinate_fn_(
    dim_vectors = dim_vectors,
    coordinates = NULL,
    fn = centroid,
    num_dims = length(cols),
    coordinate_name = "old centroids",
    fn_name = "centroid",
    dim_var_name = "cols",
    grp_id = grp_id,
    allow_len_one = FALSE
  )

  # Find to if specified
  to <- apply_coordinate_fn_(
    dim_vectors = dim_vectors,
    coordinates = to,
    fn = to_fn,
    num_dims = length(cols),
    coordinate_name = "to",
    fn_name = "to_fn",
    dim_var_name = "cols",
    grp_id = grp_id,
    allow_len_one = TRUE
  )

  # Move centroid
  to_move <- to - old_centroid
  dim_vectors <-
    purrr::map2(.x = dim_vectors, .y = to_move, .f = ~ {
      .x + .y
    })

  # Add dim_vectors as columns with the suffix
  data <-
    add_dimensions_(
      data = data,
      new_vectors = dim_vectors,
      suffix = suffix
    )

  # Add info columns
  if (!is.null(change_col_name)) {
    data[[change_col_name]] <- list_coordinates_(to_move, cols)
  }


  data
}
