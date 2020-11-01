# TODO Convert to exported function
# Shuffle hierarchical data frame grouping.
# Note: Not exactly a quick operation :-)
shuffle_hierarchy_ <- function(data, group_cols, cols_to_shuffle = group_cols) {

  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_data_frame(data, add = assert_collection)
  checkmate::assert_character(group_cols, min.chars = 1, any.missing = FALSE,
                              unique = TRUE, add = assert_collection)
  checkmate::assert_character(cols_to_shuffle, min.chars = 1, any.missing = FALSE,
                              unique = TRUE, add = assert_collection)
  checkmate::reportAssertions(assert_collection)
  if (length(setdiff(cols_to_shuffle, group_cols))){
    assert_collection$push("'cols_to_shuffle' can only contain names that are also in 'group_cols'.")
  }
  if (dplyr::is_grouped_df(data)){
    warning("'data' is already grouped. Those groups will be ignored.")
  }
  checkmate::reportAssertions(assert_collection)
  # End of argument checks ####

  data_env <- environment()
  plyr::l_ply(rev(seq_along(group_cols)), function(gc_idx){
    if (group_cols[[gc_idx]] %in% cols_to_shuffle){
      to_group_by <- head(group_cols, n = gc_idx - 1)
      data <- dplyr::group_by(data, !!!rlang::syms(to_group_by))
      data <- run_by_group(data, fn = shuffle_uniques_, col = group_cols[[gc_idx]])
      data_env[["data"]] <- data
    }
  })

  # Ungroup and return
  data %>%
    dplyr::ungroup()

}

# Extract unique values in the column
# and sort 'data' by their shuffled index
shuffle_uniques_ <- function(data, grp_id, col){
  tmp_index <- create_tmp_var(data)

  uniques <- unique(data[[col]]) %>%
    tibble::enframe(name = NULL, value = col) %>%
    dplyr::sample_frac() %>%
    dplyr::mutate(!!tmp_index := dplyr::row_number())

  data %>%
    dplyr::left_join(uniques, by = col) %>%
    dplyr::arrange(!!as.name(tmp_index)) %>%
    base_deselect_(cols = tmp_index)
}
