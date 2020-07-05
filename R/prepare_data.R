

#   __________________ #< 1da7e5c1e61869688d7c0889889264c0 ># __________________
#   Prepare 'data' and 'cols'                                               ####


prepare_input_data_ <- function(data,
                                cols,
                                min_dims = 1,
                                new_name = NULL,
                                allow_missing = FALSE) {
  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()

  checkmate::assert_number(min_dims, lower = 1, add = assert_collection)

  # Initial check of 'data'
  checkmate::assert(
    checkmate::check_data_frame(data),
    checkmate::check_vector(data, strict = TRUE, any.missing = allow_missing),
    checkmate::check_factor(data, any.missing = allow_missing)
  )

  # Cannot be non-data-frame list
  if (!is.data.frame(data) && is.list(data)) {
    assert_collection$push("when 'data' is not a data.frame, it cannot be a list.")
    checkmate::reportAssertions(assert_collection)
  }

  # Convert to data frame if vector
  if (!is.list(data)) {
    if (!is.null(cols)) {
      assert_collection$push("when 'data' is not a data.frame, 'col(s)' must be 'NULL'.")
      checkmate::reportAssertions(assert_collection)
    }
    data <- data.frame(
      "Value" = data,
      stringsAsFactors = FALSE
    )
    cols <- "Value"
    was_vector <- TRUE
    if (min_dims == 2) {
      data[["Index"]] <- seq_len(nrow(data))
      cols <- c("Index", "Value")
      data <- data[, cols, drop = FALSE]
    }
  } else {
    was_vector <- FALSE
  }

  # If data frame and using row numbers as cols
  use_index <- is.null(cols)
  if (isTRUE(use_index)) {
    cols <- create_tmp_var(data, ".index_col_")
    data <- data %>%
      dplyr::mutate(!!cols := dplyr::row_number())
  }

  # new_name can be string or NULL
  checkmate::assert_string(x = new_name, null.ok = TRUE)
  if (is.null(new_name)) {
    if (isTRUE(was_vector) && min_dims == 2) {
      new_name <- "Value"
    } else {
      new_name <- cols
    }
  }

  # Check min dimensions
  if (length(cols) < min_dims) {
    assert_collection$push(paste0(
      "This mutator method requires at least ",
      min_dims,
      " dimensions / columns."
    ))
  }

  checkmate::assert_character(
    cols,
    min.chars = 1,
    min.len = 1,
    any.missing = FALSE,
    add = assert_collection
  )

  checkmate::reportAssertions(assert_collection)

  if (length(setdiff(cols, colnames(data))) > 0) {
    assert_collection$push(paste0(
      "These names in the 'col(s)' argument were not found in 'data': ",
      paste0(setdiff(cols, colnames(data)), collapse = ", "),
      "."
    ))
  }

  checkmate::reportAssertions(assert_collection)
  # End of argument checks ####

  list(
    "data" = data,
    "cols" = cols,
    "new_name" = new_name,
    "use_index" = use_index,
    "was_vector" = was_vector
  )
}

prepare_output_data_ <- function(data,
                                 cols,
                                 use_index,
                                 to_vector,
                                 exclude_cols = NULL,
                                 group_keys = NULL) {
  # Remove tmp column if 'cols' was 'NULL'
  if (isTRUE(use_index)) {
    data[[cols]] <- NULL
  }

  # Return as vector if that is what we were passed
  if (isTRUE(to_vector) &&
    ncol(data) == 1) {
    return(data[[1]])
  }

  if (!is.null(exclude_cols) && length(exclude_cols) > 0) {
    data <- data[, names(data) %ni% exclude_cols, drop = FALSE]
  }

  # When 'data' contains group summaries, we add the group keys
  if (!is.null(group_keys) &&
    length(intersect(colnames(group_keys), colnames(data))) == 0 &&
    nrow(data) == nrow(group_keys)) {
    data <- dplyr::bind_cols(group_keys, data)
  }

  # Reset index
  row.names(data) <- NULL

  # Convert to tibble
  data <- dplyr::as_tibble(data)

  data
}
