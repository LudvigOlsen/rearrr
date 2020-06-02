

#   __________________ #< 1da7e5c1e61869688d7c0889889264c0 ># __________________
#   Prepare 'data' and 'col'                                                ####


prepare_input_data <- function(data, col, new_name=NULL){

  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()

  # Initial check of 'data'
  checkmate::assert(
    checkmate::check_data_frame(data),
    checkmate::check_vector(
      data, strict = TRUE, any.missing = FALSE),
    checkmate::check_factor(
      data, any.missing = FALSE)
  )

  # Cannot be non-data-frame list
  if (!is.data.frame(data) && is.list(data)){
    assert_collection$push(
      "when 'data' is not a data frame, it cannot be a list."
    )
    checkmate::reportAssertions(assert_collection)
  }

  # Convert to data frame if vector
  if (!is.list(data)){
    if (!is.null(col)){
      assert_collection$push(
        "when 'data' is not a data frame, 'col' must be 'NULL'."
      )
      checkmate::reportAssertions(assert_collection)
    }
    data <- data.frame("Value" = data,
                       stringsAsFactors = FALSE)
    col = "Value"
    was_vector <- TRUE
  } else {
    was_vector <- FALSE
  }

  # If data frame and using row numbers as col
  use_index <- is.null(col)
  if (isTRUE(use_index)){
    col <- create_tmp_var(data, ".index_col_")
    data <- data %>%
      dplyr::mutate(!!col := dplyr::row_number())
  }

  # New Name can be string or NULL
  checkmate::assert_string(x = new_name, null.ok = TRUE)
  if (is.null(new_name)){
    new_name <- col
  }

  checkmate::reportAssertions(assert_collection)
  # End of argument checks ####

  list("data" = data,
       "col" = col,
       "new_name" = new_name,
       "use_index" = use_index,
       "was_vector" = was_vector)
}

prepare_output_data <- function(data, col, use_index, was_vector){

  # Remove tmp column if 'col' was 'NULL'
  if (isTRUE(use_index)){
    data[[col]] <- NULL
  }

  # Return as vector if that is what we were passed
  if (isTRUE(was_vector) &&
      ncol(data) == 1) {
    return(data[[1]])
  }

  # Reset index
  row.names(data) <- NULL

  data
}
