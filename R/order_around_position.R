

# TODO Could be an exported function
order_around_position <- function(data, col, position, decreasing_before, decreasing_after) {

  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_data_frame(data, min.rows = 2, add = assert_collection)
  checkmate::assert_string(col, add = assert_collection)
  checkmate::assert_count(position, positive = TRUE, add = assert_collection)
  checkmate::assert_flag(decreasing_before, add = assert_collection)
  checkmate::assert_flag(decreasing_after, add = assert_collection)
  checkmate::reportAssertions(assert_collection)
  # End of argument checks ####

  size <- nrow(data)

  # Get indices before and after position
  pre_position_indices <- seq_len(position - 1)
  post_position_indices <- seq_len(size - position) + position

  # Reorder pre-position rows
  if (length(pre_position_indices) > 0) {
    # Extract rows pre position
    pre_data <- data[pre_position_indices, , drop = FALSE]
    # Order the rows
    pre_data <- pre_data[order(pre_data[[col]], decreasing = decreasing_before), ,
      drop = FALSE
    ]
    # Transfer row order to data
    data[pre_position_indices, ] <- pre_data
  }

  # Reorder post-position rows
  if (length(post_position_indices) > 0) {
    # Extract rows post position
    post_data <- data[post_position_indices, , drop = FALSE]
    # Order the rows
    post_data <- post_data[order(post_data[[col]], decreasing = decreasing_after), ,
      drop = FALSE
    ]
    # Transfer row order to data
    data[post_position_indices, ] <- post_data
  }

  data
}
