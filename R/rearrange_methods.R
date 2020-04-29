

#   __________________ #< d453411db0c73678e24552a628a3b4ad ># __________________
#   Rearrange methods                                                       ####


##  .................. #< ea7382303d9437ddfea6104fe80a10a9 ># ..................
##  Center by                                                               ####


rearrange_center_by <- function(data, col,
                                shuffle_members,
                                what = "max") {

  size <- nrow(data)

  if (size < 2){
    return(data)
  }

  # NOTE: The extra comma is on purpose!
  # 'drop' is required for single-column data frames
  data <- data[order(data[[col]], decreasing = what == "min"),
               , drop = FALSE]

  # If uneven length
  # extract max and remove from vec
  middle_row <- NULL
  if (size %% 2 == 1){
    if (what == "max"){
      middle_val <- max(data[[col]])
    } else if (what == "min"){
      middle_val <- min(data[[col]])
    }
    # Extract and remove middle row
    middle_row <- data[min(which(data[[col]] == middle_val)), , drop = FALSE]
    data <- data[-min(which(data[[col]] == middle_val)), , drop = FALSE]
    size <- size - 1
  }

  # Create extreme pairs factor
  extreme_pairs <- create_rearrange_factor_pair_extremes_(
    size = size)

  if (isTRUE(shuffle_members)){
    # Add noise to the pair indices to randomize the order
    # of the members
    member_noise <- runif(length(extreme_pairs), -0.1, 0.1)
    extreme_pairs <- as.numeric(extreme_pairs) + member_noise
  }

  # Convert to ordering index
  new_order <- seq_len(size)[order(extreme_pairs)]
  # Add ordering index to data
  tmp_order_var <- create_tmp_var(data, tmp_var = ".tmp_ordering_factor")
  data[[tmp_order_var]] <- new_order
  # Order 'data'
  data <- data[order(data[[tmp_order_var]]), ]
  data[[tmp_order_var]] <- NULL

  # Insert middle if it was removed
  if (!is.null(middle_row)){
    data <- insert_row(
      data = data,
      new_row = middle_row,
      after = size %/% 2)
  }

  data
}


##  .................. #< 0261a5aba8208b7b279b03d55666b490 ># ..................
##  Order by group                                                          ####


order_by_group <- function(data, group_col, shuffle_members, shuffle_pairs){

  backup_group_col <- isTRUE(shuffle_pairs) || isTRUE(shuffle_members)
  if (isTRUE(backup_group_col)){
    tmp_backup_group_col <- create_tmp_var(data, tmp_var = "group_col_backup_")
    data[[tmp_backup_group_col]] <- data[[group_col]]
  }

  if (isTRUE(shuffle_pairs)){
    # Randomize levels
    data[[group_col]] <- as.numeric(
      factor(data[[group_col]], levels = sample(unique(data[[group_col]])))
    )
  }
  if (isTRUE(shuffle_members)){
    # Add random noise to the group indices
    indices_noise <- runif(nrow(data), -0.1, 0.1)
    data[[group_col]] <- as.numeric(data[[group_col]]) + indices_noise

  }

  # Order data frame by the groups
  data <- data[order(data[[group_col]]), , drop = FALSE]

  if (isTRUE(backup_group_col)){
    # Revert to original group col
    data[[group_col]] <- data[[tmp_backup_group_col]]
    data[[tmp_backup_group_col]] <- NULL
  }

  data
}


##  .................. #< 1b0eae3be399f927b4ee93f2f360f9e4 ># ..................
##  Reposition at                                                           ####


rearrange_position_at <- function(data,
                                  col,
                                  position,
                                  shuffle_members,
                                  what = "max") {

  size <- nrow(data)
  even_nrows <- size %% 2 == 0

  if (position > nrow(data)){
    stop("'position' was higher than the number of rows in 'data'.")
  }

  if (size < 2){
    return(data)
  }

  # Convert percentage to index position
  if (is_between_(position, 0, 1)){
    # TODO floor or round?
    position <- floor(unname(
      quantile(seq_len(size), probs = position)
    ))
  }

  # Find if the position is above the midline
  middle_position <- size / 2
  above_midline <- position > middle_position

  # Check if edge case
  # Order is determined by position
  if (position %in% c(size, 1)){
    data <- data[order(data[[col]],
                       decreasing = ifelse(
                         what == "max",
                         !isTRUE(above_midline),
                         isTRUE(above_midline))
    ),
    , drop = FALSE]
    return(data)
  }

  # Find extra positions
  if (isTRUE(above_midline)){
    n_extra_positions <- size - 2 * (size - position) - 1
    if (n_extra_positions == 0) {
      # position is center
      return(rearrange(
        data = data,
        col = col,
        method = paste0("center_", what),
        shuffle_members = shuffle_members
      ))
    }
    data <- data[order(data[[col]], decreasing = what != "max"), , drop = FALSE]
    extra_positions <- seq_len(n_extra_positions)
  } else {
    n_extra_positions <- size - 2 * position + 1
    data <- data[order(data[[col]], decreasing = what == "max"), , drop = FALSE]
    extra_positions <- rev((size + 1) - seq_len(n_extra_positions))
  }

  # Order the excess rows correctly from the beginning
  extra_rows <- data[extra_positions, , drop = FALSE]
  center_rows <- data[-extra_positions, , drop = FALSE]

  # Center max or min
  center_rows <- rearrange(
    data = center_rows,
    col = col,
    method = paste0("center_", what),
    shuffle_members = shuffle_members
  )

  # Combine subsets
  if (isTRUE(above_midline)){
    data <- dplyr::bind_rows(extra_rows, center_rows)
  } else {
    data <- dplyr::bind_rows(center_rows, extra_rows)
  }

  data

}


##  .................. #< 9b7a497230634fb37d1ebff7f7cbd104 ># ..................
##  Reverse windows                                                         ####


rearrange_rev_windows <- function(data, window_size, keep_windows, factor_name){

  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_data_frame(data, add = assert_collection)
  checkmate::assert_count(window_size, positive = TRUE, add = assert_collection)
  checkmate::assert_flag(keep_windows, add = assert_collection)
  checkmate::assert_string(factor_name, add = assert_collection)
  checkmate::reportAssertions(assert_collection)
  # End of argument checks ####

  size <- nrow(data)
  if (size < 2){
    return(data)
  }
  num_windows <- ceiling(size / window_size)
  windows <- rep(seq_len(num_windows), each = window_size)
  rev_indices <- rep(rev(seq_len(window_size)), times = num_windows)
  new_order <- windows + (rev_indices * (0.5 / window_size))
  new_order <- head(new_order, size)
  if (isTRUE(keep_windows)) {
    data[[factor_name]] <- head(windows, size)
  }

  data[order(new_order), , drop = FALSE]
}
