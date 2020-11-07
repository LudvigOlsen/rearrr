

#   __________________ #< d453411db0c73678e24552a628a3b4ad ># __________________
#   Rearrange methods                                                       ####


##  .................. #< ea7382303d9437ddfea6104fe80a10a9 ># ..................
##  Center by                                                               ####


rearrange_center_by <- function(data, cols,
                                shuffle_sides,
                                what = "max",
                                ...) {
  stopifnot(length(cols) == 1)
  col <- cols

  size <- nrow(data)

  if (size < 2) {
    return(data)
  }

  # NOTE: The extra comma is on purpose!
  # 'drop' is required for single-column data frames
  data <- data[order(data[[col]], decreasing = what == "min"), ,
    drop = FALSE
  ]

  # If uneven length
  # extract max and remove from vec
  middle_row <- NULL
  if (size %% 2 == 1) {
    if (what == "max") {
      middle_val <- max(data[[col]])
    } else if (what == "min") {
      middle_val <- min(data[[col]])
    }
    # Extract and remove middle row
    middle_row <- data[min(which(data[[col]] == middle_val)), , drop = FALSE]
    data <- data[-min(which(data[[col]] == middle_val)), , drop = FALSE]
    size <- size - 1
  }

  # Create extreme pairs factor
  extreme_pairs <- create_rearrange_factor_pair_extremes_(
    size = size
  )

  if (isTRUE(shuffle_sides)) {
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
  if (!is.null(middle_row)) {
    data <- insert_row_(
      data = data,
      new_row = middle_row,
      after = size %/% 2
    )
  }

  data
}


##  .................. #< 0261a5aba8208b7b279b03d55666b490 ># ..................
##  Order by group                                                          ####


order_by_group <- function(data, group_col, shuffle_members, shuffle_pairs, ...) {

  backup_group_col <- isTRUE(shuffle_pairs) || isTRUE(shuffle_members)
  if (isTRUE(backup_group_col)) {
    tmp_backup_group_col <- create_tmp_var(data, tmp_var = "group_col_backup_")
    data[[tmp_backup_group_col]] <- data[[group_col]]
  }

  if (isTRUE(shuffle_pairs)) {
    # Randomize levels
    data[[group_col]] <- as.numeric(
      factor(data[[group_col]], levels = sample(unique(data[[group_col]])))
    )
  }
  if (isTRUE(shuffle_members)) {
    # Add random noise to the group indices
    indices_noise <- runif(nrow(data), -0.1, 0.1)
    data[[group_col]] <- as.numeric(data[[group_col]]) + indices_noise
  }

  # Order data frame by the groups
  data <- data[order(data[[group_col]]), , drop = FALSE]

  if (isTRUE(backup_group_col)) {
    # Revert to original group col
    data[[group_col]] <- data[[tmp_backup_group_col]]
    data[[tmp_backup_group_col]] <- NULL
  }

  data
}


##  .................. #< 1b0eae3be399f927b4ee93f2f360f9e4 ># ..................
##  Reposition at                                                           ####


rearrange_position_at <- function(data,
                                  cols,
                                  position,
                                  shuffle_sides,
                                  what = "max",
                                  ...) {
  stopifnot(length(cols) == 1)
  col <- cols

  size <- nrow(data)
  even_nrows <- size %% 2 == 0

  if (position > nrow(data)) {
    stop("'position' was higher than the number of rows in 'data'.")
  }

  if (size < 2) {
    return(data)
  }

  # Convert percentage to index position
  if (is_between_(position, 0, 1)) {
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
  if (position %in% c(size, 1)) {
    data <- data[order(data[[col]],
      decreasing = ifelse(
        what == "max",
        !isTRUE(above_midline),
        isTRUE(above_midline)
      )
    ), ,
    drop = FALSE
    ]
    return(data)
  }

  # Find extra positions
  if (isTRUE(above_midline)) {
    n_extra_positions <- size - 2 * (size - position) - 1
    if (n_extra_positions == 0) {
      # position is center
      return(
        centering_rearranger_(
          data = data,
          col = col,
          shuffle_sides = shuffle_sides,
          what = what
        )
      )
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
  center_rows <- centering_rearranger_(
    data = center_rows,
    col = col,
    shuffle_sides = shuffle_sides,
    what = what
  )

  # Combine subsets
  if (isTRUE(above_midline)) {
    data <- dplyr::bind_rows(extra_rows, center_rows)
  } else {
    data <- dplyr::bind_rows(center_rows, extra_rows)
  }

  data
}


##  .................. #< 9b7a497230634fb37d1ebff7f7cbd104 ># ..................
##  Reverse windows                                                         ####

# 'col' is a required arg in the function but is ignored
rearrange_rev_windows <- function(data, window_size, factor_name, cols = NULL, overwrite, ...) {
  size <- nrow(data)
  if (size < 2) {
    return(data)
  }
  num_windows <- ceiling(size / window_size)
  windows <- rep(seq_len(num_windows), each = window_size)
  rev_indices <- rep(rev(seq_len(window_size)), times = num_windows)
  new_order <- windows + (rev_indices * (0.5 / window_size))
  new_order <- head(new_order, size)

  # Add windows factor
  data <- add_info_col_(
    data = data,
    nm = factor_name,
    content = head(windows, size),
    overwrite = overwrite
  )

  data[order(new_order), , drop = FALSE]
}


##  .................. #< 440b147b963f8a7fd202661bfc3b068e ># ..................
##  By Distance - closest to / furthest from                                ####


rearrange_by_distance <- function(data,
                                  grp_id,
                                  cols,
                                  overwrite,
                                  origin,
                                  origin_fn,
                                  shuffle_ties,
                                  decreasing,
                                  origin_col_name,
                                  distance_col_name,
                                  ...) {

  if (nrow(data) < 2) {
    return(data)
  }

  # Number of dimensions
  # Each column is a dimension
  num_dims <- length(cols)

  # Convert columns to list of vectors
  dim_vectors <- as.list(data[, cols, drop = FALSE])

  # Find origin if specified
  origin <- apply_coordinate_fn_(
    dim_vectors = dim_vectors,
    coordinates = origin,
    fn = origin_fn,
    num_dims = length(cols),
    coordinate_name = "origin",
    fn_name = "origin_fn",
    dim_var_name = "cols",
    grp_id = grp_id,
    allow_len_one = TRUE
  )

  tmp_distances_col <- create_tmp_var(data, ".tmp_distances")
  data[[tmp_distances_col]] <- calculate_distances_(dim_vectors = dim_vectors, to = origin)

  if (isTRUE(shuffle_ties)) {
    # Shuffle the data frame
    data <- data[sample(seq_len(nrow(data))), , drop = FALSE]
  } else {
    # pre-order
    data <- data %>%
      dplyr::arrange(!!!rlang::syms(cols))
  }

  # Order by distances
  data <- data[order(data[[tmp_distances_col]], decreasing = decreasing), , drop = FALSE]

  # Extract distances
  distances <- data[[tmp_distances_col]]
  data[[tmp_distances_col]] <- NULL

  # Add origin coordinates column
  data <- add_info_col_(
    data = data,
    nm = origin_col_name,
    content = list_coordinates_(origin, cols),
    overwrite = overwrite
  )

  # Add distances column
  data <- add_info_col_(
    data = data,
    nm = distance_col_name,
    content = distances,
    overwrite = overwrite
  )

  data
}


##  .................. #< da4fe0fffb24210b784112591123dcc6 ># ..................
##  Pair extremes                                                           ####


# TODO Add aggregate_fn for recursive pairings
rearrange_pair_extremes <- function(data,
                                    cols,
                                    overwrite,
                                    unequal_method,
                                    num_pairings,
                                    balance,
                                    shuffle_members,
                                    shuffle_pairs,
                                    factor_name,
                                    ...) {
  stopifnot(length(cols) == 1)
  col <- cols

  if (nrow(data) < 2) {
    return(data)
  }

  equal_nrows <- nrow(data) %% 2 == 0

  # Prepare factor names for output
  factor_names <- factor_name
  if (!is.null(factor_names) && num_pairings > 1){
    factor_names <- paste0(factor_name, "_", seq_len(num_pairings))
  }

  ## First extreme pairing

  # Create num_pairings tmp column names
  tmp_rearrange_vars <- purrr::map(.x = seq_len(num_pairings), .f = ~ {
    create_tmp_var(data, paste0(".tmp_rearrange_col_", .x))
  }) %>% unlist(recursive = TRUE)

  # Pair the extreme values and order by the pairs
  data <- order_and_pair_extremes_(
    data = data,
    col = col,
    new_col = tmp_rearrange_vars[[1]],
    unequal_method = unequal_method
  )

  # Order data by the pairs
  data <- order_by_group(
    data = data,
    group_col = tmp_rearrange_vars[[1]],
    shuffle_members = FALSE, # Done at the end
    shuffle_pairs = FALSE # Done at the end
  )

  # TODO Perform recursive pairing

  if (num_pairings > 1){

    # Get environment so we can update `data`
    pairing_env <- environment()

    # Perform num_pairings-1 pairings
    plyr::l_ply(seq_len(num_pairings - 1), function(i) {

      # Note that `tmp_rearrange_vars[[i]]` is for the previous level
      # and `tmp_rearrange_vars[[i + 1]]` is for the current level

      # What to balance ("mean", "spread", "min", or "max")
      if (length(balance) > 1) {
        current_balance_target <- balance[[i]]
      } else {
        current_balance_target <- balance
      }

      # Define function to summarize with
      if (current_balance_target == "mean") {
        summ_fn <- sum
      } else if (current_balance_target == "spread") {
        summ_fn <- function(v) {
          sum(abs(diff(v)))
        }
      } else if (current_balance_target == "min") {
        summ_fn <- min
      } else if (current_balance_target == "max") {
        summ_fn <- max
      }

      # Aggregate values for pairs from previous pairing
      tmp_group_scores <- data %>%
        dplyr::group_by(!!as.name(tmp_rearrange_vars[[i]])) %>%
        dplyr::summarize(group_aggr = summ_fn(!!as.name(col)))

      if (!equal_nrows & unequal_method == "first") {

        # Reorder with first group always first
        # (otherwise doesn't work with negative numbers)
        tmp_group_scores_sorted <- tmp_group_scores %>%
          dplyr::filter(dplyr::row_number() == 1) %>%
          dplyr::bind_rows(
            tmp_group_scores %>%
              dplyr::filter(dplyr::row_number() != 1) %>%
              dplyr::arrange(.data$group_aggr))
      } else {
        tmp_group_scores_sorted <- tmp_group_scores %>%
          dplyr::arrange(.data$group_aggr)
      }

      # Pair the extreme pairs and order by the new pairs
      tmp_rearrange <- order_and_pair_extremes_(
        data = tmp_group_scores_sorted,
        col = "group_aggr",
        new_col = tmp_rearrange_vars[[i + 1]],
        unequal_method = unequal_method
      ) %>%
        base_select_(cols = c(tmp_rearrange_vars[[i]], tmp_rearrange_vars[[i + 1]]))

      # Add the new pair identifiers to `data`
      data <- data %>%
        dplyr::left_join(tmp_rearrange, by = tmp_rearrange_vars[[i]])

      # Order data by the pairs
      data <- order_by_group(
        data = data,
        group_col = tmp_rearrange_vars[[i + 1]],
        shuffle_members = FALSE,
        shuffle_pairs = FALSE
      )

      # Update `data` in parent environment
      pairing_env[["data"]] <- data

    })
  }

  # Find columns to shuffle
  shuffling_group_cols <- rev(tmp_rearrange_vars)
  cols_to_shuffle <- c()
  if (isTRUE(shuffle_members))
    cols_to_shuffle <- shuffling_group_cols
  if (isTRUE(shuffle_pairs)) {
    shuffling_group_cols <- c(shuffling_group_cols, col)
    cols_to_shuffle <- c(cols_to_shuffle, col)
  }

  # Shuffle members and/or pairs if specified
  if (length(cols_to_shuffle) > 0) {
    data <- dplyr::ungroup(data) %>%
      shuffle_hierarchy(group_cols = shuffling_group_cols,
                        cols_to_shuffle = cols_to_shuffle,
                        leaf_has_groups = !shuffle_members)
  }

  if (!is.null(factor_names)){
    # Convert to factors and give correct names
    rearrange_factors <- as.list(data[, tmp_rearrange_vars, drop=FALSE])
    rearrange_factors <- purrr::map(rearrange_factors, .f = ~{as.factor(.x)})
    names(rearrange_factors) <- factor_names

    # Remove tmp vars and add factors
    data <- base_deselect_(data, cols = tmp_rearrange_vars) %>%
      add_dimensions_(new_vectors = rearrange_factors,
                      overwrite = overwrite)
  } else {
    # If names are NULL, we just remove the tmp columns
    data <- base_deselect_(data, cols = tmp_rearrange_vars)
  }

  data
}

# Wrapper for running extreme pairing and ordering data by it
order_and_pair_extremes_ <- function(data, col, new_col, unequal_method){

  # Order data frame
  data <- data[order(data[[col]]), , drop = FALSE]

  # Add rearrange factor (of type integer)
  data[[new_col]] <-
    create_rearrange_factor_pair_extremes_(
      size = nrow(data),
      unequal_method = unequal_method
    )

  data
}
