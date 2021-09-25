

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


order_by_group <- function(data, group_col, shuffle_members, shuffle_groups, ...) {

  backup_group_col <- isTRUE(shuffle_groups) || isTRUE(shuffle_members)
  if (isTRUE(backup_group_col)) {
    tmp_backup_group_col <- create_tmp_var(data, tmp_var = "group_col_backup_")
    data[[tmp_backup_group_col]] <- data[[group_col]]
  }

  if (isTRUE(shuffle_groups)) {
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
                                    order_by_aggregates,
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

  # Create tmp group aggregate column names
  tmp_aggregate_vars <- character(0)
  if (num_pairings > 1) {
    tmp_aggregate_vars <-
      purrr::map(.x = seq_len(num_pairings - 1), .f = ~ {
        create_tmp_var(data, paste0(".tmp_aggregate_col_", .x))
      }) %>% unlist(recursive = TRUE)
  }

  # Pair the extreme values and order by the pairs
  data <- order_and_group_extremes_(
    data = data,
    col = col,
    new_col = tmp_rearrange_vars[[1]],
    group_fn = create_rearrange_factor_pair_extremes_,
    unequal_method = unequal_method
  )

  # Order data by the pairs
  data <- order_by_group(
    data = data,
    group_col = tmp_rearrange_vars[[1]],
    shuffle_members = FALSE, # Done at the end
    shuffle_groups = FALSE # Done at the end
  )

  if (num_pairings > 1){

    # Get environment so we can update `data`
    pairing_env <- environment()

    # Perform num_pairings-1 pairings
    plyr::l_ply(seq_len(num_pairings - 1), function(i) {

      # Note that `tmp_rearrange_vars[[i]]` is for the previous level
      # and `tmp_rearrange_vars[[i + 1]]` is for the current level

      # What to balance ("mean", "spread", "min", or "max")
      summ_fn <- get_summ_fn_(balance = balance, i = i)

      # Get name of new group aggregate column
      tmp_group_aggr_col <- tmp_aggregate_vars[[i]]

      # Aggregate values for pairs from previous pairing
      tmp_group_scores <- data %>%
        dplyr::group_by(!!as.name(tmp_rearrange_vars[[i]])) %>%
        dplyr::summarize(!!tmp_group_aggr_col := summ_fn(!!as.name(col))) %>%
        dplyr::arrange(!!as.name(tmp_group_aggr_col))

      if (num_pairings > 1 && nrow(tmp_group_scores) < 2){
        warning(
          simpleWarning(
            "Fewer than 2 aggregated scores. Consider reducing `num_pairings`.",
            call = if (p <- sys.parent(9))
              sys.call(p)
          )
        )
      }

      if (!equal_nrows & unequal_method == "first") {

        # Reorder with first group always first
        # (otherwise doesn't work with negative numbers)
        tmp_group_scores_sorted <- tmp_group_scores %>%
          dplyr::filter(dplyr::row_number() == 1) %>%
          dplyr::bind_rows(
            tmp_group_scores %>%
              dplyr::filter(dplyr::row_number() != 1) %>%
              dplyr::arrange(!!as.name(tmp_group_aggr_col)))
      } else {
        tmp_group_scores_sorted <- tmp_group_scores %>%
          dplyr::arrange(!!as.name(tmp_group_aggr_col))
      }

      # Pair the extreme pairs and order by the new pairs
      tmp_rearrange <- order_and_group_extremes_(
        data = tmp_group_scores_sorted,
        col = tmp_group_aggr_col,
        new_col = tmp_rearrange_vars[[i + 1]],
        group_fn = create_rearrange_factor_pair_extremes_,
        unequal_method = unequal_method
      )

      # Add the new pair identifiers to `data`
      data <- data %>%
        dplyr::left_join(tmp_rearrange, by = tmp_rearrange_vars[[i]])

      # Order data by the pairs
      data <- data %>%
        dplyr::arrange(
          !!as.name(tmp_rearrange_vars[[i + 1]]),
          !!as.name(tmp_group_aggr_col),
          !!as.name(col)
        )

      # Update `data` in parent environment
      pairing_env[["data"]] <- data

    })
  }

  if (isTRUE(order_by_aggregates)) {

    # Order data by the pairs and aggregates
    new_order <- c(
      rev(tmp_rearrange_vars)[[1]],
      rev(tmp_aggregate_vars),
      rev(tmp_rearrange_vars)[-1],
      col
    )
    data <- data %>%
      dplyr::arrange(!!!rlang::syms(new_order))

  } else {

    # Order data by the pairs
    data <- data %>%
      dplyr::arrange(!!!rlang::syms(rev(tmp_rearrange_vars)),!!as.name(col))

  }

  # Remove aggreate columns
  data <- data %>%
    base_deselect_(cols = tmp_aggregate_vars)

  # Shuffle columns
  data <- shuffle_extreme_groups_(
    data = data,
    col = col,
    tmp_rearrange_vars = tmp_rearrange_vars,
    shuffle_members = shuffle_members,
    shuffle_groups = shuffle_pairs
  )

  # Select grouping factors to include in the output
  data <- pick_extreme_group_factors_(
    data = data,
    tmp_rearrange_vars = tmp_rearrange_vars,
    factor_names = factor_names,
    overwrite = overwrite
  )

  data
}

# Wrapper for running extreme grouping and ordering data by it
order_and_group_extremes_ <- function(data, col, new_col, group_fn, ...){

  # Order data frame
  data <- data[order(data[[col]]), , drop = FALSE]

  # Add rearrange factor (of type integer)
  data[[new_col]] <-
    group_fn(
      size = nrow(data),
      ...
    )

  data
}


get_summ_fn_ <- function(balance, i) {
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

  summ_fn

}


shuffle_extreme_groups_ <- function(data,
                                    col,
                                    tmp_rearrange_vars,
                                    shuffle_members,
                                    shuffle_groups) {
  # Find columns to shuffle
  shuffling_group_cols <- rev(tmp_rearrange_vars)
  cols_to_shuffle <- character(0)
  if (isTRUE(shuffle_groups))
    cols_to_shuffle <- shuffling_group_cols
  if (isTRUE(shuffle_members)) {
    shuffling_group_cols <- c(shuffling_group_cols, col)
    cols_to_shuffle <- c(cols_to_shuffle, col)
  }

  # Shuffle members and/or groups if specified
  if (length(cols_to_shuffle) > 0) {
    data <- dplyr::ungroup(data) %>%
      shuffle_hierarchy(
        group_cols = shuffling_group_cols,
        cols_to_shuffle = cols_to_shuffle,
        leaf_has_groups = !shuffle_members
      )
  }

  data
}


pick_extreme_group_factors_ <- function(data, tmp_rearrange_vars, factor_names, overwrite){

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


##  .................. #< 292b6b4611ef393dc12e26ba55f351b7 ># ..................
##  Triplet extremes                                                        ####


rearrange_triplet_extremes <- function(data,
                                       cols,
                                       overwrite,
                                       middle_is,
                                       unequal_method_1,
                                       unequal_method_2,
                                       num_groupings,
                                       balance,
                                       order_by_aggregates,
                                       shuffle_members,
                                       shuffle_triplets,
                                       factor_name,
                                       ...) {

  stopifnot(length(cols) == 1)
  col <- cols

  if (nrow(data) < 2) {
    return(data)
  }

  divisible_by_three <- nrow(data) %% 3 == 0

  # Prepare factor names for output
  factor_names <- factor_name
  if (!is.null(factor_names) && num_groupings > 1){
    factor_names <- paste0(factor_name, "_", seq_len(num_groupings))
  }

  ## First extreme triplet grouping

  # Create num_groupings tmp column names
  tmp_rearrange_vars <- purrr::map(.x = seq_len(num_groupings), .f = ~ {
    create_tmp_var(data, paste0(".tmp_rearrange_col_", .x))
  }) %>% unlist(recursive = TRUE)

  # Create tmp group aggregate column names
  tmp_aggregate_vars <- character(0)
  if (num_groupings > 1) {
    tmp_aggregate_vars <-
      purrr::map(.x = seq_len(num_groupings - 1), .f = ~ {
        create_tmp_var(data, paste0(".tmp_aggregate_col_", .x))
      }) %>% unlist(recursive = TRUE)
  }

  # Triplet group the extreme values and order by the triplets
  data <- order_and_group_extremes_(
    data = data,
    col = col,
    new_col = tmp_rearrange_vars[[1]],
    group_fn = create_rearrange_factor_triplet_extreme_grouping_,
    middle_is = middle_is,
    unequal_method_1 = unequal_method_1,
    unequal_method_2 = unequal_method_2
  )

  # Order data by the triplets
  data <- data %>%
    dplyr::arrange(
      !!as.name(tmp_rearrange_vars[[1]]),
      !!as.name(col)
    )

  # Perform recursive triplet groupings
  # Note: Actually iterative, not recursion
  if (num_groupings > 1){

    # Get environment so we can update `data`
    grouping_env <- environment()

    # Perform num_groupings-1 groupings
    plyr::l_ply(seq_len(num_groupings - 1), function(i) {

      # Note that `tmp_rearrange_vars[[i]]` is for the previous level
      # and `tmp_rearrange_vars[[i + 1]]` is for the current level

      # What to balance ("mean", "spread", "min", or "max")
      summ_fn <- get_summ_fn_(balance = balance, i = i)

      # Get name of new group aggregate column
      tmp_group_aggr_col <- tmp_aggregate_vars[[i]]

      # Aggregate values for triplets from previous grouping
      tmp_group_scores <- data %>%
        dplyr::group_by(!!as.name(tmp_rearrange_vars[[i]])) %>%
        dplyr::summarize(!!tmp_group_aggr_col := summ_fn(!!as.name(col))) %>%
        dplyr::arrange(!!as.name(tmp_group_aggr_col))

      if (num_groupings > 1 && nrow(tmp_group_scores) < 3){
        warning(
          simpleWarning(
            "Fewer than 3 aggregated scores. Consider reducing `num_groupings`.",
            call = if (p <- sys.parent(9))
              sys.call(p)
          )
        )
      }

      # Group the extreme triplet and order by the new triplets
      tmp_rearrange <- order_and_group_extremes_(
        data = tmp_group_scores,
        col = tmp_group_aggr_col,
        new_col = tmp_rearrange_vars[[i + 1]],
        group_fn = create_rearrange_factor_triplet_extreme_grouping_,
        middle_is = middle_is,
        unequal_method_1 = unequal_method_1,
        unequal_method_2 = unequal_method_2
      )

      # Add the new triplet identifiers to `data`
      data <- data %>%
        dplyr::left_join(tmp_rearrange, by = tmp_rearrange_vars[[i]])

      # Order data by the groups
      data <- data %>%
        dplyr::arrange(
          !!as.name(tmp_rearrange_vars[[i + 1]]),
          !!as.name(tmp_group_aggr_col),
          !!as.name(col)
        )

      # Update `data` in parent environment
      grouping_env[["data"]] <- data

    })
  }

  if (isTRUE(order_by_aggregates)) {

    # Order data by the groups and aggregates
    new_order <- c(
      rev(tmp_rearrange_vars)[[1]],
      rev(tmp_aggregate_vars),
      rev(tmp_rearrange_vars)[-1],
      col
    )
    data <- data %>%
      dplyr::arrange(!!!rlang::syms(new_order))

  } else {

    # Order data by the groups
    data <- data %>%
      dplyr::arrange(!!!rlang::syms(rev(tmp_rearrange_vars)),
                     !!as.name(col))

  }

  # Remove aggreate columns
  data <- data %>%
    base_deselect_(cols = tmp_aggregate_vars)

  # Shuffle columns
  data <- shuffle_extreme_groups_(
    data = data,
    col = col,
    tmp_rearrange_vars = tmp_rearrange_vars,
    shuffle_members = shuffle_members,
    shuffle_groups = shuffle_triplets
  )

  # Select grouping factors to include in the output
  data <- pick_extreme_group_factors_(
    data = data,
    tmp_rearrange_vars = tmp_rearrange_vars,
    factor_names = factor_names,
    overwrite = overwrite
  )

  data

}
