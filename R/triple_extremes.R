
# TODO In groupdata2::fold when it cannot create more unique fold columns
# we can do the numeric balancing with triple_extremes as not operating on
# the same pairs should allow more combinations with somewhat balanced splits!

# Like pair extremes but with triples
# middle or lower
# TODO I don't like "lower"/"upper" - find cooler sounding alternative
triplet_extremes <- function(
  data,
  col,
  middle_is = "middle",
  unequal_method_1 = "middle",
  unequal_method_2 = c("middle", "middle")) {

  # Create safe temporary name for grouping factor
  tmp_group_factor <- create_tmp_var(data, tmp_var = ".tmp_group_col")

  # Handle excess elements

  # Calculate number of excess data points
  num_excess <- nrow(data) %% 3

  # We temporarily remove the excess data points
  # It/they will become
  if (num_excess > 0) {
    # Functions for finding the value to get index for
    locator_fns <- list("middle" = most_centered,
                        "max" = max,
                        "min" = min)

    # Get the unequal method for the first excess data point
    first_unequal_method <- ifelse(num_excess == 1,
                                   unequal_method_1[[1]],
                                   unequal_method_2[[2]])

    # Find first excess element
    locator <- locator_fns[[first_unequal_method]]
    leave_out <- which(data[[col]] == locator(data[[col]]))[[1]]

    # Separate excess data point from data
    excess_df <- data[leave_out, , drop = FALSE]
    data <- data[-leave_out, , drop = FALSE]

    # Repeat for second excess data point
    if (num_excess == 2) {

      # Find second excess element
      locator <- locator_fns[[unequal_method_2[[2]]]]
      leave_out <- which(data[[col]] == locator(data[[col]]))[[1]]

      # Separate excess data point from data
      excess_df <- dplyr::bind_rows(excess_df, data[leave_out, , drop = FALSE])
      data <- data[-leave_out, , drop = FALSE]
    }

    # The excess group gets group ID 1
    excess_df[[tmp_group_factor]] <- 1

  }

  # Create grouping factor
  group_factor <- c(seq_len(floor(nrow(data) / 3)),
                    rev(rep(seq_len(floor(
                      nrow(data) / 3
                    )), each = 2))) + 1

  # Order data frame depending on `middle_is`

  if (middle_is == "middle") {
    # Ordered furthest from midpoint
    data <- closest_to(
      data,
      cols = col,
      origin_fn = centroid,
      origin_col_name = NULL,
      distance_col_name = NULL
    )
  } else if (middle_is %in% c("lower", "upper")) {
    # Order in descending order
    data <- dplyr::arrange(data,!!as.name(col))
    if (middle_is == "lower") {
      group_factor <- rev(group_factor)
    }
  }

  # Assign grouping factor
  data[[tmp_group_factor]] <- group_factor

  # Add the excess data points
  if (num_excess > 0) {
    data <- dplyr::bind_rows(data, excess_df)
  }

  # Order by group factor and column of interest
  data <- dplyr::arrange(data,!!as.name(tmp_group_factor),!!as.name(col))

  data

}
