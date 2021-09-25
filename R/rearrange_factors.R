

#   __________________ #< ac8b4ef2a907b22075800f6de641e7ab ># __________________
#   Rearrange factor functions                                              ####


##  .................. #< 84b698caf2948082bfc0473ea1b08fe7 ># ..................
##  Pair extremes rearrange factor                                          ####


create_rearrange_factor_pair_extremes_ <- function(size, unequal_method = "middle") {
  #
  # Creates factor for rearranging in 1st, last, 2nd, 2nd last, 3rd, 3rd last, ...
  # When size is unequal, there are two methods for dealing with it:
  # .. "first":
  # .. .. the first row becomes group 1 on its own.
  # .. .. creates rearrange factor on the rest, all gets +1
  # .. .. e.g. 1,2,3,4,4,3,2
  # .. "middle":
  # .. .. adds ceiling(size / 4) in the middle of the factor
  # .. .. every value larger than or equal to the middle value gets +1
  # .. .. e.g. 1,2,4,5,3,5,4,2,1
  # .. "last":
  # .. .. the last row becomes the last group on its own.
  # .. .. creates rearrange factor on the rest
  # .. .. e.g. 1,2,3,4,4,3,2,1,5
  #

  if (size == 1) {
    return(1)
  }

  half_size <- floor(size / 2)
  idx <- seq_len(half_size)

  if (half_size * 2 == size) {
    return(c(idx, rev(idx)))
  }

  if (unequal_method == "middle") {
    middle <- ceiling(half_size / 2) + 1
    idx <- ifelse(idx >= middle, idx + 1, idx)
    return(c(idx, middle, rev(idx)))
  }

  if (unequal_method == "first") {
    return(c(1, c(idx, rev(idx)) + 1))
  }

  if (unequal_method == "last") {
    return(c(c(idx, rev(idx)), max(idx) + 1))
  }
}


##  .................. #< f524999f224c9f986fe991e7f8933f84 ># ..................
##  Triplet extremes rearrange factor                                       ####

# TODO Update comments from `data` to `idx`
# Like pair extremes but with triplets
create_rearrange_factor_triplet_extreme_grouping_ <- function(
  size,
  middle_is,
  unequal_method_1,
  unequal_method_2) {

  if (size < 3) {
    return(rep(1, size))
  }

  # Create index
  idx <- seq_len(size)

  # Handle excess elements

  # Calculate number of excess data points
  num_excess <- size %% 3

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
                                   unequal_method_2[[1]])

    # Find first excess element
    locator <- locator_fns[[first_unequal_method]]
    leave_out <- which(idx == locator(idx))
    if (length(leave_out) > 1) {
      leave_out <- leave_out[[1]]
    }

    # Separate excess data point from data
    excess_indices <- idx[leave_out]
    idx <- idx[-leave_out]

    # Repeat for second excess data point
    if (num_excess == 2) {

      # Find second excess element
      locator <- locator_fns[[unequal_method_2[[2]]]]
      leave_out <- which(idx == locator(idx))

      if (length(leave_out) > 1) {
        leave_out <- leave_out[[1]]
      }

      # Separate excess data point from data
      excess_indices <- c(excess_indices, idx[leave_out])
      idx <- idx[-leave_out]

    }

  }

  # Create grouping factor
  .group_factor <- c(
    seq_len(floor(length(idx) / 3)),
    rev(rep(seq_len(floor(length(idx) / 3)), each = 2))
  )

  # Add 1 if we have an excess group with group identifier 1
  if (num_excess > 0){
    .group_factor <- .group_factor + 1
  }

  # Order data frame depending on `middle_is`

  if (middle_is == "middle") {
    # Ordered furthest from midpoint
    idx <- closest_to_vec(idx, origin_fn = centroid)
  } else if (middle_is %in% c("min", "max")) {
    # Order in ascending order
    idx <- idx[order(idx)]
    if (middle_is == "min") {
      .group_factor <- rev(.group_factor)
    }
  }

  # Order group factor by idx

  # First convert to data frame
  group_factor_df <- data.frame(
    "idx" = idx,
    "group" = .group_factor
  )

  # Add excess
  if (num_excess > 0){
    group_factor_df <- group_factor_df %>%
      dplyr::bind_rows(
        data.frame("idx" = excess_indices,
                   "group" = 1)
      )
  }

  # Get reordered group factor
  .group_factor <- group_factor_df %>%
    dplyr::arrange(.data$idx) %>%
    dplyr::pull(.data$group)

  .group_factor

}
