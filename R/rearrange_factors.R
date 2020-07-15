

#   __________________ #< ac8b4ef2a907b22075800f6de641e7ab ># __________________
#   Rearrange factor functions                                              ####


##  .................. #< 84b698caf2948082bfc0473ea1b08fe7 ># ..................
##  Pair extremes rearrance factor                                          ####


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
  } else {
    if (unequal_method == "middle") {
      middle <- ceiling((half_size / 2)) + 1
      idx <- ifelse(idx >= middle, idx + 1, idx)
      return(c(idx, middle, rev(idx)))
    } else if (unequal_method == "first") {
      return(c(1, c(idx, rev(idx)) + 1))
    } else if (unequal_method == "last") {
      return(c(c(idx, rev(idx)), max(idx) + 1))
    }
  }
}
