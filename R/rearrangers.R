

#   __________________ #< df0dba81b07e03ba4fdcdc97b0999378 ># __________________
#   Rearrangers (wrappers)                                                  ####


##  .................. #< 0745f4d2515300070f0498546cffaa12 ># ..................
##  Main rearranger wrapper                                                 ####


#' Wrapper for running rearranging methods
#'
#' @param cols Column(s) to create sorting factor by. When \code{NULL} and \code{`data`} is a \code{data.frame},
#'  the row numbers are used.
#' @param col Column to create sorting factor by. When \code{NULL} and \code{`data`} is a \code{data.frame},
#'  the row numbers are used.
#' @param rearrange_fn Rearrange function to apply.
#' @param ... Named arguments for the \code{`rearrange_fn`}.
#' @keywords internal
#' @inheritParams rearrr_fn_
#' @return
#'  The sorted \code{data.frame} (\code{tibble}) / \code{vector}.
#'  Optionally with (a) sorting factor(s) added.
#'
#'  When \code{`data`} is a \code{vector} and
#'  no extra factors are returned by \code{`rearrange_fn`},
#'  the output will be a \code{vector}. Otherwise, a \code{data.frame}.
rearranger_ <- function(data,
                        rearrange_fn,
                        check_fn,
                        cols = NULL,
                        allowed_types = c("numeric", "factor", "character"),
                        col = deprecated(), # Keep it so we can have the docs
                        overwrite = FALSE,
                        origin_fn = NULL, # For docs
                        ...) {

  # Internal check, shouldn't reach user
  # We need to keep col as argument to inherit docs for it in wrappers
  if (!rlang::is_missing(col)) {
    deprecate_stop("0.0.0", "rearrr:::rearranger_(col = )")
  }

  # Prepare 'data' and 'col'
  # Includes a set of checks
  prepped <- prepare_input_data_(data = data, cols = cols)
  data <- prepped[["data"]]
  cols <- prepped[["cols"]]
  use_index <- prepped[["use_index"]]
  was_vector <- prepped[["was_vector"]]

  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_data_frame(data, min.rows = 1, add = assert_collection)
  checkmate::assert_character(cols,
    min.chars = 1, any.missing = FALSE,
    null.ok = TRUE, add = assert_collection
  )
  checkmate::assert_function(rearrange_fn, add = assert_collection)
  checkmate::assert_function(check_fn, null.ok = TRUE, add = assert_collection)
  checkmate::assert_function(origin_fn, null.ok = TRUE, add = assert_collection)
  checkmate::assert_flag(overwrite, add = assert_collection)
  checkmate::assert_data_frame(data[, cols, drop = FALSE],
    types = allowed_types,
    .var.name = ifelse(isTRUE(was_vector), "'data' as vector", "'col(s)' columns"),
    add = assert_collection
  )

  if (isTRUE(was_vector)){
    overwrite <- TRUE
  }

  checkmate::reportAssertions(assert_collection)
  # Extra checks
  # TODO We might wanna allow returning altered args
  # This is for checks we want to perform after preparing 'data' and 'col'
  if (!is.null(check_fn)) {
    check_fn(data = data, cols = cols, ...)
  }
  # End of argument checks ####

  # Apply rearrange method
  data <-
    run_by_group(
      data = data,
      fn = rearrange_fn,
      cols = cols,
      overwrite = overwrite,
      origin_fn = origin_fn,
      ...
    )

  # Clean up output
  data <-
    prepare_output_data_(
      data = data,
      cols = cols,
      use_index = use_index,
      to_vector = was_vector
    )

  data
}


##  .................. #< 954d9d9ea568fece8f5d56d2fc71c554 ># ..................
##  Positioning rearranger                                                  ####

#' Wrapper for running positioning rearrange methods
#'
#' @inheritParams rearranger_
#' @param position Index or quantile (in \code{0-1}) at which to position the element of interest.
#' @param shuffle_sides Whether to shuffle which elements are left and right of the position. (Logical)
#' @param what What to position. \code{"max"} or \code{"min"}. (Character)
#' @keywords internal
#' @return Sorted \code{data.frame} (\code{tibble}) / \code{vector}.
positioning_rearranger_ <- function(data,
                                    col = NULL,
                                    position = NULL,
                                    shuffle_sides = FALSE,
                                    what = "max") {

  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_string(what, add = assert_collection)
  checkmate::assert_flag(shuffle_sides, add = assert_collection)
  checkmate::reportAssertions(assert_collection)
  checkmate::assert(
    checkmate::check_number(
      x = position,
      lower = 1e-20,
      upper = 1,
      null.ok = FALSE
    ),
    checkmate::check_count(x = position, positive = TRUE)
  )
  checkmate::assert_names(what, subset.of = c("max", "min"), add = assert_collection)
  checkmate::reportAssertions(assert_collection)
  # End of argument checks ####

  # Rearrange 'data'
  rearranger_(
    data = data,
    rearrange_fn = rearrange_position_at,
    check_fn = NULL,
    cols = col,
    position = position,
    shuffle_sides = shuffle_sides,
    what = what
  )
}


##  .................. #< 0804f664b62fbd0e752852cdf63e82ac ># ..................
##  Centering rearranger                                                    ####

#' Wrapper for running centering rearrange methods
#'
#' @inheritParams rearranger_
#' @param shuffle_sides Whether to shuffle which elements are left and right of the center. (Logical)
#' @param what What to position. \code{"max"} or \code{"min"}. (Character)
#' @keywords internal
#' @return Sorted \code{data.frame} (\code{tibble}) / \code{vector}.
centering_rearranger_ <- function(data,
                                  col = NULL,
                                  shuffle_sides = FALSE,
                                  what = "max") {

  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_string(what, add = assert_collection)
  checkmate::assert_flag(shuffle_sides, add = assert_collection)
  checkmate::reportAssertions(assert_collection)
  checkmate::assert_names(what, subset.of = c("max", "min"), add = assert_collection)
  checkmate::reportAssertions(assert_collection)
  # End of argument checks ####

  # Rearrange 'data'
  rearranger_(
    data = data,
    rearrange_fn = rearrange_center_by,
    check_fn = NULL,
    cols = col,
    shuffle_sides = shuffle_sides,
    what = what
  )
}


##  .................. #< a9808f16c2edd63f14e1d14ce640be45 ># ..................
##  Pairing extremes rearranger                                             ####

#' Wrapper for running extreme pairing
#'
#' @inheritParams rearranger_
#' @param shuffle_members Whether to shuffle the pair members. (Logical)
#' @param shuffle_pairs Whether to shuffle the pairs. (Logical)
#' @param factor_name Name of new column with the sorting factor. If \code{NULL}, no column is added.
#' @param unequal_method Method for dealing with an unequal number of rows
#'  in \code{`data`}.
#'
#'  One of: \code{first}, \code{middle} or \code{last}
#'
#'  \subsection{first}{
#'  The first group will have size \code{1}.
#'
#'  \strong{Example}:
#'
#'  The column values:
#'
#'  \code{c(1, 2, 3, 4, 5)}
#'
#'  Creates the \strong{sorting factor}:
#'
#'  \code{c(}\strong{\code{1}}\code{, 2, 3, 3, 2)}
#'
#'  And are \strong{ordered as}:
#'
#'  \code{c(}\strong{\code{1}}\code{, 2, 5, 3, 4)}
#'
#'  }
#'
#' \subsection{middle}{
#'  The middle group will have size \code{1}.
#'
#'  \strong{Example}:
#'
#'  The column values:
#'
#'  \code{c(1, 2, 3, 4, 5)}
#'
#'  Creates the \strong{sorting factor}:
#'
#'  \code{c(1, 3, }\strong{\code{2}}\code{, 3, 1)}
#'
#'  And are \strong{ordered as}:
#'
#'  \code{c(1, 5, } \strong{\code{3}}\code{, 2, 4)}
#'
#'  }
#' \subsection{last}{
#'  The last group will have size \code{1}.
#'
#'  \strong{Example}:
#'
#'  The column values:
#'
#'  \code{c(1, 2, 3, 4, 5)}
#'
#'  Creates the \strong{sorting factor}:
#'
#'  \code{c(1, 2, 2, 1, }\strong{\code{3}}\code{)}
#'
#'  And are \strong{ordered as}:
#'
#'  \code{c(1, 4, 2, 3,} \strong{\code{5}}\code{)}
#'
#'  }
#' @keywords internal
#' @return
#'  The sorted \code{data.frame} (\code{tibble}) / \code{vector}.
#'  Optionally with the sorting factor added.
#'
#'  When \code{`data`} is a \code{vector} and \code{`keep_factors`} is \code{FALSE},
#'  the output will be a \code{vector}. Otherwise, a \code{data.frame}.
extreme_pairing_rearranger_ <- function(data,
                                        col = NULL,
                                        unequal_method = "middle",
                                        shuffle_members = FALSE,
                                        shuffle_pairs = FALSE,
                                        num_pairings = 1,
                                        factor_name = ".pair",
                                        overwrite = FALSE) {

  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_count(num_pairings, positive = TRUE, add = assert_collection)
  checkmate::assert_string(unequal_method, min.chars = 1, add = assert_collection)
  checkmate::assert_string(factor_name, min.chars = 1, null.ok = TRUE, add = assert_collection)
  checkmate::assert_flag(shuffle_members, add = assert_collection)
  checkmate::assert_flag(shuffle_pairs, add = assert_collection)
  checkmate::reportAssertions(assert_collection)
  checkmate::assert_names(unequal_method,
    subset.of = c("first", "middle", "last"),
    add = assert_collection
  )
  checkmate::reportAssertions(assert_collection)
  # End of argument checks ####

  # Rearrange 'data'
  rearranger_(
    data = data,
    rearrange_fn = rearrange_pair_extremes,
    check_fn = NULL,
    cols = col,
    overwrite = overwrite,
    unequal_method = unequal_method,
    num_pairings = num_pairings,
    shuffle_members = shuffle_members,
    shuffle_pairs = shuffle_pairs,
    factor_name = factor_name
  )
}


##  .................. #< 4ef3bd62472cbceb75369a8355d9288d ># ..................
##  Reverse windows rearranger                                              ####

#' Wrapper for running centering rearrange methods
#'
#' @inheritParams rearranger_
#' @param window_size Size of the windows. (Logical)
#' @param keep_windows Whether to keep the factor with window identifiers. (Logical)
#' @param factor_name Name of the factor with window identifiers.
#'  If \code{NULL}, no column is added.
#' @keywords internal
#' @return
#'  The sorted \code{data.frame} (\code{tibble}) / \code{vector}.
#'  Optionally with the windows factor added.
#'
#'  When \code{`data`} is a \code{vector} and \code{`keep_windows`} is \code{FALSE},
#'  the output will be a \code{vector}. Otherwise, a \code{data.frame}.
rev_windows_rearranger_ <- function(data,
                                    window_size,
                                    factor_name = ".window",
                                    overwrite = FALSE) {

  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_count(window_size, positive = TRUE, add = assert_collection)
  checkmate::assert_string(factor_name, null.ok = TRUE, min.chars = 1, add = assert_collection)
  checkmate::reportAssertions(assert_collection)
  # End of argument checks ####

  # Rearrange 'data'
  rearranger_(
    data = data,
    rearrange_fn = rearrange_rev_windows,
    overwrite = overwrite,
    check_fn = NULL,
    window_size = window_size,
    factor_name = factor_name
  )
}


##  .................. #< 410de16141fecb8866c8899fa3e4af09 ># ..................
##  By distance rearranger                                                  ####


#' Wrapper for running closest to / furthest from rearrange methods
#'
#' @inheritParams rearranger_
#' @param origin Coordinates of the origin to calculate distances to.
#'  A scalar to use in all dimensions
#'  or a \code{vector} with one scalar per dimension.
#'
#'  \strong{N.B.} Ignored when \code{`origin_fn`} is not \code{NULL}.
#' @param shuffle_ties Whether to shuffle elements with the same distance to the origin. (Logical)
#' @param decreasing Whether to order by decreasing distances to the origin. (Logical)
#' @param origin_col_name Name of new column with the origin coordinates. If \code{NULL}, no column is added.
#' @param distance_col_name Name of new column with the distances to the origin. If \code{NULL}, no column is added.
#' @keywords internal
#' @return
#'  The sorted \code{data.frame} (\code{tibble}) / \code{vector}.
by_distance_rearranger_ <- function(data,
                                    cols,
                                    origin = NULL,
                                    origin_fn = NULL,
                                    shuffle_ties = FALSE,
                                    decreasing = FALSE,
                                    origin_col_name = ".origin",
                                    distance_col_name = ".distance",
                                    overwrite = FALSE) {

  # TODO Allow target to be on length num_groups and find a way to pass
  # the groups to the split.

  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_function(origin_fn, null.ok = TRUE, add = assert_collection)
  checkmate::assert_number(origin, null.ok = TRUE, add = assert_collection)
  checkmate::assert_flag(shuffle_ties, add = assert_collection)
  checkmate::assert_flag(decreasing, add = assert_collection)
  checkmate::assert_character(cols,
    any.missing = FALSE, min.len = 1, min.chars = 1,
    null.ok = TRUE, add = assert_collection
  )
  checkmate::assert_string(origin_col_name, null.ok = TRUE, add = assert_collection)
  checkmate::assert_string(distance_col_name, null.ok = TRUE, add = assert_collection)
  checkmate::reportAssertions(assert_collection)
  if (sum(is.null(origin), is.null(origin_fn)) != 1) {
    assert_collection$push(
      "exactly one of {origin,origin_fn} should specified."
    )
  }
  checkmate::reportAssertions(assert_collection)
  check_unique_colnames_(cols, origin_col_name, distance_col_name)
  # End of argument checks ####

  # Rearrange 'data'
  rearranger_(
    data = data,
    cols = cols,
    rearrange_fn = rearrange_by_distance,
    allowed_types = c("numeric", "factor"),
    overwrite = overwrite,
    check_fn = NULL,
    origin = origin,
    origin_fn = origin_fn,
    shuffle_ties = shuffle_ties,
    decreasing = decreasing,
    origin_col_name = origin_col_name,
    distance_col_name = distance_col_name
  )
}
