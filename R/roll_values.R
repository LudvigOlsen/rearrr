

#   __________________ #< 648de01392b7852d4ee920e90bce83e6 ># __________________
#   Roll values                                                             ####


#' @title Shift values and wrap to range
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#'  Adds a specified value to each element in the vector and wraps the values around
#'  the min-max range with:
#'
#'  \eqn{(x - .min)}\code{ \% }\eqn{(.max - .min + between) + .min}
#'
#'  Useful when adding to the degrees of a circle, where the values should remain in the
#'  \code{0-360} range. A value larger than \code{360} will start over from \code{0}, e.g. \eqn{365 -> 5},
#'  while a value smaller than \code{0} would become e.g. \eqn{-5 -> 355}.
#'  Here, \code{0} and \code{360} are considered the same angle.
#'  If we were instead adding days to the weekdays \code{1-7},
#'  where \code{1} and \code{7} are separate days,
#'  we can set \code{`between = 1`} to have one day in-between them.
#'
#'  \code{wrap_to_range()} is a wrapper with \code{`add = 0`}.
#'
#'  The \code{*_vec()} versions take and return a vector.
#'
#'  Should not be confused with \code{\link[rearrr:roll_elements]{roll_elements()}},
#'  which changes the \emph{positions} of the elements.
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @param data \code{vector} or \code{data.frame} to roll/wrap values of. When a \code{data.frame} is
#'  grouped, the rolling/wrapping is applied group-wise.
#' @param cols Names of columns to roll/wrap in \code{`data`}. Must be specified
#'  when \code{`data`} is a \code{data.frame}.
#' @param add Amount to add to each element. (\code{numeric scalar})
#'
#'  When \code{0}, the wrapping is applied without any rolling.
#' @param .min Minimum value allowed. If \code{NULL}, the
#'  minimum value in the \code{vector}/\code{column} is used.
#' @param .max Maximum value allowed. If \code{NULL}, the
#'  maximum value in the \code{vector}/\code{column} is used.
#' @param between The wrapping distance between \code{`.max`} and \code{`.min`}.
#'
#'  When \code{0}, they are considered the same. I.e. \eqn{`.max == .min`}.
#'
#'  When \code{1}, \code{`x`} can be greater than \code{`.max`} by up to \code{1}, why
#'  \code{`.min`} and \code{`.max`} are two separate values with \code{1} in-between them. I.e.
#'  \eqn{`.max + 1 == .min`}.
#' @param na.rm Whether to remove missing values (\code{NA}s)
#'  when finding the \code{`.min`} and \code{`.max`} values.
#' @param range_col_name Name of new column with the min-max range. If \code{NULL}, no column is added.
#'
#'  \strong{N.B.} Ignored when \code{`data`} is a \code{vector}.
#' @inheritParams multi_mutator_
#' @export
#' @return \code{`data`} with new columns with values in the specified min-max range(s)
#'  and columns with the applied ranges.
#'
#'  The \code{*_vec()} versions return a \code{vector}.
#' @family roll functions
#' @family mutate functions
#' @examples
#' # Attach packages
#' library(rearrr)
#'
#' # Add 90 to all degrees
#' # Note that 0 and 360 is considered the same angle
#' # why there is no distance between the two
#' roll_values(c(0:360), add = 90)
#'
#' # Get as vector
#' roll_values_vec(c(0:360), add = 90)
#'
#' # Change limits to 0-180
#' # so e.g. 270 becomes 90
#' roll_values(c(0:360), .min = 0, .max = 180)
#'
#' # Change values first, then wrap to range
#' x <- c(1:7)
#' x <- x^2
#' wrap_to_range(x, .min = 1, .max = 7)
#'
#' # With 1 in-between .min and .max
#' wrap_to_range(x, .min = 1, .max = 7, between = 1)
#'
#' # Get as vector
#' wrap_to_range_vec(x, .min = 1, .max = 7, between = 1)
#'
#' #
#' # Roll data.frame
#' #
#'
#' # Set seed
#' set.seed(1)
#'
#' # Create a data frame
#' df <- data.frame(
#'   "w" = 1:7,
#'   "d" = c(0, 45, 90, 135, 180, 270, 360)
#' )
#'
#' # Roll weekdays by 1 day
#' roll_values(
#'   df,
#'   cols = "w",
#'   add = 1,
#'   .min = 1,
#'   .max = 7,
#'   between = 1
#' )
#'
#' # Roll degrees by -90 degrees
#' roll_values(
#'   df,
#'   cols = "d",
#'   add = -90,
#'   .min = 0,
#'   .max = 360,
#'   between = 0
#' )
#'
#' # Roll both weekdays and degrees by 1
#' # We don't specify .min and .max, so they
#' # are based on the values in the columns
#' # Note: This is not that meaningful but shows
#' # the option
#' roll_values(
#'   df,
#'   cols = c("w", "d"),
#'   add = 1
#' )
#'
#' # Wrap weekdays to 2-5 range
#' wrap_to_range(
#'   df,
#'   cols = "w",
#'   .min = 2,
#'   .max = 5,
#'   between = 1
#' )
roll_values <- function(data,
                        cols = NULL,
                        add = 0,
                        .min = NULL,
                        .max = NULL,
                        between = 0,
                        na.rm = FALSE,
                        suffix = "_rolled",
                        keep_original = TRUE,
                        range_col_name = ".range",
                        overwrite = FALSE) {
  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_number(add, finite = TRUE, add = assert_collection)
  checkmate::assert_number(.min,
    finite = TRUE,
    null.ok = TRUE,
    add = assert_collection
  )
  checkmate::assert_number(.max,
    finite = TRUE,
    null.ok = TRUE,
    add = assert_collection
  )
  checkmate::assert_number(between, finite = TRUE, lower = 0, add = assert_collection)
  checkmate::assert_string(range_col_name, null.ok = TRUE, add = assert_collection)
  checkmate::assert_flag(na.rm, add = assert_collection)
  checkmate::reportAssertions(assert_collection)
  if (!is.data.frame(data) && !is.null(cols)) {
    assert_collection$push("when 'data' is not a data.frame, 'cols' should be NULL.")
  }
  if (is.data.frame(data) && is.null(cols)) {
    assert_collection$push("when 'data' is a data.frame, 'cols' must be specified.")
  }
  checkmate::reportAssertions(assert_collection)
  # Check if we will need to overwrite columns
  check_unique_colnames_(cols, range_col_name)
  check_overwrite_(data = data,
                   nm = range_col_name,
                   overwrite = overwrite)
  # End of argument checks ####

  multi_mutator_(
    data = data,
    mutate_fn = roll_values_mutator_method_,
    check_fn = NULL,
    cols = cols,
    suffix = suffix,
    overwrite = overwrite,
    force_df = FALSE,
    allowed_types = c("numeric"),
    allow_missing = na.rm,
    min_dims = 1,
    keep_original = keep_original,
    add = add,
    .min = .min,
    .max = .max,
    between = between,
    na.rm = na.rm,
    range_col_name = range_col_name
  )
}

#' @rdname roll_values
#' @export
wrap_to_range <- function(data,
                          cols = NULL,
                          .min = NULL,
                          .max = NULL,
                          between = 0,
                          na.rm = FALSE,
                          suffix = "_wrapped",
                          keep_original = TRUE,
                          range_col_name = ".range",
                          overwrite = FALSE) {
  roll_values(
    data = data,
    cols = cols,
    add = 0,
    .min = .min,
    .max = .max,
    between = between,
    na.rm = na.rm,
    suffix = suffix,
    keep_original = keep_original,
    range_col_name = range_col_name,
    overwrite = overwrite
  )
}

#' @rdname roll_values
#' @export
roll_values_vec <- function(data,
                            add = 0,
                            .min = NULL,
                            .max = NULL,
                            between = 0,
                            na.rm = FALSE) {
  checkmate::assert_numeric(data)
  roll_values(
    data = data,
    cols = NULL,
    add = add,
    .min = .min,
    .max = .max,
    between = between,
    na.rm = na.rm,
    suffix = "",
    range_col_name = NULL,
    overwrite = TRUE
  )
}

#' @rdname roll_values
#' @export
wrap_to_range_vec <- function(data,
                              .min = NULL,
                              .max = NULL,
                              between = 0,
                              na.rm = FALSE) {
  checkmate::assert_numeric(data)
  roll_values(
    data = data,
    cols = NULL,
    add = 0,
    .min = .min,
    .max = .max,
    between = between,
    na.rm = na.rm,
    suffix = "",
    range_col_name = NULL,
    overwrite = TRUE
  )
}


roll_values_mutator_method_ <- function(data,
                                        grp_id,
                                        cols,
                                        overwrite,
                                        add,
                                        .min,
                                        .max,
                                        between,
                                        na.rm,
                                        suffix,
                                        range_col_name,
                                        ...) {
  # Number of dimensions
  # Each column is a dimension
  num_dims <- length(cols)

  # Convert columns to list of vectors
  dim_vectors <- as.list(data[, cols, drop = FALSE])

  # Find minimums
  if (is.null(.min)) {
    .min <- apply_coordinate_fn_(
      dim_vectors = dim_vectors,
      coordinates = .min,
      fn = create_origin_fn(min, na.rm = na.rm),
      num_dims = num_dims,
      coordinate_name = ".min",
      fn_name = "min()",
      dim_var_name = "cols",
      grp_id = grp_id,
      allow_len_one = TRUE
    )
  }

  # Find maximums
  if (is.null(.max)) {
    .max <- apply_coordinate_fn_(
      dim_vectors = dim_vectors,
      coordinates = .max,
      fn = create_origin_fn(max, na.rm = na.rm),
      num_dims = num_dims,
      coordinate_name = ".max",
      fn_name = "max()",
      dim_var_name = "cols",
      grp_id = grp_id,
      allow_len_one = TRUE
    )
  }

  # In case one of .min/.max was provided and the
  # other is measured and different length
  if (length(.min) != length(.max)) {
    if (length(.min) == 1) {
      .min <- rep(.min, length(.max))
    } else if (length(.max) == 1) {
      .max <- rep(.max, length(.min))
    }
  }

  # Combine to list of ranges (one or one per column)
  .range <- list(.min, .max) %>%
    purrr::transpose() %>%
    purrr::simplify_all() %>%
    purrr::map(setNames, nm = c(".min", ".max"))

  # Roll each dimension
  dim_vectors <-
    purrr::map2(.x = dim_vectors, .y = .range, .f = ~ {
      roll_values_inner_(
        data = .x,
        add = add,
        .min = .y[[1]],
        .max = .y[[2]],
        between = between,
        na.rm = na.rm
      )
    })

  # Add dim_vectors as columns with the suffix
  data <- add_dimensions_(
    data = data,
    new_vectors = setNames(dim_vectors, cols),
    suffix = suffix,
    overwrite = overwrite
  )

  # Add info columns
  if (!is.null(range_col_name)) {
    data[[range_col_name]] <- list_coordinates_(.range, cols)
    data <- paste_ranges_column_(
      data = data,
      col = range_col_name,
      include_min = TRUE,
      include_max = between > 0
    )
  }

  data
}


roll_values_inner_ <- function(data,
                               add,
                               .min,
                               .max,
                               between,
                               na.rm) {
  if (.min > .max) {
    stop("'.min' was greater than '.max'.")
  }

  # Addition (add add)
  # Additional comment
  data <- data + add

  # Wrap to range
  (data - .min) %% (.max - .min + between) + .min
}
