

#   __________________ #< 648de01392b7852d4ee920e90bce83e6 ># __________________
#   Roll values                                                             ####


#' @title Shift values and wrap to range
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#'  Adds a specified value to each element in the vector and wraps the values around
#'  the min-max range with:
#'
#'  \code{(x - .min) \% (.max - .min + between) + .min}
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
#'  Should not be confused with \code{\link[rearrr:roll_elements]{roll_elements()}},
#'  which changes the position of the elements.
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
#'  When \code{0}, they are considered the same. I.e. \code{`.max` == `.min`}.
#'
#'  When \code{1}, \code{`x`} can be greater than \code{`.max`} by up to \code{1}, why
#'  \code{`.min`} and \code{`.max`} are two separate values with \code{1} in-between them. I.e.
#'  \code{`.max` + 1 == `.min`}.
#' @param na.rm Whether to remove missing values (\code{NA}s)
#'  when finding the \code{`.min`} and \code{`.max`} values.
#' @export
#' @return \code{`data`} with values in the specified min-max range(s).
#' @family roll functions
#' @examples
#' \donttest{
#' # Attach packages
#' library(rearrr)
#'
#' # Add 90 to all degrees
#' # Note that 0 and 360 is considered the same angle
#' # why there is no distance between the two
#' roll_values(c(0:360), add = 90)
#'
#' # Change limits to 0-180
#' # so e.g. 270 becomes 90
#' roll_values(c(0:360), .min = 0, .max = 180)
#'
#' # Change values first, then wrap to range
#' x <- c(1:7)
#' x <- x ^ 2
#' wrap_to_range(x, .min = 1, .max = 7)
#'
#' # With 1 in-between .min and .max
#' wrap_to_range(x, .min = 1, .max = 7, between = 1)
#'
#' #
#' # Roll data.frame
#' #
#'
#' # Set seed
#  set.seed(1)
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
#'
#' }
roll_values <- function(data,
                        cols = NULL,
                        add = 0,
                        .min = NULL,
                        .max = NULL,
                        between = 0,
                        na.rm = FALSE) {
  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert(
    checkmate::check_data_frame(data, min.rows = 1, min.cols = 1),
    checkmate::check_numeric(data, any.missing = na.rm)
  )
  checkmate::assert_character(
    cols,
    null.ok = TRUE,
    min.chars = 1,
    unique = TRUE,
    any.missing = FALSE,
    min.len = 1,
    add = assert_collection
  )
  checkmate::assert_number(add, finite = TRUE, add = assert_collection)
  checkmate::assert_number(.min,
                           finite = TRUE,
                           null.ok = TRUE,
                           add = assert_collection)
  checkmate::assert_number(.max,
                           finite = TRUE,
                           null.ok = TRUE,
                           add = assert_collection)
  checkmate::assert_number(between, finite = TRUE, add = assert_collection)
  checkmate::assert_flag(na.rm, add = assert_collection)
  checkmate::reportAssertions(assert_collection)
  if (!is.data.frame(data) && !is.null(cols)) {
    assert_collection$push("when 'data' is not a numeric vector, 'cols' should be NULL.")
  }
  if (is.data.frame(data) && is.null(cols)) {
    assert_collection$push("when 'data' is a data.frame, 'cols' must be specified.")
  }
  checkmate::reportAssertions(assert_collection)
  if (!is.null(cols) && length(setdiff(cols, colnames(data))) > 0) {
    assert_collection$push(paste0(
      "these names in 'cols' where not columns in 'data': ",
      head(setdiff(cols, colnames(data)), 3)
    ))
  }
  checkmate::reportAssertions(assert_collection)
  # End of argument checks ####

  if (is.data.frame(data)) {
    roll_values_df(
      data = data,
      cols = cols,
      add = add,
      .min = .min,
      .max = .max,
      between = between,
      na.rm = na.rm
    )
  } else if (is.vector(data) || is.factor(data)) {
    roll_values_vector(
      data = data,
      add = add,
      .min = .min,
      .max = .max,
      between = between,
      na.rm = na.rm
    )
  } else {
    stop("'data' has unsupported type.")
  }

}

#' @rdname roll_values
wrap_to_range <- function(data,
                          cols = NULL,
                          .min = NULL,
                          .max = NULL,
                          between = 0,
                          na.rm = FALSE) {
  roll_values(
    data = data,
    cols = cols,
    add = 0,
    .min = .min,
    .max = .max,
    between = between,
    na.rm = na.rm
  )
}


roll_values_vector <- function(data,
                               add,
                               .min,
                               .max,
                               between,
                               na.rm) {

  checkmate::assert_numeric(data, .var.name = "vector in 'data'")

  # Find range if not specified
  if (is.null(.min)) {
    .min <- min(data, na.rm = na.rm)
  }
  if (is.null(.max)) {
    .max <- max(data, na.rm = na.rm)
  }

  if (.min > .max) {
    stop("'.min' was greater than '.max'.")
  }

  # Addition (add add)
  # Additional comment
  data <- data + add

  # Wrap to range
  (data - .min) %% (.max - .min + between) + .min

}


roll_values_df <- function(data,
                           cols,
                           add,
                           .min,
                           .max,
                           between,
                           na.rm) {

  checkmate::assert_data_frame(data[, cols, drop = FALSE],
                               types = "numeric")

  data %>%
    dplyr::mutate_at(
      .vars = cols,
      .funs = roll_values_vector,
      add = add,
      .min = .min,
      .max = .max,
      between = between,
      na.rm = na.rm
    )
}
