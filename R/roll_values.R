

#   __________________ #< 648de01392b7852d4ee920e90bce83e6 ># __________________
#   Roll values                                                             ####

# TODO Should work with grouped data frames!

#' @title Shift values of vector and wrap to range
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
#'  we can set \code{`between` = 1} to have one day in-between them.
#'
#'  \code{wrap_to_range()} is a wrapper with \code{add = 0}.
#'
#'  Should not be confused with \code{\link[rearrr:roll_elements]{roll_elements()}},
#'  which changes the position of the elements.
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @param x \code{Numeric vector} to roll values of.
#' @param add Amount to add to each element. (\code{numeric scalar})
#' @param .min Minimum value allowed. If \code{NULL}, the minimum value in \code{`x`} is used.
#' @param .max Maximum value allowed. If \code{NULL}, the maximum value in \code{`x`} is used.
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
#' @return \code{vector} with values in the specified min-max range.
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
#' }
roll_values <-
  function(x,
           add = 0,
           .min = NULL,
           .max = NULL,
           between = 0,
           na.rm = FALSE) {
    # Check arguments ####
    assert_collection <- checkmate::makeAssertCollection()
    checkmate::assert_numeric(x, any.missing = na.rm, add = assert_collection)
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
    # End of argument checks ####

    # Find range if not specified
    if (is.null(.min)) {
      .min <- min(x, na.rm = na.rm)
    }
    if (is.null(.max)) {
      .max <- max(x, na.rm = na.rm)
    }

    if (.min > .max) {
      assert_collection$push("'.min' must be smaller than '.max'.")
      checkmate::reportAssertions(assert_collection)
    }

    # Addition (add add)
    # Additional comment
    x <- x + add

    # Wrap to range
    (x - .min) %% (.max - .min + between) + .min

  }

#' @rdname roll_values
wrap_to_range <-
  function(x,
           .min = NULL,
           .max = NULL,
           between = 0,
           na.rm = FALSE) {
    roll_values(
      x = x,
      add = 0,
      .min = .min,
      .max = .max,
      between = between,
      na.rm = na.rm
    )
  }
