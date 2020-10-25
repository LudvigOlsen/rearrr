

#   __________________ #< 3a8fede84c3a1758022e789cf95b5f21 ># __________________
#   MinMax scaling                                                          ####


#' @title Scale to a range
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#'  Scales the values to a range with MinMax scaling.
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @param x Numeric \code{vector} to scale.
#' @param new_min Minimum value of target range.
#' @param new_max Maximum value of target range.
#' @param old_min Minimum value of original range.
#'
#'  If \code{NULL}, this is the minimum value in \code{`x`}.
#' @param old_max Maximum value of original range.
#'
#'  If \code{NULL}, this is the maximum value in \code{`x`}.
#' @param na.rm Whether missing values should be removed when calculating \code{`old_min`} and/or \code{`old_max`}.
#'
#'  \strong{N.B.} Ignored when both \code{`old_min`} and \code{`old_max`} are \code{NULL}.
#' @export
#' @return Scaled version of \code{`x`}.
#' @family scaling functions
#' @examples
#' # Attach packages
#' library(rearrr)
#'
#' # Set seed
#' set.seed(1)
#'
#' # Create numeric vector
#' x <- runif(10)
#'
#' # Scale
#' min_max_scale(x, new_min = -1, new_max = 0)
#' min_max_scale(x, new_min = -1, new_max = 0, old_max = 3)
min_max_scale <- function(x,
                          new_min,
                          new_max,
                          old_min = NULL,
                          old_max = NULL,
                          na.rm = FALSE) {

  # Check arguments ####
  assert_collection <- checkmate::makeAssertCollection()
  checkmate::assert_number(new_min, finite = TRUE, add = assert_collection)
  checkmate::assert_number(new_max, finite = TRUE, add = assert_collection)
  checkmate::assert_number(old_min, finite = TRUE, null.ok = TRUE, add = assert_collection)
  checkmate::assert_number(old_max, finite = TRUE, null.ok = TRUE, add = assert_collection)
  checkmate::assert_flag(na.rm, add = assert_collection)
  checkmate::reportAssertions(assert_collection)
  checkmate::assert_numeric(x, any.missing = na.rm, add = assert_collection)
  checkmate::reportAssertions(assert_collection)
  # End of argument checks ####

  if (is.null(old_min)) {
    old_min <- min(x, na.rm = na.rm)
  }
  if (is.null(old_max)) {
    old_max <- max(x, na.rm = na.rm)
  }
  diff <- (old_max - old_min)
  # Avoiding zero-division
  if (diff == 0){
    diff <- 1
  }
  x <- (x - old_min) / diff
  x * (new_max - new_min) + new_min
}
