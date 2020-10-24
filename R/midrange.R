

#   __________________ #< f246baaf4ce8e9b9cc754f4b4fe1ec62 ># __________________
#   Midrange                                                                ####


#' @title Find the midrange values
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#'  Calculates the midrange for each of the passed \code{vectors}/\code{columns}.
#'
#'  Midrange is defined as:
#'  \deqn{(max x + min x) / 2}
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @inheritParams apply_coord_fn_
#' @param na.rm Whether to ignore missing values when calculating
#'  \code{min} and \code{max} values. (Logical)
#' @family coordinate functions
#' @export
#' @return Either a \code{vector} with the midrange of each supplied \code{vector}
#'  or a \code{data.frame} with the midrange of each supplied column along with any grouping variables.
#' @examples
#' # Attach packages
#' library(rearrr)
#' library(dplyr)
#'
#' # Set seed
#' set.seed(1)
#'
#' # Create three vectors
#' x <- runif(10)
#' y <- runif(10)
#' z <- runif(10)
#'
#' # Find midrange for each vector
#' midrange(x, y, z)
#'
#' #
#' # For data.frames
#' #
#'
#' # Create data frame
#' df <- data.frame(
#'   "x" = x,
#'   "y" = y,
#'   "z" = z,
#'   "g" = rep(1:2, each = 5)
#' )
#'
#' # Find midrange for each column
#' midrange(df, cols = c("x", "y", "z"))
#'
#' # When 'df' is grouped
#' df %>%
#'   dplyr::group_by(g) %>%
#'   midrange(cols = c("x", "y", "z"))
midrange <- function(..., cols = NULL, na.rm = FALSE) {
  # Midrange coordinate function
  midrange_coord_fn <- create_origin_fn(function(x) {
    (max(x, na.rm = na.rm) + min(x, na.rm = na.rm)) / 2
  })

  # Apply midrange function
  apply_coord_fn_(
    ...,
    cols = cols,
    coord_fn = midrange_coord_fn,
    fn_name = "midrange_fn",
    coordinate_name = "midrange"
  )
}
