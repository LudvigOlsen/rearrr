

#   __________________ #< 9d281822e7dafc7afc466a38db278b1e ># __________________
#   Centroid                                                                ####


#' @title Find the coordinates for the centroid
#' @description
#'  \Sexpr[results=rd, stage=render]{lifecycle::badge("experimental")}
#'
#'  Calculates the mean of each passed \code{vector}/\code{column}.
#' @author Ludvig Renbo Olsen, \email{r-pkgs@@ludvigolsen.dk}
#' @inheritParams apply_coord_fn_
#' @param na.rm Whether to ignore missing values when calculating means. (Logical)
#' @family coordinate functions
#' @export
#' @return Means of the supplied \code{vectors}/\code{columns}. Either as a \code{vector} or a \code{data.frame}.
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
#' # Find centroid coordinates
#' # Aka. the means of each vector
#' centroid(x, y, z)
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
#' # Find centroid coordinates
#' # Aka. the means of each column
#' centroid(df, cols = c("x", "y", "z"))
#'
#' # When 'df' is grouped
#' df %>%
#'   dplyr::group_by(g) %>%
#'   centroid(cols = c("x", "y", "z"))
centroid <- function(..., cols = NULL, na.rm = FALSE) {
  # Apply centroid function
  apply_coord_fn_(
    ...,
    cols = cols,
    coord_fn = create_origin_fn(mean, na.rm = na.rm),
    fn_name = "centroid_fn",
    coordinate_name = "centroid"
  )
}
